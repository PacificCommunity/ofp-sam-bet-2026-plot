#!/usr/bin/env bash
set -euo pipefail

OUT_DIR="${OUTPUT_DIR:-outputs}"
INPUT_DIR="${INPUT_DIR:-inputs}"
ROOT="$(pwd)"

runtime_package_specs() {
  printf "%s" "${KFLOW_REPO_RUNTIME_PACKAGES:-${KFLOW_RUNTIME_PACKAGES:-}}"
}

runtime_update_mode() {
  printf "%s" "${KFLOW_REPO_RUNTIME_UPDATE:-${TUNA_FLOW_RUNTIME_UPDATE:-${KFLOW_RUNTIME_UPDATE:-auto}}}"
}

runtime_packages_disabled() {
  case "$(runtime_package_specs)" in
    ""|0|false|FALSE|no|NO|off|OFF|none|NONE|skip|SKIP) return 0 ;;
    *) return 1 ;;
  esac
}

runtime_updates_disabled() {
  case "$(runtime_update_mode)" in
    ""|0|false|FALSE|no|NO|off|OFF|none|NONE|skip|SKIP|never|NEVER) return 0 ;;
    *) return 1 ;;
  esac
}

runtime_updates_direct() {
  case "$(runtime_update_mode)" in
    auto|AUTO|always|ALWAYS|token|TOKEN|direct|DIRECT|url|URL|download|DOWNLOAD) return 0 ;;
    *) return 1 ;;
  esac
}

ensure_runtime_library() {
  local preferred="${R_LIBS_USER:-${KFLOW_RUNTIME_LIBRARY:-}}"
  local fallback="${ROOT}/.R-library"
  if [[ -z "$preferred" ]]; then
    preferred="$fallback"
  fi
  if mkdir -p "$preferred" 2>/dev/null && [[ -w "$preferred" ]]; then
    export R_LIBS_USER="$preferred"
  else
    export R_LIBS_USER="$fallback"
    mkdir -p "$R_LIBS_USER"
  fi
  export KFLOW_RUNTIME_LIBRARY="$R_LIBS_USER"
  export KFLOW_RUNTIME_STATE_DIR="${KFLOW_RUNTIME_STATE_DIR:-${ROOT}/.kflow-runtime-cache}"
  mkdir -p "$KFLOW_RUNTIME_STATE_DIR" 2>/dev/null || true
}

runtime_private_packages_required() {
  case "${KFLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES:-false}" in
    1|true|TRUE|yes|YES|on|ON) return 0 ;;
    *) return 1 ;;
  esac
}

drop_runtime_tokens() {
  unset GIT_PAT GITHUB_PAT GITHUB_TOKEN GH_TOKEN KFLOW_GITHUB_TOKEN KFLOW_PERSONAL_TOKEN
}

install_missing_runtime_packages() {
  runtime_packages_disabled && return 0
  runtime_updates_disabled && return 0
  ensure_runtime_library
  Rscript - <<'RS'
truthy <- function(value) tolower(value) %in% c("1", "true", "yes", "y", "on", "always")
spec_text <- Sys.getenv("KFLOW_REPO_RUNTIME_PACKAGES", Sys.getenv("KFLOW_RUNTIME_PACKAGES", ""))
parts <- trimws(strsplit(spec_text, ",", fixed = TRUE)[[1]])
parts <- parts[nzchar(parts) & grepl("=", parts, fixed = TRUE)]
if (!length(parts)) quit(save = "no", status = 0)
specs <- lapply(parts, function(part) {
  eq <- regexpr("=", part, fixed = TRUE)[1]
  package <- trimws(substr(part, 1, eq - 1))
  repo_ref <- trimws(substr(part, eq + 1, nchar(part)))
  at <- regexpr("@", repo_ref, fixed = TRUE)[1]
  if (at > 0) {
    repo <- substr(repo_ref, 1, at - 1)
    ref <- substr(repo_ref, at + 1, nchar(repo_ref))
  } else {
    repo <- repo_ref
    ref <- "main"
  }
  list(package = package, repo = repo, ref = ref)
})
lib <- Sys.getenv("R_LIBS_USER", "")
if (!nzchar(lib)) quit(save = "no", status = 43)
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(unique(c(lib, .libPaths())))
desc_field <- function(desc, name) {
  value <- tryCatch(desc[[name]], error = function(e) "")
  if (is.null(value) || !length(value) || is.na(value[[1L]])) "" else as.character(value[[1L]])
}
installed_desc <- function(package) {
  desc <- tryCatch(
    suppressWarnings(utils::packageDescription(package, lib.loc = lib)),
    error = function(e) NULL
  )
  if (length(desc) == 1L && is.na(desc[[1L]])) NULL else desc
}
needs_install <- function(spec) {
  desc <- installed_desc(spec$package)
  if (is.null(desc)) return(TRUE)
  installed_sha <- desc_field(desc, "RemoteSha")
  ref_is_sha <- grepl("^[0-9a-f]{7,40}$", spec$ref, ignore.case = TRUE)
  if (ref_is_sha) {
    return(!nzchar(installed_sha) || !startsWith(tolower(installed_sha), tolower(spec$ref)))
  }
  TRUE
}
missing <- specs[vapply(specs, needs_install, logical(1))]
if (!length(missing)) quit(save = "no", status = 0)
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("remotes", quietly = TRUE)) {
  utils::install.packages("remotes", lib = lib, dependencies = TRUE, repos = getOption("repos"))
}
token <- ""
token_name <- ""
for (name in c("GITHUB_PAT", "GIT_PAT", "GITHUB_TOKEN", "GH_TOKEN", "KFLOW_GITHUB_TOKEN", "KFLOW_PERSONAL_TOKEN")) {
  value <- Sys.getenv(name, "")
  if (nzchar(value)) {
    token <- value
    token_name <- name
    break
  }
}
if (nzchar(token_name)) {
  message("[kflow-runtime-update] Runtime GitHub access has token from ", token_name, ".")
} else {
  message("[kflow-runtime-update] Runtime GitHub access has no token.")
}
download_github_archive <- function(repo, ref) {
  archive <- tempfile(pattern = "kflow-runtime-", fileext = ".tar.gz")
  url <- if (nzchar(token)) {
    sprintf("https://api.github.com/repos/%s/tarball/%s", repo, ref)
  } else {
    sprintf("https://codeload.github.com/%s/tar.gz/%s", repo, ref)
  }
  curl <- Sys.which("curl")
  if (nzchar(curl)) {
    args <- c("-sSL", "--retry", "3", "--retry-delay", "2", "-w", "%{http_code}", "-o", archive)
    if (nzchar(token)) {
      args <- c(
        "-H", paste("Authorization: Bearer", token),
        "-H", "Accept: application/vnd.github+json",
        args
      )
    }
    output <- tryCatch(suppressWarnings(system2(curl, c(args, url), stdout = TRUE, stderr = TRUE)),
      error = function(e) structure(conditionMessage(e), status = 1L))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    code <- tail(grep("^[0-9]{3}$", as.character(output), value = TRUE), 1)
    if (!identical(as.integer(status), 0L) || !length(code) || !grepl("^2", code)) {
      stop("download failed from ", url, " (curl exit ", status, ", http ", ifelse(length(code), code, "unknown"), ")")
    }
  } else {
    headers <- if (nzchar(token)) c(Authorization = paste("Bearer", token)) else NULL
    status <- utils::download.file(url, archive, mode = "wb", quiet = TRUE, method = "libcurl", headers = headers)
    if (!identical(status, 0L)) {
      stop("download failed from ", url)
    }
  }
  archive
}
clone_github_source <- function(repo, ref) {
  git <- Sys.which("git")
  if (!nzchar(git)) {
    stop("git is required for runtime package source fallback", call. = FALSE)
  }
  source_dir <- tempfile(pattern = "kflow-runtime-src-")
  unlink(source_dir, recursive = TRUE, force = TRUE)
  git_url <- sprintf("https://github.com/%s.git", repo)
  askpass <- ""
  if (nzchar(token)) {
    askpass <- tempfile(pattern = "kflow-git-askpass-")
    writeLines(c(
      "#!/bin/sh",
      "case \"$1\" in",
      "  *Username*) printf '%s\\n' x-access-token ;;",
      "  *) printf '%s\\n' \"$KFLOW_GIT_ASKPASS_TOKEN\" ;;",
      "esac"
    ), askpass)
    Sys.chmod(askpass, mode = "0700")
    on.exit(unlink(askpass), add = TRUE)
  }
  run_git <- function(args) {
    env <- character()
    if (nzchar(askpass)) {
      env <- c(
        paste0("GIT_ASKPASS=", askpass),
        "GIT_TERMINAL_PROMPT=0",
        paste0("KFLOW_GIT_ASKPASS_TOKEN=", token)
      )
    }
    status <- system2(git, args, env = env, stdout = FALSE, stderr = FALSE)
    identical(as.integer(status), 0L)
  }
  if (!run_git(c("clone", "--quiet", "--depth", "50", git_url, source_dir))) {
    stop("git clone failed for ", repo, call. = FALSE)
  }
  if (!run_git(c("-C", source_dir, "checkout", "--quiet", ref))) {
    if (!run_git(c("-C", source_dir, "fetch", "--quiet", "--depth", "1", "origin", ref)) ||
        !run_git(c("-C", source_dir, "checkout", "--quiet", "FETCH_HEAD"))) {
      stop("git checkout failed for ", repo, "@", ref, call. = FALSE)
    }
  }
  resolved_sha <- tryCatch(
    trimws(system2(git, c("-C", source_dir, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[[1]]),
    error = function(e) ""
  )
  if (nzchar(resolved_sha)) {
    attr(source_dir, "kflow_resolved_sha") <- resolved_sha
  }
  source_dir
}
install_local_source <- function(path, lib) {
  r_bin <- file.path(R.home("bin"), "R")
  output <- tryCatch(
    system2(
      r_bin,
      c("CMD", "INSTALL", "-l", lib, path),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (!identical(as.integer(status), 0L)) {
    detail <- paste(tail(as.character(output), 30), collapse = "\n")
    stop("R CMD INSTALL failed for ", basename(path), "\n", detail, call. = FALSE)
  }
}
write_remote_metadata <- function(package, repo, ref, sha, lib) {
  desc_path <- system.file("DESCRIPTION", package = package, lib.loc = lib)
  if (!nzchar(desc_path) || !file.exists(desc_path)) return(invisible(FALSE))
  repo_parts <- strsplit(repo, "/", fixed = TRUE)[[1]]
  remote <- list(
    RemoteType = "github",
    RemoteHost = "api.github.com",
    RemoteUsername = repo_parts[[1]],
    RemoteRepo = repo_parts[[length(repo_parts)]],
    RemoteRef = ref,
    RemoteSha = sha
  )
  fields <- as.list(read.dcf(desc_path)[1, ])
  fields[names(remote)] <- remote
  write.dcf(as.data.frame(fields, stringsAsFactors = FALSE, check.names = FALSE), desc_path)
  meta_path <- system.file("Meta", "package.rds", package = package, lib.loc = lib)
  if (file.exists(meta_path)) {
    meta <- readRDS(meta_path)
    meta[["DESCRIPTION"]][names(remote)] <- unlist(remote)
    saveRDS(meta, meta_path)
  }
  invisible(TRUE)
}
for (spec in missing) {
  message("[kflow-runtime-update] Installing/updating runtime package ", spec$package, " from ", spec$repo, "@", spec$ref, ".")
  err <- tryCatch({
    ref_is_sha <- grepl("^[0-9a-f]{7,40}$", spec$ref, ignore.case = TRUE)
    source_path <- if (ref_is_sha) {
      tryCatch(
        download_github_archive(spec$repo, spec$ref),
        error = function(err) {
          message("[kflow-runtime-update] Runtime archive download failed for ", spec$package,
                  "; trying git clone fallback.")
          clone_github_source(spec$repo, spec$ref)
        }
      )
    } else {
      clone_github_source(spec$repo, spec$ref)
    }
    resolved_sha <- attr(source_path, "kflow_resolved_sha", exact = TRUE)
    if (is.null(resolved_sha) || !nzchar(resolved_sha)) {
      resolved_sha <- spec$ref
    }
    if (!identical(resolved_sha, spec$ref)) {
      message("[kflow-runtime-update] Resolved ", spec$repo, "@", spec$ref, " to ", substr(resolved_sha, 1, 12), ".")
    }
    on.exit(unlink(source_path, recursive = TRUE, force = TRUE), add = TRUE)
    install_local_source(source_path, lib)
    write_remote_metadata(spec$package, spec$repo, spec$ref, resolved_sha, lib)
    message("[kflow-runtime-update] Installed ", spec$package, " at ", substr(resolved_sha, 1, 12), ".")
    NULL
  }, error = function(e) e)
  if (inherits(err, "error")) {
    message("[kflow-runtime-update] Runtime package install failed for ", spec$package, ": ", conditionMessage(err))
  }
}
missing_after <- specs[!vapply(specs, function(spec) requireNamespace(spec$package, quietly = TRUE), logical(1))]
if (length(missing_after) && truthy(Sys.getenv("KFLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES", "false"))) {
  message("[kflow-runtime-update] Required runtime package(s) unavailable: ",
          paste(vapply(missing_after, function(spec) spec$package, character(1)), collapse = ", "))
  quit(save = "no", status = 44)
}
quit(save = "no", status = 0)
RS
}

prepare_runtime_packages() {
  runtime_packages_disabled && return 0
  ensure_runtime_library
  if runtime_updates_direct; then
    install_missing_runtime_packages
    drop_runtime_tokens
    return 0
  fi
  if [[ -x /usr/local/bin/30-update-kflow-runtime-packages ]]; then
    if bash /usr/local/bin/30-update-kflow-runtime-packages; then
      :
    else
      update_status=$?
      if runtime_private_packages_required || [[ "$update_status" -eq 42 || "$update_status" -eq 43 ]]; then
        exit "$update_status"
      fi
      echo "[kflow-runtime-update] Runtime package update failed; continuing with bundled packages." >&2
    fi
  else
    echo "[kflow-runtime-update] Runtime updater not found; using bundled packages." >&2
  fi
  install_missing_runtime_packages
  drop_runtime_tokens
}

mkdir -p "${OUT_DIR}" "${INPUT_DIR}"

echo "BET results task"
echo "Input directory: ${INPUT_DIR}"
echo "Output directory: ${OUT_DIR}"

prepare_runtime_packages
Rscript R/build_plots.R
