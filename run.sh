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
    direct|DIRECT|url|URL|download|DOWNLOAD) return 0 ;;
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
  installed_ref <- desc_field(desc, "RemoteRef")
  ref_is_sha <- grepl("^[0-9a-f]{7,40}$", spec$ref, ignore.case = TRUE)
  if (ref_is_sha) {
    return(!nzchar(installed_sha) || !startsWith(tolower(installed_sha), tolower(spec$ref)))
  }
  !identical(installed_ref, spec$ref)
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
  message("[kflow-runtime-update] Runtime archive download has GitHub token from ", token_name, ".")
} else {
  message("[kflow-runtime-update] Runtime archive download has no GitHub token.")
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
    args <- c("-fsSL", "--retry", "3", "--retry-delay", "2", "-w", "%{http_code}", "-o", archive)
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
    if (!identical(as.integer(status), 0L)) {
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
for (spec in missing) {
  message("[kflow-runtime-update] Installing missing runtime package ", spec$package, " from ", spec$repo, "@", spec$ref, ".")
  err <- tryCatch({
    archive <- download_github_archive(spec$repo, spec$ref)
    on.exit(unlink(archive), add = TRUE)
    remotes::install_local(
      archive,
      lib = lib,
      upgrade = "never",
      force = TRUE,
      quiet = TRUE
    )
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
