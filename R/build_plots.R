`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

truthy_env <- function(name, default = TRUE) {
  value <- env(name, if (isTRUE(default)) "true" else "false")
  tolower(value) %in% c("1", "true", "yes", "y", "on", "always")
}

first_text <- function(x, default = "") {
  value <- tryCatch(as.character(x), error = function(e) character())
  if (!length(value) || is.na(value[[1L]]) || !nzchar(value[[1L]])) default else value[[1L]]
}

bind_rows_fill <- function(rows) {
  rows <- rows[vapply(rows, function(x) is.data.frame(x) && nrow(x), logical(1))]
  if (!length(rows)) return(data.frame(stringsAsFactors = FALSE))
  cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    missing <- setdiff(cols, names(x))
    for (name in missing) x[[name]] <- NA
    x[, cols, drop = FALSE]
  })
  do.call(rbind, rows)
}

call_with_supported_args <- function(fun, args) {
  supported <- names(formals(fun))
  if (!"..." %in% supported) {
    args <- args[names(args) %in% supported]
  }
  do.call(fun, args)
}

payload_label <- function(payload, fallback) {
  reg <- tryCatch(payload$data$info$registry, error = function(e) NULL)
  info <- tryCatch(payload$data$info, error = function(e) NULL)
  for (name in c("plot_label", "model_label", "model_token", "job_key")) {
    value <- first_text(tryCatch(reg[[name]], error = function(e) NULL))
    if (nzchar(value)) return(value)
  }
  for (name in c("plot_label", "model_label", "model_token", "job_key")) {
    value <- first_text(tryCatch(info[[name]], error = function(e) NULL))
    if (nzchar(value)) return(value)
  }
  fallback
}

payload_manifest <- function(folder) {
  json_file <- file.path(folder, "model_payload_manifest.json")
  csv_file <- file.path(folder, "model_payload_manifest.csv")
  if (file.exists(json_file) && requireNamespace("jsonlite", quietly = TRUE)) {
    out <- tryCatch(jsonlite::read_json(json_file, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(out)) return(as.data.frame(out, stringsAsFactors = FALSE))
  }
  if (file.exists(csv_file)) {
    return(tryCatch(utils::read.csv(csv_file, stringsAsFactors = FALSE), error = function(e) NULL))
  }
  NULL
}

payload_label_from_manifest <- function(manifest, fallback) {
  if (!is.data.frame(manifest) || !nrow(manifest)) return(fallback)
  for (name in c("model_label", "plot_label", "model_token", "job_key")) {
    if (!name %in% names(manifest)) next
    value <- first_text(manifest[[name]][[1]], "")
    if (nzchar(value)) return(value)
  }
  fallback
}

payload_path_parts <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts[nzchar(parts)]
}

payload_diagnostic_anchor <- function(folder) {
  parts <- payload_path_parts(folder)
  n <- length(parts)
  if (n < 2L) return(NA_integer_)
  for (i in seq_len(n - 1L)) {
    current <- parts[[i]]
    next_part <- parts[[i + 1L]]
    if (identical(current, "jitter") && grepl("^jitter_seed_[0-9]+$", next_part)) return(i)
    if (identical(current, "retro") && grepl("^peel_[0-9]+$", next_part)) return(i)
    if (identical(current, "hessian") && grepl("^part_[0-9]+$", next_part)) return(i)
    if (identical(current, "profile") && i + 2L <= n && grepl("^scalar_", parts[[i + 2L]])) return(i)
    if (identical(current, "selftest") && next_part %in% c("refit", "sim", "truth", "truth_eval", "inputs", "recovery")) return(i)
  }
  NA_integer_
}

payload_is_diagnostic_output_folder <- function(folder) {
  is.finite(payload_diagnostic_anchor(folder))
}

payload_has_attached_checks <- function(payload) {
  attached <- tryCatch(payload$data$info$attached_checks, error = function(e) NULL)
  is.list(attached) && length(attached) > 0
}

payload_prefer_main_rows <- function(rows) {
  rows <- bind_rows_fill(rows)
  if (!nrow(rows)) return(rows)

  rows$path_depth <- suppressWarnings(as.integer(rows$path_depth))
  rows$has_attached_checks <- as.logical(rows$has_attached_checks)
  rows$has_attached_checks[is.na(rows$has_attached_checks)] <- FALSE

  out <- lapply(split(seq_len(nrow(rows)), rows$model_label), function(idx) {
    group <- rows[idx, , drop = FALSE]
    # For duplicate labels, keep the richer attached-check payload first, then
    # the shallower path. This avoids counting nested check payloads as models.
    group <- group[order(!group$has_attached_checks, group$path_depth, group$payload_file), , drop = FALSE]
    group[1L, , drop = FALSE]
  })
  out <- bind_rows_fill(out)
  out <- out[order(out$model_label, out$path_depth, out$payload_file), , drop = FALSE]
  out[, setdiff(names(out), c("has_attached_checks", "path_depth")), drop = FALSE]
}

payloads <- function(input_dir) {
  files <- list.files(input_dir, pattern = "^model_payload[.]rds$", recursive = TRUE, full.names = TRUE)
  files <- files[!vapply(dirname(files), payload_is_diagnostic_output_folder, logical(1))]
  rows <- lapply(files, function(file) {
    folder <- dirname(file)
    payload <- tryCatch(readRDS(file), error = function(e) NULL)
    if (is.null(payload)) return(NULL)
    manifest <- payload_manifest(folder)
    label <- payload_label_from_manifest(manifest, basename(folder))
    if (identical(label, basename(folder))) {
      label <- payload_label(payload, basename(folder))
    }
    data.frame(
      model_label = label,
      model_folder = normalizePath(folder, winslash = "/", mustWork = FALSE),
      payload_file = normalizePath(file, winslash = "/", mustWork = FALSE),
      manifest_file = normalizePath(file.path(folder, "model_payload_manifest.json"), winslash = "/", mustWork = FALSE),
      has_attached_checks = payload_has_attached_checks(payload),
      path_depth = length(payload_path_parts(folder)),
      stringsAsFactors = FALSE
    )
  })
  payload_prefer_main_rows(rows)
}

find_report_selection <- function(input_dir) {
  explicit <- env("PLOT_REPORT_SELECTION", env("MFCLSHINY_REPORT_SELECTION_FILE", ""))
  candidates <- character()
  if (nzchar(explicit)) candidates <- c(candidates, explicit)
  candidates <- c(
    candidates,
    file.path(getwd(), "report-selection.json"),
    file.path(getwd(), "config", "report-selection.json")
  )
  if (dir.exists(input_dir)) {
    candidates <- c(
      candidates,
      list.files(input_dir, pattern = "^report-selection[.]json$", recursive = TRUE, full.names = TRUE)
    )
  }
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  candidates[file.exists(candidates)][1] %||% ""
}

write_payload_index <- function(payload_index, output_dir) {
  index_dir <- file.path(output_dir, "indices")
  dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(payload_index, file.path(index_dir, "payload-index.csv"), row.names = FALSE)
  invisible(payload_index)
}

write_plot_summary <- function(result, payload_index, output_dir) {
  figure_index <- result$figures %||% data.frame()
  table_index <- result$tables %||% data.frame()
  summary <- data.frame(
    payloads = nrow(payload_index),
    figures = if (nrow(figure_index)) length(unique(figure_index$figure)) else 0L,
    figure_files = nrow(figure_index),
    tables = if (nrow(table_index)) length(unique(table_index$table)) else 0L,
    table_files = nrow(table_index),
    build_errors = if (is.data.frame(result$log)) sum(result$log$status == "error", na.rm = TRUE) else NA_integer_,
    html = file.exists(file.path(output_dir, "review", "plot-report.html")),
    source = "mfclshiny_shiny_registry",
    stringsAsFactors = FALSE
  )
  index_dir <- file.path(output_dir, "indices")
  dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary, file.path(index_dir, "plot-summary.csv"), row.names = FALSE)
  invisible(summary)
}

polish_output_caption <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\s+", " ", x)
  x <- gsub(
    "\\s+(for|in|from|of|used in) the [0-9]{4}\\s+bigeye tuna\\s*(\\(BET\\))?\\s+assessment\\b",
    "",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  x <- gsub(
    "^the [0-9]{4}\\s+bigeye tuna\\s*(\\(BET\\))?\\s+assessment\\s+",
    "",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  x <- gsub(
    "\\bthe [0-9]{4}\\s+bigeye tuna\\s*(\\(BET\\))?\\s+assessment\\b",
    "",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  x <- gsub("\\s+([.,;:])", "\\1", x, perl = TRUE)
  x <- trimws(gsub("\\s+", " ", x))
  ifelse(nzchar(x), paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))), x)
}

polish_output_metadata <- function(x) {
  if (!is.data.frame(x) || !nrow(x)) return(x)
  text_cols <- unique(c(
    grep("caption", names(x), ignore.case = TRUE, value = TRUE),
    intersect(c("alt_text", "description"), names(x))
  ))
  for (col in text_cols) {
    x[[col]] <- polish_output_caption(x[[col]])
  }
  x
}

report_slug <- function(x, fallback = "item") {
  x <- tolower(trimws(as.character(x %||% fallback)))
  x <- gsub("[^a-z0-9]+", "-", x, perl = TRUE)
  x <- gsub("(^-+|-+$)", "", x, perl = TRUE)
  if (!nzchar(x)) fallback else x
}

markdown_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("\\\\", "\\\\\\\\", x, perl = TRUE)
  x <- gsub("([\\[\\]\\(\\)])", "\\\\\\1", x, perl = TRUE)
  x
}

qmd_option_escape <- function(x) {
  x <- as.character(x %||% "")
  gsub('"', '\\"', x, fixed = TRUE)
}

html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

index_value <- function(row, name, default = "") {
  if (!is.data.frame(row) || !nrow(row) || !name %in% names(row)) return(default)
  value <- as.character(row[[name]][[1L]] %||% default)
  if (is.na(value) || !nzchar(value)) default else value
}

ensure_index_columns <- function(index, id_col) {
  index <- as.data.frame(index %||% data.frame(), stringsAsFactors = FALSE)
  needed <- c(id_col, "relative_path", "file", "label", "caption", "description", "format", "placement", "status")
  for (name in needed) {
    if (!name %in% names(index)) index[[name]] <- ""
  }
  if (!nrow(index)) return(index)
  if (!any(nzchar(as.character(index$relative_path)))) {
    index$relative_path <- as.character(index$file %||% "")
  }
  index$format <- tolower(as.character(index$format %||% ""))
  missing_format <- !nzchar(index$format)
  if (any(missing_format)) {
    index$format[missing_format] <- tolower(tools::file_ext(index$relative_path[missing_format]))
  }
  index
}

appendix_figure <- function(rows) {
  text <- paste(
    unique(tolower(c(
      as.character(rows$figure %||% ""),
      as.character(rows$label %||% ""),
      as.character(rows$description %||% "")
    ))),
    collapse = " "
  )
  grepl("length|weight|frequency|residual|fit by fishery|fishery[. -]*[0-9]|selectivity", text, perl = TRUE)
}

excluded_report_figure <- function(rows) {
  text <- paste(
    unique(tolower(c(
      as.character(rows$figure %||% ""),
      as.character(rows$label %||% ""),
      as.character(rows$description %||% ""),
      as.character(rows$caption %||% "")
    ))),
    collapse = " "
  )
  ids <- unique(report_slug(as.character(rows$figure %||% ""), "figure"))
  any(ids %in% c(
    "tag-recapture-pressure",
    "tag-recapture-pressure-by-fishery",
    "tag-recapture-pressure-release-group",
    "tag-recapture-pressure-release-group-by-fishery"
  )) || grepl("observed[- ]to[- ]expected recapture pressure|recapture pressure by release group", text, perl = TRUE)
}

all_figure_ids <- function(figure_index) {
  figure_ids <- unique(as.character(figure_index$figure %||% ""))
  figure_ids[nzchar(figure_ids) & !is.na(figure_ids)]
}

report_figure_ids <- function(figure_index) {
  figure_ids <- all_figure_ids(figure_index)
  keep <- vapply(figure_ids, function(id) {
    rows <- figure_index[as.character(figure_index$figure) == id, , drop = FALSE]
    placement <- tolower(as.character(rows$placement %||% ""))
    !all(placement == "exclude", na.rm = TRUE) && !excluded_report_figure(rows)
  }, logical(1))
  figure_ids[keep]
}

report_item_placement <- function(rows, fallback_appendix = FALSE) {
  placement <- tolower(as.character(rows$placement %||% ""))
  placement <- placement[placement %in% c("main", "appendix", "exclude", "auto")]
  placement <- placement[nzchar(placement)]
  if (length(placement) && any(placement == "exclude")) return("exclude")
  if (length(placement) && any(placement == "main")) return("main")
  if (length(placement) && any(placement == "appendix")) return("appendix")
  if (isTRUE(fallback_appendix)) "appendix" else "main"
}

overview_placement <- function(rows) {
  if (excluded_report_figure(rows)) return("Excluded")
  placement <- report_item_placement(rows, fallback_appendix = appendix_figure(rows))
  if (identical(placement, "exclude")) return("Excluded")
  if (identical(placement, "appendix")) "Appendix" else "Main"
}

preferred_figure_row <- function(rows, output_dir) {
  rows <- rows[nzchar(as.character(rows$relative_path %||% "")), , drop = FALSE]
  if (!nrow(rows)) return(data.frame(stringsAsFactors = FALSE))
  rows$format <- tolower(as.character(rows$format %||% ""))
  png_rows <- rows[rows$format == "png", , drop = FALSE]
  row <- if (nrow(png_rows)) png_rows[1L, , drop = FALSE] else rows[1L, , drop = FALSE]
  rel <- index_value(row, "relative_path", index_value(row, "file", ""))
  if (grepl("[.][Pp][Nn][Gg]$", rel)) {
    jpg_rel <- sub("[.][Pp][Nn][Gg]$", ".jpg", rel)
    if (file.exists(file.path(output_dir, jpg_rel))) {
      row$relative_path <- jpg_rel
      row$file <- jpg_rel
      row$format <- "jpg"
    }
  }
  row
}

report_path <- function(relative_path, prefix = env("REPORT_READY_PATH_PREFIX", "generated/outputs")) {
  relative_path <- gsub("^/+", "", as.character(relative_path %||% ""), perl = TRUE)
  prefix <- gsub("/+$", "", as.character(prefix %||% "generated/outputs"), perl = TRUE)
  if (!nzchar(prefix)) relative_path else file.path(prefix, relative_path)
}

write_report_ready_figures_qmd <- function(figure_index, output_dir, ready_dir) {
  figure_index <- ensure_index_columns(figure_index, "figure")
  figure_ids <- report_figure_ids(figure_index)
  groups <- list(main = character(), appendix = character())
  for (id in figure_ids) {
    rows <- figure_index[as.character(figure_index$figure) == id, , drop = FALSE]
    placement <- report_item_placement(rows, fallback_appendix = appendix_figure(rows))
    if (identical(placement, "exclude")) next
    if (identical(placement, "appendix")) groups$appendix <- c(groups$appendix, id) else groups$main <- c(groups$main, id)
  }

  lines <- c(
    "\\clearpage",
    "",
    "# Figures",
    "",
    "<!--",
    "Auto-generated by the BET results task. Edit this file in the report repository to choose, remove, reorder, or rewrite figures.",
    "If this section file is deleted, the next report job will seed a fresh copy from generated/outputs/report-ready/figures.qmd.",
    "-->",
    ""
  )
  add_figure <- function(lines, id) {
    rows <- figure_index[as.character(figure_index$figure) == id, , drop = FALSE]
    row <- preferred_figure_row(rows, output_dir)
    if (!nrow(row)) return(lines)
    label <- index_value(row, "label", id)
    caption <- index_value(row, "caption", label)
    rel <- report_path(index_value(row, "relative_path", index_value(row, "file", "")))
    fig_id <- paste0("fig-", report_slug(id, "figure"))
    lines <- c(
      lines,
      paste0("### ", label),
      "",
      paste0("<!-- figure: ", id, " -->"),
      "\\vspace*{0.04\\textheight}",
      "",
      paste0("![", markdown_escape(caption), "](", rel, "){#", fig_id, " fig-align=\"center\" width=100%}"),
      "",
      "\\FloatBarrier",
      "\\clearpage",
      ""
    )
    lines
  }
  if (length(groups$main)) {
    lines <- c(lines, "## Main figure set", "")
    for (id in groups$main) lines <- add_figure(lines, id)
  }
  if (length(groups$appendix)) {
    lines <- c(lines, "## Appendix figure set", "")
    for (id in groups$appendix) lines <- add_figure(lines, id)
  }
  if (!length(figure_ids)) {
    lines <- c(lines, "No report-ready figures were produced.", "")
  }
  writeLines(lines, file.path(ready_dir, "figures.qmd"))
  invisible(file.path(ready_dir, "figures.qmd"))
}

write_report_ready_tables_qmd <- function(table_index, ready_dir) {
  table_index <- ensure_index_columns(table_index, "table")
  table_ids <- unique(as.character(table_index$table %||% ""))
  table_ids <- table_ids[nzchar(table_ids) & !is.na(table_ids)]
  table_ids <- table_ids[vapply(table_ids, function(id) {
    rows <- table_index[as.character(table_index$table) == id, , drop = FALSE]
    !identical(report_item_placement(rows), "exclude")
  }, logical(1))]
  lines <- c(
    "# Tables",
    "",
    "<!--",
    "Auto-generated by the BET results task. Edit this file in the report repository to choose, remove, reorder, or rewrite tables.",
    "If this section file is deleted, the next report job will seed a fresh copy from generated/outputs/report-ready/tables.qmd.",
    "-->",
    ""
  )
  if (!length(table_ids)) {
    lines <- c(lines, "No report-ready tables were produced.", "")
  }
  for (id in table_ids) {
    rows <- table_index[as.character(table_index$table) == id, , drop = FALSE]
    row <- rows[1L, , drop = FALSE]
    label <- index_value(row, "label", id)
    caption <- index_value(row, "caption", label)
    rel <- report_path(index_value(row, "relative_path", index_value(row, "file", "")))
    tbl_id <- paste0("tbl-", report_slug(id, "table"))
    chunk <- paste0("tbl_", gsub("-", "_", report_slug(id, "table")))
    lines <- c(
      lines,
      paste0("## ", label),
      "",
      paste0("<!-- table: ", id, " -->"),
      paste0("```{r ", chunk, "}"),
      paste0("#| label: ", tbl_id),
      paste0("#| tbl-cap: \"", qmd_option_escape(caption), "\""),
      "#| echo: false",
      "#| warning: false",
      "#| message: false",
      paste0("table_file <- \"", qmd_option_escape(rel), "\""),
      "if (file.exists(table_file)) {",
      "  table_data <- utils::read.csv(table_file, check.names = FALSE)",
      "  knitr::kable(table_data)",
      "} else {",
      "  knitr::asis_output(paste0(\"Missing table file: `\", table_file, \"`\"))",
      "}",
      "```",
      ""
    )
  }
  writeLines(lines, file.path(ready_dir, "tables.qmd"))
  invisible(file.path(ready_dir, "tables.qmd"))
}

write_report_ready_map <- function(figure_index, table_index, output_dir, overview_dir) {
  figure_index <- ensure_index_columns(figure_index, "figure")
  table_index <- ensure_index_columns(table_index, "table")
  figure_ids <- all_figure_ids(figure_index)
  table_ids <- unique(as.character(table_index$table %||% ""))
  table_ids <- table_ids[nzchar(table_ids) & !is.na(table_ids)]
  figure_cards <- character()
  for (id in figure_ids) {
    rows <- figure_index[as.character(figure_index$figure) == id, , drop = FALSE]
    row <- preferred_figure_row(rows, output_dir)
    if (!nrow(row)) next
    rel <- index_value(row, "relative_path", index_value(row, "file", ""))
    preview <- paste0("../", rel)
    label <- index_value(row, "label", id)
    caption <- index_value(row, "caption", label)
    placement <- overview_placement(rows)
    marker <- paste0("<!-- figure: ", id, " -->")
    figure_cards <- c(
      figure_cards,
      paste0(
        "<article class=\"card\"><div class=\"thumb\"><img src=\"", html_escape(preview), "\" alt=\"\"></div>",
        "<div class=\"meta\"><span>", html_escape(placement), "</span><code>", html_escape(id), "</code></div>",
        "<h3>", html_escape(label), "</h3>",
        "<p>", html_escape(caption), "</p>",
        "<div class=\"links\"><a href=\"../report-ready/figures.qmd\">figures.qmd</a><code>", html_escape(marker), "</code></div></article>"
      )
    )
  }
  table_cards <- character()
  for (id in table_ids) {
    rows <- table_index[as.character(table_index$table) == id, , drop = FALSE]
    row <- rows[1L, , drop = FALSE]
    label <- index_value(row, "label", id)
    caption <- index_value(row, "caption", label)
    rel <- index_value(row, "relative_path", index_value(row, "file", ""))
    marker <- paste0("<!-- table: ", id, " -->")
    table_cards <- c(
      table_cards,
      paste0(
        "<article class=\"card table-card\"><div class=\"meta\"><span>Table</span><code>", html_escape(id), "</code></div>",
        "<h3>", html_escape(label), "</h3>",
        "<p>", html_escape(caption), "</p>",
        "<div class=\"links\"><a href=\"../", html_escape(rel), "\">CSV</a><a href=\"../report-ready/tables.qmd\">tables.qmd</a><code>", html_escape(marker), "</code></div></article>"
      )
    )
  }
  html <- c(
    "<!doctype html>",
    "<html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>BET results map</title>",
    "<style>",
    "body{margin:0;background:#eef6f7;color:#173248;font-family:system-ui,-apple-system,Segoe UI,sans-serif;}main{max-width:1320px;margin:0 auto;padding:32px;}h1{font-size:34px;margin:0 0 8px;}h2{font-size:20px;margin:34px 0 14px}.lead{color:#5b7181;margin:0 0 22px}.summary{display:flex;gap:10px;flex-wrap:wrap;margin:18px 0 28px}.pill{background:#fff;border:1px solid #cfe0e8;border-radius:999px;padding:8px 12px;font-weight:800}.grid{display:grid;gap:14px;grid-template-columns:repeat(auto-fill,minmax(320px,1fr))}.card{background:#fff;border:1px solid #cfdee6;border-radius:8px;box-shadow:0 8px 24px rgba(20,53,73,.07);overflow:hidden}.thumb{background:#f8fbfc;border-bottom:1px solid #e1ebf0;height:210px;display:flex;align-items:center;justify-content:center}.thumb img{max-width:100%;max-height:100%;display:block}.card h3{font-size:16px;margin:12px 14px 8px}.card p{color:#506676;font-size:13px;line-height:1.45;margin:0 14px 14px}.meta{align-items:center;display:flex;gap:8px;margin:12px 14px 0}.meta span{background:#e9f5ee;border:1px solid #b9dcc8;border-radius:999px;color:#1e7450;font-size:12px;font-weight:900;padding:3px 8px}.meta code,.links code{background:#f5f8fa;border:1px solid #dbe7ed;border-radius:6px;color:#5b6f7d;font-size:12px;padding:2px 5px}.links{align-items:center;border-top:1px solid #eef3f5;display:flex;gap:8px;flex-wrap:wrap;margin-top:auto;padding:10px 14px}.links a{border:1px solid #c9dde8;border-radius:999px;color:#1b6187;font-size:12px;font-weight:900;padding:4px 8px;text-decoration:none}.table-card{padding-top:2px}.table-card .links{border-top:0;padding-top:0}</style>",
    "</head><body><main>",
    "<h1>BET results map</h1>",
    "<p class=\"lead\">Read-only map of all generated figures and tables, including items not currently placed in the report.</p>",
    "<div class=\"summary\">",
    paste0("<div class=\"pill\">", length(figure_ids), " figures</div>"),
    paste0("<div class=\"pill\">", length(table_ids), " tables</div>"),
    "<div class=\"pill\">QMD: figures.qmd, tables.qmd</div>",
    "</div>",
    "<h2>Figures</h2>",
    "<section class=\"grid\">",
    if (length(figure_cards)) figure_cards else "<p>No figures were produced.</p>",
    "</section>",
    "<h2>Tables</h2>",
    "<section class=\"grid\">",
    if (length(table_cards)) table_cards else "<p>No tables were produced.</p>",
    "</section>",
    "</main></body></html>"
  )
  dir.create(overview_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(html, file.path(overview_dir, "report-map.html"))
  invisible(file.path(overview_dir, "report-map.html"))
}

write_report_ready_figure_gallery <- function(figure_index, output_dir, overview_dir) {
  figure_index <- ensure_index_columns(figure_index, "figure")
  figure_ids <- all_figure_ids(figure_index)
  records <- lapply(figure_ids, function(id) {
    rows <- figure_index[as.character(figure_index$figure) == id, , drop = FALSE]
    row <- preferred_figure_row(rows, output_dir)
    if (!nrow(row)) return(NULL)
    rel <- index_value(row, "relative_path", index_value(row, "file", ""))
    data.frame(
      figure = id,
      label = index_value(row, "label", id),
      caption = index_value(row, "caption", index_value(row, "label", id)),
      placement = overview_placement(rows),
      image = paste0("../", rel),
      file = rel,
      anchor = paste0("figure-", report_slug(id, "figure")),
      stringsAsFactors = FALSE
    )
  })
  records <- bind_rows_fill(records)
  if (!nrow(records)) {
    records <- data.frame(
      figure = character(),
      label = character(),
      caption = character(),
      placement = character(),
      image = character(),
      file = character(),
      anchor = character(),
      stringsAsFactors = FALSE
    )
  }

  figure_block <- function(row) {
    placement_class <- gsub("[^a-z0-9]+", "-", tolower(row$placement), perl = TRUE)
    placement_class <- gsub("(^-+|-+$)", "", placement_class, perl = TRUE)
    paste0(
      "<article class=\"figure-card ", html_escape(placement_class), "\" id=\"", html_escape(row$anchor), "\">",
      "<header><div><span class=\"placement\">", html_escape(row$placement), "</span>",
      "<code>", html_escape(row$figure), "</code></div>",
      "<a href=\"", html_escape(row$image), "\">open</a></header>",
      "<figure><img loading=\"lazy\" src=\"", html_escape(row$image), "\" alt=\"", html_escape(row$caption), "\"></figure>",
      "<h2>", html_escape(row$label), "</h2>",
      "<p>", html_escape(row$caption), "</p>",
      "<footer><code>", html_escape(row$file), "</code></footer>",
      "</article>"
    )
  }
  main_count <- sum(records$placement == "Main")
  appendix_count <- sum(records$placement == "Appendix")
  excluded_count <- sum(records$placement == "Excluded")
  html <- c(
    "<!doctype html>",
    "<html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>BET results figure overview</title>",
    "<style>",
    paste0(
      "body{margin:0;background:#f5f8f8;color:#132d3d;font-family:system-ui,-apple-system,Segoe UI,sans-serif;}",
      "main{max-width:1720px;margin:0 auto;padding:18px 20px 34px;}",
      ".top{align-items:center;display:flex;justify-content:space-between;gap:16px;margin-bottom:14px;}",
      ".top h1{font-size:24px;line-height:1.1;margin:0 0 4px;}",
      ".top p{color:#526a79;font-size:13px;margin:0;}",
      ".summary{display:flex;gap:7px;flex-wrap:wrap;justify-content:flex-end;}",
      ".pill{background:#fff;border:1px solid #d1e0e7;border-radius:999px;font-size:12px;font-weight:850;padding:5px 9px;}",
      ".grid{display:grid;gap:12px;grid-template-columns:repeat(auto-fill,minmax(260px,1fr));}",
      ".figure-card{background:#fff;border:1px solid #ccdce4;border-radius:8px;display:flex;flex-direction:column;min-width:0;overflow:hidden;box-shadow:0 6px 18px rgba(21,56,77,.07);}",
      ".figure-card header{align-items:center;background:#eef5f7;border-bottom:1px solid #d8e5eb;display:flex;gap:8px;justify-content:space-between;padding:7px 9px;}",
      ".figure-card header div{align-items:center;display:flex;gap:8px;min-width:0;}",
      ".figure-card header a{border:1px solid #bdd6e3;border-radius:999px;color:#17617f;font-size:12px;font-weight:900;padding:3px 7px;text-decoration:none;white-space:nowrap;}",
      ".placement{background:#e8f5ee;border:1px solid #b9dcc8;border-radius:999px;color:#1b744d;font-size:11px;font-weight:900;padding:2px 7px;}",
      ".appendix .placement{background:#eef2fb;border-color:#cbd6f1;color:#2e5594}.excluded{opacity:.82}.excluded .placement{background:#f8eeee;border-color:#e1c4c4;color:#934141}",
      "code{background:#f7fafb;border:1px solid #dbe7ed;border-radius:6px;color:#526b7a;font-size:12px;padding:2px 5px;}",
      ".figure-card figure{align-items:center;background:#fff;display:flex;height:176px;justify-content:center;margin:0;padding:8px;}",
      ".figure-card img{display:block;height:100%;max-height:100%;max-width:100%;object-fit:contain;width:100%;}",
      ".figure-card h2{font-size:13px;line-height:1.25;margin:9px 10px 5px;}",
      ".figure-card p{color:#425d6e;font-size:12px;line-height:1.35;margin:0 10px 9px;max-height:48px;overflow:hidden;}",
      ".figure-card footer{border-top:1px solid #edf3f6;margin-top:auto;padding:7px 10px 9px;}",
      ".figure-card footer code{display:block;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}",
      "@media (min-width:1500px){.grid{grid-template-columns:repeat(auto-fill,minmax(245px,1fr));}.figure-card figure{height:160px;}}",
      "@media (max-width:760px){main{padding:14px 10px 24px}.top{display:block}.summary{justify-content:flex-start;margin-top:10px}.grid{grid-template-columns:repeat(auto-fill,minmax(210px,1fr));}.figure-card figure{height:148px}}"
    ),
    "</style>",
    "</head><body><main>",
    "<div class=\"top\"><div>",
    "<h1>BET results figure overview</h1>",
    "<p>All generated figures are shown here; badges indicate whether each item is currently main, appendix, or excluded from the report seed.</p>",
    "</div><div class=\"summary\">",
    paste0("<span class=\"pill\">", nrow(records), " figures</span>"),
    paste0("<span class=\"pill\">", main_count, " main</span>"),
    paste0("<span class=\"pill\">", appendix_count, " appendix</span>"),
    paste0("<span class=\"pill\">", excluded_count, " excluded</span>"),
    "</div></div>",
    "<section class=\"grid\">",
    if (nrow(records)) {
      vapply(seq_len(nrow(records)), function(i) figure_block(records[i, , drop = FALSE]), character(1))
    } else {
      "<p>No figures were produced.</p>"
    },
    "</section>",
    "</main></body></html>"
  )
  dir.create(overview_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(html, file.path(overview_dir, "report-ready-figures.html"))
  invisible(file.path(overview_dir, "report-ready-figures.html"))
}

write_report_ready_outputs <- function(result, output_dir) {
  ready_dir <- file.path(output_dir, "report-ready")
  overview_dir <- file.path(output_dir, "overview")
  dir.create(ready_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(overview_dir, recursive = TRUE, showWarnings = FALSE)
  figures <- polish_output_metadata(result$figures %||% data.frame())
  tables <- polish_output_metadata(result$tables %||% data.frame())
  figure_qmd <- write_report_ready_figures_qmd(figures, output_dir, ready_dir)
  table_qmd <- write_report_ready_tables_qmd(tables, ready_dir)
  gallery_html <- write_report_ready_figure_gallery(figures, output_dir, overview_dir)
  map_html <- write_report_ready_map(figures, tables, output_dir, overview_dir)
  index <- data.frame(
    file = c("figures.qmd", "tables.qmd", "../overview/report-ready-figures.html", "../overview/report-map.html"),
    path = c(figure_qmd, table_qmd, gallery_html, map_html),
    purpose = c("report figure section seed", "report table section seed", "report-ready figure gallery", "read-only output map"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(index, file.path(ready_dir, "report-ready-files.csv"), row.names = FALSE)
  invisible(index)
}

write_clean_indices_and_review <- function(result,
                                           output_dir,
                                           title,
                                           species_code,
                                           species_label,
                                           assessment_year,
                                           render_html = FALSE) {
  result$figures <- polish_output_metadata(result$figures %||% data.frame())
  result$tables <- polish_output_metadata(result$tables %||% data.frame())
  index_dir <- file.path(output_dir, "indices")
  review_dir <- file.path(output_dir, "review")
  dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)
  if (nrow(result$figures)) {
    utils::write.csv(result$figures, file.path(index_dir, "figure-index.csv"), row.names = FALSE)
  }
  if (nrow(result$tables)) {
    utils::write.csv(result$tables, file.path(index_dir, "table-index.csv"), row.names = FALSE)
  }
  mfclshiny::write_report_figure_review(
    figure_index = result$figures,
    table_index = result$tables,
    output_dir = output_dir,
    title = title,
    species_code = species_code,
    species_label = species_label,
    assessment_year = assessment_year,
    qmd_file = file.path("review", "plot-report.qmd"),
    html_file = file.path("review", "plot-report.html"),
    render_html = isTRUE(render_html),
    figure_log = result$log %||% NULL,
    table_log = result$table_log %||% NULL
  )
  result
}

move_if_exists <- function(from, to) {
  if (!file.exists(from)) return(FALSE)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  file.copy(from, to, overwrite = TRUE)
  unlink(from, recursive = TRUE, force = TRUE)
  TRUE
}

organize_result_outputs <- function(output_dir) {
  index_dir <- file.path(output_dir, "indices")
  log_dir <- file.path(output_dir, "logs")
  review_dir <- file.path(output_dir, "review")
  dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)

  index_files <- c(
    "payload-index.csv",
    "figure-index.csv",
    "table-index.csv",
    "mfclshiny-figure-index.csv",
    "mfclshiny-table-index.csv",
    "mfclshiny-report-files.csv",
    "plot-summary.csv",
    "figure-optimization.csv"
  )
  for (file in index_files) {
    target_name <- sub("^mfclshiny-", "", file)
    move_if_exists(file.path(output_dir, file), file.path(index_dir, target_name))
  }

  log_files <- list.files(
    output_dir,
    pattern = "(build-log|table-log|figure-log|errors?)[.]csv$",
    full.names = FALSE,
    ignore.case = TRUE
  )
  for (file in log_files) {
    move_if_exists(file.path(output_dir, file), file.path(log_dir, file))
  }

  unlink(file.path(output_dir, "_review"), recursive = TRUE, force = TRUE)

  readme <- file.path(output_dir, "README.txt")
  writeLines(
    c(
      "BET results outputs",
      "",
      "Start here:",
      "- overview/report-ready-figures.html: one-page visual check of the report-ready figures.",
      "- overview/report-map.html: figure/table map with QMD markers.",
      "- report-ready/figures.qmd and report-ready/tables.qmd: section seeds consumed by the report task.",
      "",
      "Folders:",
      "- figures/: image files referenced by report-ready QMD.",
      "- tables/: table files referenced by report-ready QMD.",
      "- indices/: payload, figure, table, optimization, and summary CSVs.",
      "- logs/: build logs and errors when present.",
      "- review/: detailed plot-report.qmd; plot-report.html only when PLOT_RENDER_REVIEW_HTML=true."
    ),
    readme
  )
  invisible(readme)
}

short_error <- function(x) {
  x <- as.character(x %||% "")
  x <- trimws(paste(x, collapse = " "))
  if (!nzchar(x)) "unknown error" else substr(x, 1L, 240L)
}

empty_optimization_log <- function() {
  data.frame(
    file = character(),
    original_bytes = numeric(),
    optimized_bytes = numeric(),
    candidate_bytes = numeric(),
    saved_bytes = numeric(),
    optimized = logical(),
    method = character(),
    webp_file = character(),
    webp_bytes = numeric(),
    webp_saved_bytes = numeric(),
    webp_created = logical(),
    pdf_file = character(),
    pdf_bytes = numeric(),
    pdf_saved_bytes = numeric(),
    pdf_created = logical(),
    reason = character(),
    stringsAsFactors = FALSE
  )
}

pngquant_quality <- function(value = env("PLOT_PNGQUANT_QUALITY", "60-85")) {
  value <- trimws(as.character(value %||% "60-85"))
  if (!grepl("^[0-9]{1,3}-[0-9]{1,3}$", value)) {
    return("60-85")
  }
  parts <- suppressWarnings(as.integer(strsplit(value, "-", fixed = TRUE)[[1]]))
  if (length(parts) != 2L || any(!is.finite(parts)) || any(parts < 0L) || any(parts > 100L) || parts[[1]] > parts[[2]]) {
    return("60-85")
  }
  value
}

webp_quality <- function(value = env("PLOT_WEBP_QUALITY", "72")) {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value) || value < 1 || value > 100) 72 else value
}

jpeg_quality <- function(value = env("PLOT_JPEG_QUALITY", "82")) {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value) || value < 1 || value > 100) 82 else value
}

jpeg_sampling_factor <- function(value = env("PLOT_JPEG_SAMPLING_FACTOR", "4:2:0")) {
  value <- trimws(as.character(value %||% "4:2:0"))
  if (value %in% c("4:4:4", "4:2:2", "4:2:0")) value else "4:2:0"
}

pngquant_speed <- function(value = env("PLOT_PNGQUANT_SPEED", "1")) {
  value <- suppressWarnings(as.integer(value))
  if (!is.finite(value) || value < 1L || value > 11L) 1L else value
}

optimizer_mode <- function(value = env("PLOT_OPTIMIZE_MODE", "lossy")) {
  value <- tolower(trimws(as.character(value %||% "lossy")))
  value <- gsub("[^a-z0-9]+", "-", value)
  aliases <- c(
    lossy = "lossy",
    pngquant = "lossy",
    small = "lossy",
    max = "lossy",
    lossless = "lossless",
    imagemagick = "lossless",
    convert = "lossless",
    off = "none",
    none = "none",
    false = "none",
    "0" = "none"
  )
  if (value %in% names(aliases)) aliases[[value]] else "lossy"
}

replace_if_smaller <- function(file, tmp, before, row, method, reason) {
  candidate_bytes <- suppressWarnings(file.info(tmp)$size)
  if (!is.finite(candidate_bytes) || candidate_bytes <= 0) {
    return(row(FALSE, method, candidate_bytes, "optimizer produced an empty file"))
  }
  if (candidate_bytes >= before) {
    return(row(FALSE, method, candidate_bytes, "already optimal or candidate larger"))
  }
  ok <- file.copy(tmp, file, overwrite = TRUE)
  if (!isTRUE(ok)) {
    return(row(FALSE, method, candidate_bytes, "could not replace original file"))
  }
  row(TRUE, method, candidate_bytes, reason)
}

optimize_png_file <- function(file,
                              output_dir,
                              mode = optimizer_mode(),
                              quality = pngquant_quality(),
                              pngquant_bin = Sys.which("pngquant"),
                              convert_bin = Sys.which("convert")) {
  root <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  full_file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  rel_file <- if (startsWith(full_file, paste0(root, "/"))) {
    substr(full_file, nchar(root) + 2L, nchar(full_file))
  } else {
    basename(full_file)
  }
  before <- suppressWarnings(file.info(file)$size)
  row <- function(optimized, method, candidate_bytes, reason) {
    after <- if (isTRUE(optimized)) candidate_bytes else before
    data.frame(
      file = rel_file,
      original_bytes = before,
      optimized_bytes = after,
      candidate_bytes = candidate_bytes,
      saved_bytes = if (isTRUE(optimized)) before - after else 0,
      optimized = isTRUE(optimized),
      method = method,
      reason = reason,
      stringsAsFactors = FALSE
    )
  }
  if (!is.finite(before) || before <= 0) {
    return(row(FALSE, "none", NA_real_, "missing or empty file"))
  }

  mode <- optimizer_mode(mode)
  if (identical(mode, "none")) {
    return(row(FALSE, "none", NA_real_, "optimization disabled"))
  }

  tmp <- tempfile(pattern = paste0(tools::file_path_sans_ext(basename(file)), "-"), fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  if (identical(mode, "lossy") && nzchar(pngquant_bin)) {
    args <- c(
      "--force",
      "--strip",
      "--speed", as.character(pngquant_speed()),
      "--quality", quality,
      "--output", tmp,
      normalizePath(file, winslash = "/", mustWork = TRUE)
    )
    output <- tryCatch(
      system2(pngquant_bin, args, stdout = TRUE, stderr = TRUE),
      error = function(e) structure(conditionMessage(e), status = 1L)
    )
    status <- attr(output, "status") %||% 0L
    if (identical(as.integer(status), 0L) && file.exists(tmp)) {
      return(replace_if_smaller(file, tmp, before, row, "pngquant", paste0("lossy PNG quality ", quality)))
    }
    pngquant_message <- short_error(output)
  } else {
    pngquant_message <- "pngquant unavailable"
  }

  if (nzchar(convert_bin)) {
    unlink(tmp)
    input <- normalizePath(file, winslash = "/", mustWork = TRUE)
    args <- if (identical(mode, "lossy")) {
      c(input, "-strip", "-colors", "256", paste0("PNG8:", tmp))
    } else {
      c(
        input,
        "-strip",
        "-define", "png:compression-level=9",
        "-define", "png:compression-filter=5",
        "-define", "png:compression-strategy=1",
        tmp
      )
    }
    output <- tryCatch(
      system2(convert_bin, args, stdout = TRUE, stderr = TRUE),
      error = function(e) structure(conditionMessage(e), status = 1L)
    )
    status <- attr(output, "status") %||% 0L
    if (!identical(as.integer(status), 0L) || !file.exists(tmp)) {
      return(row(FALSE, "imagemagick", NA_real_, short_error(output)))
    }
    method <- if (identical(mode, "lossy")) "imagemagick-png8" else "imagemagick"
    reason <- if (identical(mode, "lossy")) "lossy PNG8 palette fallback" else "lossless PNG strip/recompress"
    return(replace_if_smaller(file, tmp, before, row, method, reason))
  }

  row(FALSE, "none", NA_real_, paste("pngquant/ImageMagick unavailable:", pngquant_message))
}

create_webp_file <- function(file,
                             output_dir,
                             quality = webp_quality(),
                             cwebp_bin = Sys.which("cwebp")) {
  root <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  full_file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  rel_file <- if (startsWith(full_file, paste0(root, "/"))) {
    substr(full_file, nchar(root) + 2L, nchar(full_file))
  } else {
    basename(full_file)
  }
  target <- sub("[.][Pp][Nn][Gg]$", ".webp", file)
  rel_target <- sub("[.][Pp][Nn][Gg]$", ".webp", rel_file)
  before <- suppressWarnings(file.info(file)$size)
  row <- function(created, bytes, reason) {
    data.frame(
      file = rel_file,
      webp_file = rel_target,
      webp_bytes = bytes,
      webp_saved_bytes = if (isTRUE(created) && is.finite(before)) before - bytes else 0,
      webp_created = isTRUE(created),
      webp_reason = reason,
      stringsAsFactors = FALSE
    )
  }
  if (!is.finite(before) || before <= 0) {
    return(row(FALSE, NA_real_, "missing or empty PNG source"))
  }
  if (!nzchar(cwebp_bin)) {
    return(row(FALSE, NA_real_, "cwebp unavailable"))
  }
  tmp <- tempfile(pattern = paste0(tools::file_path_sans_ext(basename(file)), "-"), fileext = ".webp")
  on.exit(unlink(tmp), add = TRUE)
  args <- c(
    "-quiet",
    "-preset", "drawing",
    "-q", as.character(quality),
    "-m", "6",
    "-mt",
    "-sharp_yuv",
    normalizePath(file, winslash = "/", mustWork = TRUE),
    "-o", tmp
  )
  output <- tryCatch(
    system2(cwebp_bin, args, stdout = TRUE, stderr = TRUE),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
  status <- attr(output, "status") %||% 0L
  if (!identical(as.integer(status), 0L) || !file.exists(tmp)) {
    return(row(FALSE, NA_real_, short_error(output)))
  }
  candidate_bytes <- suppressWarnings(file.info(tmp)$size)
  if (!is.finite(candidate_bytes) || candidate_bytes <= 0) {
    return(row(FALSE, candidate_bytes, "cwebp produced an empty file"))
  }
  if (candidate_bytes >= before) {
    unlink(target)
    return(row(FALSE, candidate_bytes, "WebP candidate larger than PNG"))
  }
  ok <- file.copy(tmp, target, overwrite = TRUE)
  if (!isTRUE(ok)) {
    return(row(FALSE, candidate_bytes, "could not write WebP file"))
  }
  row(TRUE, candidate_bytes, paste0("WebP quality ", quality))
}

create_pdf_jpeg_file <- function(file,
                                 output_dir,
                                 quality = jpeg_quality(),
                                 sampling_factor = jpeg_sampling_factor(),
                                 convert_bin = Sys.which("convert")) {
  root <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  full_file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  rel_file <- if (startsWith(full_file, paste0(root, "/"))) {
    substr(full_file, nchar(root) + 2L, nchar(full_file))
  } else {
    basename(full_file)
  }
  target <- sub("[.][Pp][Nn][Gg]$", ".jpg", file)
  rel_target <- sub("[.][Pp][Nn][Gg]$", ".jpg", rel_file)
  before <- suppressWarnings(file.info(file)$size)
  row <- function(created, bytes, reason) {
    data.frame(
      file = rel_file,
      pdf_file = rel_target,
      pdf_bytes = bytes,
      pdf_saved_bytes = if (isTRUE(created) && is.finite(before)) before - bytes else 0,
      pdf_created = isTRUE(created),
      pdf_reason = reason,
      stringsAsFactors = FALSE
    )
  }
  if (!is.finite(before) || before <= 0) {
    return(row(FALSE, NA_real_, "missing or empty PNG source"))
  }
  if (!nzchar(convert_bin)) {
    return(row(FALSE, NA_real_, "ImageMagick convert unavailable"))
  }
  tmp <- tempfile(pattern = paste0(tools::file_path_sans_ext(basename(file)), "-"), fileext = ".jpg")
  on.exit(unlink(tmp), add = TRUE)
  args <- c(
    normalizePath(file, winslash = "/", mustWork = TRUE),
    "-background", "white",
    "-alpha", "remove",
    "-alpha", "off",
    "-strip",
    "-interlace", "Plane",
    "-sampling-factor", sampling_factor,
    "-quality", as.character(quality),
    tmp
  )
  output <- tryCatch(
    system2(convert_bin, args, stdout = TRUE, stderr = TRUE),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
  status <- attr(output, "status") %||% 0L
  if (!identical(as.integer(status), 0L) || !file.exists(tmp)) {
    return(row(FALSE, NA_real_, short_error(output)))
  }
  candidate_bytes <- suppressWarnings(file.info(tmp)$size)
  if (!is.finite(candidate_bytes) || candidate_bytes <= 0) {
    return(row(FALSE, candidate_bytes, "JPEG conversion produced an empty file"))
  }
  if (candidate_bytes >= before) {
    unlink(target)
    return(row(FALSE, candidate_bytes, "JPEG candidate larger than PNG"))
  }
  ok <- file.copy(tmp, target, overwrite = TRUE)
  if (!isTRUE(ok)) {
    return(row(FALSE, candidate_bytes, "could not write JPEG file"))
  }
  row(TRUE, candidate_bytes, paste0("JPEG quality ", quality, ", sampling ", sampling_factor))
}

optimize_plot_figures <- function(output_dir, enabled = TRUE) {
  log_file <- file.path(output_dir, "figure-optimization.csv")
  if (!isTRUE(enabled)) {
    result <- empty_optimization_log()
    utils::write.csv(result, log_file, row.names = FALSE)
    message("Figure optimization disabled; wrote empty figure-optimization.csv.")
    return(result)
  }

  figure_dir <- file.path(output_dir, "figures")
  files <- if (dir.exists(figure_dir)) {
    list.files(figure_dir, pattern = "[.]png$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  } else {
    character()
  }
  if (!length(files)) {
    result <- empty_optimization_log()
    utils::write.csv(result, log_file, row.names = FALSE)
    message("No PNG report figures found for optimization.")
    return(result)
  }

  mode <- optimizer_mode()
  quality <- pngquant_quality()
  webp_enabled <- truthy_env("PLOT_WEBP_FIGURES", TRUE)
  pdf_jpeg_enabled <- truthy_env("PLOT_PDF_JPEG_FIGURES", TRUE)
  webp_q <- webp_quality()
  jpeg_q <- jpeg_quality()
  jpeg_sampling <- jpeg_sampling_factor()
  pngquant_bin <- Sys.which("pngquant")
  convert_bin <- Sys.which("convert")
  png_rows <- lapply(
    files,
    optimize_png_file,
    output_dir = output_dir,
    mode = mode,
    quality = quality,
    pngquant_bin = pngquant_bin,
    convert_bin = convert_bin
  )
  png_result <- bind_rows_fill(png_rows)
  if (isTRUE(webp_enabled)) {
    webp_rows <- lapply(files, create_webp_file, output_dir = output_dir, quality = webp_q, cwebp_bin = Sys.which("cwebp"))
    webp_result <- bind_rows_fill(webp_rows)
    result <- merge(png_result, webp_result, by = "file", all = TRUE, sort = FALSE)
  } else {
    webp_result <- empty_optimization_log()
    result <- png_result
    result$webp_file <- ""
    result$webp_bytes <- NA_real_
    result$webp_saved_bytes <- 0
    result$webp_created <- FALSE
    result$webp_reason <- "WebP disabled"
  }
  if (isTRUE(pdf_jpeg_enabled)) {
    jpeg_rows <- lapply(
      files,
      create_pdf_jpeg_file,
      output_dir = output_dir,
      quality = jpeg_q,
      sampling_factor = jpeg_sampling,
      convert_bin = convert_bin
    )
    jpeg_result <- bind_rows_fill(jpeg_rows)
    result <- merge(result, jpeg_result, by = "file", all = TRUE, sort = FALSE)
  } else {
    result$pdf_file <- ""
    result$pdf_bytes <- NA_real_
    result$pdf_saved_bytes <- 0
    result$pdf_created <- FALSE
    result$pdf_reason <- "PDF JPEG disabled"
  }
  utils::write.csv(result, log_file, row.names = FALSE)
  saved <- sum(result$saved_bytes, na.rm = TRUE)
  optimized <- sum(result$optimized, na.rm = TRUE)
  webp_saved <- sum(result$webp_saved_bytes, na.rm = TRUE)
  webp_created <- sum(result$webp_created, na.rm = TRUE)
  pdf_saved <- sum(result$pdf_saved_bytes, na.rm = TRUE)
  pdf_created <- sum(result$pdf_created, na.rm = TRUE)
  message(
    "Optimized ", optimized, " PNG figure(s); saved ",
    format(round(saved / 1024 / 1024, 2), nsmall = 2), " MB using ",
    mode, " mode. Created ", webp_created, " WebP sidecar(s); HTML can save ",
    format(round(webp_saved / 1024 / 1024, 2), nsmall = 2), " MB. Created ",
    pdf_created, " PDF JPEG sidecar(s); PDF can save ",
    format(round(pdf_saved / 1024 / 1024, 2), nsmall = 2), " MB."
  )
  invisible(result)
}

input_dir <- env("INPUT_DIR", "inputs")
out_dir <- env("OUTPUT_DIR", "outputs")
title <- env("PLOT_TITLE", "BET 2026 report-ready figures")
species_code <- env("FLOW_SPECIES", "BET")
species_label <- env("FLOW_SPECIES_LABEL", "bigeye tuna")
assessment_year <- env("FLOW_ASSESSMENT_YEAR", "2026")
optimize_figures <- truthy_env("PLOT_OPTIMIZE_FIGURES", TRUE)
render_review_html <- truthy_env("PLOT_RENDER_REVIEW_HTML", FALSE)
max_fisheries <- suppressWarnings(as.integer(env("PLOT_MAX_FISHERIES", "18")))
if (!is.finite(max_fisheries) || max_fisheries < 1L) max_fisheries <- 18L
selection_file <- find_report_selection(input_dir)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
unlink(
  file.path(out_dir, c("plot-report.html", "plot-report.qmd", "_review", "review/plot-report.html")),
  recursive = TRUE,
  force = TRUE
)

payload_index <- payloads(input_dir)
if (!nrow(payload_index)) {
  stop("No model_payload.rds files found in upstream inputs.", call. = FALSE)
}
write_payload_index(payload_index, out_dir)

if (!requireNamespace("mfclshiny", quietly = TRUE) ||
    !"build_app_report_figures" %in% getNamespaceExports("mfclshiny")) {
  stop("mfclshiny::build_app_report_figures is required for BET plot export.", call. = FALSE)
}

message("Building BET plots with mfclshiny Shiny report registry.")
message("Payloads: ", nrow(payload_index))
if (nzchar(selection_file)) message("Report selection: ", selection_file)
message("Output: ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))

result <- call_with_supported_args(
  mfclshiny::build_app_report_figures,
  list(
    model_dir = input_dir,
    folders = payload_index$model_folder,
    output_dir = out_dir,
    title = title,
    formats = "png",
    build_payloads = FALSE,
    overwrite = TRUE,
    render_html = FALSE,
    qmd_file = "plot-report.qmd",
    html_file = "plot-report.html",
    figure_dir = "figures",
    table_dir = "tables",
    copy_legacy_root = FALSE,
    species_code = species_code,
    species_label = species_label,
    assessment_year = assessment_year,
    max_fisheries = max_fisheries,
    selection_file = selection_file
  )
)

if (is.null(result) || !is.data.frame(result$figures)) {
  stop("mfclshiny Shiny registry export did not return a figure index.", call. = FALSE)
}
if (!nrow(result$figures)) {
  warning("mfclshiny Shiny registry export produced no report-ready figures; writing empty report-ready indices and logs.")
}

result <- write_clean_indices_and_review(
  result = result,
  output_dir = out_dir,
  title = title,
  species_code = species_code,
  species_label = species_label,
  assessment_year = assessment_year,
  render_html = render_review_html
)
write_plot_summary(result, payload_index, out_dir)
optimize_plot_figures(out_dir, enabled = optimize_figures)
write_report_ready_outputs(result, out_dir)
organize_result_outputs(out_dir)

message("Wrote ", length(unique(result$figures$figure)), " report-ready mfclshiny figure(s).")
if (is.data.frame(result$log) && any(result$log$status == "error", na.rm = TRUE)) {
  message("Some registered plots failed; see mfclshiny-figure-build-log.csv.")
}
