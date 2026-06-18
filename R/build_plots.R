`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
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

payloads <- function(input_dir) {
  files <- list.files(input_dir, pattern = "^model_payload[.]rds$", recursive = TRUE, full.names = TRUE)
  rows <- lapply(files, function(file) {
    payload <- tryCatch(readRDS(file), error = function(e) NULL)
    if (is.null(payload)) return(NULL)
    folder <- dirname(file)
    data.frame(
      model_label = payload_label(payload, basename(folder)),
      model_folder = normalizePath(folder, winslash = "/", mustWork = FALSE),
      payload_file = normalizePath(file, winslash = "/", mustWork = FALSE),
      stringsAsFactors = FALSE
    )
  })
  bind_rows_fill(rows)
}

write_payload_index <- function(payload_index, output_dir) {
  table_dir <- file.path(output_dir, "tables")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(payload_index, file.path(table_dir, "payload-index.csv"), row.names = FALSE)
  utils::write.csv(payload_index, file.path(output_dir, "payload-index.csv"), row.names = FALSE)
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
    html = file.exists(file.path(output_dir, "plot-report.html")),
    source = "mfclshiny_shiny_registry",
    stringsAsFactors = FALSE
  )
  utils::write.csv(summary, file.path(output_dir, "plot-summary.csv"), row.names = FALSE)
  invisible(summary)
}

input_dir <- env("INPUT_DIR", "inputs")
out_dir <- env("OUTPUT_DIR", "outputs")
title <- env("PLOT_TITLE", "BET 2026 report-ready figures")
species_code <- env("FLOW_SPECIES", "BET")
species_label <- env("FLOW_SPECIES_LABEL", "bigeye tuna")
assessment_year <- env("FLOW_ASSESSMENT_YEAR", "2026")
max_fisheries <- suppressWarnings(as.integer(env("PLOT_MAX_FISHERIES", "18")))
if (!is.finite(max_fisheries) || max_fisheries < 1L) max_fisheries <- 18L

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

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
    render_html = TRUE,
    qmd_file = "plot-report.qmd",
    html_file = "plot-report.html",
    figure_dir = "figures",
    table_dir = "tables",
    copy_legacy_root = FALSE,
    species_code = species_code,
    species_label = species_label,
    assessment_year = assessment_year,
    max_fisheries = max_fisheries
  )
)

if (is.null(result) || !is.data.frame(result$figures) || !nrow(result$figures)) {
  stop("mfclshiny Shiny registry export produced no report-ready figures.", call. = FALSE)
}

write_plot_summary(result, payload_index, out_dir)

message("Wrote ", length(unique(result$figures$figure)), " report-ready mfclshiny figure(s).")
if (is.data.frame(result$log) && any(result$log$status == "error", na.rm = TRUE)) {
  message("Some registered plots failed; see mfclshiny-figure-build-log.csv.")
}
