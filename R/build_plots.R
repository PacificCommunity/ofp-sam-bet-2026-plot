`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

slug <- function(x, default = "figure") {
  x <- tolower(gsub("[^A-Za-z0-9]+", "-", as.character(x %||% default)))
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) default else x
}

safe_df <- function(x) {
  tryCatch(as.data.frame(x), error = function(e) data.frame())
}

first_text <- function(x, default = "") {
  value <- tryCatch(as.character(x), error = function(e) character())
  if (!length(value) || is.na(value[[1]]) || !nzchar(value[[1]])) default else value[[1]]
}

payload_label <- function(payload, fallback) {
  reg <- tryCatch(payload$data$info$registry, error = function(e) NULL)
  for (name in c("plot_label", "model_label", "model_token", "job_key")) {
    value <- first_text(tryCatch(reg[[name]], error = function(e) NULL))
    if (nzchar(value)) return(value)
  }
  fallback
}

payloads <- function(input_dir) {
  files <- list.files(input_dir, pattern = "^model_payload[.]rds$", recursive = TRUE, full.names = TRUE)
  rows <- lapply(files, function(file) {
    payload <- tryCatch(readRDS(file), error = function(e) NULL)
    if (is.null(payload)) return(NULL)
    label <- payload_label(payload, basename(dirname(file)))
    list(file = file, folder = dirname(file), label = label, payload = payload)
  })
  rows[!vapply(rows, is.null, logical(1))]
}

flq_slot <- function(payload_list, slot_name) {
  rows <- lapply(payload_list, function(item) {
    rep <- tryCatch(item$payload$data$RepOut, error = function(e) NULL)
    if (is.null(rep)) return(data.frame())
    obj <- tryCatch(slot(rep, slot_name), error = function(e) NULL)
    df <- safe_df(obj)
    if (!nrow(df) || !"data" %in% names(df)) return(data.frame())
    df$Scenario <- item$label
    df$source_payload <- basename(item$file)
    df
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) return(out)
  for (name in intersect(c("age", "year", "unit", "season", "area", "data"), names(out))) {
    out[[name]] <- suppressWarnings(as.numeric(as.character(out[[name]])))
  }
  out <- out[is.finite(out$data), , drop = FALSE]
  out
}

fishery_label <- function(unit) {
  unit <- suppressWarnings(as.integer(unit))
  paste0("Fishery ", unit)
}

top_units <- function(df, n = 12L) {
  if (!"unit" %in% names(df)) return(numeric())
  df$.abs_data <- abs(df$data)
  s <- stats::aggregate(.abs_data ~ unit, df, sum, na.rm = TRUE)
  names(s)[2] <- "total"
  head(s$unit[order(-s$total)], n)
}

call_with_supported_args <- function(fun, args) {
  supported <- names(formals(fun))
  if (!"..." %in% supported) args <- args[names(args) %in% supported]
  do.call(fun, args)
}

row_value <- function(df, name, i, default = "") {
  if (!name %in% names(df)) return(default)
  value <- first_text(tryCatch(df[[name]][[i]], error = function(e) NULL), default = default)
  if (!nzchar(value)) default else value
}

theme_report <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "#eef4f7", colour = "#cbdde7"),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_blank()
    )
}

save_plot <- function(plot, output_dir, id, label, caption, width = 12, height = 8, dpi = 220) {
  figure_dir <- file.path(output_dir, "figures")
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  file <- paste0(slug(id), ".png")
  path <- file.path(figure_dir, file)
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = dpi, bg = "white")
  data.frame(
    figure = gsub("-", "_", slug(id)),
    file = file,
    relative_path = file.path("figures", file),
    label = label,
    caption = caption,
    alt_text = caption,
    description = "Payload-derived mfclshiny report figure.",
    format = "png",
    rows = NA_integer_,
    models = NA_integer_,
    width = width,
    height = height,
    dpi = dpi,
    status = "ok",
    stringsAsFactors = FALSE
  )
}

build_cpue <- function(payload_list) {
  obs <- flq_slot(payload_list, "cpue_obs")
  fit <- flq_slot(payload_list, "cpue_pred")
  if (!nrow(obs) || !nrow(fit)) return(NULL)
  names(obs)[names(obs) == "data"] <- "obs_log"
  names(fit)[names(fit) == "data"] <- "fit_log"
  keys <- intersect(c("Scenario", "age", "year", "unit", "season", "area", "iter"), intersect(names(obs), names(fit)))
  df <- merge(obs, fit, by = keys)
  df <- df[is.finite(df$obs_log) & is.finite(df$fit_log) & abs(df$obs_log) < 15 & abs(df$fit_log) < 15, , drop = FALSE]
  if (!nrow(df)) return(NULL)
  keep <- top_units(transform(df, data = exp(obs_log)), 12)
  df <- df[df$unit %in% keep, , drop = FALSE]
  df$fishery <- fishery_label(df$unit)
  df$year_season <- df$year + (df$season - 1) / 4
  df$obs <- exp(df$obs_log)
  df$fit <- exp(df$fit_log)
  ggplot2::ggplot(df, ggplot2::aes(x = year_season)) +
    ggplot2::geom_point(ggplot2::aes(y = obs), colour = "#586270", alpha = 0.45, size = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = fit, colour = Scenario), linewidth = 0.8, alpha = 0.9) +
    ggplot2::facet_wrap(~fishery, scales = "free_y", ncol = 3) +
    ggplot2::labs(x = "Year", y = "CPUE") +
    theme_report()
}

build_catch <- function(payload_list) {
  obs <- flq_slot(payload_list, "catch_obs")
  fit <- flq_slot(payload_list, "catch_pred")
  if (!nrow(obs) || !nrow(fit)) return(NULL)
  names(obs)[names(obs) == "data"] <- "Observed"
  names(fit)[names(fit) == "data"] <- "Predicted"
  keys <- intersect(c("Scenario", "age", "year", "unit", "season", "area", "iter"), intersect(names(obs), names(fit)))
  df <- merge(obs, fit, by = keys)
  if (!nrow(df)) return(NULL)
  df$data <- df$Observed
  keep <- top_units(df, 12)
  df <- df[df$unit %in% keep, , drop = FALSE]
  df$fishery <- fishery_label(df$unit)
  df$year_season <- df$year + (df$season - 1) / 4
  ggplot2::ggplot(df, ggplot2::aes(x = year_season)) +
    ggplot2::geom_point(ggplot2::aes(y = Observed), colour = "#586270", alpha = 0.45, size = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = Predicted, colour = Scenario), linewidth = 0.8, alpha = 0.9) +
    ggplot2::facet_wrap(~fishery, scales = "free_y", ncol = 3) +
    ggplot2::labs(x = "Year", y = "Catch") +
    theme_report()
}

build_selectivity <- function(payload_list) {
  df <- flq_slot(payload_list, "sel")
  if (!nrow(df)) return(NULL)
  keep <- top_units(df[df$data > 0, , drop = FALSE], 12)
  df <- df[df$unit %in% keep & is.finite(df$age), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  df$fishery <- fishery_label(df$unit)
  ggplot2::ggplot(df, ggplot2::aes(x = age, y = data, colour = Scenario, group = interaction(Scenario, unit))) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    ggplot2::facet_wrap(~fishery, ncol = 3) +
    ggplot2::labs(x = "Age", y = "Selectivity") +
    theme_report()
}

build_size_at_age <- function(payload_list) {
  len <- flq_slot(payload_list, "mean_laa")
  wgt <- flq_slot(payload_list, "mean_waa")
  if (!nrow(len) && !nrow(wgt)) return(NULL)
  len$Quantity <- "Mean length at age"
  wgt$Quantity <- "Mean weight at age"
  df <- dplyr::bind_rows(len, wgt)
  df <- df[is.finite(df$age) & is.finite(df$data), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  ggplot2::ggplot(df, ggplot2::aes(x = age, y = data, colour = Scenario, linetype = factor(season))) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    ggplot2::facet_wrap(~Quantity, scales = "free_y", ncol = 1) +
    ggplot2::labs(x = "Age", y = NULL, linetype = "Season") +
    theme_report()
}

build_regional_series <- function(payload_list, slot_name, label, y_label, scale = 1) {
  df <- flq_slot(payload_list, slot_name)
  if (!nrow(df) || !"area" %in% names(df)) return(NULL)
  df <- stats::aggregate(data ~ Scenario + year + area, df, sum, na.rm = TRUE)
  df$data <- df$data * scale
  df <- df[is.finite(df$year) & is.finite(df$data), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  df$Region <- paste("Region", df$area)
  ggplot2::ggplot(df, ggplot2::aes(x = year, y = data, colour = Scenario)) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    ggplot2::facet_wrap(~Region, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = "Year", y = y_label) +
    theme_report()
}

input_dir <- env("INPUT_DIR", "inputs")
out_dir <- env("OUTPUT_DIR", "outputs")
report_dir <- file.path(out_dir, "report-figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

payload_list <- payloads(input_dir)
if (!length(payload_list)) stop("No model_payload.rds files found in upstream inputs.", call. = FALSE)

payload_folders <- vapply(payload_list, `[[`, character(1), "folder")
base_result <- NULL
if (requireNamespace("mfclshiny", quietly = TRUE) &&
    "build_report_figures" %in% getNamespaceExports("mfclshiny")) {
  base_args <- list(
    model_dir = input_dir,
    folders = payload_folders,
    output_dir = report_dir,
    title = env("PLOT_TITLE", "BET 2026 report-ready figures"),
    figure_basename = "key-quantities",
    formats = "png",
    build_payloads = FALSE,
    overwrite = TRUE,
    render_html = FALSE,
    species_code = env("FLOW_SPECIES", "BET"),
    species_label = env("FLOW_SPECIES_LABEL", "bigeye tuna"),
    assessment_year = env("FLOW_ASSESSMENT_YEAR", "2026")
  )
  base_result <- call_with_supported_args(mfclshiny::build_report_figures, base_args)
}

figure_index <- if (!is.null(base_result)) base_result$figures else data.frame()
table_index <- if (!is.null(base_result)) base_result$tables else data.frame()

extra_specs <- list(
  list(id = "cpue-fits", label = "CPUE fits", plot = build_cpue(payload_list), caption = "Observed and fitted CPUE series by fishery for the selected BET model payloads."),
  list(id = "catch-fits", label = "Catch fits", plot = build_catch(payload_list), caption = "Observed and fitted catch series by fishery for the selected BET model payloads."),
  list(id = "selectivity", label = "Selectivity", plot = build_selectivity(payload_list), caption = "Selectivity-at-age curves by fishery for the selected BET model payloads."),
  list(id = "size-at-age", label = "Size at age", plot = build_size_at_age(payload_list), caption = "Mean length and mean weight at age from the selected BET model payloads."),
  list(id = "biomass-by-region", label = "Biomass by region", plot = build_regional_series(payload_list, "adultBiomass", "Adult biomass by region", "Adult biomass"), caption = "Adult biomass time series by model and region."),
  list(id = "recruitment-by-region", label = "Recruitment by region", plot = build_regional_series(payload_list, "rec_region", "Recruitment by region", "Recruitment", 1 / 1e6), caption = "Recruitment time series by model and region, in millions of fish.")
)

extra_rows <- list()
for (spec in extra_specs) {
  if (is.null(spec$plot)) next
  extra_rows[[length(extra_rows) + 1L]] <- save_plot(
    spec$plot,
    output_dir = report_dir,
    id = spec$id,
    label = spec$label,
    caption = spec$caption
  )
}
if (length(extra_rows)) {
  figure_index <- dplyr::bind_rows(figure_index, dplyr::bind_rows(extra_rows))
}

write.csv(figure_index, file.path(report_dir, "figure-index.csv"), row.names = FALSE)
write.csv(table_index, file.path(report_dir, "table-index.csv"), row.names = FALSE)

if (requireNamespace("mfclshiny", quietly = TRUE) &&
    "write_report_figure_review" %in% getNamespaceExports("mfclshiny")) {
  review_args <- list(
    figure_index = figure_index,
    table_index = table_index,
    output_dir = report_dir,
    title = env("PLOT_TITLE", "BET 2026 report-ready figures"),
    species_code = env("FLOW_SPECIES", "BET"),
    species_label = env("FLOW_SPECIES_LABEL", "bigeye tuna"),
    assessment_year = env("FLOW_ASSESSMENT_YEAR", "2026"),
    render_html = TRUE
  )
  call_with_supported_args(mfclshiny::write_report_figure_review, review_args)
} else {
  qmd <- file.path(report_dir, "mfclshiny-report-figures.qmd")
  html <- file.path(report_dir, "mfclshiny-report-figures.html")
  lines <- c(
    "---",
    paste0("title: \"", env("PLOT_TITLE", "BET 2026 report-ready figures"), "\""),
    "format: html",
    "---",
    "",
    "# Report-ready figures",
    ""
  )
  if (nrow(figure_index)) {
    for (i in seq_len(nrow(figure_index))) {
      label <- row_value(figure_index, "label", i, row_value(figure_index, "figure", i, paste("Figure", i)))
      rel_path <- row_value(figure_index, "relative_path", i, row_value(figure_index, "file", i, ""))
      caption <- row_value(figure_index, "caption", i, "")
      lines <- c(
        lines,
        paste0("## ", label),
        "",
        paste0("![](", rel_path, ")"),
        "",
        paste0("*", caption, "*"),
        ""
      )
    }
  }
  writeLines(lines, qmd)
  if (nzchar(Sys.which("quarto"))) {
    old <- setwd(report_dir)
    status <- tryCatch(
      system2("quarto", c("render", basename(qmd), "--output", basename(html))),
      finally = setwd(old)
    )
    if (!identical(status, 0L)) writeLines("<html><body><p>Quarto render failed.</p></body></html>", html)
  } else {
    writeLines("<html><body><p>Quarto is not available in this image.</p></body></html>", html)
  }
}

file.copy(file.path(report_dir, "mfclshiny-report-figures.html"), file.path(out_dir, "plot-report.html"), overwrite = TRUE)
file.copy(file.path(report_dir, "mfclshiny-report-figures.qmd"), file.path(out_dir, "plot-report.qmd"), overwrite = TRUE)
if (dir.exists(file.path(report_dir, "figures"))) {
  dir.create(file.path(out_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(file.path(report_dir, "figures"), full.names = TRUE), file.path(out_dir, "figures"), overwrite = TRUE)
}
if (dir.exists(file.path(report_dir, "tables"))) {
  dir.create(file.path(out_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(file.path(report_dir, "tables"), full.names = TRUE), file.path(out_dir, "tables"), overwrite = TRUE)
}
file.copy(file.path(report_dir, "figure-index.csv"), file.path(out_dir, "figure-index.csv"), overwrite = TRUE)
file.copy(file.path(report_dir, "table-index.csv"), file.path(out_dir, "table-index.csv"), overwrite = TRUE)
for (name in c("mfclshiny-report-summary.csv", "report-files.csv")) {
  src <- file.path(report_dir, name)
  if (file.exists(src)) file.copy(src, file.path(out_dir, name), overwrite = TRUE)
}

summary <- data.frame(
  payloads = length(payload_list),
  figures = length(unique(figure_index$figure)),
  figure_files = nrow(figure_index),
  tables = if (nrow(table_index)) length(unique(table_index$table)) else 0L,
  html = file.exists(file.path(out_dir, "plot-report.html")),
  stringsAsFactors = FALSE
)
write.csv(summary, file.path(out_dir, "plot-summary.csv"), row.names = FALSE)
