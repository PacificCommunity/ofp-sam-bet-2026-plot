`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

slug <- function(x, default = "figure") {
  x <- tolower(gsub("[^A-Za-z0-9]+", "-", as.character(x %||% default)))
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) default else x
}

html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

first_text <- function(x, default = "") {
  value <- tryCatch(as.character(x), error = function(e) character())
  if (!length(value) || is.na(value[[1]]) || !nzchar(value[[1]])) default else value[[1]]
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

safe_array_to_df <- function(x, value_col = "data") {
  out <- tryCatch(as.data.frame(x), error = function(e) NULL)
  if (!is.null(out)) return(out)
  d <- dim(x)
  if (is.null(d) || length(d) == 0) {
    vals <- suppressWarnings(as.numeric(x))
    out <- data.frame(data = vals)
    names(out)[[1]] <- value_col
    return(out)
  }
  dn <- tryCatch(dimnames(x), error = function(e) NULL)
  if (is.null(dn)) dn <- vector("list", length(d))
  if (length(dn) < length(d)) {
    dn <- c(dn, vector("list", length(d) - length(dn)))
  } else if (length(dn) > length(d)) {
    dn <- dn[seq_along(d)]
  }
  dim_cols <- names(dn)
  if (is.null(dim_cols) || length(dim_cols) != length(d) || any(!nzchar(dim_cols))) {
    dim_cols <- paste0("dim", seq_along(d))
  }
  grid <- expand.grid(lapply(d, seq_len), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  names(grid) <- dim_cols
  for (i in seq_along(d)) {
    labels <- dn[[i]]
    if (is.null(labels) || length(labels) != d[[i]]) {
      labels <- as.character(seq_len(d[[i]]))
    } else {
      labels <- as.character(labels)
    }
    grid[[dim_cols[[i]]]] <- labels[grid[[dim_cols[[i]]]]]
  }
  grid[[value_col]] <- as.vector(x)
  grid
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

to_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

flq_slot <- function(payload_list, slot_name) {
  rows <- lapply(payload_list, function(item) {
    rep <- tryCatch(item$payload$data$RepOut, error = function(e) NULL)
    if (is.null(rep)) return(data.frame())
    obj <- tryCatch(slot(rep, slot_name), error = function(e) NULL)
    if (is.null(obj)) return(data.frame())
    df <- safe_array_to_df(obj)
    if (!nrow(df) || !"data" %in% names(df)) return(data.frame())
    df$Scenario <- item$label
    df$source_payload <- basename(item$file)
    df
  })
  out <- bind_rows_fill(rows)
  if (!nrow(out)) return(out)
  for (name in intersect(c("age", "year", "unit", "season", "area", "iter", "data"), names(out))) {
    out[[name]] <- to_numeric(out[[name]])
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
  df <- df[is.finite(df$unit), , drop = FALSE]
  if (!nrow(df)) return(numeric())
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

save_table <- function(df, output_dir, id, label, caption) {
  table_dir <- file.path(output_dir, "tables")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  file <- paste0(slug(id), ".csv")
  utils::write.csv(df, file.path(table_dir, file), row.names = FALSE)
  data.frame(
    table = gsub("-", "_", slug(id)),
    file = file,
    relative_path = file.path("tables", file),
    label = label,
    caption = caption,
    description = caption,
    format = "csv",
    rows = nrow(df),
    status = "ok",
    stringsAsFactors = FALSE
  )
}

annual_sum <- function(df) {
  df <- df[is.finite(df$year), , drop = FALSE]
  if (!nrow(df)) return(df)
  has_area <- "area" %in% names(df) && any(is.finite(df$area))
  has_season <- "season" %in% names(df) && any(is.finite(df$season))
  by <- c("Scenario", "year")
  if (has_area) by <- c(by, "area")
  if (has_season) by <- c(by, "season")
  summed <- stats::aggregate(data ~ ., df[, c(by, "data"), drop = FALSE], sum, na.rm = TRUE)
  stats::aggregate(data ~ Scenario + year, summed, mean, na.rm = TRUE)
}

annual_mean <- function(df) {
  df <- df[is.finite(df$year), , drop = FALSE]
  if (!nrow(df)) return(df)
  stats::aggregate(data ~ Scenario + year, df, mean, na.rm = TRUE)
}

key_series <- function(payload_list) {
  adult <- annual_sum(flq_slot(payload_list, "adultBiomass"))
  adult_nofish <- annual_sum(flq_slot(payload_list, "adultBiomass_nofish"))
  rec <- annual_sum(flq_slot(payload_list, "rec_region"))
  fbar <- annual_mean(flq_slot(payload_list, "AggregateF"))

  rows <- list()
  if (nrow(adult)) {
    rows[[length(rows) + 1L]] <- transform(adult, metric = "Adult biomass (kt)", value = data / 1000)
  }
  if (nrow(adult) && nrow(adult_nofish)) {
    names(adult)[names(adult) == "data"] <- "adult"
    names(adult_nofish)[names(adult_nofish) == "data"] <- "nofish"
    dep <- merge(adult, adult_nofish, by = c("Scenario", "year"))
    dep$value <- dep$adult / dep$nofish
    rows[[length(rows) + 1L]] <- data.frame(
      Scenario = dep$Scenario,
      year = dep$year,
      metric = "Spawning depletion",
      value = dep$value,
      stringsAsFactors = FALSE
    )
  }
  if (nrow(rec)) {
    rows[[length(rows) + 1L]] <- transform(rec, metric = "Recruitment (millions)", value = data / 1e6)
  }
  if (nrow(fbar)) {
    rows[[length(rows) + 1L]] <- transform(fbar, metric = "Fishing mortality", value = data)
  }
  out <- bind_rows_fill(rows)
  if (!nrow(out)) return(out)
  out <- out[, intersect(c("Scenario", "year", "metric", "value"), names(out)), drop = FALSE]
  out[is.finite(out$value), , drop = FALSE]
}

latest_by_metric <- function(df) {
  if (!nrow(df)) return(df)
  rows <- by(df, interaction(df$Scenario, df$metric, drop = TRUE), function(x) {
    x <- x[order(x$year), , drop = FALSE]
    x[nrow(x), , drop = FALSE]
  })
  do.call(rbind, rows)
}

build_key_quantities <- function(key_df) {
  if (!nrow(key_df)) return(NULL)
  ggplot2::ggplot(key_df, ggplot2::aes(x = year, y = value, colour = Scenario)) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = "Year", y = NULL) +
    theme_report()
}

build_depletion <- function(key_df) {
  df <- key_df[key_df$metric == "Spawning depletion", , drop = FALSE]
  if (!nrow(df)) return(NULL)
  ggplot2::ggplot(df, ggplot2::aes(x = year, y = value, colour = Scenario)) +
    ggplot2::geom_hline(yintercept = c(0.2, 0.5), colour = "#8b98a5", linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.95) +
    ggplot2::labs(x = "Year", y = "Spawning depletion") +
    theme_report()
}

build_cpue <- function(payload_list) {
  obs <- flq_slot(payload_list, "cpue_obs")
  fit <- flq_slot(payload_list, "cpue_pred")
  if (!nrow(obs) || !nrow(fit)) return(NULL)
  names(obs)[names(obs) == "data"] <- "Observed"
  names(fit)[names(fit) == "data"] <- "Predicted"
  keys <- intersect(c("Scenario", "age", "year", "unit", "season", "area", "iter"), intersect(names(obs), names(fit)))
  df <- merge(obs, fit, by = keys)
  df <- df[is.finite(df$Observed) & is.finite(df$Predicted), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  df$data <- df$Observed
  keep <- top_units(df, 12)
  df <- df[df$unit %in% keep, , drop = FALSE]
  if (!nrow(df)) return(NULL)
  df$fishery <- fishery_label(df$unit)
  df$year_season <- df$year + (df$season - 1) / 4
  ggplot2::ggplot(df, ggplot2::aes(x = year_season)) +
    ggplot2::geom_point(ggplot2::aes(y = Observed), colour = "#586270", alpha = 0.45, size = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = Predicted, colour = Scenario), linewidth = 0.8, alpha = 0.9) +
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
  df <- df[is.finite(df$Observed) & is.finite(df$Predicted), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  df$data <- df$Observed
  keep <- top_units(df, 12)
  df <- df[df$unit %in% keep, , drop = FALSE]
  if (!nrow(df)) return(NULL)
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
  df <- bind_rows_fill(list(len, wgt))
  df <- df[is.finite(df$age) & is.finite(df$data), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  ggplot2::ggplot(df, ggplot2::aes(x = age, y = data, colour = Scenario, linetype = factor(season))) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    ggplot2::facet_wrap(~Quantity, scales = "free_y", ncol = 1) +
    ggplot2::labs(x = "Age", y = NULL, linetype = "Season") +
    theme_report()
}

build_regional_series <- function(payload_list, slot_name, y_label, scale = 1) {
  df <- flq_slot(payload_list, slot_name)
  if (!nrow(df) || !"area" %in% names(df)) return(NULL)
  df <- df[is.finite(df$area) & is.finite(df$year), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  df <- stats::aggregate(data ~ Scenario + year + area, df, sum, na.rm = TRUE)
  df$data <- df$data * scale
  df$Region <- paste("Region", df$area)
  ggplot2::ggplot(df, ggplot2::aes(x = year, y = data, colour = Scenario)) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    ggplot2::facet_wrap(~Region, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = "Year", y = y_label) +
    theme_report()
}

write_plot_review_html <- function(figure_index, table_index, out_dir, title, species_label, assessment_year) {
  html_file <- file.path(out_dir, "plot-report.html")
  figure_index <- as.data.frame(figure_index %||% data.frame(), stringsAsFactors = FALSE)
  table_index <- as.data.frame(table_index %||% data.frame(), stringsAsFactors = FALSE)
  tabs <- if (nrow(figure_index) && "description" %in% names(figure_index)) {
    as.character(figure_index$description)
  } else {
    rep("Figures", nrow(figure_index))
  }
  tabs[!nzchar(tabs) | is.na(tabs)] <- "Figures"
  figure_index$.tab <- tabs

  by_tab_cards <- character()
  if (nrow(figure_index)) {
    for (tab in unique(figure_index$.tab)) {
      idx <- which(figure_index$.tab == tab)
      cards <- character()
      for (i in idx) {
        label <- row_value(figure_index, "label", i, row_value(figure_index, "figure", i, paste("Figure", i)))
        rel_path <- row_value(figure_index, "relative_path", i, row_value(figure_index, "file", i, ""))
        caption <- row_value(figure_index, "caption", i, "")
        figure_id <- row_value(figure_index, "figure", i, "")
        format <- row_value(figure_index, "format", i, tools::file_ext(rel_path))
        cards <- c(cards, sprintf(
          paste0(
            '<article class="figure-card">',
            '<div class="card-head"><span>%s</span><code>%s</code></div>',
            '<h3>%s</h3>',
            '<a href="%s"><img src="%s" alt="%s"></a>',
            '<p>%s</p>',
            '<div class="file-row"><a href="%s">Open %s</a></div>',
            '</article>'
          ),
          html_escape(tab), html_escape(figure_id), html_escape(label),
          html_escape(rel_path), html_escape(rel_path), html_escape(caption),
          html_escape(caption), html_escape(rel_path), html_escape(toupper(format))
        ))
      }
      by_tab_cards <- c(by_tab_cards, sprintf(
        '<section class="tab-section"><div class="section-title"><h2>%s</h2><span>%d figures</span></div><div class="grid">%s</div></section>',
        html_escape(tab), length(idx), paste(cards, collapse = "\n")
      ))
    }
  }
  table_cards <- character()
  if (nrow(table_index)) {
    for (i in seq_len(nrow(table_index))) {
      label <- row_value(table_index, "label", i, row_value(table_index, "table", i, paste("Table", i)))
      rel_path <- row_value(table_index, "relative_path", i, row_value(table_index, "file", i, ""))
      caption <- row_value(table_index, "caption", i, "")
      table_cards <- c(table_cards, sprintf(
        '<li><a href="%s">%s</a><span>%s</span></li>',
        html_escape(rel_path), html_escape(label), html_escape(caption)
      ))
    }
  }
  lines <- c(
    "<!doctype html>",
    '<html lang="en">',
    "<head>",
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    sprintf("<title>%s</title>", html_escape(title)),
    "<style>",
    "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;margin:0;background:#f4f8fa;color:#16293a}",
    "main{max-width:1240px;margin:0 auto;padding:32px 24px 56px}",
    "header{margin-bottom:26px;border-bottom:1px solid #d8e6ee;padding-bottom:18px}",
    "h1{font-size:32px;line-height:1.15;margin:0 0 8px}",
    ".meta{color:#5c7184;font-weight:600}",
    ".summary{display:flex;gap:10px;flex-wrap:wrap;margin-top:16px}",
    ".summary span{background:#fff;border:1px solid #d9e5ec;border-radius:999px;padding:6px 10px;font-size:12px;font-weight:800;color:#31566f}",
    ".tab-section{margin-top:28px}",
    ".section-title{display:flex;align-items:baseline;justify-content:space-between;gap:14px;margin-bottom:12px}",
    ".section-title h2{font-size:20px;margin:0}",
    ".section-title span{color:#617589;font-weight:700;font-size:13px}",
    ".grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(420px,1fr));gap:22px}",
    ".figure-card{background:#fff;border:1px solid #d9e5ec;border-radius:8px;padding:16px;box-shadow:0 10px 28px rgba(18,41,58,.055)}",
    ".card-head{display:flex;justify-content:space-between;align-items:center;gap:8px;margin-bottom:8px;color:#5c7184;font-size:12px;font-weight:800;text-transform:uppercase}",
    ".card-head code{background:#eef5f8;border:1px solid #d7e5ed;border-radius:999px;color:#31566f;font-size:11px;padding:2px 7px;text-transform:none}",
    ".figure-card h3{font-size:17px;margin:0 0 12px}",
    ".figure-card img{display:block;width:100%;height:auto;border:1px solid #e5edf2}",
    ".figure-card p{font-size:13px;line-height:1.45;color:#445d72;margin:12px 0 0}",
    ".file-row{margin-top:12px}",
    ".file-row a{border:1px solid #cddfe9;border-radius:999px;color:#1f678f;display:inline-block;font-size:12px;font-weight:800;padding:4px 9px;text-decoration:none}",
    ".tables{margin-top:28px;background:#fff;border:1px solid #d9e5ec;border-radius:8px;padding:18px}",
    ".tables h2{font-size:17px;margin:0 0 12px}",
    ".tables ul{list-style:none;padding:0;margin:0;display:grid;gap:10px}",
    ".tables li{display:flex;gap:10px;align-items:baseline;flex-wrap:wrap}",
    ".tables a{font-weight:700;color:#175a86;text-decoration:none}",
    ".tables span{color:#5c7184;font-size:13px}",
    "</style>",
    "</head>",
    "<body><main>",
    sprintf(
      paste0(
        "<header><h1>%s</h1><div class=\"meta\">%s assessment %s</div>",
        "<div class=\"summary\"><span>%d figures</span><span>%d tables</span><span>%d source tabs</span></div></header>"
      ),
      html_escape(title), html_escape(species_label), html_escape(assessment_year),
      length(unique(figure_index$figure)), if (nrow(table_index)) length(unique(table_index$table)) else 0L,
      length(unique(figure_index$.tab))
    ),
    by_tab_cards
  )
  if (length(table_cards)) {
    lines <- c(lines, '<section class="tables"><h2>Tables</h2><ul>', table_cards, "</ul></section>")
  }
  lines <- c(lines, "</main></body></html>")
  writeLines(lines, html_file)
  html_file
}

input_dir <- env("INPUT_DIR", "inputs")
out_dir <- env("OUTPUT_DIR", "outputs")
title <- env("PLOT_TITLE", "BET 2026 report-ready figures")
species_code <- env("FLOW_SPECIES", "BET")
species_label <- env("FLOW_SPECIES_LABEL", "bigeye tuna")
assessment_year <- env("FLOW_ASSESSMENT_YEAR", "2026")
work_dir <- file.path(tempdir(), paste0("report-figures-", Sys.getpid()))

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

payload_list <- payloads(input_dir)
if (!length(payload_list)) stop("No model_payload.rds files found in upstream inputs.", call. = FALSE)

payload_index <- data.frame(
  model_label = vapply(payload_list, `[[`, character(1), "label"),
  payload_file = vapply(payload_list, `[[`, character(1), "file"),
  stringsAsFactors = FALSE
)

payload_folders <- vapply(payload_list, `[[`, character(1), "folder")
base_result <- NULL
used_app_report <- FALSE
if (requireNamespace("mfclshiny", quietly = TRUE) &&
    "build_app_report_figures" %in% getNamespaceExports("mfclshiny")) {
  base_args <- list(
    model_dir = input_dir,
    folders = payload_folders,
    output_dir = work_dir,
    title = title,
    formats = "png",
    build_payloads = FALSE,
    overwrite = TRUE,
    render_html = TRUE,
    qmd_file = "mfclshiny-app-report-figures.qmd",
    html_file = "mfclshiny-app-report-figures.html",
    figure_dir = "figures",
    table_dir = "tables",
    copy_legacy_root = FALSE,
    species_code = species_code,
    species_label = species_label,
    assessment_year = assessment_year,
    max_fisheries = as.integer(env("PLOT_MAX_FISHERIES", "18"))
  )
  base_result <- tryCatch(
    call_with_supported_args(mfclshiny::build_app_report_figures, base_args),
    error = function(e) {
      message("mfclshiny::build_app_report_figures did not produce figures: ", conditionMessage(e))
      NULL
    }
  )
  used_app_report <- !is.null(base_result) && is.data.frame(base_result$figures) && nrow(base_result$figures) > 0L
}

if ((is.null(base_result) || !is.data.frame(base_result$figures) || !nrow(base_result$figures)) &&
    requireNamespace("mfclshiny", quietly = TRUE) &&
    "build_report_figures" %in% getNamespaceExports("mfclshiny")) {
  base_args <- list(
    model_dir = input_dir,
    folders = payload_folders,
    output_dir = work_dir,
    title = title,
    figure_basename = "key-quantities",
    formats = "png",
    build_payloads = FALSE,
    overwrite = TRUE,
    render_html = TRUE,
    qmd_file = "mfclshiny-report-figures.qmd",
    html_file = "mfclshiny-report-figures.html",
    figure_dir = "figures",
    table_dir = "tables",
    copy_legacy_root = FALSE,
    plot_style = "shiny_stock",
    species_code = species_code,
    species_label = species_label,
    assessment_year = assessment_year
  )
  base_result <- tryCatch(
    call_with_supported_args(mfclshiny::build_report_figures, base_args),
    error = function(e) {
      message("mfclshiny::build_report_figures did not produce figures: ", conditionMessage(e))
      NULL
    }
  )
}

figure_index <- if (!is.null(base_result) && is.data.frame(base_result$figures)) base_result$figures else data.frame()
table_index <- if (!is.null(base_result) && is.data.frame(base_result$tables)) base_result$tables else data.frame()
existing_figures <- unique(as.character(figure_index$figure %||% character()))

key_df <- key_series(payload_list)
extra_specs <- list(
  list(id = "key-quantities", label = "Key quantities", plot = build_key_quantities(key_df), caption = paste("Key annual quantities for the selected", species_label, "model payloads, including depletion, adult biomass, recruitment, and fishing mortality.")),
  list(id = "spawning-depletion", label = "Spawning depletion", plot = build_depletion(key_df), caption = paste("Estimated spawning depletion for the selected", species_label, "models. Dashed reference lines are shown at 20% and 50% of unfished spawning biomass.")),
  list(id = "cpue-fits", label = "CPUE fits", plot = build_cpue(payload_list), caption = paste("Observed and fitted CPUE series by fishery for the selected", species_label, "model payloads.")),
  list(id = "catch-fits", label = "Catch fits", plot = build_catch(payload_list), caption = paste("Observed and fitted catch series by fishery for the selected", species_label, "model payloads.")),
  list(id = "selectivity", label = "Selectivity", plot = build_selectivity(payload_list), caption = paste("Selectivity-at-age curves by fishery for the selected", species_label, "model payloads.")),
  list(id = "size-at-age", label = "Size at age", plot = build_size_at_age(payload_list), caption = paste("Mean length and mean weight at age from the selected", species_label, "model payloads.")),
  list(id = "biomass-by-region", label = "Biomass by region", plot = build_regional_series(payload_list, "adultBiomass", "Adult biomass"), caption = paste("Adult biomass time series by model and region for", species_label, ".")),
  list(id = "recruitment-by-region", label = "Recruitment by region", plot = build_regional_series(payload_list, "rec_region", "Recruitment (millions)", 1 / 1e6), caption = paste("Recruitment time series by model and region for", species_label, ", in millions of fish."))
)

extra_rows <- list()
include_derived_fallback <- !isTRUE(used_app_report) ||
  tolower(env("PLOT_INCLUDE_DERIVED_FALLBACK", "false")) %in% c("true", "1", "yes", "y", "on")
if (isTRUE(include_derived_fallback)) {
  for (spec in extra_specs) {
    spec_figure <- gsub("-", "_", slug(spec$id))
    if (spec_figure %in% existing_figures) next
    if (is.null(spec$plot)) next
    extra_rows[[length(extra_rows) + 1L]] <- save_plot(
      spec$plot,
      output_dir = work_dir,
      id = spec$id,
      label = spec$label,
      caption = spec$caption
    )
  }
}
if (length(extra_rows)) {
  figure_index <- bind_rows_fill(list(figure_index, bind_rows_fill(extra_rows)))
}

table_rows <- list(
  save_table(payload_index, work_dir, "payload-index", "Payload index", "Model payload files used to build these report-ready figures.")
)
if (nrow(key_df)) {
  table_rows[[length(table_rows) + 1L]] <- save_table(key_df, work_dir, "key-quantities-timeseries", "Key quantities time series", "Annual key quantities used in the generated report-ready figures.")
  table_rows[[length(table_rows) + 1L]] <- save_table(latest_by_metric(key_df), work_dir, "key-quantities-latest", "Latest key quantities", "Latest available annual values for each key quantity and model.")
}
table_index <- bind_rows_fill(list(table_index, bind_rows_fill(table_rows)))

if (!nrow(figure_index)) {
  stop("No report-ready figures were produced from the model payloads.", call. = FALSE)
}

dir.create(file.path(out_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
if (dir.exists(file.path(work_dir, "figures"))) {
  invisible(file.copy(list.files(file.path(work_dir, "figures"), full.names = TRUE), file.path(out_dir, "figures"), overwrite = TRUE))
}
if (dir.exists(file.path(work_dir, "tables"))) {
  invisible(file.copy(list.files(file.path(work_dir, "tables"), full.names = TRUE), file.path(out_dir, "tables"), overwrite = TRUE))
}
root_files <- list.files(work_dir, full.names = TRUE, recursive = FALSE)
root_files <- root_files[file.info(root_files)$isdir %in% FALSE]
if (length(root_files)) {
  invisible(file.copy(root_files, out_dir, overwrite = TRUE))
}

write.csv(figure_index, file.path(out_dir, "figure-index.csv"), row.names = FALSE)
write.csv(table_index, file.path(out_dir, "table-index.csv"), row.names = FALSE)
invisible(write_plot_review_html(figure_index, table_index, out_dir, title, species_label, assessment_year))

summary <- data.frame(
  payloads = length(payload_list),
  figures = length(unique(figure_index$figure)),
  figure_files = nrow(figure_index),
  tables = if (nrow(table_index)) length(unique(table_index$table)) else 0L,
  html = file.exists(file.path(out_dir, "plot-report.html")),
  stringsAsFactors = FALSE
)
write.csv(summary, file.path(out_dir, "plot-summary.csv"), row.names = FALSE)
