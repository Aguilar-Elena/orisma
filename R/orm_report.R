#' Generate all ORISMA outputs and reports
#'
#' @description
#' `orm_report()` takes a completed `orisma_result` object and generates
#' the full set of outputs:
#'
#' **Data files**
#' - `orisma_corpus.csv` - all records after deduplication
#' - `orisma_matrix.csv` - binary risk category matrix
#' - `orisma_indicators.csv` - WRDI, RCS, MGP per category
#' - `dedup_log.csv` - deduplication statistics
#' - `prisma_log.csv` - PRISMA-compatible flow log
#' - `analysis.orisma` - reproducibility certificate (JSON)
#'
#' **Visualisations** (PNG + SVG)
#' - Heatmap: risk categories × materials / clusters
#' - Co-occurrence matrix plot
#' - Temporal trend by category
#' - WRDI bar chart
#' - RCS bubble chart
#' - Gap map (WRDI vs RCS scatter)
#'
#' **Reports**
#' - `orisma_report.html` - interactive executive report (no R needed to open)
#' - `orisma_indicators.csv` - flat table for Excel / manual use
#'
#' @param result An `orisma_result` object from [orm_analyse()] or [orm_run()].
#' @param lang Character. `"en"` or `"es"`. Report language.
#' @param out_dir Character. Output directory. Created if it does not exist.
#'   Default `"orisma_output"`.
#' @param formats Character vector. Which report formats to generate.
#'   Options: `"html"`, `"csv"`, `"plots"`, `"certificate"`. Default: all.
#' @param verbose Logical. Print progress?
#'
#' @return Invisibly returns the output directory path.
#'
#' @examples
#' \dontrun{
#' refs   <- orm_load("my_references/")
#' result <- orm_run(refs)
#'
#' # Generate all outputs in Spanish
#' orm_report(result, lang = "es", out_dir = "resultados/")
#'
#' # Only CSV files and reproducibility certificate
#' orm_report(result, formats = c("csv", "certificate"))
#' }
#'
#' @export
orm_report <- function(result,
                       lang     = getOption("orisma.lang", "en"),
                       out_dir  = getOption("orisma.out_dir", "orisma_output"),
                       formats  = c("html", "csv", "plots", "certificate"),
                       verbose  = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)

  if (!inherits(result, "orisma_result")) {
    stop("'result' must be an orisma_result object from orm_analyse() or orm_run().",
         call. = FALSE)
  }

  # Create output directory
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  if (verbose) {
    cli::cli_h1(.msg("phase_report", lang))
    cli::cli_alert_info(.msg("report_start", lang, out_dir = out_dir))
  }

  # ── 1. CSV data files ────────────────────────────────────────────────────────
  if ("csv" %in% formats) {

    # Corpus
    corpus_path <- file.path(out_dir, "orisma_corpus.csv")
    readr::write_csv(result$refs, corpus_path)

    # Binary matrix
    mat_df      <- as.data.frame(result$matrix)
    mat_df      <- cbind(record_id = rownames(result$matrix), mat_df)
    matrix_path <- file.path(out_dir, "orisma_matrix.csv")
    readr::write_csv(mat_df, matrix_path)

    # Indicators
    ind_path <- file.path(out_dir, "orisma_indicators.csv")
    readr::write_csv(result$indicators, ind_path)

    # MGP if available
    if (!is.null(result$MGP)) {
      mgp_path <- file.path(out_dir, "orisma_mgp.csv")
      readr::write_csv(result$MGP, mgp_path)
    }

    # PRISMA log
    prisma_log <- .build_prisma_log(result, lang)
    prisma_path <- file.path(out_dir, "prisma_log.csv")
    readr::write_csv(prisma_log, prisma_path)

    if (verbose) {
      cli::cli_alert_success(paste0("CSV files saved to: ", out_dir))
    }
  }

  # ── 2. Visualisations ────────────────────────────────────────────────────────
  if ("plots" %in% formats) {
    plots_dir <- file.path(out_dir, "plots")
    if (!dir.exists(plots_dir)) dir.create(plots_dir)

    tryCatch({
      # Heatmap: co-occurrence matrix
      .plot_cooccur(result, plots_dir, lang)

      # WRDI bar chart
      .plot_wrdi(result, plots_dir, lang)

      # RCS bubble chart
      .plot_rcs(result, plots_dir, lang)

      # Temporal trend
      if (!is.null(result$temporal)) {
        .plot_temporal(result, plots_dir, lang)
      }

      # Gap map (WRDI vs RCS)
      .plot_gap_map(result, plots_dir, lang)

      if (verbose) {
        cli::cli_alert_success(paste0("Plots saved to: ", plots_dir))
      }
    }, error = function(e) {
      cli::cli_alert_warning(paste0("Some plots could not be generated: ", e$message))
    })
  }

  # ── 3. Reproducibility certificate ──────────────────────────────────────────
  if ("certificate" %in% formats) {
    cert <- .build_certificate(result)
    cert_path <- file.path(out_dir, "analysis.orisma")
    jsonlite::write_json(cert, cert_path, pretty = TRUE, auto_unbox = TRUE)

    if (verbose) {
      cli::cli_alert_success(
        .msg("report_cert", lang, file = basename(cert_path))
      )
    }
  }

  # ── 4. HTML executive report ─────────────────────────────────────────────────
  if ("html" %in% formats) {
    html_path <- file.path(out_dir, "orisma_report.html")
    tryCatch({
      .render_html_report(result, html_path, lang, out_dir)
      if (verbose) {
        cli::cli_alert_success(
          .msg("report_html", lang, file = basename(html_path))
        )
      }
    }, error = function(e) {
      cli::cli_alert_warning(paste0(
        "HTML report could not be rendered: ", e$message,
        "\nCSV outputs are still available."
      ))
    })
  }

  if (verbose) {
    cli::cli_rule()
    cli::cli_alert_success(.msg("report_done", lang, out_dir = out_dir))
  }

  invisible(out_dir)
}


# ── Report internals ──────────────────────────────────────────────────────────

#' @noRd
.build_prisma_log <- function(result, lang) {
  ps <- attr(result, "pipeline_summary")

  data.frame(
    phase = c(
      "Records identified (all databases)",
      "Duplicates removed",
      "Records after deduplication",
      "Records screened",
      "Records included in analysis"
    ),
    n = c(
      if (!is.null(ps)) ps$n_loaded   else result$n_records,
      if (!is.null(ps)) ps$n_removed  else 0L,
      if (!is.null(ps)) ps$n_deduped  else result$n_records,
      if (!is.null(ps)) ps$n_deduped  else result$n_records,
      result$n_records
    ),
    stringsAsFactors = FALSE
  )
}


#' @noRd
.build_certificate <- function(result) {
  list(
    orisma_version  = attr(result, "orisma_version"),
    analysis_date   = format(attr(result, "orisma_created"), "%Y-%m-%d %H:%M:%S"),
    dict_name       = attr(result, "dict_name"),
    dict_version    = attr(result, "dict_version"),
    n_records       = result$n_records,
    n_categories    = result$n_categories,
    WRDI_global     = result$WRDI_global,
    r_version       = paste(R.Version()$major, R.Version()$minor, sep = "."),
    platform        = R.Version()$platform,
    digest_matrix   = digest::digest(result$matrix, algo = "md5"),
    digest_refs     = digest::digest(result$refs, algo = "md5")
  )
}


#' @noRd
.plot_cooccur <- function(result, dir, lang) {
  mat <- result$cooccur_mat
  labels <- result$categories$label

  rownames(mat) <- labels
  colnames(mat) <- labels

  png(file.path(dir, "cooccurrence_heatmap.png"),
      width = 2400, height = 2000, res = 300)
  pheatmap::pheatmap(
    mat,
    cluster_rows  = TRUE,
    cluster_cols  = TRUE,
    color         = grDevices::colorRampPalette(c("white", "#1D9E75"))(50),
    main          = if (lang == "es") "Matriz de co-ocurrencia de riesgos"
                    else "Risk category co-occurrence matrix",
    fontsize      = 9,
    border_color  = "white"
  )
  grDevices::dev.off()
}


#' @noRd
.plot_wrdi <- function(result, dir, lang) {
  df <- result$indicators
  df <- df[!is.na(df$WRDI), ]
  df <- df[order(df$WRDI), ]

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x    = WRDI,
    y    = stats::reorder(label, WRDI),
    fill = WRDI
  )) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_gradient(low = "#9FE1CB", high = "#0F6E56") +
    ggplot2::geom_vline(xintercept = result$WRDI_global,
                        linetype = "dashed", colour = "grey40") +
    ggplot2::labs(
      title = if (lang == "es") "Indice de Desconexion Tecnico-Laboral (WRDI)"
              else "Worker-Risk Disconnection Index (WRDI)",
      subtitle = if (lang == "es")
        paste0("WRDI global: ", result$WRDI_global,
               " * linea punteada = valor global")
      else
        paste0("Global WRDI: ", result$WRDI_global,
               " * dashed line = global value"),
      x = "WRDI (0 = fully connected * 1 = fully disconnected)",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  ggplot2::ggsave(file.path(dir, "wrdi_chart.png"), p,
                  width = 10, height = 6, dpi = 300)
}


#' @noRd
.plot_rcs <- function(result, dir, lang) {
  df <- result$indicators

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x    = RCS,
    y    = pct_records,
    size = n_records,
    colour = RCS > 1
  )) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    ggrepel::geom_text_repel(ggplot2::aes(label = label),
                             size = 3, max.overlaps = 10) +
    ggplot2::scale_colour_manual(
      values = c("TRUE" = "#0F6E56", "FALSE" = "#D85A30"),
      labels = c("TRUE" = "Over-represented", "FALSE" = "Under-represented"),
      name   = NULL
    ) +
    ggplot2::scale_size_continuous(name = "N studies", range = c(3, 12)) +
    ggplot2::labs(
      title = if (lang == "es") "Indice de Saturacion por Categoria (RCS)"
              else "Risk Category Saturation Index (RCS)",
      subtitle = if (lang == "es") "RCS > 1 = sobrerepresentado * < 1 = infrarepresentado"
                 else "RCS > 1 = over-represented * < 1 = under-represented",
      x = "RCS",
      y = if (lang == "es") "% de estudios" else "% of studies"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(file.path(dir, "rcs_chart.png"), p,
                  width = 10, height = 7, dpi = 300)
}


#' @noRd
.plot_temporal <- function(result, dir, lang) {
  df_long <- result$temporal %>%
    tidyr::pivot_longer(
      cols      = dplyr::starts_with("cat_"),
      names_to  = "category",
      values_to = "n"
    ) %>%
    dplyr::mutate(
      category = gsub("^cat_", "", .data$category)
    )

  p <- ggplot2::ggplot(df_long,
                       ggplot2::aes(x = year, y = n, colour = category)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::labs(
      title  = if (lang == "es") "Evolucion temporal por categoria de riesgo"
               else "Temporal trend by risk category",
      x      = if (lang == "es") "Ano" else "Year",
      y      = if (lang == "es") "Numero de estudios" else "Number of studies",
      colour = if (lang == "es") "Categoria" else "Category"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")

  ggplot2::ggsave(file.path(dir, "temporal_trend.png"), p,
                  width = 12, height = 6, dpi = 300)
}


#' @noRd
.plot_gap_map <- function(result, dir, lang) {
  df <- result$indicators

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x      = RCS,
    y      = WRDI,
    size   = n_records,
    colour = RCS
  )) +
    ggplot2::geom_point(alpha = 0.85) +
    ggrepel::geom_text_repel(ggplot2::aes(label = label),
                             size = 3, max.overlaps = 12) +
    ggplot2::geom_hline(yintercept = result$WRDI_global,
                        linetype = "dashed", colour = "grey60") +
    ggplot2::geom_vline(xintercept = 1,
                        linetype = "dashed", colour = "grey60") +
    ggplot2::scale_colour_gradient(low = "#9FE1CB", high = "#0F6E56") +
    ggplot2::scale_size_continuous(range = c(3, 12)) +
    ggplot2::annotate("text", x = max(df$RCS, na.rm=TRUE) * 0.95,
                      y = max(df$WRDI, na.rm=TRUE) * 0.97,
                      label = if (lang == "es") "Alta saturacion\nAlta desconexion"
                              else "High saturation\nHigh disconnection",
                      size = 3, colour = "grey40", hjust = 1) +
    ggplot2::labs(
      title  = if (lang == "es") "Mapa de lagunas * ORISMA Gap Map"
               else "ORISMA Gap Map",
      subtitle = if (lang == "es")
        "Cuadrante superior derecho = sobreestudiado pero sin datos de trabajadores"
      else
        "Upper-right quadrant = over-studied but lacking worker exposure data",
      x      = "RCS (Risk Category Saturation)",
      y      = "WRDI (Worker-Risk Disconnection)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::guides(colour = "none")

  ggplot2::ggsave(file.path(dir, "gap_map.png"), p,
                  width = 10, height = 8, dpi = 300)
}


#' @noRd
.render_html_report <- function(result, html_path, lang, out_dir) {
  # Use a minimal self-contained Rmarkdown template
  template <- system.file("extdata", "report_template.Rmd",
                          package = "orisma")

  if (!file.exists(template)) {
    # Fallback: generate minimal HTML directly
    .write_minimal_html(result, html_path, lang)
    return(invisible())
  }

  rmarkdown::render(
    input       = template,
    output_file = html_path,
    params      = list(result = result, lang = lang, out_dir = out_dir),
    quiet       = TRUE
  )
}


#' @noRd
.write_minimal_html <- function(result, path, lang) {
  title <- if (lang == "es") "Informe ORISMA" else "ORISMA Report"
  ind   <- result$indicators

  rows <- paste0(
    apply(ind, 1, function(r) {
      paste0("<tr><td>", r["label"], "</td><td>", r["n_records"],
             "</td><td>", r["pct_records"], "%</td><td>",
             r["WRDI"], "</td><td>", r["RCS"], "</td></tr>")
    }),
    collapse = "\n"
  )

  html <- paste0(
    "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
    "<title>", title, "</title>",
    "<style>body{font-family:sans-serif;max-width:900px;margin:2rem auto;padding:0 1rem}",
    "table{border-collapse:collapse;width:100%}",
    "th,td{border:1px solid #ddd;padding:8px;text-align:left}",
    "th{background:#0F6E56;color:white}tr:nth-child(even){background:#f9f9f9}</style>",
    "</head><body>",
    "<h1>ORISMA</h1>",
    "<h2>", title, "</h2>",
    "<p><strong>", if (lang=="es") "Registros analizados" else "Records analysed",
    ":</strong> ", result$n_records, "</p>",
    "<p><strong>WRDI (global):</strong> ", result$WRDI_global,
    " (", round(result$WRDI_global * 100, 1), "%)</p>",
    "<h3>", if (lang=="es") "Indicadores por categoria" else "Indicators by category", "</h3>",
    "<table><thead><tr>",
    "<th>", if (lang=="es") "Categoria" else "Category", "</th>",
    "<th>N</th><th>%</th><th>WRDI</th><th>RCS</th>",
    "</tr></thead><tbody>", rows, "</tbody></table>",
    "<p style='color:grey;font-size:12px;margin-top:2rem'>",
    "Generated by orisma v", as.character(utils::packageVersion("orisma")),
    " &middot; Dr. Ra&uacute;l Aguilar-Elena &middot; GPRL &middot; VIU",
    "</p></body></html>"
  )

  writeLines(html, path)
}
