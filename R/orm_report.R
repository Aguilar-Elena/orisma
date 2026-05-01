#' Generate all ORISMA outputs and reports
#'
#' @description
#' `orm_report()` takes a completed `orisma_result` object and generates
#' the full set of outputs including improved visualisations and a rich
#' bilingual HTML executive report.
#'
#' @param result An `orisma_result` object from [orm_analyse()] or [orm_run()].
#' @param lang Character. `"en"` or `"es"`. Report language.
#' @param out_dir Character. Output directory. Created if it does not exist.
#' @param formats Character vector. Which outputs to generate.
#'   Options: `"html"`, `"csv"`, `"plots"`, `"certificate"`. Default: all.
#' @param min_records Integer. Minimum records for a category to appear in
#'   plots. Default `1`.
#' @param top_n Integer. Number of top categories to show in temporal plot.
#'   Default `8`.
#' @param verbose Logical. Print progress?
#'
#' @return Invisibly returns the output directory path.
#' @export
orm_report <- function(result,
                       lang        = getOption("orisma.lang", "en"),
                       out_dir     = getOption("orisma.out_dir", "orisma_output"),
                       formats     = c("html", "csv", "plots", "certificate"),
                       min_records = 1L,
                       top_n       = 8L,
                       verbose     = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)
  if (!inherits(result, "orisma_result")) {
    stop("'result' must be an orisma_result object from orm_analyse() or orm_run().",
         call. = FALSE)
  }

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  if (verbose) {
    cli::cli_h1(orm_msg("phase_report", lang))
    cli::cli_alert_info(orm_msg("report_start", lang, out_dir = out_dir))
  }

  # ── 1. CSV files ─────────────────────────────────────────────────────────────
  if ("csv" %in% formats) {
    readr::write_csv(result$refs,       file.path(out_dir, "orisma_corpus.csv"))
    readr::write_csv(result$indicators, file.path(out_dir, "orisma_indicators.csv"))

    mat_df <- as.data.frame(result$matrix)
    mat_df <- cbind(record_id = rownames(result$matrix), mat_df)
    readr::write_csv(mat_df, file.path(out_dir, "orisma_matrix.csv"))

    if (!is.null(result$MGP)) {
      readr::write_csv(result$MGP, file.path(out_dir, "orisma_mgp.csv"))
    }

    prisma_log <- .build_prisma_log(result, lang)
    readr::write_csv(prisma_log, file.path(out_dir, "prisma_log.csv"))

    if (verbose) cli::cli_alert_success(paste0("CSV files saved to: ", out_dir))
  }

  # ── 2. Plots ──────────────────────────────────────────────────────────────────
  if ("plots" %in% formats) {
    plots_dir <- file.path(out_dir, "plots")
    if (!dir.exists(plots_dir)) dir.create(plots_dir)

    tryCatch({
      .plot_wrdi_v2(result, plots_dir, lang, min_records)
      .plot_rcs_v2(result, plots_dir, lang, min_records)
      .plot_gap_map_v2(result, plots_dir, lang, min_records)
      .plot_temporal_v2(result, plots_dir, lang, top_n)
      .plot_cooccur_v2(result, plots_dir, lang, min_records)
      .plot_distribution_v2(result, plots_dir, lang, min_records)
      if (verbose) cli::cli_alert_success(paste0("Plots saved to: ", plots_dir))
    }, error = function(e) {
      cli::cli_alert_warning(paste0("Some plots could not be generated: ", e$message))
    })
  }

  # ── 3. Certificate ────────────────────────────────────────────────────────────
  if ("certificate" %in% formats) {
    cert      <- .build_certificate(result)
    cert_path <- file.path(out_dir, "analysis.orisma")
    jsonlite::write_json(cert, cert_path, pretty = TRUE, auto_unbox = TRUE)
    if (verbose) {
      cli::cli_alert_success(orm_msg("report_cert", lang, file = basename(cert_path)))
    }
  }

  # ── 4. HTML report ────────────────────────────────────────────────────────────
  if ("html" %in% formats) {
    html_path <- file.path(out_dir, "orisma_report.html")
    tryCatch({
      .write_html_report_v2(result, html_path, lang, out_dir, min_records)
      if (verbose) {
        cli::cli_alert_success(orm_msg("report_html", lang, file = basename(html_path)))
      }
    }, error = function(e) {
      cli::cli_alert_warning(paste0("HTML report error: ", e$message))
    })
  }

  if (verbose) {
    cli::cli_rule()
    cli::cli_alert_success(orm_msg("report_done", lang, out_dir = out_dir))
  }

  invisible(out_dir)
}


# =============================================================================
# IMPROVED PLOT FUNCTIONS
# =============================================================================

#' @noRd
.plot_wrdi_v2 <- function(result, dir, lang, min_records = 1) {
  df <- result$indicators %>%
    dplyr::filter(.data$n_records >= min_records, !is.na(.data$WRDI)) %>%
    dplyr::arrange(.data$WRDI) %>%
    dplyr::mutate(
      label_w = stringr::str_wrap(.data$label, 35),
      fill_col = dplyr::case_when(
        .data$WRDI >= 0.8 ~ "high",
        .data$WRDI >= 0.5 ~ "mid",
        TRUE              ~ "low"
      )
    )

  title_txt    <- if (lang == "es") "Indice de Desconexion Tecnico-Laboral (WRDI)"
                  else "Worker-Risk Disconnection Index (WRDI)"
  subtitle_txt <- if (lang == "es")
    paste0("WRDI global: ", result$WRDI_global,
           " * linea punteada = valor global\n0 = todos los estudios incluyen trabajadores * 1 = ningun estudio incluye trabajadores")
  else
    paste0("Global WRDI: ", result$WRDI_global,
           " * dashed line = global value\n0 = all studies include worker data * 1 = no study includes worker data")

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x    = .data$WRDI,
    y    = reorder(.data$label_w, .data$WRDI),
    fill = .data$fill_col
  )) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$WRDI, 2)),
                       hjust = -0.15, size = 3, colour = "grey30") +
    ggplot2::geom_vline(xintercept = result$WRDI_global,
                        linetype = "dashed", colour = "grey40", linewidth = 0.8) +
    ggplot2::scale_fill_manual(
      values = c("high" = "#0F6E56", "mid" = "#4DAF8D", "low" = "#9FE1CB")
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.25)) +
    ggplot2::labs(title = title_txt, subtitle = subtitle_txt,
                  x = "WRDI", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      plot.subtitle      = ggplot2::element_text(size = 9, colour = "grey40")
    )

  ggplot2::ggsave(file.path(dir, "wrdi_chart.png"), p,
                  width = 12, height = max(6, nrow(df) * 0.45), dpi = 300,
                  limitsize = FALSE)
}


#' @noRd
.plot_rcs_v2 <- function(result, dir, lang, min_records = 1) {
  df <- result$indicators %>%
    dplyr::filter(.data$n_records >= min_records) %>%
    dplyr::mutate(
      label_w   = stringr::str_wrap(.data$label, 25),
      saturated = .data$RCS > 1
    )

  title_txt    <- if (lang == "es") "Indice de Saturacion por Categoria (RCS)"
                  else "Risk Category Saturation Index (RCS)"
  subtitle_txt <- if (lang == "es") "RCS > 1 = sobrerepresentado * < 1 = infrarepresentado"
                  else "RCS > 1 = over-represented * RCS < 1 = under-represented"

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x      = .data$RCS,
    y      = .data$pct_records,
    size   = .data$n_records,
    colour = .data$saturated
  )) +
    ggplot2::geom_point(alpha = 0.85) +
    ggrepel::geom_text_repel(ggplot2::aes(label = .data$label_w),
                              size = 3, max.overlaps = 20,
                              box.padding = 0.4) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                        colour = "grey50", linewidth = 0.7) +
    ggplot2::scale_colour_manual(
      values = c("TRUE" = "#0F6E56", "FALSE" = "#D85A30"),
      labels = if (lang == "es") c("TRUE" = "Sobrerepresentado",
                                    "FALSE" = "Infrarepresentado")
               else c("TRUE" = "Over-represented",
                      "FALSE" = "Under-represented"),
      name = NULL
    ) +
    ggplot2::scale_size_continuous(
      name   = if (lang == "es") "N estudios" else "N studies",
      range  = c(3, 14)
    ) +
    ggplot2::labs(
      title    = title_txt,
      subtitle = subtitle_txt,
      x        = "RCS",
      y        = if (lang == "es") "% de estudios" else "% of studies"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "right",
      plot.subtitle   = ggplot2::element_text(size = 9, colour = "grey40")
    )

  ggplot2::ggsave(file.path(dir, "rcs_chart.png"), p,
                  width = 12, height = 8, dpi = 300)
}


#' @noRd
.plot_gap_map_v2 <- function(result, dir, lang, min_records = 1) {
  df <- result$indicators %>%
    dplyr::filter(.data$n_records >= min_records, !is.na(.data$WRDI)) %>%
    dplyr::mutate(label_w = stringr::str_wrap(.data$label, 22))

  title_txt    <- if (lang == "es") "Mapa de lagunas * ORISMA Gap Map"
                  else "ORISMA Gap Map * Evidence Landscape"
  subtitle_txt <- if (lang == "es")
    "Cuadrante superior derecho = sobreestudiado pero sin datos de trabajadores"
  else
    "Upper-right quadrant = over-studied but lacking worker exposure data"

  wrdi_global <- result$WRDI_global

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x      = .data$RCS,
    y      = .data$WRDI,
    size   = .data$n_records,
    colour = .data$RCS
  )) +
    ggplot2::annotate("rect",
                      xmin = 1, xmax = Inf, ymin = wrdi_global, ymax = 1,
                      fill = "#FFE5E0", alpha = 0.3) +
    ggplot2::annotate("text",
                      x = max(df$RCS, na.rm = TRUE) * 0.85,
                      y = 0.97,
                      label = if (lang == "es") "Alta saturacion\nAlta desconexion"
                              else "High saturation\nHigh disconnection",
                      size = 3, colour = "#D85A30", hjust = 1) +
    ggplot2::geom_point(alpha = 0.85) +
    ggrepel::geom_text_repel(ggplot2::aes(label = .data$label_w),
                              size = 2.8, max.overlaps = 15,
                              box.padding = 0.5) +
    ggplot2::geom_hline(yintercept = wrdi_global,
                        linetype = "dashed", colour = "grey50", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = 1,
                        linetype = "dashed", colour = "grey50", linewidth = 0.7) +
    ggplot2::scale_colour_gradient(low = "#9FE1CB", high = "#0F6E56",
                                   guide = "none") +
    ggplot2::scale_size_continuous(
      name  = if (lang == "es") "N estudios" else "N studies",
      range = c(3, 14)
    ) +
    ggplot2::labs(
      title    = title_txt,
      subtitle = subtitle_txt,
      x        = "RCS (Risk Category Saturation)",
      y        = "WRDI (Worker-Risk Disconnection)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
    )

  ggplot2::ggsave(file.path(dir, "gap_map.png"), p,
                  width = 12, height = 9, dpi = 300)
}


#' @noRd
.plot_temporal_v2 <- function(result, dir, lang, top_n = 8) {
  if (is.null(result$temporal)) return(invisible(NULL))

  # Top N categories by total records
  top_cats <- result$indicators %>%
    dplyr::arrange(dplyr::desc(.data$n_records)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(.data$category)

  cat_cols <- paste0("cat_", top_cats)
  available <- intersect(cat_cols, names(result$temporal))
  if (length(available) == 0) return(invisible(NULL))

  # Labels for top cats
  cat_labels <- result$indicators %>%
    dplyr::filter(.data$category %in% top_cats) %>%
    dplyr::select(.data$category, .data$label) %>%
    dplyr::mutate(label_w = stringr::str_wrap(.data$label, 28))

  temporal_long <- result$temporal %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(available),
      names_to  = "cat_key",
      values_to = "n"
    ) %>%
    dplyr::mutate(category = gsub("^cat_", "", .data$cat_key)) %>%
    dplyr::left_join(cat_labels, by = "category")

  title_txt    <- if (lang == "es") "Evolucion temporal por categoria de riesgo"
                  else "Risk category evolution over time"
  subtitle_txt <- if (lang == "es")
    paste0("Top ", top_n, " categorias * literatura de AM metalica")
  else
    paste0("Top ", top_n, " categories * Metal AM literature")

  p <- ggplot2::ggplot(temporal_long,
                       ggplot2::aes(x     = .data$year,
                                    y     = .data$n,
                                    colour = .data$label_w,
                                    group  = .data$label_w)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_x_continuous(breaks = seq(
      min(result$temporal$year, na.rm = TRUE),
      max(result$temporal$year, na.rm = TRUE), 1)
    ) +
    ggplot2::labs(
      title    = title_txt,
      subtitle = subtitle_txt,
      x        = if (lang == "es") "Ano" else "Year",
      y        = if (lang == "es") "Numero de estudios" else "Number of studies",
      colour   = if (lang == "es") "Categoria" else "Category"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right",
      legend.text     = ggplot2::element_text(size = 9),
      plot.subtitle   = ggplot2::element_text(size = 9, colour = "grey40")
    )

  ggplot2::ggsave(file.path(dir, "temporal_trend.png"), p,
                  width = 13, height = 7, dpi = 300)
}


#' @noRd
.plot_cooccur_v2 <- function(result, dir, lang, min_records = 2) {
  # Filter to categories with at least min_records
  active_cats <- result$indicators %>%
    dplyr::filter(.data$n_records >= min_records) %>%
    dplyr::pull(.data$category)

  mat <- result$cooccur_mat
  idx <- rownames(mat) %in% active_cats
  mat <- mat[idx, idx]

  if (nrow(mat) < 2) return(invisible(NULL))

  # Use labels instead of keys
  label_map <- result$indicators %>%
    dplyr::filter(.data$category %in% rownames(mat)) %>%
    dplyr::select(.data$category, .data$label)

  new_names <- label_map$label[match(rownames(mat), label_map$category)]
  new_names <- stringr::str_wrap(new_names, 25)
  rownames(mat) <- new_names
  colnames(mat) <- new_names

  title_txt <- if (lang == "es") "Matriz de co-ocurrencia de riesgos"
               else "Risk co-occurrence matrix"

  png(file.path(dir, "cooccurrence_heatmap.png"),
      width = 3200, height = 2800, res = 300)
  pheatmap::pheatmap(
    mat,
    cluster_rows  = TRUE,
    cluster_cols  = TRUE,
    color         = grDevices::colorRampPalette(c("white", "#9FE1CB", "#0F6E56"))(50),
    main          = title_txt,
    fontsize      = 8,
    fontsize_row  = 7,
    fontsize_col  = 7,
    border_color  = "white",
    angle_col     = 45
  )
  grDevices::dev.off()
}


#' @noRd
.plot_distribution_v2 <- function(result, dir, lang, min_records = 1) {
  df <- result$indicators %>%
    dplyr::filter(.data$n_records >= min_records) %>%
    dplyr::arrange(dplyr::desc(.data$n_records)) %>%
    dplyr::mutate(
      label_w  = stringr::str_wrap(.data$label, 38),
      block    = dplyr::case_when(
        grepl("Fall|Collision|Entrap|Overturn|Vehicle|Electrical|Thermal contact|Explosion|Fire|Irritant|Toxic",
              .data$label, ignore.case = TRUE) ~ "A - Safety",
        grepl("Chemical|Carcinogen|Asbestos|Ionising|Non-ionising|Noise|Vibration|Thermal stress",
              .data$label, ignore.case = TRUE) ~ "B - Hygiene",
        grepl("Manual|Posture|Repetitive|Mental|Thermal comfort|Lighting|Indoor|Display",
              .data$label, ignore.case = TRUE) ~ "C - Ergonomics",
        grepl("Work content|Workload|Working time|Participation|Role|Professional|Interpersonal|Team|Harassment|Violence|Emotional",
              .data$label, ignore.case = TRUE) ~ "D - Psychosociology",
        grepl("Virus|Bacteria|Fungi|Parasit|Prion",
              .data$label, ignore.case = TRUE) ~ "E - Biological",
        TRUE ~ "F - Emerging"
      )
    )

  block_colors <- c(
    "A - Safety"         = "#D85A30",
    "B - Hygiene"        = "#E8A838",
    "C - Ergonomics"     = "#4DAF8D",
    "D - Psychosociology" = "#7B68EE",
    "E - Biological"     = "#E05C8A",
    "F - Emerging"       = "#0F6E56"
  )

  title_txt    <- if (lang == "es") "Distribucion de categorias de riesgo"
                  else "Risk category distribution"
  subtitle_txt <- if (lang == "es")
    paste0("N = ", result$n_records, " registros * Superdiccionario ORISMA v2.0")
  else
    paste0("N = ", result$n_records, " records * ORISMA Superdictionary v2.0")

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x    = .data$n_records,
    y    = reorder(.data$label_w, .data$n_records),
    fill = .data$block
  )) +
    ggplot2::geom_col(alpha = 0.9) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(.data$n_records, " (", .data$pct_records, "%)")),
      hjust = -0.1, size = 3, colour = "grey30"
    ) +
    ggplot2::scale_fill_manual(values = block_colors, name = "Block") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(
      title    = title_txt,
      subtitle = subtitle_txt,
      x        = if (lang == "es") "Numero de registros" else "Number of records",
      y        = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position    = "right",
      plot.subtitle      = ggplot2::element_text(size = 9, colour = "grey40")
    )

  ggplot2::ggsave(file.path(dir, "risk_distribution.png"), p,
                  width  = 14,
                  height = max(6, nrow(df) * 0.45),
                  dpi    = 300, limitsize = FALSE)
}


# =============================================================================
# IMPROVED HTML REPORT
# =============================================================================

#' @noRd
.write_html_report_v2 <- function(result, path, lang, out_dir, min_records = 1) {

  is_es   <- lang == "es"
  n_rec   <- result$n_records
  wrdi    <- result$WRDI_global
  wrdi_pct <- round(wrdi * 100, 1)
  n_cats  <- result$n_categories
  ps      <- attr(result, "pipeline_summary")
  n_load  <- if (!is.null(ps)) ps$n_loaded  else n_rec
  n_dedup <- if (!is.null(ps)) ps$n_removed else 0L
  date_str <- format(Sys.time(), "%Y-%m-%d %H:%M")

  # Active categories only
  ind_active <- result$indicators %>%
    dplyr::filter(.data$n_records >= min_records) %>%
    dplyr::arrange(dplyr::desc(.data$n_records))

  # Top gaps: high WRDI + present in literature
  top_gaps <- ind_active %>%
    dplyr::filter(.data$WRDI >= 0.7, .data$n_records >= 2) %>%
    dplyr::arrange(dplyr::desc(.data$WRDI), dplyr::desc(.data$n_records))

  # Translate labels
  lbl <- function(en, es) if (is_es) es else en

  # Build indicator rows
  ind_rows <- paste0(apply(ind_active, 1, function(r) {
    wrdi_val <- as.numeric(r["WRDI"])
    rcs_val  <- as.numeric(r["RCS"])
    wrdi_col <- if (is.na(wrdi_val)) "#888"
                else if (wrdi_val >= 0.8) "#D85A30"
                else if (wrdi_val >= 0.5) "#E8A838"
                else "#0F6E56"
    rcs_badge <- if (is.na(rcs_val)) ""
                 else if (rcs_val > 1)
                   paste0('<span style="background:#E8F5F0;color:#0F6E56;padding:2px 6px;border-radius:4px;font-size:11px">',
                          if(is_es) "sobrerepresentado" else "over-represented", '</span>')
                 else
                   paste0('<span style="background:#FFF3F0;color:#D85A30;padding:2px 6px;border-radius:4px;font-size:11px">',
                          if(is_es) "infrarepresentado" else "under-represented", '</span>')

    paste0(
      "<tr>",
      "<td>", r["label"], "</td>",
      "<td style='text-align:center;font-weight:bold'>", r["n_records"], "</td>",
      "<td style='text-align:center'>", r["pct_records"], "%</td>",
      "<td style='text-align:center;color:", wrdi_col, ";font-weight:bold'>",
        if (is.na(wrdi_val)) "N/A" else round(wrdi_val, 3), "</td>",
      "<td style='text-align:center'>", round(as.numeric(r["RCS"]), 2), "</td>",
      "<td>", rcs_badge, "</td>",
      "</tr>"
    )
  }), collapse = "\n")

  # Gap rows
  gap_rows <- if (nrow(top_gaps) > 0) {
    paste0(apply(top_gaps, 1, function(r) {
      paste0(
        "<tr>",
        "<td>", r["label"], "</td>",
        "<td style='text-align:center;color:#D85A30;font-weight:bold'>",
          round(as.numeric(r["WRDI"]), 3), "</td>",
        "<td style='text-align:center'>", r["n_records"], "</td>",
        "<td>",
          if (is_es)
            "Riesgo presente en literatura pero sin datos directos de trabajadores"
          else
            "Risk present in literature but lacking direct worker exposure data",
        "</td>",
        "</tr>"
      )
    }), collapse = "\n")
  } else {
    paste0("<tr><td colspan='4'>",
           if(is_es) "No se detectaron lagunas criticas" else "No critical gaps detected",
           "</td></tr>")
  }

  # WRDI color for gauge
  wrdi_color <- if (wrdi >= 0.8) "#D85A30"
                else if (wrdi >= 0.5) "#E8A838"
                else "#0F6E56"

  # Plots paths (relative)
  plots_exist <- dir.exists(file.path(out_dir, "plots"))

  html <- paste0('<!DOCTYPE html>
<html lang="', if(is_es) "es" else "en", '">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>', if(is_es) "Informe ORISMA" else "ORISMA Report", '</title>
<style>
  :root {
    --green-dark: #0F6E56;
    --green-mid:  #4DAF8D;
    --green-light:#9FE1CB;
    --orange:     #D85A30;
    --amber:      #E8A838;
    --grey-bg:    #F8F9FA;
    --grey-border:#E0E0E0;
  }
  * { box-sizing: border-box; margin: 0; padding: 0; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    background: #fff;
    color: #2c2c2c;
    line-height: 1.6;
  }
  .header {
    background: var(--green-dark);
    color: white;
    padding: 2rem 3rem;
  }
  .header h1 { font-size: 2rem; font-weight: 700; letter-spacing: -0.5px; }
  .header p  { opacity: 0.85; margin-top: 0.25rem; font-size: 0.95rem; }
  .header .meta { margin-top: 1rem; font-size: 0.85rem; opacity: 0.7; }
  .container { max-width: 1100px; margin: 0 auto; padding: 2rem 2rem; }
  .cards { display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem; margin: 2rem 0; }
  .card {
    background: var(--grey-bg);
    border: 1px solid var(--grey-border);
    border-radius: 10px;
    padding: 1.5rem;
    text-align: center;
  }
  .card .value { font-size: 2.5rem; font-weight: 700; }
  .card .label { font-size: 0.85rem; color: #666; margin-top: 0.25rem; }
  .card .sublabel { font-size: 0.75rem; color: #999; margin-top: 0.1rem; }
  .section { margin: 2.5rem 0; }
  .section h2 {
    font-size: 1.2rem;
    font-weight: 600;
    color: var(--green-dark);
    border-bottom: 2px solid var(--green-light);
    padding-bottom: 0.5rem;
    margin-bottom: 1rem;
  }
  .section h3 { font-size: 1rem; font-weight: 600; margin: 1.5rem 0 0.5rem; }
  table { border-collapse: collapse; width: 100%; font-size: 0.88rem; }
  th {
    background: var(--green-dark);
    color: white;
    padding: 10px 12px;
    text-align: left;
    font-weight: 500;
  }
  td { border: 1px solid var(--grey-border); padding: 8px 12px; }
  tr:nth-child(even) td { background: var(--grey-bg); }
  .plot-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; margin: 1rem 0; }
  .plot-grid img { width: 100%; border-radius: 8px; border: 1px solid var(--grey-border); }
  .plot-full img  { width: 100%; border-radius: 8px; border: 1px solid var(--grey-border); margin: 0.5rem 0; }
  .alert-box {
    background: #FFF8E1;
    border-left: 4px solid var(--amber);
    padding: 1rem 1.25rem;
    border-radius: 0 6px 6px 0;
    margin: 1rem 0;
    font-size: 0.9rem;
  }
  .gap-box {
    background: #FFF3F0;
    border-left: 4px solid var(--orange);
    padding: 1rem 1.25rem;
    border-radius: 0 6px 6px 0;
    margin: 1rem 0;
    font-size: 0.9rem;
  }
  .prisma {
    display: flex;
    gap: 0.5rem;
    align-items: center;
    flex-wrap: wrap;
    margin: 1rem 0;
  }
  .prisma-step {
    background: var(--green-dark);
    color: white;
    padding: 0.4rem 0.8rem;
    border-radius: 20px;
    font-size: 0.82rem;
  }
  .prisma-arrow { color: #999; font-size: 1.2rem; }
  .footer {
    background: var(--grey-bg);
    border-top: 1px solid var(--grey-border);
    padding: 1.5rem 3rem;
    font-size: 0.8rem;
    color: #888;
    margin-top: 3rem;
  }
  @media (max-width: 700px) {
    .cards { grid-template-columns: 1fr; }
    .plot-grid { grid-template-columns: 1fr; }
  }
</style>
</head>
<body>

<div class="header">
  <h1>ORISMA</h1>
  <p>', if(is_es) "Informe de mapeo sistematico de evidencia sobre riesgos laborales"
         else "Occupational Risk Integrated Systematic Mapping and Analysis Report", '</p>
  <div class="meta">
    ', if(is_es) "Generado el" else "Generated on", ': ', date_str, ' &nbsp;|&nbsp;
    ', if(is_es) "Diccionario" else "Dictionary", ': iso45001_insst v2.0.0 (56 categorias) &nbsp;|&nbsp;
    orisma v0.1.0
  </div>
</div>

<div class="container">

  <!-- PRISMA flow -->
  <div class="section">
    <h2>', if(is_es) "Flujo de seleccion (PRISMA)" else "Selection flow (PRISMA)", '</h2>
    <div class="prisma">
      <div class="prisma-step">',
        if(is_es) paste0("Registros identificados: ", n_load)
        else      paste0("Records identified: ", n_load),
      '</div>
      <div class="prisma-arrow">&#8594;</div>
      <div class="prisma-step">',
        if(is_es) paste0("Duplicados eliminados: ", n_dedup)
        else      paste0("Duplicates removed: ", n_dedup),
      '</div>
      <div class="prisma-arrow">&#8594;</div>
      <div class="prisma-step">',
        if(is_es) paste0("Registros unicos: ", n_rec)
        else      paste0("Unique records: ", n_rec),
      '</div>
      <div class="prisma-arrow">&#8594;</div>
      <div class="prisma-step">',
        if(is_es) paste0("Analizados: ", n_rec)
        else      paste0("Analysed: ", n_rec),
      '</div>
    </div>
  </div>

  <!-- KPI cards -->
  <div class="cards">
    <div class="card">
      <div class="value" style="color:', wrdi_color, '">', wrdi, '</div>
      <div class="label">WRDI ', if(is_es) "(global)" else "(global)", '</div>
      <div class="sublabel">',
        if(is_es) paste0(wrdi_pct, "% sin datos de trabajadores")
        else      paste0(wrdi_pct, "% lack worker exposure data"),
      '</div>
    </div>
    <div class="card">
      <div class="value" style="color:var(--green-dark)">', n_rec, '</div>
      <div class="label">', if(is_es) "Registros analizados" else "Records analysed", '</div>
      <div class="sublabel">WoS + Scopus, ', if(is_es) "deduplicados" else "deduplicated", '</div>
    </div>
    <div class="card">
      <div class="value" style="color:var(--green-dark)">', nrow(ind_active), '</div>
      <div class="label">', if(is_es) "Categorias detectadas" else "Categories detected", '</div>
      <div class="sublabel">', if(is_es) paste0("de ", n_cats, " en el diccionario")
                               else      paste0("of ", n_cats, " in dictionary"), '</div>
    </div>
  </div>

  <!-- Interpretacion automatica -->
  <div class="section">
    <h2>', if(is_es) "Interpretacion automatica de resultados"
           else "Automatic results interpretation", '</h2>
    <div class="alert-box">
      <strong>', if(is_es) "WRDI global: " else "Global WRDI: ", wrdi, '</strong> &mdash; ',
      if (wrdi >= 0.8)
        if(is_es) "ALERTA CRITICA: la gran mayoria de estudios caracteriza riesgos sin medir exposicion real de trabajadores. La evidencia disponible tiene muy baja aplicabilidad preventiva directa."
        else "CRITICAL ALERT: the vast majority of studies characterise risks without measuring real worker exposure. Available evidence has very low direct preventive applicability."
      else if (wrdi >= 0.5)
        if(is_es) "ATENCION: mas de la mitad de los estudios no incluyen datos de exposicion directa de trabajadores. El campo muestra una desconexion tecnico-laboral significativa."
        else "ATTENTION: more than half of studies do not include direct worker exposure data. The field shows significant technical-labour disconnection."
      else
        if(is_es) "El campo muestra una conexion razonable entre investigacion tecnica y exposicion de trabajadores."
        else "The field shows a reasonable connection between technical research and worker exposure.",
    '</div>
  </div>

  <!-- Plots: distribucion y temporal -->
  <div class="section">
    <h2>', if(is_es) "Distribucion de riesgos y evolucion temporal"
           else "Risk distribution and temporal evolution", '</h2>',
  if (plots_exist) paste0('
    <div class="plot-full"><img src="plots/risk_distribution.png"
      alt="', if(is_es) "Distribucion de riesgos" else "Risk distribution", '"></div>
    <div class="plot-full"><img src="plots/temporal_trend.png"
      alt="', if(is_es) "Evolucion temporal" else "Temporal trend", '"></div>')
  else "",
  '</div>

  <!-- Plots: WRDI y RCS -->
  <div class="section">
    <h2>', if(is_es) "Indicadores de desconexion y saturacion"
           else "Disconnection and saturation indicators", '</h2>',
  if (plots_exist) paste0('
    <div class="plot-grid">
      <div><img src="plots/wrdi_chart.png"
        alt="WRDI"></div>
      <div><img src="plots/rcs_chart.png"
        alt="RCS"></div>
    </div>')
  else "",
  '</div>

  <!-- Gap map -->
  <div class="section">
    <h2>', if(is_es) "Mapa de lagunas de conocimiento" else "Knowledge gap map", '</h2>',
  if (plots_exist) paste0('
    <div class="plot-full"><img src="plots/gap_map.png" alt="Gap Map"></div>')
  else "",
  '</div>

  <!-- Co-occurrence -->
  <div class="section">
    <h2>', if(is_es) "Matriz de co-ocurrencia de riesgos"
           else "Risk co-occurrence matrix", '</h2>',
  if (plots_exist) paste0('
    <div class="plot-full"><img src="plots/cooccurrence_heatmap.png"
      alt="', if(is_es) "Co-ocurrencia" else "Co-occurrence", '"></div>')
  else "",
  '</div>

  <!-- Gap table -->
  <div class="section">
    <h2>', if(is_es) "Lagunas criticas detectadas (WRDI >= 0.7)"
           else "Critical gaps detected (WRDI >= 0.7)", '</h2>
    <div class="gap-box">',
      if(is_es) "Estas categorias de riesgo aparecen en la literatura pero carecen de datos directos de exposicion de trabajadores. Son prioritarias para investigacion futura y evaluacion de riesgos."
      else "These risk categories appear in the literature but lack direct worker exposure data. They are priority areas for future research and risk assessment.",
    '</div>
    <table>
      <thead><tr>
        <th>', if(is_es) "Categoria" else "Category", '</th>
        <th>WRDI</th>
        <th>N</th>
        <th>', if(is_es) "Interpretacion" else "Interpretation", '</th>
      </tr></thead>
      <tbody>', gap_rows, '</tbody>
    </table>
  </div>

  <!-- Full indicators table -->
  <div class="section">
    <h2>', if(is_es) "Indicadores completos por categoria"
           else "Full indicators by category", '</h2>
    <table>
      <thead><tr>
        <th>', if(is_es) "Categoria de riesgo" else "Risk category", '</th>
        <th>N</th>
        <th>%</th>
        <th>WRDI</th>
        <th>RCS</th>
        <th>', if(is_es) "Saturacion" else "Saturation", '</th>
      </tr></thead>
      <tbody>', ind_rows, '</tbody>
    </table>
  </div>

</div><!-- /container -->

<div class="footer">
  <strong>ORISMA v0.1.0</strong> &mdash;
  Dr. Ra&uacute;l Aguilar-Elena (raguilar@universidadviu.com) &middot; GPRL &middot; VIU &nbsp;|&nbsp;
  Ana Delgado-Garc&iacute;a (a.delgado@usal.es) &middot; USAL<br>
  ', if(is_es) "Diccionario" else "Dictionary", ': INSST + ISO 45001 + NIOSH + EU-OSHA &nbsp;|&nbsp;
  ', if(is_es) "Generado el" else "Generated", ': ', date_str, ' &nbsp;|&nbsp;
  <a href="https://github.com/Aguilar-Elena/orisma">github.com/Aguilar-Elena/orisma</a>
</div>

</body>
</html>')

  writeLines(html, path, useBytes = FALSE)
  invisible(path)
}
