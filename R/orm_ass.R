#' Abstract Sufficiency Score (ASS)
#'
#' @description
#' `orm_ass()` computes an **Abstract Sufficiency Score** (0-5) for each
#' record, measuring how much preventively useful information the abstract
#' contains for an occupational health practitioner.
#'
#' The score is **cumulative and hierarchical** - a record cannot reach
#' level N without satisfying all previous levels:
#'
#' - **0** Non-informative abstract for OHS purposes
#' - **1** Mentions a hazard or risk, but no occupational context
#' - **2** Mentions occupational/workplace context
#' - **3** Mentions exposure measurement or quantification
#' - **4** Mentions exposure in workers with some result
#' - **5** Mentions exposure, worker population, method AND control/prevention
#'
#' @param mx An `orisma_matrix` object from [orm_extract()].
#' @param text_col Character. Text field to score. Default `"abstract"`,
#'   falls back to `"title"` if abstract is mostly empty.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical.
#'
#' @return The `orisma_matrix` object with added columns:
#'   `ass_score` (0-5), `ass_label` (descriptive label),
#'   `ass_level_reached` (highest level passed).
#'
#' @export
orm_ass <- function(mx,
                    text_col = "abstract",
                    lang     = getOption("orisma.lang", "en"),
                    verbose  = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)
  if (!inherits(mx, "orisma_matrix"))
    stop("'mx' must be an orisma_matrix object from orm_extract().", call. = FALSE)

  is_es <- lang == "es"
  refs  <- mx$refs

  # Select text column
  if (!text_col %in% names(refs) ||
      mean(is.na(refs[[text_col]]) | refs[[text_col]] == "") > 0.5) {
    text_col <- "title"
    if (verbose) cli::cli_alert_warning(
      if(is_es) "Abstract no disponible, usando titulo (scores seran mas bajos)"
      else "Abstract not available, using title (scores will be lower)"
    )
  }

  text_all <- tolower(as.character(refs[[text_col]]))
  text_all[is.na(text_all)] <- ""

  # ── Level definitions (cumulative) ───────────────────────────────────────────

  # Level 1: mentions a hazard/risk term
  pat_l1 <- paste0("\\b(",
    "hazard|risk|exposure|toxic|carcinogen|nanoparticle|fume|dust|noise|",
    "vibration|radiation|chemical|biological|ergonomic|psychosocial|",
    "emission|particle|contaminant|pollutant|agent|substance",
    ")\\b")

  # Level 2: occupational/workplace context
  pat_l2 <- paste0("\\b(",
    "occupational|workplace|worker|workers|employee|employees|operator|",
    "operators|workforce|personnel|staff|professional|industrial|",
    "work.related|work environment|manufacturing|factory|plant|",
    "laboratory worker|healthcare worker|construction worker",
    ")\\b")

  # Level 3: exposure measurement/quantification
  pat_l3 <- paste0("\\b(",
    "measured|measurement|concentration|level|levels|monitored|monitoring|",
    "sampling|sampl|personal exposure|area sampling|breathing zone|",
    "biomonitoring|biological monitoring|urinary|blood level|",
    "TWA|STEL|OEL|TLV|PEL|mg.m|ug.m|ppm|ppb|dB|fiber.cm|",
    "quantif|detected|assess|assessed|evaluated|characteriz",
    ")\\b")

  # Level 4: worker exposure with result
  pat_l4_worker <- paste0("\\b(",
    "worker exposure|workers were exposed|workers exposed|",
    "exposed workers|occupational exposure|personal exposure|",
    "operator exposure|employee exposure|workforce exposure|",
    "breathing zone|inhalation exposure|dermal exposure|",
    "biological monitoring of workers",
    ")\\b")
  pat_l4_result <- paste0("\\b(",
    "result|results|found|showed|demonstrated|indicated|revealed|",
    "exceeded|below|above|higher|lower|significant|associated|",
    "increased|decreased|elevated|risk assessment|health effect|",
    "adverse effect|symptom|disease|injury",
    ")\\b")

  # Level 5: exposure + population + method + control/prevention
  pat_l5_method <- paste0("\\b(",
    "cross.sectional|cohort|case.control|longitudinal|randomized|",
    "survey|questionnaire|interview|observational|experimental|",
    "study design|n=|sample size|participants|subjects|",
    "NIOSH|OSHA method|ISO method|EN standard|sampling method",
    ")\\b")
  pat_l5_prevention <- paste0("\\b(",
    "prevention|preventive|control measure|engineering control|",
    "administrative control|PPE|personal protective equipment|",
    "ventilation|enclosure|substitution|elimination|hierarchy of controls|",
    "protective measure|safety measure|intervention|recommendation|",
    "guideline|limit value|exposure limit|protective action|",
    "risk management|mitigation|reduce exposure|lower exposure",
    ")\\b")

  # ── Compute scores (cumulative) ───────────────────────────────────────────────
  scores <- vapply(seq_along(text_all), function(i) {
    txt <- text_all[i]
    if (nchar(txt) < 20) return(0L)

    l1 <- grepl(pat_l1, txt, perl = TRUE)
    if (!l1) return(0L)

    l2 <- grepl(pat_l2, txt, perl = TRUE)
    if (!l2) return(1L)

    l3 <- grepl(pat_l3, txt, perl = TRUE)
    if (!l3) return(2L)

    l4 <- grepl(pat_l4_worker, txt, perl = TRUE) &&
          grepl(pat_l4_result,  txt, perl = TRUE)
    if (!l4) return(3L)

    l5 <- grepl(pat_l5_method,     txt, perl = TRUE) &&
          grepl(pat_l5_prevention, txt, perl = TRUE)
    if (!l5) return(4L)

    return(5L)
  }, integer(1))

  # ── Labels ────────────────────────────────────────────────────────────────────
  labels <- dplyr::case_when(
    scores == 0 ~ if(is_es) "0 - No informativo para PRL"
                  else "0 - Non-informative for OHS",
    scores == 1 ~ if(is_es) "1 - Menciona riesgo sin contexto laboral"
                  else "1 - Mentions hazard, no occupational context",
    scores == 2 ~ if(is_es) "2 - Contexto laboral presente"
                  else "2 - Occupational context present",
    scores == 3 ~ if(is_es) "3 - Menciona exposicion o medicion"
                  else "3 - Mentions exposure or measurement",
    scores == 4 ~ if(is_es) "4 - Exposicion en trabajadores con resultado"
                  else "4 - Worker exposure with result",
    scores == 5 ~ if(is_es) "5 - Abstract completo: exposicion + poblacion + metodo + prevencion"
                  else "5 - Complete: exposure + population + method + prevention",
    TRUE        ~ "?"
  )

  # ── Attach to refs ────────────────────────────────────────────────────────────
  mx$refs$ass_score <- scores
  mx$refs$ass_label <- labels

  if (verbose) {
    cli::cli_alert_success(paste0(
      if(is_es) "ASS calculado para " else "ASS computed for ",
      nrow(mx$refs), if(is_es) " registros" else " records"
    ))
    dist_tbl <- table(scores)
    for (s in 0:5) {
      n <- if (as.character(s) %in% names(dist_tbl))
             dist_tbl[as.character(s)] else 0L
      pct <- round(100 * n / nrow(mx$refs), 1)
      cli::cli_alert_info(paste0(
        "  Level ", s, ": ", n, " (", pct, "%) - ",
        switch(as.character(s),
          "0" = if(is_es) "no informativo" else "non-informative",
          "1" = if(is_es) "riesgo sin contexto" else "hazard no context",
          "2" = if(is_es) "contexto laboral" else "occupational context",
          "3" = if(is_es) "medicion" else "measurement",
          "4" = if(is_es) "exposicion trabajadores" else "worker exposure",
          "5" = if(is_es) "completo" else "complete"
        )
      ))
    }
  }

  mx
}


#' Plot ASS distribution
#'
#' @description
#' Generates a bar chart showing the distribution of Abstract Sufficiency
#' Scores across the corpus.
#'
#' @param mx An `orisma_matrix` object after running [orm_ass()].
#' @param out_dir Character or NULL. Directory to save the plot.
#' @param lang Character. `"en"` or `"es"`.
#'
#' @return A ggplot2 object invisibly.
#' @export
orm_ass_plot <- function(mx,
                          out_dir = NULL,
                          lang    = getOption("orisma.lang", "en")) {

  if (!"ass_score" %in% names(mx$refs))
    stop("Run orm_ass() first.", call. = FALSE)

  is_es <- lang == "es"

  df <- mx$refs %>%
    dplyr::count(.data$ass_score) %>%
    dplyr::mutate(
      pct   = round(100 * .data$n / sum(.data$n), 1),
      label_s = dplyr::case_when(
        .data$ass_score == 0 ~ if(is_es) "0\nNo informativo" else "0\nNon-informative",
        .data$ass_score == 1 ~ if(is_es) "1\nRiesgo sin\ncontexto" else "1\nHazard no\ncontext",
        .data$ass_score == 2 ~ if(is_es) "2\nContexto\nlaboral" else "2\nOccupational\ncontext",
        .data$ass_score == 3 ~ if(is_es) "3\nMedicion\nexposicion" else "3\nExposure\nmeasurement",
        .data$ass_score == 4 ~ if(is_es) "4\nExposicion\ntrabajadores" else "4\nWorker\nexposure",
        .data$ass_score == 5 ~ if(is_es) "5\nCompleto" else "5\nComplete",
        TRUE ~ as.character(.data$ass_score)
      ),
      fill_col = dplyr::case_when(
        .data$ass_score <= 1 ~ "#D85A30",
        .data$ass_score == 2 ~ "#E8A838",
        .data$ass_score == 3 ~ "#F5C842",
        .data$ass_score == 4 ~ "#4DAF8D",
        .data$ass_score == 5 ~ "#0F6E56",
        TRUE ~ "#888"
      )
    )

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x    = factor(.data$ass_score),
    y    = .data$n,
    fill = .data$fill_col
  )) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(
      label = paste0(.data$n, "\n(", .data$pct, "%)")
    ), vjust = -0.3, size = 3.5, colour = "grey30") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_discrete(labels = setNames(df$label_s, as.character(df$ass_score))) +
    ggplot2::labs(
      title    = if(is_es) "Distribucion del Abstract Sufficiency Score (ASS)"
                 else "Abstract Sufficiency Score (ASS) distribution",
      subtitle = if(is_es)
        paste0("N = ", sum(df$n), " registros . 0 = no informativo . 5 = completo para PRL")
      else
        paste0("N = ", sum(df$n), " records . 0 = non-informative . 5 = complete for OHS"),
      x = "ASS",
      y = if(is_es) "Numero de registros" else "Number of records"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40")
    )

  if (!is.null(out_dir)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    ggplot2::ggsave(file.path(out_dir, "ass_distribution.png"), p,
                    width = 10, height = 6, dpi = 300)
  }

  invisible(p)
}
