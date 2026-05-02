#' Bridge Article Detection and Priority Ranking
#'
#' @description
#' `orm_bridge()` identifies **bridge articles** - studies that connect
#' technical science with real occupational prevention. These are the
#' highest-value articles for an occupational health practitioner because
#' they have already done the translation from laboratory to workplace.
#'
#' A bridge article simultaneously mentions:
#' 1. **Technology/process** (what was studied)
#' 2. **Hazardous agent** (what risk was characterised)
#' 3. **Workers** (real people in real workplaces)
#' 4. **Exposure measurement** (quantitative data)
#' 5. **Prevention/recommendation** (actionable output)
#'
#' Articles meeting 4 or 5 criteria are classified as **strong bridges**.
#' Articles meeting 3 criteria (must include workers + measurement) are
#' **partial bridges**. Others are technical studies.
#'
#' @param mx An `orisma_matrix` object, ideally after running [orm_ass()].
#' @param text_col Character. Text field to analyse. Default `"abstract"`.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical.
#'
#' @return The `orisma_matrix` object with added columns:
#'   `bridge_score` (0-5), `bridge_type` (Strong/Partial/Technical),
#'   `bridge_criteria` (which criteria were met).
#'
#' @export
orm_bridge <- function(mx,
                        text_col = "abstract",
                        lang     = getOption("orisma.lang", "en"),
                        verbose  = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)
  if (!inherits(mx, "orisma_matrix"))
    stop("'mx' must be an orisma_matrix object from orm_extract().", call. = FALSE)

  is_es <- lang == "es"
  refs  <- mx$refs

  if (!text_col %in% names(refs) ||
      mean(is.na(refs[[text_col]]) | refs[[text_col]] == "") > 0.5) {
    text_col <- "title"
  }

  text_all <- tolower(as.character(refs[[text_col]]))
  text_all[is.na(text_all)] <- ""

  # ── Bridge criteria patterns ─────────────────────────────────────────────────

  # Criterion 1: Technology / process
  pat_c1 <- paste0("\\b(",
    "additive manufacturing|3D print|powder bed|laser melting|sintering|",
    "directed energy|electron beam|fused deposition|FDM|SLM|EBM|LPBF|DED|",
    "welding|machining|grinding|painting|coating|plating|casting|forging|",
    "nanotechnology|nanomaterial|semiconductor|pharmaceutical|",
    "construction|demolition|renovation|agriculture|mining|",
    "healthcare|hospital|laboratory|office work|transport|logistics",
    ")\\b")

  # Criterion 2: Hazardous agent
  pat_c2 <- paste0("\\b(",
    "nanoparticle|ultrafine|aerosol|fume|metal fume|dust|respirable|",
    "chemical|solvent|VOC|isocyanate|carcinogen|mutagen|toxic|",
    "noise|vibration|radiation|laser|UV|infrared|EMF|",
    "biological|pathogen|bacteria|virus|fungi|allergen|",
    "ergonomic|manual handling|repetitive|posture|",
    "psychosocial|stress|burnout|harassment",
    ")\\b")

  # Criterion 3: Workers (MANDATORY for bridge)
  pat_c3 <- paste0("\\b(",
    "worker|workers|employee|employees|operator|operators|",
    "occupational|workforce|personnel|staff|",
    "worker exposure|worker health|worker safety|",
    "exposed worker|occupational cohort|study population",
    ")\\b")

  # Criterion 4: Exposure measurement (MANDATORY for bridge)
  pat_c4 <- paste0("\\b(",
    "measured|measurement|monitored|sampling|concentration|",
    "breathing zone|personal air|area sampling|biological monitoring|",
    "TWA|STEL|OEL|TLV|mg.m|ug.m|ppm|ppb|dB|fiber|",
    "dose|dosimetry|quantif|detected|assessed exposure",
    ")\\b")

  # Criterion 5: Prevention / recommendation
  pat_c5 <- paste0("\\b(",
    "prevention|preventive|control measure|engineering control|",
    "administrative control|PPE|personal protective|ventilation|",
    "enclosure|substitution|elimination|protective measure|",
    "safety measure|intervention|recommendation|guideline|",
    "limit value|exposure limit|risk management|mitigation|",
    "reduce exposure|lower exposure|protective action|",
    "health surveillance|medical monitoring|awareness",
    ")\\b")

  # ── Score each record ─────────────────────────────────────────────────────────
  results <- lapply(seq_along(text_all), function(i) {
    txt <- text_all[i]
    if (nchar(txt) < 30) {
      return(list(score = 0L, criteria = character(0),
                  type = if(is_es) "Tecnico" else "Technical"))
    }

    c1 <- grepl(pat_c1, txt, perl = TRUE)
    c2 <- grepl(pat_c2, txt, perl = TRUE)
    c3 <- grepl(pat_c3, txt, perl = TRUE)
    c4 <- grepl(pat_c4, txt, perl = TRUE)
    c5 <- grepl(pat_c5, txt, perl = TRUE)

    met      <- c(c1, c2, c3, c4, c5)
    score    <- sum(met)
    criteria <- c("technology","agent","workers","measurement","prevention")[met]

    # Bridge requires BOTH workers (c3) AND measurement (c4)
    is_bridge <- c3 && c4

    type <- if (score >= 4 && is_bridge)
      if(is_es) "Puente fuerte" else "Strong bridge"
    else if (score >= 3 && is_bridge)
      if(is_es) "Puente parcial" else "Partial bridge"
    else
      if(is_es) "Tecnico" else "Technical"

    list(score = as.integer(score), criteria = criteria, type = type)
  })

  mx$refs$bridge_score    <- vapply(results, `[[`, integer(1), "score")
  mx$refs$bridge_type     <- vapply(results, `[[`, character(1), "type")
  mx$refs$bridge_criteria <- vapply(results, function(x)
    paste(x$criteria, collapse = "+"), character(1))

  if (verbose) {
    n_strong  <- sum(mx$refs$bridge_type %in% c("Strong bridge", "Puente fuerte"))
    n_partial <- sum(mx$refs$bridge_type %in% c("Partial bridge", "Puente parcial"))
    n_tech    <- nrow(mx$refs) - n_strong - n_partial

    cli::cli_alert_success(paste0(
      if(is_es) "Articulos puente detectados:" else "Bridge articles detected:"
    ))
    cli::cli_alert_info(paste0(
      "  ", if(is_es) "Puentes fuertes: " else "Strong bridges: ", n_strong,
      " (", round(100*n_strong/nrow(mx$refs), 1), "%)"
    ))
    cli::cli_alert_info(paste0(
      "  ", if(is_es) "Puentes parciales: " else "Partial bridges: ", n_partial,
      " (", round(100*n_partial/nrow(mx$refs), 1), "%)"
    ))
    cli::cli_alert_info(paste0(
      "  ", if(is_es) "Tecnicos: " else "Technical: ", n_tech,
      " (", round(100*n_tech/nrow(mx$refs), 1), "%)"
    ))
  }

  mx
}


#' Generate priority reading ranking
#'
#' @description
#' `orm_ranking()` produces a **priority reading list** for occupational
#' health practitioners, ranking articles by their combined relevance score
#' (bridge score + ASS score + number of risk categories detected).
#'
#' Articles at the top of the list are those most likely to contain
#' actionable preventive information and should be read first in full.
#'
#' @param mx An `orisma_matrix` object after running [orm_bridge()] and
#'   optionally [orm_ass()].
#' @param top_n Integer. Number of top articles to return. Default `20`.
#' @param out_dir Character or NULL. Directory to save the ranking CSV.
#' @param lang Character. `"en"` or `"es"`.
#'
#' @return A data frame with the top_n priority articles.
#' @export
orm_ranking <- function(mx,
                         top_n   = 20L,
                         out_dir = NULL,
                         lang    = getOption("orisma.lang", "en")) {

  .check_lang(lang)
  if (!inherits(mx, "orisma_matrix"))
    stop("'mx' must be an orisma_matrix object.", call. = FALSE)
  if (!"bridge_score" %in% names(mx$refs))
    stop("Run orm_bridge() first.", call. = FALSE)

  is_es <- lang == "es"
  refs  <- mx$refs

  # Combined priority score
  ass_score    <- if ("ass_score" %in% names(refs)) refs$ass_score else 0L
  bridge_score <- refs$bridge_score
  n_cats       <- rowSums(mx$matrix)

  # Weighted score: bridge (0-5) x2 + ASS (0-5) x1.5 + categories x0.5
  priority_score <- (bridge_score * 2) + (ass_score * 1.5) + (n_cats * 0.5)

  ranking <- refs %>%
    dplyr::mutate(
      priority_score = priority_score,
      ass_score_col  = ass_score,
      n_categories   = n_cats
    ) %>%
    dplyr::arrange(dplyr::desc(.data$priority_score)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::select(
      dplyr::any_of(c(
        "record_id", "title", "authors", "year", "doi", "source_db",
        "bridge_type", "bridge_score", "bridge_criteria",
        "ass_score_col", "n_categories", "priority_score"
      ))
    )

  names(ranking)[names(ranking) == "ass_score_col"] <- "ass_score"

  if (!is.null(out_dir)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    readr::write_csv(ranking, file.path(out_dir, "orisma_priority_ranking.csv"))
  }

  class(ranking) <- c("orisma_ranking", "data.frame")
  ranking
}


#' Print method for orisma_ranking
#' @param x An `orisma_ranking` object.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.orisma_ranking <- function(x, ...) {
  cat("\n-- ORISMA Priority Reading Ranking --\n")
  cat(" Top", nrow(x), "articles by preventive relevance\n\n")
  for (i in seq_len(min(nrow(x), 20))) {
    cat(sprintf(" %2d. [%s] score=%.1f  ASS=%s  cats=%s\n",
                i,
                x$bridge_type[i],
                x$priority_score[i],
                if("ass_score" %in% names(x)) x$ass_score[i] else "?",
                if("n_categories" %in% names(x)) x$n_categories[i] else "?"))
    cat(sprintf("     %s\n", substr(x$title[i], 1, 80)))
    if (!is.na(x$doi[i]) && x$doi[i] != "")
      cat(sprintf("     DOI: %s\n", x$doi[i]))
    cat("\n")
  }
  invisible(x)
}
