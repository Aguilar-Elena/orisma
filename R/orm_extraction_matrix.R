#' Generate a guided extraction matrix for manual review
#'
#' @description
#' `orm_extraction_matrix()` generates a structured extraction template
#' pre-filled with automatically extracted information. The practitioner
#' completes the remaining fields using the full PDF.
#'
#' Articles are selected and ranked by combined bridge score + ASS score.
#' The matrix contains auto-filled bibliographic data, ORISMA scores,
#' detected technology and risk categories, and empty fields for manual
#' completion with full-text PDFs.
#'
#' @param mx An `orisma_matrix` object after [orm_bridge()] and [orm_ass()].
#' @param result An `orisma_result` object from [orm_run()].
#' @param top_n Integer. Max articles to include. Default `30`.
#' @param min_bridge_score Integer. Min bridge score. Default `2`.
#' @param out_dir Character. Output directory.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical.
#'
#' @return Invisibly returns the path to the saved CSV.
#' @export
orm_extraction_matrix <- function(mx,
                                   result,
                                   top_n            = 30L,
                                   min_bridge_score = 2L,
                                   out_dir          = "orisma_output",
                                   lang             = getOption("orisma.lang", "en"),
                                   verbose          = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)
  if (!inherits(mx, "orisma_matrix"))
    stop("'mx' must be an orisma_matrix object.", call. = FALSE)
  if (!inherits(result, "orisma_result"))
    stop("'result' must be an orisma_result object.", call. = FALSE)
  if (!"bridge_score" %in% names(mx$refs))
    stop("Run orm_bridge() first.", call. = FALSE)

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  is_es   <- lang == "es"
  refs    <- mx$refs
  bin_mat <- mx$matrix

  # Priority score
  ass_score      <- if ("ass_score" %in% names(refs)) refs$ass_score else 0L
  bridge_score   <- refs$bridge_score
  n_cats         <- rowSums(bin_mat)
  priority_score <- (bridge_score * 2) + (ass_score * 1.5) + (n_cats * 0.5)

  refs_ranked <- refs %>%
    dplyr::mutate(
      priority_score = priority_score,
      ass_score_col  = ass_score,
      n_categories   = n_cats
    ) %>%
    dplyr::filter(.data$bridge_score >= min_bridge_score) %>%
    dplyr::arrange(dplyr::desc(.data$priority_score)) %>%
    dplyr::slice_head(n = top_n)

  if (nrow(refs_ranked) == 0) {
    cli::cli_alert_warning(
      if(is_es) "Sin articulos con bridge_score >= min_bridge_score. Reduzca el umbral."
      else "No articles with bridge_score >= min_bridge_score. Reduce threshold."
    )
    return(invisible(NULL))
  }

  # Risk categories per article
  cat_labels <- result$indicators$label
  names(cat_labels) <- result$indicators$category
  bin_sub <- bin_mat[refs_ranked$record_id, , drop = FALSE]
  detected_risks <- apply(bin_sub, 1, function(row) {
    detected <- names(row)[row == 1L]
    labels   <- cat_labels[detected]
    paste(labels[!is.na(labels)], collapse = "; ")
  })

  # Auto-detect technology
  text_all <- tolower(as.character(refs_ranked$abstract))
  text_all[is.na(text_all)] <- ""

  tech_patterns <- list(
    "PBF/SLM/LPBF"  = "powder bed|selective laser|SLM|LPBF|laser powder",
    "DED"            = "directed energy|DED|WAAM|laser metal deposition",
    "EBM"            = "electron beam melting|EBM",
    "FDM/FFF"        = "fused deposition|FDM|FFF|fused filament",
    "Welding"        = "welding|arc welding|MIG|TIG",
    "Nanotechnology" = "nanotechnology|nanomaterial|nanoparticle",
    "Healthcare"     = "hospital|healthcare|clinical|medical",
    "Construction"   = "construction|demolition|renovation",
    "Mining"         = "mining|underground|quarry",
    "Agriculture"    = "agriculture|farm|pesticide"
  )

  auto_tech <- vapply(text_all, function(txt) {
    detected <- vapply(names(tech_patterns), function(nm)
      grepl(tech_patterns[[nm]], txt, perl = TRUE, ignore.case = TRUE),
      logical(1))
    if (any(detected)) paste(names(tech_patterns)[detected], collapse = "; ")
    else if(is_es) "No detectada" else "Not detected"
  }, character(1))

  # Clean authors (handle list columns from synthesisr)
  clean_authors <- vapply(refs_ranked$authors, function(a) {
    if (is.list(a)) paste(unlist(a), collapse = "; ")
    else as.character(a)
  }, character(1))

  # Complete placeholder
  ph <- if(is_es) "[COMPLETAR]" else "[COMPLETE]"
  ph_yn <- if(is_es) "[SI/NO/QUIZA]" else "[YES/NO/MAYBE]"

  # Build matrix — all character to avoid type conflicts
  em <- data.frame(
    record_id        = refs_ranked$record_id,
    title            = as.character(refs_ranked$title),
    authors          = clean_authors,
    year             = as.character(refs_ranked$year),
    doi              = as.character(refs_ranked$doi),
    source_db        = as.character(refs_ranked$source_db),
    bridge_type      = as.character(refs_ranked$bridge_type),
    bridge_score     = as.character(refs_ranked$bridge_score),
    bridge_criteria  = as.character(refs_ranked$bridge_criteria),
    ass_score        = as.character(refs_ranked$ass_score_col),
    priority_score   = as.character(round(refs_ranked$priority_score, 1)),
    auto_technology  = auto_tech,
    auto_risk_cats   = as.character(detected_risks[refs_ranked$record_id]),
    study_design     = ph,
    study_population = ph,
    n_participants   = ph,
    exposure_agent   = ph,
    exposure_level   = ph,
    exposure_units   = ph,
    exposure_method  = ph,
    main_result      = ph,
    health_effect    = ph,
    prevention_recs  = ph,
    limitations      = ph,
    quality_score    = ph,
    include_final    = ph_yn,
    reviewer_notes   = ph,
    stringsAsFactors = FALSE
  )

  mat_path <- file.path(out_dir, "orisma_extraction_matrix.csv")
  readr::write_csv(em, mat_path, na = "")

  if (verbose) {
    cli::cli_alert_success(paste0(
      if(is_es) "Matriz guardada: " else "Matrix saved: ", mat_path
    ))
    cli::cli_alert_info(paste0(
      nrow(em), if(is_es) " articulos incluidos" else " articles included"
    ))
    n_strong <- sum(em$bridge_type %in% c("Strong bridge", "Puente fuerte"))
    cli::cli_alert_info(paste0(
      "  ", if(is_es) "Puentes fuertes: " else "Strong bridges: ", n_strong,
      " | ", if(is_es) "Otros: " else "Others: ", nrow(em) - n_strong
    ))
    cli::cli_alert_info(
      if(is_es)
        "Complete las columnas [COMPLETAR] usando los PDFs completos."
      else
        "Complete the [COMPLETE] columns using full-text PDFs."
    )
  }

  invisible(mat_path)
}
