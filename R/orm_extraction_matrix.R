#' Generate a guided extraction matrix for manual review
#'
#' @description
#' `orm_extraction_matrix()` generates a **structured extraction template**
#' pre-filled with information automatically extracted from abstracts.
#' The practitioner completes the remaining fields using the full PDF.
#'
#' The matrix is designed for the top-ranked articles (bridge articles and
#' high-ASS records) and contains:
#'
#' - Bibliographic data (auto-filled)
#' - Technology/process (auto-detected)
#' - Hazardous agent (auto-detected from risk categories)
#' - Study population (auto-detected)
#' - Exposure measurement details (to complete with PDF)
#' - Main result (to complete with PDF)
#' - Preventive recommendations (to complete with PDF)
#' - Quality assessment (to complete manually)
#'
#' @param mx An `orisma_matrix` object after running [orm_bridge()] and
#'   [orm_ass()].
#' @param result An `orisma_result` object from [orm_run()].
#' @param top_n Integer. Number of articles to include. Default `30`.
#' @param min_bridge_score Integer. Min bridge score to include. Default `2`.
#' @param out_dir Character. Directory to save the extraction matrix.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical.
#'
#' @return Invisibly returns the path to the saved extraction matrix CSV.
#' @export
orm_extraction_matrix <- function(mx,
                                   result,
                                   top_n             = 30L,
                                   min_bridge_score  = 2L,
                                   out_dir           = "orisma_output",
                                   lang              = getOption("orisma.lang", "en"),
                                   verbose           = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)
  if (!inherits(mx, "orisma_matrix"))
    stop("'mx' must be an orisma_matrix object.", call. = FALSE)
  if (!inherits(result, "orisma_result"))
    stop("'result' must be an orisma_result object.", call. = FALSE)
  if (!"bridge_score" %in% names(mx$refs))
    stop("Run orm_bridge() first.", call. = FALSE)

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  is_es <- lang == "es"
  refs  <- mx$refs
  bin_mat <- mx$matrix

  # Select top articles
  ass_score    <- if ("ass_score" %in% names(refs)) refs$ass_score else 0L
  bridge_score <- refs$bridge_score
  n_cats       <- rowSums(bin_mat)
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
      if(is_es) "Ningún artículo supera min_bridge_score. Reduzca el umbral."
      else "No articles exceed min_bridge_score. Try reducing the threshold."
    )
    return(invisible(NULL))
  }

  # Get risk categories detected for each article
  bin_sub <- bin_mat[refs_ranked$record_id, , drop = FALSE]
  cat_labels <- result$indicators$label
  names(cat_labels) <- result$indicators$category

  detected_risks <- apply(bin_sub, 1, function(row) {
    detected <- names(row)[row == 1L]
    labels   <- cat_labels[detected]
    paste(labels[!is.na(labels)], collapse = "; ")
  })

  # Auto-detect technology from abstract
  tech_patterns <- list(
    "PBF/SLM/LPBF"  = "powder bed fusion|selective laser|SLM|LPBF|laser powder",
    "DED"            = "directed energy deposition|DED|laser metal deposition|WAAM",
    "EBM"            = "electron beam melting|EBM",
    "FDM/FFF"        = "fused deposition|FDM|FFF|fused filament",
    "Welding"        = "welding|arc welding|MIG|TIG|spot weld",
    "Nanotechnology" = "nanotechnology|nanomaterial|nanoparticle|nano",
    "Healthcare"     = "hospital|healthcare|clinical|medical|nursing",
    "Construction"   = "construction|demolition|renovation|building",
    "Mining"         = "mining|mine|underground|quarry",
    "Agriculture"    = "agriculture|farm|pesticide|crop"
  )

  text_all <- tolower(as.character(refs_ranked$abstract))
  text_all[is.na(text_all)] <- tolower(as.character(refs_ranked$title[is.na(text_all)]))

  auto_tech <- vapply(text_all, function(txt) {
    detected <- vapply(names(tech_patterns), function(nm) {
      grepl(tech_patterns[[nm]], txt, perl = TRUE, ignore.case = TRUE)
    }, logical(1))
    if (any(detected)) paste(names(tech_patterns)[detected], collapse = "; ")
    else if(is_es) "No detectada automaticamente" else "Not auto-detected"
  }, character(1))

  # Build extraction matrix
  em <- data.frame(
    # ── Auto-filled: bibliographic ──────────────────────────────────────────
    record_id        = refs_ranked$record_id,
    title            = refs_ranked$title,
    authors          = refs_ranked$authors,
    year             = refs_ranked$year,
    doi              = refs_ranked$doi,
    source_db        = refs_ranked$source_db,

    # ── Auto-filled: ORISMA scores ──────────────────────────────────────────
    bridge_type      = refs_ranked$bridge_type,
    bridge_score     = refs_ranked$bridge_score,
    bridge_criteria  = refs_ranked$bridge_criteria,
    ass_score        = refs_ranked$ass_score_col,
    priority_score   = round(refs_ranked$priority_score, 1),

    # ── Auto-detected: content ──────────────────────────────────────────────
    auto_technology  = auto_tech,
    auto_risk_cats   = detected_risks[refs_ranked$record_id],

    # ── To complete with PDF ─────────────────────────────────────────────────
    study_design     = NA_character_,
    study_population = NA_character_,
    n_participants   = NA_character_,
    exposure_agent   = NA_character_,
    exposure_level   = NA_character_,
    exposure_units   = NA_character_,
    exposure_method  = NA_character_,
    main_result      = NA_character_,
    health_effect    = NA_character_,
    prevention_recs  = NA_character_,
    limitations      = NA_character_,

    # ── Quality assessment ───────────────────────────────────────────────────
    quality_score    = NA_character_,  # e.g. ROBINS-I, NOS
    include_final    = NA_character_,  # YES / NO / MAYBE
    reviewer_notes   = NA_character_,

    stringsAsFactors = FALSE
  )

  # Add column labels as first row (for Excel readability)
  col_labels <- data.frame(
    record_id        = if(is_es) "ID registro" else "Record ID",
    title            = if(is_es) "Titulo" else "Title",
    authors          = if(is_es) "Autores" else "Authors",
    year             = if(is_es) "Ano" else "Year",
    doi              = "DOI",
    source_db        = if(is_es) "Base de datos" else "Database",
    bridge_type      = if(is_es) "Tipo puente" else "Bridge type",
    bridge_score     = if(is_es) "Score puente (0-5)" else "Bridge score (0-5)",
    bridge_criteria  = if(is_es) "Criterios cumplidos" else "Criteria met",
    ass_score        = if(is_es) "Score abstract (0-5)" else "Abstract score (0-5)",
    priority_score   = if(is_es) "Score prioridad" else "Priority score",
    auto_technology  = if(is_es) "Tecnologia (auto)" else "Technology (auto)",
    auto_risk_cats   = if(is_es) "Categorias riesgo (auto)" else "Risk categories (auto)",
    study_design     = if(is_es) "[COMPLETAR] Diseno estudio" else "[COMPLETE] Study design",
    study_population = if(is_es) "[COMPLETAR] Poblacion" else "[COMPLETE] Population",
    n_participants   = if(is_es) "[COMPLETAR] N participantes" else "[COMPLETE] N participants",
    exposure_agent   = if(is_es) "[COMPLETAR] Agente exposicion" else "[COMPLETE] Exposure agent",
    exposure_level   = if(is_es) "[COMPLETAR] Nivel exposicion" else "[COMPLETE] Exposure level",
    exposure_units   = if(is_es) "[COMPLETAR] Unidades" else "[COMPLETE] Units",
    exposure_method  = if(is_es) "[COMPLETAR] Metodo medicion" else "[COMPLETE] Measurement method",
    main_result      = if(is_es) "[COMPLETAR] Resultado principal" else "[COMPLETE] Main result",
    health_effect    = if(is_es) "[COMPLETAR] Efecto salud" else "[COMPLETE] Health effect",
    prevention_recs  = if(is_es) "[COMPLETAR] Recomendaciones" else "[COMPLETE] Prevention recs",
    limitations      = if(is_es) "[COMPLETAR] Limitaciones" else "[COMPLETE] Limitations",
    quality_score    = if(is_es) "[COMPLETAR] Calidad (ROBINS-I/NOS)" else "[COMPLETE] Quality (ROBINS-I/NOS)",
    include_final    = if(is_es) "[COMPLETAR] Incluir? SI/NO" else "[COMPLETE] Include? YES/NO",
    reviewer_notes   = if(is_es) "[COMPLETAR] Notas revisor" else "[COMPLETE] Reviewer notes",
    stringsAsFactors = FALSE
  )

  em_final <- dplyr::bind_rows(col_labels, em)

  # Save
  mat_path <- file.path(out_dir, "orisma_extraction_matrix.csv")
  readr::write_csv(em_final, mat_path, na = "")

  if (verbose) {
    cli::cli_alert_success(paste0(
      if(is_es) "Matriz de extraccion guardada: " else "Extraction matrix saved: ",
      mat_path
    ))
    cli::cli_alert_info(paste0(
      if(is_es) "Artículos incluidos: " else "Articles included: ",
      nrow(em),
      if(is_es) " (bridge score >= " else " (bridge score >= ",
      min_bridge_score, ")"
    ))
    cli::cli_alert_info(
      if(is_es)
        "Complete las columnas marcadas con [COMPLETAR] usando los PDFs completos."
      else
        "Complete the columns marked [COMPLETE] using the full PDFs."
    )
  }

  invisible(mat_path)
}
