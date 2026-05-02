#' Compute risk priority scores and traffic light classification
#'
#' @description
#' `orm_priority()` assigns a priority level to each detected risk category
#' using three criteria combined into a single priority score:
#'
#' - **Frequency** (RCS): how saturated is this category in the literature
#' - **Disconnection** (WRDI): how far is the research from real worker data
#' - **Evidence volume**: number of records
#'
#' Categories whose RCS exceeds `context_rcs_threshold` are flagged as
#' **context categories** (the dominant topic of the corpus, not a risk per se)
#' and are reported separately rather than mixed with risk categories.
#'
#' Priority levels for non-context categories:
#' - **RED**: WRDI >= wrdi_high AND RCS >= 1. Over-studied technically but
#'   no worker data. Urgent preventive gap.
#' - **AMBER**: Moderate evidence OR partial worker data.
#' - **GREEN**: WRDI < wrdi_low. Good worker data connection.
#' - **GREY**: n_records < min_records. Insufficient evidence.
#'
#' @param result An `orisma_result` object.
#' @param min_records Integer. Min records for evaluation. Default `2`.
#' @param wrdi_high Numeric. WRDI threshold for high disconnection. Default `0.7`.
#' @param wrdi_low Numeric. WRDI threshold for low disconnection. Default `0.3`.
#' @param context_rcs_threshold Numeric. RCS above which a category is
#'   considered a context category (dominant topic) rather than a risk.
#'   Default `15`.
#' @param lang Character. `"en"` or `"es"`.
#'
#' @return A list with two data frames: `$risks` (priority-classified risk
#'   categories) and `$context` (dominant topic categories).
#' @export
orm_priority <- function(result,
                          min_records            = 2L,
                          wrdi_high              = 0.7,
                          wrdi_low               = 0.3,
                          context_rcs_threshold  = 15,
                          lang                   = getOption("orisma.lang", "en")) {

  .check_lang(lang)
  if (!inherits(result, "orisma_result"))
    stop("'result' must be an orisma_result object.", call. = FALSE)

  is_es <- lang == "es"
  ind   <- result$indicators

  # ── Separate context categories from risk categories ─────────────────────────
  context_cats <- ind %>%
    dplyr::filter(!is.na(.data$RCS), .data$RCS >= context_rcs_threshold)

  risk_cats <- ind %>%
    dplyr::filter(is.na(.data$RCS) | .data$RCS < context_rcs_threshold)

  # ── Classify risk categories ─────────────────────────────────────────────────
  risk_classified <- risk_cats %>%
    dplyr::mutate(
      priority = dplyr::case_when(
        .data$n_records < min_records               ~ "GREY",
        is.na(.data$WRDI)                           ~ "GREY",
        .data$WRDI >= wrdi_high & .data$RCS >= 1   ~ "RED",
        .data$WRDI >= wrdi_high & .data$RCS <  1   ~ "AMBER",
        .data$WRDI <  wrdi_low                      ~ "GREEN",
        TRUE                                        ~ "AMBER"
      ),
      priority_label = dplyr::case_when(
        .data$priority == "RED"   ~
          if(is_es) "ROJO: brecha preventiva urgente" else "RED: urgent preventive gap",
        .data$priority == "AMBER" ~
          if(is_es) "AMBAR: requiere atencion"        else "AMBER: requires attention",
        .data$priority == "GREEN" ~
          if(is_es) "VERDE: cobertura razonable"      else "GREEN: reasonable coverage",
        TRUE ~
          if(is_es) "GRIS: evidencia insuficiente"    else "GREY: insufficient evidence"
      ),
      priority_reason = dplyr::case_when(
        .data$priority == "RED" ~
          if(is_es)
            paste0("Sobreestudiado tecnicamente (RCS=", round(.data$RCS,1),
                   ") pero sin datos de trabajadores (WRDI=", round(.data$WRDI,2), ")")
          else
            paste0("Over-studied technically (RCS=", round(.data$RCS,1),
                   ") but no worker data (WRDI=", round(.data$WRDI,2), ")"),
        .data$priority == "AMBER" ~
          if(is_es)
            paste0("Evidencia parcial. WRDI=", round(.data$WRDI,2),
                   ", RCS=", round(.data$RCS,1))
          else
            paste0("Partial evidence. WRDI=", round(.data$WRDI,2),
                   ", RCS=", round(.data$RCS,1)),
        .data$priority == "GREEN" ~
          if(is_es)
            paste0("Buena conexion con datos de trabajadores (WRDI=",
                   round(.data$WRDI,2), ")")
          else
            paste0("Good connection with worker data (WRDI=",
                   round(.data$WRDI,2), ")"),
        TRUE ~
          if(is_es)
            paste0("Solo ", .data$n_records, " estudio(s). Evidencia insuficiente.")
          else
            paste0("Only ", .data$n_records, " study(ies). Insufficient evidence.")
      ),
      confidence = dplyr::case_when(
        .data$n_records >= 10 ~ if(is_es) "Alta"  else "High",
        .data$n_records >= 3  ~ if(is_es) "Media" else "Medium",
        .data$n_records >= 1  ~ if(is_es) "Baja"  else "Low",
        TRUE                  ~ if(is_es) "Nula"  else "None"
      )
    ) %>%
    dplyr::select(
      .data$category, .data$label, .data$n_records, .data$pct_records,
      .data$WRDI, .data$RCS, .data$priority, .data$priority_label,
      .data$priority_reason, .data$confidence
    ) %>%
    dplyr::arrange(
      factor(.data$priority, levels = c("RED","AMBER","GREEN","GREY")),
      dplyr::desc(.data$n_records)
    )

  result_list <- list(
    risks   = risk_classified,
    context = context_cats,
    params  = list(
      min_records           = min_records,
      wrdi_high             = wrdi_high,
      wrdi_low              = wrdi_low,
      context_rcs_threshold = context_rcs_threshold,
      lang                  = lang
    )
  )
  class(result_list) <- c("orisma_priority", "list")
  result_list
}


#' Print method for orisma_priority
#' @param x An `orisma_priority` object.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.orisma_priority <- function(x, ...) {
  lang  <- x$params$lang
  is_es <- lang == "es"

  cat("\n-- ORISMA Risk Priority Classification --\n\n")

  if (nrow(x$context) > 0) {
    cat(if(is_es) "Categorias de CONTEXTO (tema dominante del corpus):\n"
        else "CONTEXT categories (dominant corpus topic):\n")
    for (i in seq_len(nrow(x$context))) {
      cat(sprintf("  [*] %-45s  N=%-4d  RCS=%.1f\n",
                  substr(x$context$label[i], 1, 45),
                  x$context$n_records[i],
                  x$context$RCS[i]))
    }
    cat("\n")
  }

  for (lvl in c("RED","AMBER","GREEN","GREY")) {
    sub <- x$risks[x$risks$priority == lvl, ]
    if (nrow(sub) == 0) next
    sym <- switch(lvl, RED="X", AMBER="!", GREEN="v", "?")
    cat(sprintf("[%s] %s (%d)\n", sym, lvl, nrow(sub)))
    for (i in seq_len(nrow(sub))) {
      cat(sprintf("    %-45s  N=%-3d  WRDI=%.2f  RCS=%.1f  conf=%s\n",
                  substr(sub$label[i], 1, 45),
                  sub$n_records[i],
                  ifelse(is.na(sub$WRDI[i]), 0, sub$WRDI[i]),
                  ifelse(is.na(sub$RCS[i]),  0, sub$RCS[i]),
                  sub$confidence[i]))
    }
    cat("\n")
  }
  invisible(x)
}
