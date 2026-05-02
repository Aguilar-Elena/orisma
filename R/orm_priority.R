#' Compute risk priority scores and traffic light classification
#'
#' @description
#' `orm_priority()` assigns a priority level to each detected risk category
#' using three criteria:
#'
#' - **Frequency** (RCS): how saturated is this category in the literature
#' - **Disconnection** (WRDI): how far is the research from real worker data
#' - **Evidence gap**: combination of frequency and disconnection
#'
#' Priority levels:
#' - **RED**: High frequency + high disconnection = well-studied technically
#'   but no worker data. Urgent preventive gap.
#' - **AMBER**: Moderate evidence with partial worker data. Needs attention.
#' - **GREEN**: Good evidence with worker exposure data. Relatively well covered.
#' - **GREY**: Insufficient evidence (< min_records). Unknown risk level.
#'
#' @param result An `orisma_result` object.
#' @param min_records Integer. Min records for a category to be evaluated.
#'   Default `2`.
#' @param wrdi_high Numeric. WRDI threshold for high disconnection. Default `0.7`.
#' @param wrdi_low Numeric. WRDI threshold for low disconnection. Default `0.3`.
#' @param lang Character. `"en"` or `"es"`.
#'
#' @return A data frame with priority classification for each risk category.
#' @export
orm_priority <- function(result,
                          min_records = 2L,
                          wrdi_high   = 0.7,
                          wrdi_low    = 0.3,
                          lang        = getOption("orisma.lang", "en")) {

  .check_lang(lang)
  if (!inherits(result, "orisma_result")) {
    stop("'result' must be an orisma_result object.", call. = FALSE)
  }

  ind <- result$indicators

  priority_df <- ind %>%
    dplyr::mutate(
      priority = dplyr::case_when(
        .data$n_records < min_records ~ "GREY",
        is.na(.data$WRDI)             ~ "GREY",
        .data$WRDI >= wrdi_high & .data$RCS >= 1 ~ "RED",
        .data$WRDI >= wrdi_high & .data$RCS <  1 ~ "AMBER",
        .data$WRDI <  wrdi_low                   ~ "GREEN",
        TRUE                                      ~ "AMBER"
      ),
      priority_label = dplyr::case_when(
        .data$priority == "RED" ~
          if (lang == "es") "ROJO: brecha preventiva urgente"
          else "RED: urgent preventive gap",
        .data$priority == "AMBER" ~
          if (lang == "es") "AMBAR: requiere atencion"
          else "AMBER: requires attention",
        .data$priority == "GREEN" ~
          if (lang == "es") "VERDE: cobertura razonable"
          else "GREEN: reasonable coverage",
        TRUE ~
          if (lang == "es") "GRIS: evidencia insuficiente"
          else "GREY: insufficient evidence"
      ),
      priority_reason = dplyr::case_when(
        .data$priority == "RED" ~
          if (lang == "es")
            paste0("Sobreestudiado tecnicamente (RCS=", round(.data$RCS, 1),
                   ") pero sin datos de trabajadores (WRDI=", round(.data$WRDI, 2), ")")
          else
            paste0("Over-studied technically (RCS=", round(.data$RCS, 1),
                   ") but no worker data (WRDI=", round(.data$WRDI, 2), ")"),
        .data$priority == "AMBER" ~
          if (lang == "es")
            paste0("Evidencia parcial. WRDI=", round(.data$WRDI, 2),
                   ", RCS=", round(.data$RCS, 1))
          else
            paste0("Partial evidence. WRDI=", round(.data$WRDI, 2),
                   ", RCS=", round(.data$RCS, 1)),
        .data$priority == "GREEN" ~
          if (lang == "es")
            paste0("Buena conexion con datos de trabajadores (WRDI=",
                   round(.data$WRDI, 2), ")")
          else
            paste0("Good connection with worker data (WRDI=",
                   round(.data$WRDI, 2), ")"),
        TRUE ~
          if (lang == "es") paste0("Solo ", .data$n_records, " estudio(s). Evidencia insuficiente.")
          else paste0("Only ", .data$n_records, " study(ies). Insufficient evidence.")
      )
    ) %>%
    dplyr::select(
      .data$category, .data$label, .data$n_records, .data$pct_records,
      .data$WRDI, .data$RCS, .data$priority, .data$priority_label,
      .data$priority_reason
    ) %>%
    dplyr::arrange(
      factor(.data$priority, levels = c("RED", "AMBER", "GREEN", "GREY")),
      dplyr::desc(.data$n_records)
    )

  class(priority_df) <- c("orisma_priority", "data.frame")
  priority_df
}


#' Print method for orisma_priority
#' @param x An `orisma_priority` object.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.orisma_priority <- function(x, ...) {
  cat("\n-- ORISMA Risk Priority Classification --\n\n")
  for (lvl in c("RED", "AMBER", "GREEN", "GREY")) {
    sub <- x[x$priority == lvl, ]
    if (nrow(sub) == 0) next
    symbol <- switch(lvl, RED = "X", AMBER = "!", GREEN = "v", GREY = "?")
    cat(sprintf("[%s] %s (%d categories)\n", symbol, lvl, nrow(sub)))
    for (i in seq_len(nrow(sub))) {
      cat(sprintf("    %-45s  N=%-3d  WRDI=%.2f  RCS=%.1f\n",
                  substr(sub$label[i], 1, 45),
                  sub$n_records[i],
                  ifelse(is.na(sub$WRDI[i]), 0, sub$WRDI[i]),
                  ifelse(is.na(sub$RCS[i]),  0, sub$RCS[i])))
    }
    cat("\n")
  }
  invisible(x)
}
