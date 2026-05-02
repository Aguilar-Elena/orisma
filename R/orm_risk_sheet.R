#' Generate an occupational risk sheet for a specific technology or domain
#'
#' @description
#' `orm_risk_sheet()` generates a structured risk sheet — the core
#' practical output for occupational health practitioners. It synthesises
#' all ORISMA outputs into a single actionable document that can be used
#' directly in a workplace risk assessment.
#'
#' The sheet includes:
#' - Priority classification (RED/AMBER/GREEN) per risk category
#' - Evidence summary (N studies, WRDI, RCS)
#' - Applicable European regulation and limit values
#' - Key preventive requirements
#' - Knowledge gap alert when evidence is insufficient
#' - Confidence score based on dictionary term density
#'
#' @param result An `orisma_result` object.
#' @param topic Character. Description of the technology or domain being
#'   assessed (e.g. "Metal additive manufacturing", "Nanotechnology in
#'   construction"). Used in the sheet header.
#' @param out_dir Character. Output directory.
#' @param lang Character. `"en"` or `"es"`.
#' @param min_records Integer. Min records for a category to appear. Default `1`.
#' @param verbose Logical.
#'
#' @return Invisibly returns the path to the generated HTML risk sheet.
#' @export
orm_risk_sheet <- function(result,
                            topic       = "Occupational risk analysis",
                            out_dir     = "orisma_output",
                            lang        = getOption("orisma.lang", "en"),
                            min_records = 1L,
                            verbose     = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)
  if (!inherits(result, "orisma_result"))
    stop("'result' must be an orisma_result object.", call. = FALSE)

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  is_es    <- lang == "es"
  date_str <- format(Sys.time(), "%Y-%m-%d")

  # Get priority and normativa
  priority  <- orm_priority(result, min_records = min_records, lang = lang)
  normativa <- tryCatch(
    orm_normativa(result, min_records = min_records, lang = lang),
    error = function(e) NULL
  )

  # Active categories
  active <- priority[priority$priority != "GREY", ]

  # Confidence score: based on n_records relative to total
  active$confidence <- dplyr::case_when(
    active$n_records >= 10 ~ if(is_es) "Alta" else "High",
    active$n_records >= 3  ~ if(is_es) "Media" else "Medium",
    TRUE                   ~ if(is_es) "Baja" else "Low"
  )

  # Build rows for each priority level
  .make_rows <- function(df, lvl) {
    sub <- df[df$priority == lvl, ]
    if (nrow(sub) == 0) return("")

    color <- switch(lvl,
      RED   = "#D85A30",
      AMBER = "#E8A838",
      GREEN = "#0F6E56",
      "#888"
    )
    bg <- switch(lvl,
      RED   = "#FFF3F0",
      AMBER = "#FFFBF0",
      GREEN = "#F0FBF7",
      "#F8F8F8"
    )

    paste0(apply(sub, 1, function(r) {
      # Get normativa for this category
      if (!is.null(normativa)) {
        norm_row <- normativa[normativa$label == r["label"], ]
        norm_txt <- if (nrow(norm_row) > 0)
          paste0('<small><strong>', norm_row$directive[1], '</strong> ',
                 norm_row$directive_name[1], '<br>',
                 if(is_es) "Valor limite: " else "Limit value: ",
                 norm_row$limit_value[1], '</small>')
        else
          paste0('<small><em>',
                 if(is_es) "Sin normativa especifica identificada"
                 else "No specific regulation identified", '</em></small>')
      } else {
        norm_txt <- ""
      }

      wrdi_val <- as.numeric(r["WRDI"])
      wrdi_str <- if (is.na(wrdi_val)) "N/A" else
        paste0(round(wrdi_val * 100, 0), "%",
               if(is_es) " sin datos trabajadores" else " lack worker data")

      paste0(
        '<tr style="background:', bg, '">',
        '<td style="border-left:4px solid ', color, ';padding:8px 12px">',
          '<strong>', r["label"], '</strong><br>',
          '<small style="color:', color, '">', r["priority_label"], '</small>',
        '</td>',
        '<td style="text-align:center;padding:8px">',
          '<strong>', r["n_records"], '</strong><br>',
          '<small>', if(is_es) "estudios" else "studies", '</small>',
        '</td>',
        '<td style="text-align:center;padding:8px">',
          r["confidence"],
        '</td>',
        '<td style="padding:8px">', norm_txt, '</td>',
        '<td style="padding:8px;color:#666;font-size:12px">',
          r["priority_reason"],
        '</td>',
        '</tr>'
      )
    }), collapse = "\n")
  }

  red_rows   <- .make_rows(active, "RED")
  amber_rows <- .make_rows(active, "AMBER")
  green_rows <- .make_rows(active, "GREEN")

  n_red   <- sum(active$priority == "RED",   na.rm = TRUE)
  n_amber <- sum(active$priority == "AMBER", na.rm = TRUE)
  n_green <- sum(active$priority == "GREEN", na.rm = TRUE)
  n_grey  <- sum(priority$priority == "GREY", na.rm = TRUE)

  ps      <- attr(result, "pipeline_summary")
  n_load  <- if (!is.null(ps)) ps$n_loaded  else result$n_records
  n_dedup <- if (!is.null(ps)) ps$n_removed else 0L

  html <- paste0('<!DOCTYPE html>
<html lang="', if(is_es) "es" else "en", '">
<head>
<meta charset="UTF-8">
<title>ORISMA - ', if(is_es) "Ficha de Riesgo" else "Risk Sheet", '</title>
<style>
  body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
         max-width: 1000px; margin: 0 auto; padding: 2rem; color: #2c2c2c; }
  .header { background: #0F6E56; color: white; padding: 1.5rem 2rem;
             border-radius: 8px; margin-bottom: 2rem; }
  .header h1 { margin: 0; font-size: 1.5rem; }
  .header p  { margin: 0.25rem 0 0; opacity: 0.85; }
  .kpis { display: grid; grid-template-columns: repeat(4, 1fr);
           gap: 1rem; margin: 1.5rem 0; }
  .kpi { background: #F8F9FA; border-radius: 8px; padding: 1rem;
          text-align: center; border: 1px solid #E0E0E0; }
  .kpi .val { font-size: 2rem; font-weight: 700; }
  .kpi .lbl { font-size: 0.8rem; color: #666; }
  .section h2 { font-size: 1.1rem; font-weight: 600; color: #0F6E56;
                 border-bottom: 2px solid #9FE1CB; padding-bottom: 0.4rem;
                 margin: 2rem 0 1rem; }
  table { border-collapse: collapse; width: 100%; font-size: 0.88rem; }
  th { background: #0F6E56; color: white; padding: 10px 12px; text-align: left; }
  td { border-bottom: 1px solid #E0E0E0; vertical-align: top; }
  .alert { background: #FFF3F0; border-left: 4px solid #D85A30;
            padding: 1rem; border-radius: 0 6px 6px 0; margin: 1rem 0;
            font-size: 0.9rem; }
  .meta { background: #F8F9FA; border-radius: 6px; padding: 1rem;
           font-size: 0.82rem; color: #666; margin-top: 2rem; }
  .wrdi-bar { background: #E0E0E0; border-radius: 4px; height: 8px;
               margin-top: 4px; }
  .wrdi-fill { background: #D85A30; height: 8px; border-radius: 4px; }
  @media print { body { max-width: 100%; } }
</style>
</head>
<body>

<div class="header">
  <h1>ORISMA &mdash; ', if(is_es) "Ficha de Riesgos Laborales" else "Occupational Risk Sheet", '</h1>
  <p><strong>', topic, '</strong> &nbsp;|&nbsp; ', date_str, ' &nbsp;|&nbsp; orisma v0.1.0</p>
</div>

<div class="section">
  <h2>', if(is_es) "Resumen ejecutivo" else "Executive summary", '</h2>

  <div class="kpis">
    <div class="kpi">
      <div class="val" style="color:#D85A30">', result$WRDI_global, '</div>
      <div class="lbl">WRDI Global</div>
      <div class="wrdi-bar"><div class="wrdi-fill" style="width:', round(result$WRDI_global*100), '%"></div></div>
    </div>
    <div class="kpi">
      <div class="val" style="color:#D85A30">', n_red, '</div>
      <div class="lbl">', if(is_es) "Brechas CRITICAS" else "CRITICAL gaps", '</div>
    </div>
    <div class="kpi">
      <div class="val" style="color:#E8A838">', n_amber, '</div>
      <div class="lbl">', if(is_es) "Requieren atencion" else "Require attention", '</div>
    </div>
    <div class="kpi">
      <div class="val" style="color:#0F6E56">', n_green, '</div>
      <div class="lbl">', if(is_es) "Bien cubiertos" else "Well covered", '</div>
    </div>
  </div>

  <p style="font-size:0.88rem;color:#666">',
    if(is_es)
      paste0("Analisis basado en ", result$n_records, " estudios (de ", n_load,
             " identificados, ", n_dedup, " duplicados eliminados). ",
             n_grey, " categorias sin evidencia suficiente.")
    else
      paste0("Analysis based on ", result$n_records, " studies (from ", n_load,
             " identified, ", n_dedup, " duplicates removed). ",
             n_grey, " categories lack sufficient evidence."),
  '</p>
</div>

<div class="section">
  <h2>', if(is_es) "Brechas criticas (ROJO) - Accion inmediata"
         else "Critical gaps (RED) - Immediate action required", '</h2>',
  if (n_red > 0) paste0('
  <div class="alert">',
    if(is_es) "Estos riesgos estan bien documentados tecnicamente pero carecen de datos reales de exposicion de trabajadores. Son prioritarios para la evaluacion de riesgos in situ."
    else "These risks are well-documented technically but lack real worker exposure data. They are priority targets for on-site risk assessment.",
  '</div>
  <table>
    <thead><tr>
      <th>', if(is_es) "Categoria de riesgo" else "Risk category", '</th>
      <th>N</th>
      <th>', if(is_es) "Evidencia" else "Evidence", '</th>
      <th>', if(is_es) "Normativa aplicable" else "Applicable regulation", '</th>
      <th>', if(is_es) "Razon" else "Reason", '</th>
    </tr></thead>
    <tbody>', red_rows, '</tbody>
  </table>')
  else paste0('<p><em>',
    if(is_es) "No se detectaron brechas criticas." else "No critical gaps detected.",
  '</em></p>'),
  '</div>

<div class="section">
  <h2>', if(is_es) "Requieren atencion (AMBAR)"
         else "Require attention (AMBER)", '</h2>
  <table>
    <thead><tr>
      <th>', if(is_es) "Categoria de riesgo" else "Risk category", '</th>
      <th>N</th>
      <th>', if(is_es) "Evidencia" else "Evidence", '</th>
      <th>', if(is_es) "Normativa aplicable" else "Applicable regulation", '</th>
      <th>', if(is_es) "Razon" else "Reason", '</th>
    </tr></thead>
    <tbody>', amber_rows, '</tbody>
  </table>
</div>

<div class="section">
  <h2>', if(is_es) "Cobertura razonable (VERDE)"
         else "Reasonable coverage (GREEN)", '</h2>
  <table>
    <thead><tr>
      <th>', if(is_es) "Categoria de riesgo" else "Risk category", '</th>
      <th>N</th>
      <th>', if(is_es) "Evidencia" else "Evidence", '</th>
      <th>', if(is_es) "Normativa aplicable" else "Applicable regulation", '</th>
      <th>', if(is_es) "Razon" else "Reason", '</th>
    </tr></thead>
    <tbody>', green_rows, '</tbody>
  </table>
</div>

<div class="section">
  <h2>', if(is_es) "Limitaciones metodologicas" else "Methodological limitations", '</h2>
  <ul style="font-size:0.88rem;color:#555;line-height:1.8">
    <li>',
      if(is_es) "La clasificacion automatica por diccionario puede producir falsos positivos. Se recomienda validacion manual de una muestra (use orm_validate())."
      else "Automatic dictionary-based classification may produce false positives. Manual validation of a sample is recommended (use orm_validate()).",
    '</li>
    <li>',
      if(is_es) "WRDI detecta la presencia de terminos de exposicion de trabajadores en el abstract, no verifica que el estudio midio exposicion real."
      else "WRDI detects worker exposure terms in the abstract; it does not verify that the study actually measured real exposure.",
    '</li>
    <li>',
      if(is_es) "Las categorias con N < 3 tienen evidencia insuficiente para conclusiones solidas."
      else "Categories with N < 3 have insufficient evidence for robust conclusions.",
    '</li>
    <li>',
      if(is_es) "Este informe no sustituye la evaluacion de riesgos in situ obligatoria."
      else "This report does not replace the mandatory on-site risk assessment.",
    '</li>
  </ul>
</div>

<div class="meta">
  <strong>ORISMA v0.1.0</strong> &mdash;
  Dr. Ra&uacute;l Aguilar-Elena &middot; GPRL &middot; VIU &nbsp;|&nbsp;
  Ana Delgado-Garc&iacute;a &middot; USAL<br>
  ', if(is_es) "Diccionario" else "Dictionary", ': INSST + ISO 45001 + NIOSH + EU-OSHA &nbsp;|&nbsp;
  ', if(is_es) "Generado" else "Generated", ': ', date_str, ' &nbsp;|&nbsp;
  <a href="https://github.com/Aguilar-Elena/orisma">github.com/Aguilar-Elena/orisma</a><br><br>
  <em>',
    if(is_es)
      "Este documento es una herramienta de apoyo bibliometrico. No constituye asesoramiento legal ni sustituye la evaluacion de riesgos reglamentaria."
    else
      "This document is a bibliometric support tool. It does not constitute legal advice and does not replace mandatory regulatory risk assessment.",
  '</em>
</div>

</body>
</html>')

  sheet_path <- file.path(out_dir, "orisma_risk_sheet.html")
  writeLines(html, sheet_path, useBytes = FALSE)

  if (verbose) cli::cli_alert_success(paste0(
    if(is_es) "Ficha de riesgo guardada en: " else "Risk sheet saved to: ",
    sheet_path
  ))

  invisible(sheet_path)
}
