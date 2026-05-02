#' Generate an occupational risk sheet
#'
#' @description
#' `orm_risk_sheet()` generates a structured, actionable risk sheet for
#' occupational health practitioners. It synthesises ORISMA outputs into
#' a single HTML document that can be used as supporting evidence in a
#' workplace risk assessment.
#'
#' The sheet is **regulation-neutral**: it does not include country-specific
#' regulations or limit values, as these vary by jurisdiction. The practitioner
#' applies the relevant national/regional regulation based on the risk
#' categories identified.
#'
#' Content:
#' - Context analysis (dominant topic of the corpus)
#' - Priority traffic light (RED/AMBER/GREEN/GREY) per risk category
#' - Evidence summary with confidence level
#' - Knowledge gap alerts
#' - WRDI interpretation with confidence score
#' - Methodological section (bases, deduplication, WRDI definition, limits)
#'
#' @param result An `orisma_result` object.
#' @param topic Character. Technology or domain being assessed.
#' @param search_strategy Character or NULL. Description of the search strategy
#'   used (databases, keywords, date range). If NULL, a placeholder is used.
#' @param inclusion_criteria Character or NULL. Description of inclusion/
#'   exclusion criteria applied. If NULL, ORISMA defaults are described.
#' @param out_dir Character. Output directory.
#' @param lang Character. `"en"` or `"es"`.
#' @param min_records Integer. Min records for a category to appear. Default `1`.
#' @param context_rcs_threshold Numeric. RCS threshold for context detection.
#'   Default `15`.
#' @param verbose Logical.
#'
#' @return Invisibly returns the path to the generated HTML risk sheet.
#' @export
orm_risk_sheet <- function(result,
                            topic                 = "Occupational risk analysis",
                            search_strategy       = NULL,
                            inclusion_criteria    = NULL,
                            out_dir               = "orisma_output",
                            lang                  = getOption("orisma.lang", "en"),
                            min_records           = 1L,
                            context_rcs_threshold = 15,
                            verbose               = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)
  if (!inherits(result, "orisma_result"))
    stop("'result' must be an orisma_result object.", call. = FALSE)

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  is_es    <- lang == "es"
  date_str <- format(Sys.time(), "%Y-%m-%d")
  n_cats   <- length(result$dict)

  # Get priority classification
  priority <- orm_priority(result,
                            min_records           = min_records,
                            context_rcs_threshold = context_rcs_threshold,
                            lang                  = lang)

  risks   <- priority$risks
  context <- priority$context

  n_red   <- sum(risks$priority == "RED",   na.rm = TRUE)
  n_amber <- sum(risks$priority == "AMBER", na.rm = TRUE)
  n_green <- sum(risks$priority == "GREEN", na.rm = TRUE)
  n_grey  <- sum(risks$priority == "GREY",  na.rm = TRUE)

  ps      <- attr(result, "pipeline_summary")
  n_load  <- if (!is.null(ps)) ps$n_loaded  else result$n_records
  n_dedup <- if (!is.null(ps)) ps$n_removed else 0L
  n_final <- result$n_records
  wrdi    <- result$WRDI_global
  wrdi_pct <- round(wrdi * 100, 1)

  # WRDI interpretation
  wrdi_interp <- if (wrdi >= 0.8)
    if(is_es) "CRITICO: mas del 80% de los estudios no incluyen datos directos de exposicion de trabajadores"
    else "CRITICAL: more than 80% of studies lack direct worker exposure data"
  else if (wrdi >= 0.5)
    if(is_es) "MODERADO: mas de la mitad de los estudios carecen de datos de exposicion real de trabajadores"
    else "MODERATE: more than half of studies lack real worker exposure data"
  else
    if(is_es) "ACEPTABLE: la mayoria de los estudios incluyen datos de exposicion de trabajadores"
    else "ACCEPTABLE: most studies include worker exposure data"

  wrdi_color <- if (wrdi >= 0.8) "#D85A30" else if (wrdi >= 0.5) "#E8A838" else "#0F6E56"

  # Search strategy placeholder
  search_txt <- if (!is.null(search_strategy)) search_strategy
  else if (is_es)
    "Busqueda sistematica en bases de datos bibliograficas. Consulte el fichero prisma_log.csv para el detalle completo del flujo de seleccion de estudios."
  else
    "Systematic search in bibliographic databases. See prisma_log.csv for the full study selection flow."

  # Inclusion criteria placeholder
  incl_txt <- if (!is.null(inclusion_criteria)) inclusion_criteria
  else if (is_es)
    "Articulos en ingles o espanol, cualquier diseno de estudio, sin restriccion temporal. Excluidos: editoriales, cartas, actas de congresos sin abstract."
  else
    "Articles in English or Spanish, any study design, no date restriction. Excluded: editorials, letters, conference abstracts without full text."

  # Build risk rows
  .risk_rows <- function(df) {
    if (nrow(df) == 0) return("")
    paste0(apply(df, 1, function(r) {
      lvl <- r["priority"]
      color <- switch(lvl, RED="#D85A30", AMBER="#E8A838", GREEN="#0F6E56", "#888")
      bg    <- switch(lvl, RED="#FFF3F0", AMBER="#FFFBF0", GREEN="#F0FBF7", "#F8F8F8")
      wrdi_val <- as.numeric(r["WRDI"])
      wrdi_bar <- if (!is.na(wrdi_val))
        paste0('<div style="background:#E0E0E0;border-radius:3px;height:6px;margin-top:4px">',
               '<div style="background:', color, ';width:', round(wrdi_val*100), '%;height:6px;border-radius:3px"></div></div>')
      else ""

      paste0(
        '<tr style="background:', bg, '">',
        '<td style="border-left:4px solid ', color, ';padding:10px 12px">',
          '<strong>', r["label"], '</strong><br>',
          '<small style="color:', color, ';font-weight:600">',
            r["priority_label"], '</small>',
        '</td>',
        '<td style="text-align:center;padding:10px;font-weight:700">',
          r["n_records"],
        '</td>',
        '<td style="text-align:center;padding:10px">',
          '<span style="font-weight:700;color:', color, '">',
            if(is.na(wrdi_val)) "N/A" else paste0(round(wrdi_val*100,0), "%"),
          '</span>', wrdi_bar,
        '</td>',
        '<td style="text-align:center;padding:10px">',
          r["confidence"],
        '</td>',
        '<td style="padding:10px;color:#555;font-size:12px">',
          r["priority_reason"],
        '</td>',
        '</tr>'
      )
    }), collapse = "\n")
  }

  # Context rows
  context_rows <- if (nrow(context) > 0) {
    paste0(apply(context, 1, function(r) {
      paste0(
        '<tr style="background:#F0F8FF">',
        '<td style="border-left:4px solid #4A90D9;padding:10px 12px">',
          '<strong>', r["label"], '</strong><br>',
          '<small style="color:#4A90D9">',
            if(is_es) "Tema dominante del corpus - contexto tecnologico"
            else "Dominant corpus topic - technological context",
          '</small>',
        '</td>',
        '<td style="text-align:center;padding:10px;font-weight:700">', r["n_records"], '</td>',
        '<td style="text-align:center;padding:10px">',
          round(as.numeric(r["RCS"]), 1), 'x', '</td>',
        '<td colspan="2" style="padding:10px;color:#555;font-size:12px">',
          if(is_es)
            "Esta categoria describe el contexto tecnologico del corpus, no un riesgo laboral especifico. Los riesgos asociados se detallan en las secciones siguientes."
          else
            "This category describes the technological context of the corpus, not a specific occupational risk. Associated risks are detailed in the sections below.",
        '</td>',
        '</tr>'
      )
    }), collapse = "\n")
  } else ""

  html <- paste0('<!DOCTYPE html>
<html lang="', if(is_es) "es" else "en", '">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>ORISMA - ', if(is_es) "Ficha de Riesgo Laboral" else "Occupational Risk Sheet", '</title>
<style>
  :root { --green:#0F6E56; --red:#D85A30; --amber:#E8A838; --blue:#4A90D9; }
  * { box-sizing:border-box; margin:0; padding:0; }
  body { font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;
         max-width:1050px; margin:0 auto; padding:2rem; color:#2c2c2c;
         line-height:1.5; }
  .header { background:var(--green); color:white; padding:1.5rem 2rem;
             border-radius:10px; margin-bottom:2rem; }
  .header h1 { font-size:1.6rem; margin-bottom:0.25rem; }
  .header .sub { opacity:0.85; font-size:0.95rem; }
  .header .meta { margin-top:0.75rem; font-size:0.8rem; opacity:0.7; }
  .kpis { display:grid; grid-template-columns:repeat(5,1fr); gap:0.75rem;
           margin:1.5rem 0; }
  .kpi { background:#F8F9FA; border:1px solid #E0E0E0; border-radius:8px;
          padding:1rem; text-align:center; }
  .kpi .val { font-size:1.8rem; font-weight:700; }
  .kpi .lbl { font-size:0.75rem; color:#666; margin-top:0.2rem; }
  .section { margin:2rem 0; }
  .section h2 { font-size:1.05rem; font-weight:600; color:var(--green);
                 border-bottom:2px solid #9FE1CB; padding-bottom:0.4rem;
                 margin-bottom:1rem; }
  .section h3 { font-size:0.95rem; font-weight:600; color:#444;
                 margin:1.5rem 0 0.5rem; }
  table { border-collapse:collapse; width:100%; font-size:0.86rem; }
  th { background:var(--green); color:white; padding:9px 12px; text-align:left;
       font-weight:500; }
  td { border-bottom:1px solid #EEEEEE; vertical-align:top; }
  .alert-red { background:#FFF3F0; border-left:4px solid var(--red);
                padding:0.9rem 1.1rem; border-radius:0 6px 6px 0;
                margin:0.75rem 0; font-size:0.88rem; }
  .alert-blue { background:#F0F8FF; border-left:4px solid var(--blue);
                 padding:0.9rem 1.1rem; border-radius:0 6px 6px 0;
                 margin:0.75rem 0; font-size:0.88rem; }
  .alert-amber { background:#FFFBF0; border-left:4px solid var(--amber);
                  padding:0.9rem 1.1rem; border-radius:0 6px 6px 0;
                  margin:0.75rem 0; font-size:0.88rem; }
  .wrdi-global { background:#F8F9FA; border-radius:8px; padding:1rem 1.5rem;
                  margin:1rem 0; border:1px solid #E0E0E0; }
  .wrdi-bar-big { background:#E0E0E0; border-radius:6px; height:12px; margin:0.5rem 0; }
  .wrdi-fill-big { height:12px; border-radius:6px; }
  .meth-box { background:#F8F9FA; border-radius:8px; padding:1.25rem 1.5rem;
               border:1px solid #E0E0E0; font-size:0.85rem; }
  .meth-box ul { padding-left:1.25rem; }
  .meth-box li { margin:0.4rem 0; color:#555; }
  .footer { background:#F8F9FA; border-top:1px solid #E0E0E0; padding:1rem 1.5rem;
             font-size:0.78rem; color:#888; margin-top:2.5rem;
             border-radius:0 0 8px 8px; }
  .legend { display:flex; gap:1rem; flex-wrap:wrap; margin:0.5rem 0;
             font-size:0.82rem; }
  .legend-item { display:flex; align-items:center; gap:0.4rem; }
  .legend-dot { width:12px; height:12px; border-radius:50%; }
  @media print { body{max-width:100%;padding:1rem} .kpis{grid-template-columns:repeat(5,1fr)} }
</style>
</head>
<body>

<div class="header">
  <h1>ORISMA &mdash; ', if(is_es) "Ficha de Riesgos Laborales" else "Occupational Risk Sheet", '</h1>
  <div class="sub"><strong>', topic, '</strong></div>
  <div class="meta">
    ', if(is_es) "Generado" else "Generated", ': ', date_str,
    ' &nbsp;|&nbsp; orisma v0.1.0 &nbsp;|&nbsp; ',
    if(is_es) "Diccionario" else "Dictionary", ': iso45001_insst v2.0.0 (',
    n_cats, if(is_es) ' categorias)' else ' categories)',
  '</div>
</div>

<!-- KPIs -->
<div class="kpis">
  <div class="kpi">
    <div class="val" style="color:', wrdi_color, '">', wrdi, '</div>
    <div class="lbl">WRDI Global</div>
  </div>
  <div class="kpi">
    <div class="val" style="color:#D85A30">', n_red, '</div>
    <div class="lbl">', if(is_es) "Brechas CRITICAS" else "CRITICAL gaps", '</div>
  </div>
  <div class="kpi">
    <div class="val" style="color:#E8A838">', n_amber, '</div>
    <div class="lbl">', if(is_es) "Atencion" else "Attention", '</div>
  </div>
  <div class="kpi">
    <div class="val" style="color:#0F6E56">', n_green, '</div>
    <div class="lbl">', if(is_es) "Bien cubiertos" else "Well covered", '</div>
  </div>
  <div class="kpi">
    <div class="val" style="color:#888">', n_grey, '</div>
    <div class="lbl">', if(is_es) "Sin evidencia" else "No evidence", '</div>
  </div>
</div>

<!-- WRDI global -->
<div class="section">
  <h2>', if(is_es) "Indice de Desconexion Tecnico-Laboral (WRDI)"
         else "Worker-Risk Disconnection Index (WRDI)", '</h2>
  <div class="wrdi-global">
    <strong>WRDI = ', wrdi, ' (', wrdi_pct, '%)</strong> &mdash; ', wrdi_interp, '
    <div class="wrdi-bar-big">
      <div class="wrdi-fill-big" style="background:', wrdi_color,
           ';width:', wrdi_pct, '%"></div>
    </div>
    <small style="color:#888">',
      if(is_es)
        "El WRDI mide la proporcion de estudios que caracterizan un riesgo sin incluir datos directos de exposicion de trabajadores reales en condiciones de trabajo. Un WRDI alto indica que la investigacion disponible es principalmente tecnica y tiene baja transferibilidad preventiva directa."
      else
        "WRDI measures the proportion of studies that characterise a risk without including direct exposure data from real workers in actual working conditions. A high WRDI indicates that available research is primarily technical and has low direct preventive transferability.",
    '</small>
  </div>
</div>

<!-- Context categories -->',
  if (nrow(context) > 0) paste0('
<div class="section">
  <h2>', if(is_es) "Contexto tecnologico del corpus"
         else "Technological context of the corpus", '</h2>
  <div class="alert-blue">',
    if(is_es)
      "Las siguientes categorias describen el marco tecnologico del corpus (RCS muy alto = dominan el corpus). No son riesgos en si mismos sino el contexto en el que se estudian los riesgos que aparecen en las secciones siguientes."
    else
      "The following categories describe the technological framework of the corpus (very high RCS = they dominate the corpus). They are not risks per se but the context in which the risks shown in the following sections are studied.",
  '</div>
  <table>
    <thead><tr>
      <th>', if(is_es) "Categoria" else "Category", '</th>
      <th>N</th>
      <th>RCS</th>
      <th colspan="2">', if(is_es) "Interpretacion" else "Interpretation", '</th>
    </tr></thead>
    <tbody>', context_rows, '</tbody>
  </table>
</div>') else "",

'<!-- Legend -->
<div class="section">
  <h2>', if(is_es) "Clasificacion de riesgos por prioridad"
         else "Risk classification by priority", '</h2>

  <div class="legend">
    <div class="legend-item">
      <div class="legend-dot" style="background:#D85A30"></div>
      <span><strong>RED</strong>: ',
        if(is_es) "Brecha critica - sobreestudiado sin datos de trabajadores"
        else "Critical gap - over-studied without worker data", '</span>
    </div>
    <div class="legend-item">
      <div class="legend-dot" style="background:#E8A838"></div>
      <span><strong>AMBER</strong>: ',
        if(is_es) "Requiere atencion - evidencia parcial"
        else "Requires attention - partial evidence", '</span>
    </div>
    <div class="legend-item">
      <div class="legend-dot" style="background:#0F6E56"></div>
      <span><strong>GREEN</strong>: ',
        if(is_es) "Cobertura razonable - buena conexion con trabajadores"
        else "Reasonable coverage - good worker data connection", '</span>
    </div>
    <div class="legend-item">
      <div class="legend-dot" style="background:#888"></div>
      <span><strong>GREY</strong>: ',
        if(is_es) "Evidencia insuficiente"
        else "Insufficient evidence", '</span>
    </div>
  </div>

  <!-- RED -->',
  if (n_red > 0) paste0('
  <h3 style="color:#D85A30">&#9888; ',
    if(is_es) "Brechas criticas (ROJO) - Requieren evaluacion presencial inmediata"
    else "Critical gaps (RED) - Require immediate on-site assessment",
  '</h3>
  <div class="alert-red">',
    if(is_es)
      "Estos riesgos estan documentados en la literatura cientifica pero ninguno o casi ningun estudio incluye datos reales de exposicion de trabajadores. Son los candidatos prioritarios para evaluacion de riesgos in situ."
    else
      "These risks are documented in scientific literature but few or no studies include real worker exposure data. They are the top candidates for on-site risk assessment.",
  '</div>
  <table>
    <thead><tr>
      <th>', if(is_es) "Categoria de riesgo" else "Risk category", '</th>
      <th>N</th>
      <th>WRDI</th>
      <th>', if(is_es) "Confianza" else "Confidence", '</th>
      <th>', if(is_es) "Razon" else "Reason", '</th>
    </tr></thead>
    <tbody>', .risk_rows(risks[risks$priority == "RED", ]), '</tbody>
  </table>') else "",

  if (n_amber > 0) paste0('
  <h3 style="color:#E8A838">&#9654; ',
    if(is_es) "Requieren atencion (AMBAR)"
    else "Require attention (AMBER)",
  '</h3>
  <table>
    <thead><tr>
      <th>', if(is_es) "Categoria de riesgo" else "Risk category", '</th>
      <th>N</th>
      <th>WRDI</th>
      <th>', if(is_es) "Confianza" else "Confidence", '</th>
      <th>', if(is_es) "Razon" else "Reason", '</th>
    </tr></thead>
    <tbody>', .risk_rows(risks[risks$priority == "AMBER", ]), '</tbody>
  </table>') else "",

  if (n_green > 0) paste0('
  <h3 style="color:#0F6E56">&#10003; ',
    if(is_es) "Cobertura razonable (VERDE)"
    else "Reasonable coverage (GREEN)",
  '</h3>
  <table>
    <thead><tr>
      <th>', if(is_es) "Categoria de riesgo" else "Risk category", '</th>
      <th>N</th>
      <th>WRDI</th>
      <th>', if(is_es) "Confianza" else "Confidence", '</th>
      <th>', if(is_es) "Razon" else "Reason", '</th>
    </tr></thead>
    <tbody>', .risk_rows(risks[risks$priority == "GREEN", ]), '</tbody>
  </table>') else "",

'</div>

<!-- Methodological section -->
<div class="section">
  <h2>', if(is_es) "Seccion metodologica" else "Methodological section", '</h2>
  <div class="meth-box">

    <h3>', if(is_es) "1. Fuentes y estrategia de busqueda" else "1. Sources and search strategy", '</h3>
    <p style="margin:0.5rem 0;color:#555">', search_txt, '</p>

    <h3>', if(is_es) "2. Criterios de inclusion y exclusion" else "2. Inclusion and exclusion criteria", '</h3>
    <p style="margin:0.5rem 0;color:#555">', incl_txt, '</p>

    <h3>', if(is_es) "3. Proceso de deduplicacion" else "3. Deduplication process", '</h3>
    <ul>
      <li>', if(is_es) paste0("Registros identificados: ", n_load)
             else paste0("Records identified: ", n_load), '</li>
      <li>', if(is_es) paste0("Duplicados eliminados: ", n_dedup,
                               " (matching exacto por DOI + titulo normalizado + fuzzy matching)")
             else paste0("Duplicates removed: ", n_dedup,
                          " (exact DOI match + normalised title + fuzzy matching)"), '</li>
      <li>', if(is_es) paste0("Registros unicos analizados: ", n_final)
             else paste0("Unique records analysed: ", n_final), '</li>
    </ul>

    <h3>', if(is_es) "4. Definicion del WRDI" else "4. WRDI definition", '</h3>
    <ul>
      <li>', if(is_es)
        "Un estudio se considera que tiene datos de exposicion de trabajadores cuando su abstract menciona terminos como: 'worker exposure', 'occupational exposure', 'breathing zone', 'personal sampling', 'field study', 'workplace measurement'."
      else
        "A study is considered to have worker exposure data when its abstract mentions terms such as: 'worker exposure', 'occupational exposure', 'breathing zone', 'personal sampling', 'field study', 'workplace measurement'.", '</li>
      <li>', if(is_es)
        "Limitacion: la deteccion se basa en el abstract, no en el texto completo. Estudios con datos de trabajadores pero sin mencionarlos en el abstract pueden no ser detectados."
      else
        "Limitation: detection is based on the abstract, not the full text. Studies with worker data not mentioned in the abstract may not be detected.", '</li>
    </ul>

    <h3>', if(is_es) "5. Extraccion de categorias de riesgo" else "5. Risk category extraction", '</h3>
    <ul>
      <li>', if(is_es)
        paste0("Diccionario: iso45001_insst v2.0.0 (", n_cats,
               " categorias ancladas en INSST, ISO 45001:2018, NIOSH y EU-OSHA)")
      else
        paste0("Dictionary: iso45001_insst v2.0.0 (", n_cats,
               " categories anchored in INSST, ISO 45001:2018, NIOSH and EU-OSHA)"), '</li>
      <li>', if(is_es)
        "La clasificacion es automatica por busqueda de terminos en titulo, abstract y keywords. Puede producir falsos positivos. Se recomienda validacion manual de una muestra representativa mediante orm_validate()."
      else
        "Classification is automatic via term search in title, abstract and keywords. False positives may occur. Manual validation of a representative sample is recommended using orm_validate().", '</li>
      <li>', if(is_es)
        "El nivel de confianza (Alta/Media/Baja) refleja el numero de estudios que detectaron la categoria, no la calidad metodologica de dichos estudios."
      else
        "The confidence level (High/Medium/Low) reflects the number of studies detecting the category, not the methodological quality of those studies.", '</li>
    </ul>

    <h3>', if(is_es) "6. Limitaciones del analisis" else "6. Analysis limitations", '</h3>
    <ul>
      <li>', if(is_es)
        "Este informe no sustituye la evaluacion de riesgos in situ reglamentaria."
      else
        "This report does not replace the mandatory on-site regulatory risk assessment.", '</li>
      <li>', if(is_es)
        "La normativa aplicable varia por pais y sector. El tecnico debe aplicar la regulacion vigente en su jurisdiccion para cada riesgo identificado."
      else
        "Applicable regulation varies by country and sector. The practitioner must apply current regulation in their jurisdiction for each identified risk.", '</li>
      <li>', if(is_es)
        "Las categorias con confianza Baja (N < 3) tienen evidencia insuficiente para conclusiones solidas."
      else
        "Categories with Low confidence (N < 3) have insufficient evidence for robust conclusions.", '</li>
      <li>', if(is_es)
        "La busqueda bibliografica puede no ser exhaustiva si no se han consultado todas las bases de datos relevantes."
      else
        "The bibliographic search may not be exhaustive if not all relevant databases were consulted.", '</li>
    </ul>
  </div>
</div>

<div class="footer">
  <strong>ORISMA v0.1.0</strong> &mdash;
  Dr. Ra&uacute;l Aguilar-Elena &middot; GPRL &middot; VIU &nbsp;|&nbsp;
  Ana Delgado-Garc&iacute;a &middot; USAL<br>
  ', if(is_es) "Generado" else "Generated", ': ', date_str, ' &nbsp;|&nbsp;
  <a href="https://github.com/Aguilar-Elena/orisma">github.com/Aguilar-Elena/orisma</a><br>
  <em>',
    if(is_es)
      "Herramienta de apoyo bibliometrico. No constituye asesoramiento legal ni sustituye la evaluacion reglamentaria de riesgos."
    else
      "Bibliometric support tool. Does not constitute legal advice and does not replace mandatory regulatory risk assessment.",
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
