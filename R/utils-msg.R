# R/utils-msg.R
# Sistema de mensajes bilingue - funcion interna permanente del paquete
# Esto reemplaza el sistema de inst/i18n/messages.R

.orisma_messages <- list(
  en = list(
    phase_load     = "Phase 1 - Loading and ingestion",
    phase_dedup    = "Phase 2 - Deduplication",
    phase_audit    = "Phase 3 - Quality audit",
    phase_extract  = "Phase 4 - Risk category extraction",
    phase_analyse  = "Phase 5 - Bibliometric analysis",
    phase_report   = "Phase 6 - Report generation",
    load_start     = "Reading reference files from: {path}",
    load_files     = "Files found: {n} ({formats})",
    load_done      = "Records loaded: {n_total} from {n_sources} source(s)",
    load_no_files  = "No supported files found in: {path}",
    load_fmt_hint  = "Supported formats: .ris, .bib, .csv",
    dedup_start    = "Starting deduplication pipeline",
    dedup_doi      = "Step 1/3 - Exact DOI match: {n_removed} duplicates removed",
    dedup_title    = "Step 2/3 - Normalised title match: {n_removed} duplicates removed",
    dedup_fuzzy    = "Step 3/3 - Fuzzy match: {n_removed} duplicates removed",
    dedup_review   = "{n_ambiguous} ambiguous case(s) require manual review",
    dedup_done     = "Deduplication complete - {n_unique} unique records retained ({n_total_removed} removed)",
    audit_start    = "Running quality audit",
    audit_retract  = "Retracted records flagged: {n}",
    audit_pubpeer  = "PubPeer alerts flagged: {n}",
    audit_ok       = "Records passing audit: {n}",
    extract_start  = "Extracting risk categories using dictionary: {dict_name} (v{dict_version})",
    extract_done   = "Extraction complete - {n_records} records x {n_cats} risk categories",
    extract_empty  = "Warning: {n} records matched no risk category",
    analyse_start  = "Computing bibliometric indicators",
    analyse_wrdi   = "WRDI computed: {value} ({pct}% of studies lack direct worker exposure data)",
    analyse_rcs    = "RCS computed for {n_cats} risk categories",
    analyse_mgp    = "MGP computed for {n_mats} materials",
    analyse_done   = "Analysis complete",
    report_start   = "Generating reports in: {out_dir}",
    report_html    = "Executive report (HTML): {file}",
    report_pdf     = "Academic report (PDF): {file}",
    report_cert    = "Reproducibility certificate: {file}",
    report_done    = "All outputs saved to: {out_dir}",
    err_no_doi     = "Column 'doi' not found - DOI-based deduplication skipped",
    err_no_title   = "Column 'title' not found - cannot proceed",
    err_dict_miss  = "Dictionary file not found: {path}",
    err_lang       = "Language '{lang}' not supported. Use 'en' or 'es'."
  ),
  es = list(
    phase_load     = "Fase 1 - Carga e ingestion",
    phase_dedup    = "Fase 2 - Deduplicacion",
    phase_audit    = "Fase 3 - Auditoria de calidad",
    phase_extract  = "Fase 4 - Extraccion de categorias de riesgo",
    phase_analyse  = "Fase 5 - Analisis bibliometrico",
    phase_report   = "Fase 6 - Generacion de informes",
    load_start     = "Leyendo ficheros desde: {path}",
    load_files     = "Ficheros encontrados: {n} ({formats})",
    load_done      = "Registros cargados: {n_total} de {n_sources} fuente(s)",
    load_no_files  = "No se encontraron ficheros en: {path}",
    load_fmt_hint  = "Formatos admitidos: .ris, .bib, .csv",
    dedup_start    = "Iniciando pipeline de deduplicacion",
    dedup_doi      = "Paso 1/3 - DOI exacto: {n_removed} duplicados eliminados",
    dedup_title    = "Paso 2/3 - Titulo normalizado: {n_removed} duplicados eliminados",
    dedup_fuzzy    = "Paso 3/3 - Fuzzy matching: {n_removed} duplicados eliminados",
    dedup_review   = "{n_ambiguous} caso(s) ambiguo(s) requieren revision manual",
    dedup_done     = "Deduplicacion completada - {n_unique} registros unicos ({n_total_removed} eliminados)",
    audit_start    = "Ejecutando auditoria de calidad",
    audit_retract  = "Registros retractados: {n}",
    audit_pubpeer  = "Alertas PubPeer: {n}",
    audit_ok       = "Registros que superan la auditoria: {n}",
    extract_start  = "Extrayendo categorias con diccionario: {dict_name} (v{dict_version})",
    extract_done   = "Extraccion completada - {n_records} registros x {n_cats} categorias",
    extract_empty  = "Aviso: {n} registros sin categoria detectada",
    analyse_start  = "Calculando indicadores bibliometricos",
    analyse_wrdi   = "WRDI calculado: {value} ({pct}% sin datos de trabajadores)",
    analyse_rcs    = "RCS calculado para {n_cats} categorias",
    analyse_mgp    = "MGP calculado para {n_mats} materiales",
    analyse_done   = "Analisis completado",
    report_start   = "Generando informes en: {out_dir}",
    report_html    = "Informe ejecutivo (HTML): {file}",
    report_pdf     = "Informe academico (PDF): {file}",
    report_cert    = "Certificado de reproducibilidad: {file}",
    report_done    = "Todos los outputs guardados en: {out_dir}",
    err_no_doi     = "Columna 'doi' no encontrada - deduplicacion por DOI omitida",
    err_no_title   = "Columna 'title' no encontrada - no es posible continuar",
    err_dict_miss  = "Fichero de diccionario no encontrado: {path}",
    err_lang       = "Idioma '{lang}' no admitido. Use 'en' o 'es'."
  )
)

#' Internal message retrieval function
#' @noRd
.msg <- function(key, lang = getOption("orisma.lang", "en"), ...) {
  msgs <- .orisma_messages[[lang]]
  if (is.null(msgs)) {
    lang <- "en"
    msgs <- .orisma_messages[["en"]]
  }
  template <- msgs[[key]]
  if (is.null(template)) return(paste0("[missing message: ", key, "]"))
  # Use simple string interpolation instead of glue to avoid environment issues
  result <- template
  args <- list(...)
  for (nm in names(args)) {
    result <- gsub(paste0("\\{", nm, "\\}"), as.character(args[[nm]]), result)
  }
  result
}
