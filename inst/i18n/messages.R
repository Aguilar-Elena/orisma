# orisma · Bilingual message system (EN / ES)
# All user-facing strings are defined here.
# To add a new language, duplicate the "es" block and translate.

.orisma_messages <- list(

  en = list(
    # Phase headers
    phase_load     = "Phase 1 · Loading and ingestion",
    phase_dedup    = "Phase 2 · Deduplication",
    phase_audit    = "Phase 3 · Quality audit",
    phase_extract  = "Phase 4 · Risk category extraction",
    phase_analyse  = "Phase 5 · Bibliometric analysis",
    phase_report   = "Phase 6 · Report generation",

    # orm_load
    load_start     = "Reading reference files from: {path}",
    load_files     = "Files found: {n} ({formats})",
    load_done      = "Records loaded: {n_total} from {n_sources} source(s)",
    load_no_files  = "No supported files found in: {path}",
    load_fmt_hint  = "Supported formats: .ris, .bib, .csv",

    # orm_dedup
    dedup_start    = "Starting deduplication pipeline",
    dedup_doi      = "Step 1/3 · Exact DOI match: {n_removed} duplicates removed",
    dedup_title    = "Step 2/3 · Normalised title match: {n_removed} duplicates removed",
    dedup_fuzzy    = "Step 3/3 · Fuzzy match (title + year + author): {n_removed} duplicates removed",
    dedup_review   = "{n_ambiguous} ambiguous case(s) require manual review — see dedup_log.csv",
    dedup_done     = "Deduplication complete · {n_unique} unique records retained ({n_total_removed} removed)",

    # orm_audit
    audit_start    = "Running quality audit",
    audit_retract  = "Retracted records flagged: {n}",
    audit_pubpeer  = "PubPeer alerts flagged: {n}",
    audit_ok       = "Records passing audit: {n}",

    # orm_extract
    extract_start  = "Extracting risk categories using dictionary: {dict_name} (v{dict_version})",
    extract_done   = "Extraction complete · {n_records} records × {n_cats} risk categories",
    extract_empty  = "Warning: {n} records matched no risk category — check your dictionary",

    # orm_analyse
    analyse_start  = "Computing bibliometric indicators",
    analyse_wrdi   = "WRDI computed: {value} ({pct}% of studies lack direct worker exposure data)",
    analyse_rcs    = "RCS computed for {n_cats} risk categories",
    analyse_mgp    = "MGP computed for {n_mats} materials",
    analyse_done   = "Analysis complete",

    # orm_report
    report_start   = "Generating reports in: {out_dir}",
    report_html    = "Executive report (HTML): {file}",
    report_pdf     = "Academic report (PDF): {file}",
    report_cert    = "Reproducibility certificate: {file}",
    report_done    = "All outputs saved to: {out_dir}",

    # Errors
    err_no_doi     = "Column 'doi' not found — DOI-based deduplication skipped",
    err_no_title   = "Column 'title' not found — cannot proceed",
    err_dict_miss  = "Dictionary file not found: {path}",
    err_lang       = "Language '{lang}' not supported. Use 'en' or 'es'."
  ),

  es = list(
    # Cabeceras de fase
    phase_load     = "Fase 1 · Carga e ingestión",
    phase_dedup    = "Fase 2 · Deduplicación",
    phase_audit    = "Fase 3 · Auditoría de calidad",
    phase_extract  = "Fase 4 · Extracción de categorías de riesgo",
    phase_analyse  = "Fase 5 · Análisis bibliométrico",
    phase_report   = "Fase 6 · Generación de informes",

    # orm_load
    load_start     = "Leyendo ficheros de referencias desde: {path}",
    load_files     = "Ficheros encontrados: {n} ({formats})",
    load_done      = "Registros cargados: {n_total} de {n_sources} fuente(s)",
    load_no_files  = "No se encontraron ficheros compatibles en: {path}",
    load_fmt_hint  = "Formatos admitidos: .ris, .bib, .csv",

    # orm_dedup
    dedup_start    = "Iniciando pipeline de deduplicación",
    dedup_doi      = "Paso 1/3 · Coincidencia exacta por DOI: {n_removed} duplicados eliminados",
    dedup_title    = "Paso 2/3 · Coincidencia por título normalizado: {n_removed} duplicados eliminados",
    dedup_fuzzy    = "Paso 3/3 · Fuzzy matching (título + año + autor): {n_removed} duplicados eliminados",
    dedup_review   = "{n_ambiguous} caso(s) ambiguo(s) requieren revisión manual — véase dedup_log.csv",
    dedup_done     = "Deduplicación completada · {n_unique} registros únicos retenidos ({n_total_removed} eliminados)",

    # orm_audit
    audit_start    = "Ejecutando auditoría de calidad",
    audit_retract  = "Registros retractados marcados: {n}",
    audit_pubpeer  = "Alertas PubPeer marcadas: {n}",
    audit_ok       = "Registros que superan la auditoría: {n}",

    # orm_extract
    extract_start  = "Extrayendo categorías de riesgo con diccionario: {dict_name} (v{dict_version})",
    extract_done   = "Extracción completada · {n_records} registros × {n_cats} categorías de riesgo",
    extract_empty  = "Aviso: {n} registros no coincidieron con ninguna categoría — revise el diccionario",

    # orm_analyse
    analyse_start  = "Calculando indicadores bibliométricos",
    analyse_wrdi   = "WRDI calculado: {value} ({pct}% de estudios sin datos de exposición directa de trabajadores)",
    analyse_rcs    = "RCS calculado para {n_cats} categorías de riesgo",
    analyse_mgp    = "MGP calculado para {n_mats} materiales",
    analyse_done   = "Análisis completado",

    # orm_report
    report_start   = "Generando informes en: {out_dir}",
    report_html    = "Informe ejecutivo (HTML): {file}",
    report_pdf     = "Informe académico (PDF): {file}",
    report_cert    = "Certificado de reproducibilidad: {file}",
    report_done    = "Todos los outputs guardados en: {out_dir}",

    # Errores
    err_no_doi     = "Columna 'doi' no encontrada — deduplicación por DOI omitida",
    err_no_title   = "Columna 'title' no encontrada — no es posible continuar",
    err_dict_miss  = "Fichero de diccionario no encontrado: {path}",
    err_lang       = "Idioma '{lang}' no admitido. Use 'en' o 'es'."
  )
)

#' Internal: retrieve a bilingual message
#' @noRd
.msg <- function(key, lang = getOption("orisma.lang", "en"), ...) {
  msgs <- .orisma_messages[[lang]]
  if (is.null(msgs)) {
    lang <- "en"
    msgs <- .orisma_messages[["en"]]
  }
  template <- msgs[[key]]
  if (is.null(template)) return(paste0("[missing message: ", key, "]"))
  glue::glue(template, .envir = list(...))
}
