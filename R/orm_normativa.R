#' Cross-reference detected risks with applicable European regulation
#'
#' @description
#' `orm_normativa()` crosses the risk categories detected by ORISMA with
#' the main applicable European directives and ISO standards, providing
#' the occupational health practitioner with a direct regulatory anchor
#' for each identified risk.
#'
#' The regulatory database is built into ORISMA and covers EU directives,
#' Spanish INSST technical notes (NTP), and key ISO standards. It is
#' updated with each major package release.
#'
#' @param result An `orisma_result` object.
#' @param min_records Integer. Min records for a category to be included.
#'   Default `1`.
#' @param lang Character. `"en"` or `"es"`.
#'
#' @return A data frame with detected categories and their applicable
#'   regulations.
#' @export
orm_normativa <- function(result,
                           min_records = 1L,
                           lang        = getOption("orisma.lang", "en")) {

  .check_lang(lang)
  if (!inherits(result, "orisma_result"))
    stop("'result' must be an orisma_result object.", call. = FALSE)

  # Built-in regulatory database
  reg_db <- .build_reg_db(lang)

  ind <- result$indicators %>%
    dplyr::filter(.data$n_records >= min_records)

  # Join with regulatory database
  reg_df <- dplyr::left_join(ind, reg_db, by = "category") %>%
    dplyr::filter(!is.na(.data$directive)) %>%
    dplyr::select(
      .data$label, .data$n_records, .data$WRDI,
      .data$directive, .data$directive_name,
      .data$limit_value, .data$key_requirement
    ) %>%
    dplyr::arrange(dplyr::desc(.data$n_records))

  class(reg_df) <- c("orisma_normativa", "data.frame")
  reg_df
}


#' @noRd
.build_reg_db <- function(lang) {
  is_es <- lang == "es"

  data.frame(
    category = c(
      "chemical_hazardous",
      "chemical_hazardous",
      "carcinogens",
      "noise",
      "vibration",
      "non_ionising_radiation",
      "ionising_radiation",
      "manual_handling",
      "display_screen",
      "biological",
      "bacteria",
      "virus",
      "fungi",
      "asbestos",
      "explosion",
      "fire",
      "electrical_direct",
      "electrical_indirect",
      "thermal_stress",
      "nanomaterials",
      "additive_manufacturing",
      "psychosocial",
      "workload_pace",
      "harassment",
      "work_posture",
      "repetitive_movements"
    ),
    directive = c(
      "Dir. 98/24/CE",
      "Reg. REACH (CE) 1907/2006",
      "Dir. 2004/37/CE",
      "Dir. 2003/10/CE",
      "Dir. 2002/44/CE",
      "Dir. 2006/25/CE",
      "Dir. 2013/59/EURATOM",
      "Dir. 90/269/CEE",
      "Dir. 90/270/CEE",
      "Dir. 2000/54/CE",
      "Dir. 2000/54/CE",
      "Dir. 2000/54/CE",
      "Dir. 2000/54/CE",
      "Dir. 2009/148/CE",
      "Dir. 1999/92/CE (ATEX)",
      "Dir. 89/654/CEE",
      "Dir. 89/391/CEE",
      "Dir. 89/391/CEE",
      "ISO 7933 / ISO 11079",
      "Rec. CE 2022/C 229/01",
      "EN ISO 11228 / NTP 1120",
      "Acuerdo Marco Europeo 2004",
      "Acuerdo Marco Europeo 2004",
      "Dir. 2002/73/CE",
      "ISO 11228-3 / NTP 629",
      "ISO 11228-3 / NTP 629"
    ),
    directive_name = c(
      if(is_es) "Agentes quimicos" else "Chemical agents",
      if(is_es) "Registro sustancias quimicas" else "Chemical substances registration",
      if(is_es) "Agentes cancerigenos y mutagenos" else "Carcinogens and mutagens",
      if(is_es) "Ruido" else "Noise at work",
      if(is_es) "Vibraciones mecanicas" else "Mechanical vibration",
      if(is_es) "Radiacion optica artificial" else "Artificial optical radiation",
      if(is_es) "Radiaciones ionizantes" else "Ionising radiation",
      if(is_es) "Manipulacion manual de cargas" else "Manual handling of loads",
      if(is_es) "Pantallas de visualizacion" else "Display screen equipment",
      if(is_es) "Agentes biologicos" else "Biological agents",
      if(is_es) "Agentes biologicos" else "Biological agents",
      if(is_es) "Agentes biologicos" else "Biological agents",
      if(is_es) "Agentes biologicos" else "Biological agents",
      if(is_es) "Exposicion al amianto" else "Asbestos exposure",
      if(is_es) "Atmosteras explosivas" else "Explosive atmospheres",
      if(is_es) "Lugares de trabajo" else "Workplace requirements",
      if(is_es) "Marco general de seguridad" else "General safety framework",
      if(is_es) "Marco general de seguridad" else "General safety framework",
      if(is_es) "Estres termico (norma tecnica)" else "Thermal stress (technical standard)",
      if(is_es) "Recomendacion nanomateriales" else "Nanomaterials recommendation",
      if(is_es) "Fabricacion aditiva (NTP)" else "Additive manufacturing (NTP)",
      if(is_es) "Estres laboral (acuerdo)" else "Work-related stress (framework)",
      if(is_es) "Estres laboral (acuerdo)" else "Work-related stress (framework)",
      if(is_es) "Igualdad de trato" else "Equal treatment",
      if(is_es) "Movimientos repetitivos" else "Repetitive movements",
      if(is_es) "Movimientos repetitivos" else "Repetitive movements"
    ),
    limit_value = c(
      if(is_es) "VLA-ED / VLA-EC segun sustancia" else "OEL/STEL per substance",
      if(is_es) "Ver ficha de datos de seguridad" else "See safety data sheet",
      if(is_es) "VLA especifico por cancerigeno" else "Specific OEL per carcinogen",
      if(is_es) "LEX,8h <= 80 dB(A); Lpico <= 135 dB(C)" else "LEX,8h <= 80 dB(A); Lpeak <= 135 dB(C)",
      if(is_es) "HAV: A(8) <= 2.5 m/s2; WBV: A(8) <= 0.5 m/s2" else "HAV: A(8) <= 2.5 m/s2; WBV: A(8) <= 0.5 m/s2",
      if(is_es) "Segun longitud de onda y clase laser" else "Per wavelength and laser class",
      if(is_es) "Dosis efectiva: 20 mSv/ano" else "Effective dose: 20 mSv/year",
      if(is_es) "Ecuacion NIOSH / peso maximo recomendado" else "NIOSH equation / recommended weight limit",
      if(is_es) "Evaluacion ergonomica obligatoria" else "Mandatory ergonomic assessment",
      if(is_es) "Clasificacion grupos 1-4" else "Groups 1-4 classification",
      if(is_es) "Clasificacion grupos 1-4" else "Groups 1-4 classification",
      if(is_es) "Clasificacion grupos 1-4" else "Groups 1-4 classification",
      if(is_es) "Clasificacion grupos 1-4" else "Groups 1-4 classification",
      if(is_es) "0.1 fibras/cm3 (8h)" else "0.1 fibres/cm3 (8h)",
      if(is_es) "Zona ATEX clasificada" else "ATEX zone classification",
      if(is_es) "Plan de emergencia obligatorio" else "Mandatory emergency plan",
      if(is_es) "Evaluacion de riesgos obligatoria" else "Mandatory risk assessment",
      if(is_es) "Evaluacion de riesgos obligatoria" else "Mandatory risk assessment",
      if(is_es) "WBGT segun metabolismo" else "WBGT per metabolic rate",
      if(is_es) "Gestion del riesgo por analogia" else "Risk management by analogy",
      if(is_es) "Evaluacion higienica especifica" else "Specific hygiene assessment",
      if(is_es) "Evaluacion psicosocial obligatoria" else "Mandatory psychosocial assessment",
      if(is_es) "Evaluacion psicosocial obligatoria" else "Mandatory psychosocial assessment",
      if(is_es) "Protocolo de acoso obligatorio" else "Mandatory harassment protocol",
      if(is_es) "OCRA / indice de repetitividad" else "OCRA / repetitivity index",
      if(is_es) "OCRA / indice de repetitividad" else "OCRA / repetitivity index"
    ),
    key_requirement = c(
      if(is_es) "Medicion ambiental y biologica; VLA como referencia" else "Environmental and biological monitoring; OEL as reference",
      if(is_es) "Registro, evaluacion y autorizacion de sustancias" else "Registration, evaluation, authorisation of substances",
      if(is_es) "Sustitucion preferente; control de exposicion" else "Substitution preferred; exposure control",
      if(is_es) "Medicion del nivel de ruido; audiometria periodica" else "Noise level measurement; periodic audiometry",
      if(is_es) "Medicion vibraciones; vigilancia de la salud" else "Vibration measurement; health surveillance",
      if(is_es) "Evaluacion exposicion; gafas protectoras laser" else "Exposure assessment; laser protective eyewear",
      if(is_es) "Dosimetria individual; vigilancia medica" else "Individual dosimetry; medical surveillance",
      if(is_es) "Evaluacion tarea; ayudas mecanicas si procede" else "Task assessment; mechanical aids if applicable",
      if(is_es) "Evaluacion puesto; pausas; ajuste ergonomico" else "Workstation assessment; breaks; ergonomic adjustment",
      if(is_es) "Clasificacion agente; medidas de contencion" else "Agent classification; containment measures",
      if(is_es) "Clasificacion agente; medidas de contencion" else "Agent classification; containment measures",
      if(is_es) "Clasificacion agente; medidas de contencion" else "Agent classification; containment measures",
      if(is_es) "Clasificacion agente; medidas de contencion" else "Agent classification; containment measures",
      if(is_es) "Inventario; plan de desamiantado" else "Inventory; asbestos management plan",
      if(is_es) "Clasificacion zona; equipos certificados ATEX" else "Zone classification; ATEX certified equipment",
      if(is_es) "Vias de evacuacion; extincion; simulacros" else "Evacuation routes; extinguishing; drills",
      if(is_es) "Evaluacion y medidas preventivas documentadas" else "Assessment and documented preventive measures",
      if(is_es) "Evaluacion y medidas preventivas documentadas" else "Assessment and documented preventive measures",
      if(is_es) "Medicion WBGT; acceso agua; pausas termicas" else "WBGT measurement; water access; thermal breaks",
      if(is_es) "Principio de precaucion; gestion por analogia" else "Precautionary principle; analogy-based management",
      if(is_es) "Evaluacion higienica; EPIs especificos AM" else "Hygiene assessment; AM-specific PPE",
      if(is_es) "Evaluacion metodo FPSICO; plan de intervencion" else "FPSICO method assessment; intervention plan",
      if(is_es) "Evaluacion metodo FPSICO; plan de intervencion" else "FPSICO method assessment; intervention plan",
      if(is_es) "Protocolo anti-acoso; canal de denuncia" else "Anti-harassment protocol; reporting channel",
      if(is_es) "Evaluacion OCRA; rotacion de tareas" else "OCRA assessment; task rotation",
      if(is_es) "Evaluacion OCRA; rotacion de tareas" else "OCRA assessment; task rotation"
    ),
    stringsAsFactors = FALSE
  )
}


#' Print method for orisma_normativa
#' @param x An `orisma_normativa` object.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.orisma_normativa <- function(x, ...) {
  cat("\n-- ORISMA Regulatory Cross-reference --\n")
  cat(" Risk categories with applicable regulation:", nrow(x), "\n\n")
  for (i in seq_len(nrow(x))) {
    cat(sprintf(" [%d] %s\n", i, x$label[i]))
    cat(sprintf("     Directive: %s - %s\n", x$directive[i], x$directive_name[i]))
    cat(sprintf("     Limit/Reference: %s\n", x$limit_value[i]))
    cat(sprintf("     Key requirement: %s\n\n", x$key_requirement[i]))
  }
  invisible(x)
}
