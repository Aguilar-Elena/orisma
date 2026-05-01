#' Built-in risk dictionaries for ORISMA
#'
#' @description
#' ORISMA ships with a normative risk dictionary anchored in three
#' internationally recognised taxonomies:
#'
#' - **ISO 45001:2018** - international OHS management system standard
#' - **INSST** - Instituto Nacional de Seguridad y Salud en el Trabajo (Spain)
#' - **NIOSH** - National Institute for Occupational Safety and Health (USA)
#'
#' Each dictionary entry maps a **risk category** (standardised name) to a
#' **vector of search terms** (the vocabulary the scientific literature uses
#' for that category, in English). Users can extend or replace any entry.
#'
#' @format A named list. Each element is a list with:
#' \describe{
#'   \item{`label_en`}{Risk category name in English}
#'   \item{`label_es`}{Risk category name in Spanish}
#'   \item{`taxonomy`}{Source taxonomy: "ISO45001", "INSST", "NIOSH", or "ORISMA"}
#'   \item{`terms`}{Character vector of search terms (case-insensitive matching)}
#'   \item{`worker_exposure_terms`}{Terms indicating direct worker exposure data}
#' }
#'
#' @examples
#' # View available dictionaries
#' orm_dict_list()
#'
#' # Load the default dictionary
#' dict <- orm_dict()
#'
#' # View categories in a dictionary
#' orm_dict_categories(dict)
#'
#' # Extend with custom terms
#' dict <- orm_dict_add_terms(dict, category = "nanoparticles",
#'                            terms = c("nano-aerosol", "nano-dust"))
#'
#' @name orisma_dict
NULL


# ── Master dictionary ─────────────────────────────────────────────────────────

.dict_iso45001_insst <- list(

  # ── 1. Airborne particles and nanoparticles (most prevalent in AM literature)
  nanoparticles = list(
    label_en = "Nanoparticles and ultrafine particles",
    label_es = "Nanoparticulas y particulas ultrafinas",
    taxonomy  = "ISO45001/INSST",
    terms = c(
      "nanoparticle", "nanoparticles", "ultrafine particle", "ultrafine particles",
      "nano-particle", "UFP", "sub-micron particle", "aerosol emission",
      "particle emission", "particle exposure", "airborne particle",
      "PM0.1", "PM2.5", "respirable particle", "inhalable particle",
      "metal particle", "metallic particle", "powder emission",
      "particle concentration", "particle number concentration",
      "particle size distribution", "nano-aerosol"
    ),
    worker_exposure_terms = c(
      "worker exposure", "occupational exposure", "personal exposure",
      "breathing zone", "inhalation exposure", "operator exposure"
    )
  ),

  # ── 2. Industrial hygiene and chemical emissions
  hygiene_emissions = list(
    label_en = "Industrial hygiene and chemical emissions",
    label_es = "Higiene industrial y emisiones quimicas",
    taxonomy  = "ISO45001/INSST/NIOSH",
    terms = c(
      "industrial hygiene", "chemical emission", "fume emission",
      "metal fume", "volatile organic compound", "VOC", "condensate",
      "by-product", "process emission", "air quality", "contaminant",
      "pollutant", "exposure limit", "occupational exposure limit", "OEL",
      "threshold limit value", "TLV", "permissible exposure limit", "PEL",
      "time-weighted average", "TWA", "short-term exposure limit", "STEL",
      "biological monitoring", "biomonitoring", "urinary metal",
      "oxidative stress", "cytotoxicity", "genotoxicity"
    ),
    worker_exposure_terms = c(
      "worker", "operator", "technician", "employee", "personnel",
      "workforce", "occupational cohort"
    )
  ),

  # ── 3. Laser and optical radiation
  laser_radiation = list(
    label_en = "Laser and optical radiation",
    label_es = "Radiacion laser y radiacion optica",
    taxonomy  = "ISO45001/INSST",
    terms = c(
      "laser radiation", "laser safety", "laser hazard", "optical radiation",
      "laser exposure", "laser beam", "laser power", "laser wavelength",
      "eye hazard", "skin hazard", "retinal damage", "corneal damage",
      "laser class", "laser protective eyewear", "laser goggles",
      "non-ionising radiation", "NIR", "UV radiation", "infrared radiation",
      "laser interlock", "laser enclosure"
    ),
    worker_exposure_terms = c(
      "worker", "operator", "laser operator", "laser technician"
    )
  ),

  # ── 4. Inert gas asphyxiation
  asphyxiation = list(
    label_en = "Asphyxiation by inert process gases",
    label_es = "Asfixia por gases inertes de proceso",
    taxonomy  = "ISO45001/INSST",
    terms = c(
      "asphyxiation", "asphyxia", "oxygen depletion", "oxygen displacement",
      "oxygen deficiency", "inert gas", "argon atmosphere", "nitrogen atmosphere",
      "confined space", "oxygen monitoring", "oxygen sensor",
      "gas leak", "inert atmosphere", "process gas", "shielding gas",
      "build chamber atmosphere"
    ),
    worker_exposure_terms = c(
      "worker", "operator", "confined space entry"
    )
  ),

  # ── 5. Fire and explosion (dust and gas)
  explosion = list(
    label_en = "Fire and explosion risk (dust and gas)",
    label_es = "Riesgo de incendio y explosion (polvo y gas)",
    taxonomy  = "ISO45001/INSST",
    terms = c(
      "explosion", "explosive atmosphere", "dust explosion", "metal dust",
      "reactive metal", "combustible dust", "ignition", "ATEX",
      "minimum ignition energy", "MIE", "lower explosive limit", "LEL",
      "deflagration", "fire risk", "fire hazard", "flammability",
      "titanium dust", "aluminium dust", "magnesium dust",
      "electrostatic discharge", "grounding", "bonding",
      "explosion-proof", "dust cloud", "hybrid mixture"
    ),
    worker_exposure_terms = c(
      "worker", "operator", "plant personnel"
    )
  ),

  # ── 6. Ergonomic and musculoskeletal hazards
  ergonomics = list(
    label_en = "Ergonomics and musculoskeletal hazards",
    label_es = "Ergonomia y riesgos musculoesqueleticos",
    taxonomy  = "ISO45001/INSST/NIOSH",
    terms = c(
      "ergonomic", "ergonomics", "musculoskeletal", "manual handling",
      "manual lifting", "repetitive movement", "awkward posture",
      "work-related musculoskeletal disorder", "WRMSD", "WMSD",
      "upper limb disorder", "low back pain", "physical workload",
      "biomechanical", "postural assessment", "RULA", "REBA",
      "NIOSH lifting equation", "force exertion"
    ),
    worker_exposure_terms = c(
      "worker", "operator", "musculoskeletal symptom"
    )
  ),

  # ── 7. Electrical hazards
  electrical = list(
    label_en = "Electrical contact and electrical hazards",
    label_es = "Contacto electrico y riesgos electricos",
    taxonomy  = "ISO45001/INSST",
    terms = c(
      "electrical hazard", "electric shock", "electrical contact",
      "electrocution", "arc flash", "arc blast", "short circuit",
      "lockout tagout", "LOTO", "electrical safety",
      "high voltage", "live conductor", "electrical insulation",
      "residual current", "earth fault", "electrical maintenance"
    ),
    worker_exposure_terms = c(
      "worker", "electrician", "maintenance technician"
    )
  ),

  # ── 8. Fall hazards
  falls = list(
    label_en = "Falls from height and fall hazards",
    label_es = "Caidas en altura y riesgos de caida",
    taxonomy  = "ISO45001/INSST",
    terms = c(
      "fall from height", "fall hazard", "working at height",
      "elevated work", "ladder safety", "scaffold", "guardrail",
      "fall protection", "personal fall arrest", "harness",
      "slip trip fall", "floor hazard"
    ),
    worker_exposure_terms = c(
      "worker", "operator", "maintenance worker"
    )
  ),

  # ── 9. Psychosocial risks
  psychosocial = list(
    label_en = "Psychosocial risks and work stress",
    label_es = "Riesgos psicosociales y estres laboral",
    taxonomy  = "ISO45001/INSST/NIOSH",
    terms = c(
      "psychosocial risk", "work stress", "burnout", "job strain",
      "work-related stress", "mental health", "anxiety", "depression",
      "work-life balance", "shift work", "night work", "job demand",
      "job control", "effort reward imbalance", "harassment",
      "mobbing", "workplace violence", "emotional exhaustion"
    ),
    worker_exposure_terms = c(
      "worker", "employee", "healthcare worker", "shift worker"
    )
  ),

  # ── 10. Biological hazards
  biological = list(
    label_en = "Biological hazards",
    label_es = "Riesgos biologicos",
    taxonomy  = "ISO45001/INSST/NIOSH",
    terms = c(
      "biological hazard", "biohazard", "pathogen", "infection risk",
      "occupational infection", "zoonosis", "bloodborne pathogen",
      "sharps injury", "needlestick", "biosafety", "PPE biological",
      "disinfection", "decontamination"
    ),
    worker_exposure_terms = c(
      "worker", "healthcare worker", "laboratory worker"
    )
  )
)


# ── Public API for dictionaries ───────────────────────────────────────────────

#' List available built-in dictionaries
#' @export
orm_dict_list <- function() {
  cat("Available built-in dictionaries:\n")
  cat("  'iso45001_insst' - ISO 45001 + INSST + NIOSH (default, recommended)\n")
  invisible(c("iso45001_insst"))
}


#' Load a risk dictionary
#'
#' @param name Character. Dictionary name. Use [orm_dict_list()] to see options.
#'   Default `"iso45001_insst"`.
#' @return A named list (class `orisma_dict`) with risk categories.
#' @export
orm_dict <- function(name = "iso45001_insst") {
  d <- switch(name,
    iso45001_insst = .dict_iso45001_insst,
    stop(paste0("Unknown dictionary: '", name,
                "'. Use orm_dict_list() to see options."), call. = FALSE)
  )
  class(d) <- c("orisma_dict", "list")
  attr(d, "dict_name")    <- name
  attr(d, "dict_version") <- "1.0.0"
  attr(d, "dict_created") <- Sys.time()
  d
}


#' List risk categories in a dictionary
#' @param dict An `orisma_dict` object.
#' @param lang Character. `"en"` or `"es"`.
#' @export
orm_dict_categories <- function(dict, lang = getOption("orisma.lang", "en")) {
  cats <- lapply(names(dict), function(k) {
    entry <- dict[[k]]
    data.frame(
      category  = k,
      label     = if (lang == "es") entry$label_es else entry$label_en,
      taxonomy  = entry$taxonomy,
      n_terms   = length(entry$terms),
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(cats)
}


#' Add terms to an existing dictionary category
#'
#' @param dict An `orisma_dict` object.
#' @param category Character. Category key (e.g. `"nanoparticles"`).
#' @param terms Character vector. New terms to add.
#' @return Updated `orisma_dict`.
#' @export
orm_dict_add_terms <- function(dict, category, terms) {
  if (!category %in% names(dict)) {
    stop(paste0("Category '", category, "' not found. Use orm_dict_categories() to list available categories."),
         call. = FALSE)
  }
  dict[[category]]$terms <- unique(c(dict[[category]]$terms, terms))
  dict
}


#' Add a completely new risk category to a dictionary
#'
#' @param dict An `orisma_dict` object.
#' @param key Character. Short identifier key (no spaces, e.g. `"vibration"`).
#' @param label_en Character. Category name in English.
#' @param label_es Character. Category name in Spanish.
#' @param terms Character vector. Search terms.
#' @param worker_exposure_terms Character vector. Terms indicating direct worker data.
#' @param taxonomy Character. Source taxonomy label.
#' @return Updated `orisma_dict`.
#' @export
orm_dict_add_category <- function(dict, key, label_en, label_es, terms,
                                   worker_exposure_terms = character(0),
                                   taxonomy = "user") {
  if (key %in% names(dict)) {
    warning(paste0("Category '", key, "' already exists. Use orm_dict_add_terms() to add terms."))
    return(dict)
  }
  dict[[key]] <- list(
    label_en              = label_en,
    label_es              = label_es,
    taxonomy              = taxonomy,
    terms                 = terms,
    worker_exposure_terms = worker_exposure_terms
  )
  dict
}
