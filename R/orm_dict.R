#' Built-in risk dictionaries for ORISMA
#'
#' @description
#' ORISMA ships with a comprehensive normative risk dictionary anchored in four
#' internationally recognised taxonomies:
#'
#' - **INSST** Instituto Nacional de Seguridad y Salud en el Trabajo (Spain)
#' - **ISO 45001:2018** International OHS management system standard
#' - **NIOSH** National Institute for Occupational Safety and Health (USA)
#' - **EU-OSHA** European Agency for Safety and Health at Work
#'
#' The dictionary covers 58 risk categories organised in 6 blocks:
#' A) Safety at work (18), B) Industrial hygiene (8), C) Ergonomics (8),
#' D) Psychosociology (11), E) Biological hazards (5),
#' F) Emerging technologies (8).
#'
#' Category assignment is multi-label: a record may be assigned to more than one
#' category, so category frequencies do not sum to the number of records.
#'
#' @section Citing the taxonomy:
#' The dictionary is also published as an autonomous, citable resource under a
#' CC BY 4.0 licence, in CSV, JSON and SKOS/Turtle, so that it can be applied or
#' extended independently of this package:
#'
#' Aguilar-Elena, R., & Delgado-Garcia, A. (2026). *ORISMA Occupational Risk
#' Category Taxonomy* (Version 1.0.0) \[Data set\]. Zenodo.
#' \doi{10.5281/zenodo.22066582}
#'
#' If you use the dictionary, please cite it in addition to the package.
#'
#' @examples
#' # View available dictionaries
#' orm_dict_list()
#'
#' # Load the default dictionary
#' dict <- orm_dict()
#'
#' # View all categories
#' orm_dict_categories(dict)
#'
#' # Add custom terms to a category
#' dict <- orm_dict_add_terms(dict, "nanomaterials",
#'                            c("metal powder", "powder bed"))
#'
#' @name orisma_dict
NULL


# =============================================================================
# BLOQUE A - SEGURIDAD EN EL TRABAJO (18 categorias - INSST)
# =============================================================================

.dict_a <- list(

  # A01
  fall_height = list(
    label_en  = "Fall from height (fall to different level)",
    label_es  = "Caida de personas a distinto nivel",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "fall from height", "fall from elevation", "elevated work platform",
      "working at height", "roof work", "scaffold fall", "ladder fall",
      "fall arrest", "fall protection", "guardrail", "safety harness",
      "personal fall arrest system", "PFAS", "lifeline", "anchor point",
      "roof access", "elevated surface", "mezzanine fall", "tower fall",
      "climbing fall", "fall prevention", "fall restraint"
    ),
    worker_exposure_terms = c("worker", "operator", "roofer", "climber")
  ),

  # A02
  fall_same_level = list(
    label_en  = "Fall at same level (slip, trip, fall)",
    label_es  = "Caida de personas al mismo nivel",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "slip", "trip", "fall same level", "floor hazard", "wet floor",
      "slippery surface", "uneven floor", "housekeeping slip",
      "walking surface", "floor contamination", "traction loss",
      "stumble", "loss of balance", "flat surface fall",
      "spilled liquid", "floor marking", "anti-slip"
    ),
    worker_exposure_terms = c("worker", "employee", "pedestrian")
  ),

  # A03
  falling_objects_collapse = list(
    label_en  = "Falling objects by collapse or landslide",
    label_es  = "Caida de objetos por desplome o derrumbamiento",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "structural collapse", "wall collapse", "roof collapse",
      "trench collapse", "excavation collapse", "landslide",
      "falling debris", "building collapse", "cave-in",
      "unstable structure", "overhead hazard", "falling material",
      "storage collapse", "racking collapse", "shelf collapse"
    ),
    worker_exposure_terms = c("worker", "construction worker", "miner")
  ),

  # A04
  falling_objects_handling = list(
    label_en  = "Falling objects during handling or manipulation",
    label_es  = "Caida de objetos en manipulacion",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "falling object", "dropped load", "overhead load",
      "material handling fall", "load drop", "rigging failure",
      "crane load", "hoist load", "forklift load",
      "dropped tool", "object projection", "falling part",
      "unsecured load", "hard hat", "head protection"
    ),
    worker_exposure_terms = c("worker", "crane operator", "rigger")
  ),

  # A05
  stepping_objects = list(
    label_en  = "Stepping on objects (puncture wounds)",
    label_es  = "Pisadas sobre objetos",
    block     = "A - Safety",
    taxonomy  = "INSST",
    terms = c(
      "stepping on object", "puncture wound", "nail puncture",
      "sharp object floor", "foot injury", "safety footwear",
      "steel toe boot", "puncture resistant sole",
      "sharps on floor", "debris floor"
    ),
    worker_exposure_terms = c("worker", "construction worker")
  ),

  # A06
  collision_stationary = list(
    label_en  = "Collision with stationary objects",
    label_es  = "Choques contra objetos inmoviles",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "collision stationary object", "impact fixed structure",
      "bumping into", "struck by fixed object", "head bump",
      "low clearance", "protruding object", "blind corner",
      "collision wall", "struck against", "impact injury"
    ),
    worker_exposure_terms = c("worker", "operator")
  ),

  # A07
  collision_moving = list(
    label_en  = "Collision with moving objects",
    label_es  = "Choques contra objetos moviles",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "struck by moving object", "moving machinery contact",
      "impact moving part", "collision moving equipment",
      "swinging load", "pendulum load", "moving conveyor",
      "robot collision", "cobot contact", "moving arm impact",
      "automated guided vehicle", "AGV collision"
    ),
    worker_exposure_terms = c("worker", "operator", "bystander")
  ),

  # A08
  cuts_tools = list(
    label_en  = "Cuts and blows by objects or tools",
    label_es  = "Cortes o golpes por objetos o herramientas",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "laceration", "cut injury", "blade injury", "tool injury",
      "sharp edge", "cutting tool", "hand tool injury",
      "power tool injury", "knife injury", "chisel injury",
      "grinding wheel", "saw injury", "blunt trauma",
      "impact tool", "hammer injury", "hand injury"
    ),
    worker_exposure_terms = c("worker", "machinist", "operator")
  ),

  # A09
  projection_fragments = list(
    label_en  = "Projection of fragments or particles",
    label_es  = "Proyeccion de fragmentos o particulas",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "projectile", "fragment projection", "particle ejection",
      "flying debris", "chip ejection", "metal fragment",
      "grinding particle", "spark projection", "ejected part",
      "pressure release fragment", "bursting fragment",
      "eye injury projection", "face shield", "eye protection"
    ),
    worker_exposure_terms = c("worker", "machinist", "welder")
  ),

  # A10
  entrapment = list(
    label_en  = "Entrapment between objects",
    label_es  = "Atrapamiento por o entre objetos",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "entrapment", "caught between", "pinch point", "nip point",
      "machine guarding", "mechanical hazard", "rotating part",
      "in-running nip", "conveyor entrapment", "press entrapment",
      "machinery entrapment", "power transmission", "gear entrapment",
      "lockout tagout", "LOTO", "energy isolation",
      "machine safeguarding", "safety guard removal"
    ),
    worker_exposure_terms = c("worker", "machine operator", "maintenance")
  ),

  # A11
  overturn = list(
    label_en  = "Entrapment by vehicle overturn",
    label_es  = "Atrapamiento por vuelco de maquinas o vehiculos",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "vehicle overturn", "forklift overturn", "rollover",
      "tractor overturn", "ROPS", "rollover protection",
      "vehicle stability", "load stability", "tipping",
      "crane overturn", "aerial platform overturn"
    ),
    worker_exposure_terms = c("driver", "operator", "worker")
  ),

  # A12
  vehicle_collision = list(
    label_en  = "Run over or collision with vehicles",
    label_es  = "Atropellos o golpes con vehiculos",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "run over", "vehicle collision", "forklift accident",
      "pedestrian vehicle", "traffic accident workplace",
      "road accident", "vehicle pedestrian", "AGV accident",
      "reversing vehicle", "blind spot vehicle",
      "workplace transport", "loading dock accident"
    ),
    worker_exposure_terms = c("worker", "pedestrian", "driver")
  ),

  # A13
  electrical_direct = list(
    label_en  = "Direct electrical contact",
    label_es  = "Contactos electricos directos",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "electrical contact", "electric shock", "electrocution",
      "live conductor", "bare wire", "exposed conductor",
      "high voltage contact", "direct contact electrical",
      "power line contact", "arc flash", "arc blast",
      "electrical burn", "current through body"
    ),
    worker_exposure_terms = c("electrician", "worker", "operator")
  ),

  # A14
  electrical_indirect = list(
    label_en  = "Indirect electrical contact",
    label_es  = "Contactos electricos indirectos",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "indirect electrical contact", "earth fault",
      "residual current", "ground fault", "insulation failure",
      "leakage current", "step potential", "touch potential",
      "RCD", "residual current device", "earth leakage",
      "equipotential bonding", "electrical grounding"
    ),
    worker_exposure_terms = c("worker", "electrician")
  ),

  # A15
  thermal_contact = list(
    label_en  = "Thermal contact (burns and scalds)",
    label_es  = "Contactos termicos",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "thermal burn", "contact burn", "scald", "hot surface",
      "molten metal splash", "steam burn", "hot fluid",
      "cryogenic burn", "cold burn", "frostbite contact",
      "thermal contact injury", "radiant heat burn",
      "welding burn", "laser burn", "fire burn"
    ),
    worker_exposure_terms = c("worker", "welder", "operator")
  ),

  # A16
  explosion = list(
    label_en  = "Explosion",
    label_es  = "Explosiones",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "explosion", "explosive atmosphere", "dust explosion",
      "gas explosion", "ATEX", "deflagration", "detonation",
      "explosive limit", "lower explosive limit", "LEL",
      "upper explosive limit", "UEL", "flammable atmosphere",
      "combustible dust", "metal dust explosion", "titanium dust",
      "aluminium dust", "minimum ignition energy", "MIE",
      "electrostatic ignition", "explosion proof",
      "pressure vessel explosion", "BLEVE", "boilover",
      "reactive chemical", "peroxide explosion"
    ),
    worker_exposure_terms = c("worker", "operator", "plant personnel")
  ),

  # A17
  fire = list(
    label_en  = "Fire",
    label_es  = "Incendios",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "fire", "fire hazard", "fire risk", "flammable material",
      "combustible material", "ignition source", "fire prevention",
      "fire suppression", "fire extinguisher", "sprinkler",
      "fire evacuation", "fire load", "flashpoint",
      "autoignition", "flammability", "fire spread",
      "hot work", "welding fire", "grinding fire"
    ),
    worker_exposure_terms = c("worker", "fire warden", "operator")
  ),

  # A18
  toxic_exposure_safety = list(
    label_en  = "Exposure to irritant or toxic substances (safety context)",
    label_es  = "Exposicion a sustancias irritantes, corrosivas o toxicas",
    block     = "A - Safety",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "chemical burn", "corrosive substance", "acid burn",
      "alkali burn", "skin corrosion", "eye corrosion",
      "irritant skin contact", "acute toxic exposure",
      "hazardous substance spill", "chemical splash",
      "SDS", "safety data sheet", "GHS", "CLP regulation",
      "PPE chemical", "chemical resistant glove",
      "face shield chemical"
    ),
    worker_exposure_terms = c("worker", "laboratory worker", "operator")
  )
)


# =============================================================================
# BLOQUE B - HIGIENE INDUSTRIAL (8 categorias - INSST)
# =============================================================================

.dict_b <- list(

  # B01
  chemical_hazardous = list(
    label_en  = "Exposure to hazardous chemical agents",
    label_es  = "Exposicion a agentes quimicos peligrosos",
    block     = "B - Industrial hygiene",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "chemical exposure", "hazardous chemical", "toxic chemical",
      "occupational exposure limit", "OEL", "threshold limit value", "TLV",
      "time-weighted average", "TWA", "short-term exposure limit", "STEL",
      "permissible exposure limit", "PEL", "biological monitoring",
      "biomonitoring", "urinary metabolite", "blood level",
      "nanoparticle", "nanoparticles", "ultrafine particle", "UFP",
      "nano-aerosol", "airborne particle", "particle emission",
      "aerosol emission", "metal fume", "welding fume",
      "volatile organic compound", "VOC", "organic solvent",
      "isocyanate", "epoxy resin", "flux fume",
      "respirable dust", "inhalable dust", "total dust",
      "PM2.5", "PM10", "PM0.1", "particle concentration",
      "particle number concentration", "particle size distribution",
      "breathing zone", "personal air sampling", "area monitoring",
      "industrial hygiene", "exposure assessment", "air quality",
      "contaminant", "pollutant", "sensitizer", "allergen",
      "chemical contaminant", "workplace air", "ventilation",
      "local exhaust ventilation", "LEV", "general ventilation",
      "fume hood", "enclosure", "emission control",
      "metal powder", "powder handling", "powder exposure",
      "additive manufacturing emission", "3D printing emission",
      "laser sintering emission", "EBM emission", "SLM emission",
      "directed energy deposition fume", "DED emission"
    ),
    worker_exposure_terms = c(
      "worker exposure", "occupational exposure", "personal exposure",
      "breathing zone", "operator exposure", "worker", "employee"
    )
  ),

  # B02
  carcinogens = list(
    label_en  = "Exposure to carcinogens, mutagens and reprotoxic substances (CMR)",
    label_es  = "Exposicion a agentes quimicos cancerigenos o mutagenos",
    block     = "B - Industrial hygiene",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "carcinogen", "carcinogenic", "mutagen", "mutagenic",
      "reprotoxic", "CMR substance", "IARC classification",
      "group 1 carcinogen", "group 2A carcinogen",
      "occupational cancer", "lung cancer occupational",
      "bladder cancer occupational", "mesothelioma",
      "genotoxicity", "genotoxic", "DNA damage",
      "oxidative stress", "cytotoxicity", "cytotoxic",
      "chromium VI", "hexavalent chromium", "nickel carcinogen",
      "cobalt carcinogen", "beryllium carcinogen",
      "cadmium carcinogen", "arsenic carcinogen",
      "formaldehyde carcinogen", "benzene carcinogen",
      "PAH", "polycyclic aromatic hydrocarbon",
      "diesel exhaust", "silica crystalline", "silicosis",
      "wood dust", "hardwood dust", "cancer risk",
      "occupational carcinogen"
    ),
    worker_exposure_terms = c(
      "worker", "occupational exposure", "cancer incidence"
    )
  ),

  # B03
  asbestos = list(
    label_en  = "Exposure to asbestos fibres",
    label_es  = "Exposicion a fibras de amianto",
    block     = "B - Industrial hygiene",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "asbestos", "asbestos fibre", "asbestos fiber",
      "mesothelioma", "asbestosis", "chrysotile",
      "crocidolite", "amosite", "amphibole",
      "asbestos exposure", "asbestos removal",
      "asbestos abatement", "asbestos survey",
      "asbestos containing material", "ACM",
      "fibrous mineral", "man-made mineral fibre", "MMMF",
      "refractory ceramic fibre", "RCF"
    ),
    worker_exposure_terms = c(
      "worker", "asbestos worker", "insulation worker"
    )
  ),

  # B04
  ionising_radiation = list(
    label_en  = "Exposure to ionising radiation",
    label_es  = "Exposicion a radiaciones ionizantes",
    block     = "B - Industrial hygiene",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "ionising radiation", "ionizing radiation",
      "X-ray", "gamma ray", "neutron radiation",
      "alpha particle", "beta particle", "radioactive",
      "radioactivity", "radiological hazard",
      "dose equivalent", "effective dose", "sievert", "Sv",
      "gray", "Gy", "absorbed dose", "dose rate",
      "radiation protection", "radiation shielding",
      "dosimeter", "TLD", "film badge",
      "occupational dose limit", "annual dose limit",
      "NORM", "naturally occurring radioactive material",
      "radon", "radon exposure", "CT scan radiation",
      "fluoroscopy radiation", "radiation worker"
    ),
    worker_exposure_terms = c(
      "radiation worker", "worker", "radiographer"
    )
  ),

  # B05
  non_ionising_radiation = list(
    label_en  = "Exposure to non-ionising radiation",
    label_es  = "Exposicion a radiaciones no ionizantes",
    block     = "B - Industrial hygiene",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "non-ionising radiation", "non-ionizing radiation",
      "laser radiation", "laser safety", "laser hazard",
      "laser exposure", "laser class", "laser protective eyewear",
      "optical radiation", "UV radiation", "ultraviolet radiation",
      "infrared radiation", "visible radiation",
      "electromagnetic field", "EMF", "ELF", "extremely low frequency",
      "radiofrequency", "RF radiation", "microwave radiation",
      "induction heating", "magnetic field exposure",
      "electric field exposure", "ICNIRP guideline",
      "5G radiation", "wireless radiation",
      "arc welding radiation", "UV welding arc",
      "photobiological hazard", "retinal hazard",
      "skin photosensitivity", "photokeratitis"
    ),
    worker_exposure_terms = c(
      "worker", "laser operator", "welder"
    )
  ),

  # B06
  noise = list(
    label_en  = "Noise exposure",
    label_es  = "Exposicion a ruido",
    block     = "B - Industrial hygiene",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "noise", "noise exposure", "occupational noise",
      "noise induced hearing loss", "NIHL",
      "hearing loss", "hearing damage", "audiogram",
      "audiometry", "decibel", "dB", "dBA",
      "sound pressure level", "SPL", "noise level",
      "daily noise exposure", "LEX", "Lex8h",
      "peak sound pressure", "hearing protection",
      "ear plug", "ear muff", "hearing conservation",
      "noise assessment", "noise measurement",
      "noise control", "sound insulation",
      "tinnitus", "hyperacusis", "cochlear damage",
      "noise regulation", "noise standard"
    ),
    worker_exposure_terms = c(
      "worker", "noise-exposed worker", "operator"
    )
  ),

  # B07
  vibration = list(
    label_en  = "Vibration exposure (hand-arm and whole-body)",
    label_es  = "Exposicion a vibraciones",
    block     = "B - Industrial hygiene",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "vibration", "hand-arm vibration", "HAV",
      "whole-body vibration", "WBV",
      "vibration exposure", "vibration measurement",
      "vibration dose value", "VDV",
      "daily vibration exposure", "A8",
      "vibration white finger", "VWF",
      "Raynaud phenomenon occupational",
      "carpal tunnel vibration",
      "hand transmitted vibration",
      "vibrating tool", "pneumatic tool vibration",
      "grinder vibration", "chainsaw vibration",
      "vibration standard", "ISO 5349", "ISO 2631",
      "anti-vibration glove", "vibration damping"
    ),
    worker_exposure_terms = c(
      "worker", "operator", "driver"
    )
  ),

  # B08
  thermal_stress = list(
    label_en  = "Thermal stress (heat and cold)",
    label_es  = "Exposicion a temperaturas ambientales extremas",
    block     = "B - Industrial hygiene",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "heat stress", "heat strain", "thermal stress",
      "heat illness", "heat exhaustion", "heat stroke",
      "hyperthermia", "WBGT", "wet bulb globe temperature",
      "hot environment", "high temperature workplace",
      "radiant heat", "metabolic heat", "heat acclimatisation",
      "cold stress", "cold exposure", "hypothermia",
      "frostbite", "cold environment", "cold work",
      "cryogenic exposure", "freezer work",
      "thermal comfort", "thermal environment",
      "ISO 7933", "ISO 11079", "NIOSH heat criteria",
      "heat related illness", "cold injury"
    ),
    worker_exposure_terms = c(
      "worker", "outdoor worker", "furnace worker"
    )
  )
)


# =============================================================================
# BLOQUE C - ERGONOMIA (8 categorias - INSST)
# =============================================================================

.dict_c <- list(

  # C01
  manual_handling = list(
    label_en  = "Manual handling of loads",
    label_es  = "Carga fisica por manipulacion manual de cargas",
    block     = "C - Ergonomics",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "manual handling", "manual lifting", "load handling",
      "lifting task", "carrying load", "pushing pulling",
      "NIOSH lifting equation", "recommended weight limit", "RWL",
      "cumulative load", "load weight", "awkward posture lifting",
      "musculoskeletal disorder", "MSD", "WMSD",
      "low back pain", "lumbar disorder", "back injury",
      "disc herniation occupational", "manual materials handling",
      "MMH", "patient handling", "patient lifting",
      "mechanical lifting aid", "hoist", "trolley"
    ),
    worker_exposure_terms = c(
      "worker", "nurse", "warehouse worker"
    )
  ),

  # C02
  work_posture = list(
    label_en  = "Physical load by work postures",
    label_es  = "Carga fisica por posturas de trabajo",
    block     = "C - Ergonomics",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "work posture", "awkward posture", "static posture",
      "postural assessment", "RULA", "REBA", "OWAS",
      "neck posture", "shoulder posture", "back posture",
      "trunk flexion", "trunk rotation", "overhead work",
      "prolonged standing", "prolonged sitting",
      "ergonomic assessment", "postural risk",
      "musculoskeletal risk", "upper limb disorder",
      "work-related upper limb disorder", "WRULD",
      "shoulder disorder", "neck disorder",
      "ergonomic intervention"
    ),
    worker_exposure_terms = c(
      "worker", "operator", "assembly worker"
    )
  ),

  # C03
  repetitive_movements = list(
    label_en  = "Repetitive movements (high frequency)",
    label_es  = "Carga fisica por movimientos repetitivos de alta frecuencia",
    block     = "C - Ergonomics",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "repetitive movement", "repetitive motion", "repetitive task",
      "repetitive strain injury", "RSI",
      "cycle time", "repetition rate", "high frequency task",
      "hand repetition level", "HRL",
      "OCRA index", "OCRA checklist", "strain index",
      "carpal tunnel syndrome", "tendinitis", "epicondylitis",
      "De Quervain", "trigger finger", "tenosynovitis",
      "keyboard repetition", "mouse repetition",
      "assembly line repetition"
    ),
    worker_exposure_terms = c(
      "worker", "assembly worker", "keyboard worker"
    )
  ),

  # C04
  mental_load = list(
    label_en  = "Mental workload",
    label_es  = "Carga mental",
    block     = "C - Ergonomics",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "mental workload", "cognitive load", "cognitive demand",
      "attention demand", "concentration demand",
      "information overload", "decision demand",
      "working memory load", "dual task",
      "NASA-TLX", "mental effort",
      "vigilance task", "monitoring task",
      "cognitive fatigue", "mental fatigue",
      "human error", "cognitive ergonomics",
      "situation awareness", "supervisory control"
    ),
    worker_exposure_terms = c(
      "worker", "operator", "air traffic controller"
    )
  ),

  # C05
  thermal_comfort = list(
    label_en  = "Moderate thermal environment (thermal comfort)",
    label_es  = "Ambiente termico moderado",
    block     = "C - Ergonomics",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "thermal comfort", "thermal environment", "PMV", "PPD",
      "predicted mean vote", "perceived temperature",
      "office temperature", "workplace temperature",
      "air temperature", "relative humidity workplace",
      "thermal neutrality", "ISO 7730",
      "HVAC comfort", "air conditioning comfort",
      "draught discomfort", "radiant asymmetry"
    ),
    worker_exposure_terms = c(
      "worker", "office worker", "employee"
    )
  ),

  # C06
  lighting = list(
    label_en  = "Lighting conditions",
    label_es  = "Iluminacion",
    block     = "C - Ergonomics",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "lighting", "illuminance", "lux", "luminance",
      "glare", "visual comfort", "workplace lighting",
      "task lighting", "natural light", "daylight",
      "artificial lighting", "LED lighting",
      "visual fatigue", "eye strain", "visual discomfort",
      "contrast ratio", "reflectance", "light flicker",
      "emergency lighting", "EN 12464"
    ),
    worker_exposure_terms = c(
      "worker", "office worker", "visual task worker"
    )
  ),

  # C07
  indoor_air_quality = list(
    label_en  = "Indoor air quality",
    label_es  = "Calidad del ambiente interior",
    block     = "C - Ergonomics",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "indoor air quality", "IAQ", "sick building syndrome",
      "building related illness", "CO2 indoor",
      "carbon dioxide indoor", "ventilation rate",
      "air change rate", "fresh air supply",
      "indoor pollutant", "formaldehyde indoor",
      "VOC indoor", "mould indoor", "humidity indoor",
      "radon indoor", "TVOC", "total VOC",
      "office air quality", "confined space air"
    ),
    worker_exposure_terms = c(
      "worker", "office worker", "building occupant"
    )
  ),

  # C08
  display_screen = list(
    label_en  = "Display screen equipment (DSE)",
    label_es  = "Riesgos ergonomicos por uso de pantallas de visualizacion",
    block     = "C - Ergonomics",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "display screen", "DSE", "VDU", "visual display unit",
      "computer workstation", "screen work",
      "prolonged screen use", "screen time",
      "digital eye strain", "computer vision syndrome",
      "monitor ergonomics", "keyboard ergonomics",
      "mouse ergonomics", "workstation setup",
      "remote work ergonomics", "home office ergonomics",
      "laptop use", "tablet use", "smartphone use occupational"
    ),
    worker_exposure_terms = c(
      "worker", "office worker", "computer user"
    )
  )
)


# =============================================================================
# BLOQUE D - PSICOSOCIOLOGIA (11 categorias - INSST)
# =============================================================================

.dict_d <- list(

  # D01
  work_content = list(
    label_en  = "Work content (repetitiveness, low content)",
    label_es  = "Contenido de trabajo",
    block     = "D - Psychosociology",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "work content", "job content", "task variety",
      "skill utilisation", "monotonous work", "repetitive work",
      "meaningful work", "job enrichment", "task identity",
      "boredom", "underutilisation", "understimulation",
      "job simplification", "Taylorism", "deskilling"
    ),
    worker_exposure_terms = c("worker", "employee")
  ),

  # D02
  workload_pace = list(
    label_en  = "Work overload and work pace",
    label_es  = "Carga de trabajo y ritmo de trabajo",
    block     = "D - Psychosociology",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "work overload", "workload", "work pace", "time pressure",
      "deadline pressure", "work intensity", "job demands",
      "quantitative demands", "qualitative demands",
      "effort reward imbalance", "ERI",
      "demand control model", "Karasek model",
      "job strain", "high strain job",
      "work underload", "work-life balance",
      "overtime", "excessive working hours"
    ),
    worker_exposure_terms = c("worker", "employee")
  ),

  # D03
  working_time = list(
    label_en  = "Working time (shift work, night work, irregular hours)",
    label_es  = "Tiempo de trabajo",
    block     = "D - Psychosociology",
    taxonomy  = "INSST/ISO45001/NIOSH",
    terms = c(
      "shift work", "night work", "night shift",
      "rotating shift", "extended shift", "12-hour shift",
      "irregular schedule", "unpredictable schedule",
      "on-call work", "flexible working time",
      "circadian rhythm", "sleep disorder occupational",
      "fatigue shift work", "alertness impairment",
      "work schedule", "rest period", "recovery time"
    ),
    worker_exposure_terms = c("shift worker", "worker", "nurse")
  ),

  # D04
  participation_control = list(
    label_en  = "Participation and control (job autonomy)",
    label_es  = "Participacion y control",
    block     = "D - Psychosociology",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "job control", "job autonomy", "decision latitude",
      "participation", "worker participation",
      "empowerment", "self-determination work",
      "lack of control", "micromanagement",
      "algorithmic management", "surveillance work",
      "monitoring workers", "electronic monitoring",
      "performance monitoring"
    ),
    worker_exposure_terms = c("worker", "employee")
  ),

  # D05
  role_performance = list(
    label_en  = "Role performance (ambiguity, conflict)",
    label_es  = "Desempeno de rol",
    block     = "D - Psychosociology",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "role ambiguity", "role conflict", "role overload",
      "role underload", "job description clarity",
      "unclear expectations", "conflicting demands",
      "boundary spanning", "role stress",
      "boundary role", "responsibility clarity"
    ),
    worker_exposure_terms = c("worker", "manager", "employee")
  ),

  # D06
  professional_development = list(
    label_en  = "Professional development and career",
    label_es  = "Desarrollo profesional",
    block     = "D - Psychosociology",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "career development", "professional development",
      "job insecurity", "employment insecurity",
      "precarious work", "training opportunity",
      "promotion", "recognition", "reward",
      "lack of recognition", "underpromotion",
      "overqualification", "glass ceiling",
      "deskilling", "obsolescence"
    ),
    worker_exposure_terms = c("worker", "employee")
  ),

  # D07
  interpersonal_relations = list(
    label_en  = "Interpersonal relations and social support",
    label_es  = "Relaciones interpersonales y apoyo social",
    block     = "D - Psychosociology",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "social support", "interpersonal relations",
      "team cohesion", "collegial support",
      "supervisor support", "management support",
      "isolation", "social isolation", "remote work isolation",
      "loneliness work", "poor communication",
      "workplace conflict", "interpersonal conflict",
      "organisational climate", "trust"
    ),
    worker_exposure_terms = c("worker", "employee")
  ),

  # D08
  team_equipment = list(
    label_en  = "Work teams and equipment exposure",
    label_es  = "Equipos de trabajo y exposicion a otros riesgos",
    block     = "D - Psychosociology",
    taxonomy  = "INSST",
    terms = c(
      "team work", "teamwork", "team dynamics",
      "group work", "collaborative work",
      "tool design", "equipment design",
      "human machine interface", "HMI",
      "control room design", "technology acceptance",
      "digital tool adoption", "automation impact"
    ),
    worker_exposure_terms = c("worker", "team member")
  ),

  # D09
  harassment = list(
    label_en  = "Harassment (sexual, discriminatory)",
    label_es  = "Acoso",
    block     = "D - Psychosociology",
    taxonomy  = "INSST/ISO45001/EU-OSHA",
    terms = c(
      "harassment", "sexual harassment", "discriminatory harassment",
      "workplace harassment", "moral harassment",
      "psychological harassment", "mobbing", "bullying",
      "cyber bullying work", "discriminacion",
      "racial harassment", "gender harassment",
      "age discrimination", "disability discrimination",
      "LGBTQ+ discrimination", "anti-harassment policy"
    ),
    worker_exposure_terms = c("worker", "victim", "employee")
  ),

  # D10
  external_violence = list(
    label_en  = "External violence",
    label_es  = "Violencia externa",
    block     = "D - Psychosociology",
    taxonomy  = "INSST/EU-OSHA",
    terms = c(
      "workplace violence", "external violence",
      "aggression", "physical assault work",
      "verbal aggression", "threat work",
      "customer violence", "patient aggression",
      "robbery workplace", "anti-social behaviour",
      "lone worker risk", "lone working",
      "conflict management", "de-escalation"
    ),
    worker_exposure_terms = c("worker", "healthcare worker", "retail worker")
  ),

  # D11
  emotional_demands = list(
    label_en  = "Emotional demands (client contact)",
    label_es  = "Demandas emocionales",
    block     = "D - Psychosociology",
    taxonomy  = "INSST/ISO45001",
    terms = c(
      "emotional demand", "emotional labour", "emotional work",
      "surface acting", "deep acting", "emotional exhaustion",
      "burnout", "compassion fatigue", "secondary trauma",
      "vicarious trauma", "emotional dissonance",
      "client contact stress", "patient contact stress",
      "customer service stress", "empathy fatigue"
    ),
    worker_exposure_terms = c(
      "worker", "healthcare worker", "social worker"
    )
  )
)


# =============================================================================
# BLOQUE E - RIESGOS BIOLOGICOS (5 categorias - EU-OSHA/NIOSH)
# =============================================================================

.dict_e <- list(

  # E01
  virus = list(
    label_en  = "Biological hazard - Viruses",
    label_es  = "Riesgo biologico - Virus",
    block     = "E - Biological",
    taxonomy  = "EU-OSHA/NIOSH/ISO45001",
    terms = c(
      "virus", "viral", "viral infection", "occupational viral",
      "airborne virus", "droplet transmission",
      "influenza occupational", "COVID-19 occupational",
      "SARS-CoV-2", "hepatitis B occupational",
      "hepatitis C occupational", "HIV occupational",
      "needlestick virus", "bloodborne virus",
      "respiratory virus", "norovirus occupational",
      "arbovirus", "zoonotic virus", "rabies occupational",
      "Ebola occupational", "hantavirus",
      "biological risk group", "containment level",
      "biosafety level", "BSL", "antiviral PPE"
    ),
    worker_exposure_terms = c(
      "healthcare worker", "laboratory worker", "worker"
    )
  ),

  # E02
  bacteria = list(
    label_en  = "Biological hazard - Bacteria",
    label_es  = "Riesgo biologico - Bacterias",
    block     = "E - Biological",
    taxonomy  = "EU-OSHA/NIOSH",
    terms = c(
      "bacteria", "bacterial", "bacterial infection occupational",
      "Legionella", "legionellosis", "Leptospira", "leptospirosis",
      "Brucella", "brucellosis", "Mycobacterium tuberculosis",
      "tuberculosis occupational", "anthrax occupational",
      "Clostridium", "MRSA", "methicillin resistant",
      "antibiotic resistant bacteria", "nosocomial infection",
      "healthcare associated infection", "wound infection",
      "zoonotic bacteria", "Q fever", "Coxiella",
      "biological aerosol", "bioaerosol bacteria",
      "endotoxin", "gram negative bacteria exposure"
    ),
    worker_exposure_terms = c(
      "healthcare worker", "agricultural worker", "worker"
    )
  ),

  # E03
  fungi = list(
    label_en  = "Biological hazard - Fungi and moulds",
    label_es  = "Riesgo biologico - Hongos",
    block     = "E - Biological",
    taxonomy  = "EU-OSHA/NIOSH",
    terms = c(
      "fungi", "fungal", "mould", "mold", "fungal spore",
      "mycotoxin", "Aspergillus occupational",
      "Aspergillosis", "Candida occupational",
      "dermatophyte", "ringworm occupational",
      "occupational asthma fungi", "farmer lung",
      "hypersensitivity pneumonitis fungi",
      "organic dust toxic syndrome", "ODTS",
      "bioaerosol fungi", "biofilm fungi",
      "compost fungi", "agricultural fungi",
      "construction mould"
    ),
    worker_exposure_terms = c(
      "farmer", "agricultural worker", "construction worker", "worker"
    )
  ),

  # E04
  parasites = list(
    label_en  = "Biological hazard - Parasites",
    label_es  = "Riesgo biologico - Parasitos",
    block     = "E - Biological",
    taxonomy  = "EU-OSHA/NIOSH",
    terms = c(
      "parasite", "parasitic", "parasitic infection occupational",
      "Toxoplasma", "toxoplasmosis occupational",
      "Echinococcus", "hydatid disease",
      "Leishmania", "leishmaniasis occupational",
      "malaria occupational", "tick-borne disease",
      "Lyme disease", "Borrelia occupational",
      "scabies occupational", "worm infection",
      "helminth occupational", "protozoa occupational",
      "vector-borne disease", "insect vector",
      "arthropod vector"
    ),
    worker_exposure_terms = c(
      "outdoor worker", "agricultural worker", "worker"
    )
  ),

  # E05
  prions = list(
    label_en  = "Biological hazard - Prions",
    label_es  = "Riesgo biologico - Priones",
    block     = "E - Biological",
    taxonomy  = "EU-OSHA",
    terms = c(
      "prion", "prion disease", "transmissible spongiform encephalopathy",
      "TSE", "Creutzfeldt-Jakob disease", "CJD",
      "variant CJD", "vCJD", "BSE occupational",
      "bovine spongiform encephalopathy",
      "scrapie", "occupational prion",
      "surgical instrument prion", "decontamination prion",
      "autopsy prion risk", "pathology prion",
      "neurodegenerative prion"
    ),
    worker_exposure_terms = c(
      "healthcare worker", "pathologist", "veterinary worker"
    )
  )
)


# =============================================================================
# BLOQUE F - TECNOLOGIAS EMERGENTES (8 categorias - EU-OSHA 2024-2026)
# =============================================================================

.dict_f <- list(

  # F01
  additive_manufacturing = list(
    label_en  = "Additive manufacturing and 3D printing",
    label_es  = "Fabricacion aditiva e impresion 3D",
    block     = "F - Emerging technologies",
    taxonomy  = "EU-OSHA/NIOSH/ORISMA",
    terms = c(
      "additive manufacturing", "3D printing", "AM metal",
      "metal additive manufacturing", "powder bed fusion", "PBF",
      "selective laser melting", "SLM", "selective laser sintering", "SLS",
      "electron beam melting", "EBM",
      "laser powder bed fusion", "LPBF",
      "directed energy deposition", "DED",
      "laser metal deposition", "LMD",
      "wire arc additive manufacturing", "WAAM",
      "binder jetting", "material jetting",
      "fused deposition modelling", "FDM", "FFF",
      "stereolithography", "SLA", "digital light processing", "DLP",
      "metal powder", "titanium powder", "aluminium powder",
      "nickel alloy powder", "cobalt chromium powder",
      "stainless steel powder", "tool steel powder",
      "build chamber", "inert atmosphere",
      "post-processing AM", "depowdering",
      "support removal AM", "heat treatment AM",
      "AM occupational health", "AM worker exposure",
      "3D printing emission", "AM emission",
      "layer manufacturing", "rapid prototyping",
      "Industry 4.0 manufacturing"
    ),
    worker_exposure_terms = c(
      "AM worker", "operator", "3D printing operator",
      "worker", "technician"
    )
  ),

  # F02
  nanomaterials = list(
    label_en  = "Nanomaterials and nanotechnology",
    label_es  = "Nanomateriales y nanotecnologia",
    block     = "F - Emerging technologies",
    taxonomy  = "EU-OSHA/NIOSH",
    terms = c(
      "nanomaterial", "nanotechnology", "engineered nanomaterial", "ENM",
      "nanoparticle", "nano-object", "nanofibre", "nanofiber",
      "nanotube", "carbon nanotube", "CNT",
      "fullerene", "graphene", "quantum dot",
      "nano-silver", "nano-gold", "nano-copper",
      "nano-titanium dioxide", "nano-zinc oxide",
      "nano-silica", "nano-alumina",
      "NOAA", "nano-object aggregate agglomerate",
      "nano-risk", "nano-safety", "nano-toxicology",
      "nano-regulation", "REACH nanomaterial"
    ),
    worker_exposure_terms = c(
      "worker", "nano-worker", "laboratory worker"
    )
  ),

  # F03
  robotics_automation = list(
    label_en  = "Robotics, cobots and automation",
    label_es  = "Robotica, cobots y automatizacion",
    block     = "F - Emerging technologies",
    taxonomy  = "EU-OSHA/ISO45001",
    terms = c(
      "robot", "robotics", "industrial robot",
      "collaborative robot", "cobot",
      "human robot collaboration", "HRC",
      "human robot interaction", "HRI",
      "robot safety", "robot hazard",
      "robot workspace", "robot cell",
      "autonomous robot", "mobile robot",
      "autonomous guided vehicle", "AGV",
      "autonomous mobile robot", "AMR",
      "drone", "unmanned aerial vehicle", "UAV",
      "automation", "automated system",
      "cyber-physical system", "CPS",
      "industry 4.0", "smart factory",
      "robot programming", "teach pendant",
      "collaborative workspace", "ISO 10218",
      "ISO/TS 15066", "robot speed force limiting"
    ),
    worker_exposure_terms = c(
      "worker", "robot operator", "programmer"
    )
  ),

  # F04
  artificial_intelligence = list(
    label_en  = "Artificial intelligence and autonomous systems",
    label_es  = "Inteligencia artificial y sistemas autonomos",
    block     = "F - Emerging technologies",
    taxonomy  = "EU-OSHA/NIOSH",
    terms = c(
      "artificial intelligence", "AI", "machine learning",
      "deep learning", "neural network",
      "algorithm management", "algorithmic management",
      "AI monitoring", "surveillance AI",
      "facial recognition workplace", "biometric monitoring",
      "automated decision making", "AI decision",
      "digital labour platform", "gig work",
      "platform economy", "app-based work",
      "agentic AI", "autonomous AI system",
      "large language model", "LLM workplace",
      "generative AI work", "ChatGPT workplace",
      "AI ethics work", "AI safety",
      "digital twin", "predictive maintenance AI",
      "AI fatigue", "technostress"
    ),
    worker_exposure_terms = c(
      "worker", "platform worker", "gig worker"
    )
  ),

  # F05
  exoskeletons = list(
    label_en  = "Exoskeletons and wearable technology",
    label_es  = "Exoesqueletos y tecnologia vestible",
    block     = "F - Emerging technologies",
    taxonomy  = "EU-OSHA/NIOSH",
    terms = c(
      "exoskeleton", "exosuit", "powered exoskeleton",
      "passive exoskeleton", "active exoskeleton",
      "upper limb exoskeleton", "lower limb exoskeleton",
      "back support exoskeleton", "wearable robot",
      "wearable technology", "wearable sensor",
      "smart PPE", "connected PPE",
      "IoT wearable", "health monitoring wearable",
      "fatigue monitoring wearable",
      "biometric wearable", "heart rate monitor work",
      "posture sensor", "motion capture occupational"
    ),
    worker_exposure_terms = c(
      "worker", "construction worker", "assembly worker"
    )
  ),

  # F06
  synthetic_biology = list(
    label_en  = "Synthetic biology and biotechnology",
    label_es  = "Biologia sintetica y biotecnologia",
    block     = "F - Emerging technologies",
    taxonomy  = "EU-OSHA/NIOSH",
    terms = c(
      "synthetic biology", "gene editing", "CRISPR",
      "genetically modified organism", "GMO",
      "gene therapy", "cell therapy",
      "bioengineering", "bioreactor",
      "fermentation occupational", "microbial exposure lab",
      "biosafety synthetic biology",
      "containment synthetic biology",
      "dual use research", "gain of function",
      "biosecurity", "bioterrorism prevention",
      "advanced therapy medicinal product", "ATMP"
    ),
    worker_exposure_terms = c(
      "laboratory worker", "researcher", "biotech worker"
    )
  ),

  # F07
  quantum_computing = list(
    label_en  = "Quantum technologies",
    label_es  = "Tecnologias cuanticas",
    block     = "F - Emerging technologies",
    taxonomy  = "EU-OSHA/ORISMA",
    terms = c(
      "quantum computing", "quantum technology",
      "quantum sensor", "quantum communication",
      "cryogenic quantum", "dilution refrigerator",
      "superconducting qubit", "cryogenic hazard",
      "liquid helium", "liquid nitrogen cryogenic",
      "electromagnetic shielding quantum",
      "laser quantum", "photon quantum",
      "quantum laboratory safety",
      "qubit manipulation"
    ),
    worker_exposure_terms = c(
      "researcher", "laboratory worker", "quantum engineer"
    )
  ),

  # F08
  green_circular_economy = list(
    label_en  = "Green jobs and circular economy",
    label_es  = "Empleos verdes y economia circular",
    block     = "F - Emerging technologies",
    taxonomy  = "EU-OSHA/NIOSH",
    terms = c(
      "green job", "green economy", "circular economy",
      "renewable energy worker", "solar panel installation",
      "wind turbine worker", "offshore wind",
      "recycling worker", "waste management worker",
      "e-waste", "electronic waste recycling",
      "battery recycling", "lithium battery",
      "electric vehicle maintenance",
      "energy storage worker", "bioenergy worker",
      "sustainable manufacturing", "eco-design",
      "remanufacturing", "upcycling",
      "critical raw material", "rare earth extraction",
      "cobalt mining", "lithium mining"
    ),
    worker_exposure_terms = c(
      "green worker", "renewable energy worker", "worker"
    )
  )
)


# =============================================================================
# MASTER DICTIONARY - combina los 6 bloques
# =============================================================================

.dict_iso45001_insst <- c(.dict_a, .dict_b, .dict_c, .dict_d, .dict_e, .dict_f)


# =============================================================================
# PUBLIC API
# =============================================================================

#' List available built-in dictionaries
#' @export
#' @details This function takes no arguments.
#' @return A character vector with the names of the built-in dictionaries available in ORISMA.
orm_dict_list <- function() {
  cat("Available built-in dictionaries:\n")
  cat("  'iso45001_insst' - Full 58-category dictionary (default)\n")
  cat("    Blocks: A) Safety (18) | B) Hygiene (8) | C) Ergonomics (8)\n")
  cat("            D) Psychosociology (11) | E) Biological (5) | F) Emerging (8)\n")
  cat("    Anchored in: INSST + ISO 45001 + NIOSH + EU-OSHA\n")
  cat("    Taxonomy DOI: 10.5281/zenodo.22066582 (CC BY 4.0)\n")
  invisible(c("iso45001_insst"))
}


#' Load a risk dictionary
#'
#' @param name Character. Dictionary name. Default `"iso45001_insst"`.
#' @return A named list (class `orisma_dict`).
#' @export
orm_dict <- function(name = "iso45001_insst") {
  d <- switch(name,
    iso45001_insst = .dict_iso45001_insst,
    stop(paste0("Unknown dictionary: '", name,
                "'. Use orm_dict_list() to see options."),
         call. = FALSE)
  )
  class(d) <- c("orisma_dict", "list")
  attr(d, "dict_name")    <- name
  attr(d, "dict_version") <- "2.0.0"
  attr(d, "dict_n_cats")  <- length(d)
  attr(d, "dict_created") <- Sys.time()
  d
}


#' List risk categories in a dictionary
#' @param dict An `orisma_dict` object.
#' @param lang Character. `"en"` or `"es"`.
#' @export
#' @return A data frame containing the available risk categories, including category keys, labels, blocks and dictionary metadata.
orm_dict_categories <- function(dict,
                                lang = getOption("orisma.lang", "en")) {
  cats <- lapply(names(dict), function(k) {
    entry <- dict[[k]]
    data.frame(
      key      = k,
      block    = entry$block,
      label    = if (lang == "es") entry$label_es else entry$label_en,
      taxonomy = entry$taxonomy,
      n_terms  = length(entry$terms),
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(cats)
}


#' Add terms to an existing dictionary category
#' @param dict An `orisma_dict` object.
#' @param category Character. Category key.
#' @param terms Character vector. New terms to add.
#' @return Updated `orisma_dict`.
#' @export
orm_dict_add_terms <- function(dict, category, terms) {
  if (!category %in% names(dict)) {
    stop(paste0("Category '", category,
                "' not found. Use orm_dict_categories() to list categories."),
         call. = FALSE)
  }
  dict[[category]]$terms <- unique(c(dict[[category]]$terms, terms))
  dict
}


#' Add a new risk category to a dictionary
#' @param dict An `orisma_dict` object.
#' @param key Character. Short identifier (no spaces).
#' @param label_en Character. Category name in English.
#' @param label_es Character. Category name in Spanish.
#' @param terms Character vector. Search terms.
#' @param worker_exposure_terms Character vector. Worker exposure indicators.
#' @param taxonomy Character. Source taxonomy label.
#' @param block Character. Block label (e.g. "A - Safety").
#' @return Updated `orisma_dict`.
#' @export
orm_dict_add_category <- function(dict, key, label_en, label_es, terms,
                                   worker_exposure_terms = character(0),
                                   taxonomy = "user",
                                   block = "G - Custom") {
  if (key %in% names(dict)) {
    warning(paste0("Category '", key,
                   "' already exists. Use orm_dict_add_terms() instead."))
    return(dict)
  }
  dict[[key]] <- list(
    label_en              = label_en,
    label_es              = label_es,
    block                 = block,
    taxonomy              = taxonomy,
    terms                 = terms,
    worker_exposure_terms = worker_exposure_terms
  )
  dict
}


#' Print method for orisma_dict
#' @param x An `orisma_dict` object.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.orisma_dict <- function(x, ...) {
  cat("\n-- ORISMA Risk Dictionary --\n")
  cat(" Name:       ", attr(x, "dict_name"), "\n")
  cat(" Version:    ", attr(x, "dict_version"), "\n")
  cat(" Categories: ", length(x), "\n\n")
  df <- orm_dict_categories(x)
  blocks <- unique(df$block)
  for (b in blocks) {
    sub <- df[df$block == b, ]
    cat(" ", b, "(", nrow(sub), "categories )\n")
  }
  cat("\nUse orm_dict_categories() for full detail.\n")
  invisible(x)
}
# orisma dict v2.0.0
