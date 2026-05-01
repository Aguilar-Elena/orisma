#' orisma: Occupational Risk Integrated Systematic Mapping and Analysis
#'
#' @description
#' A systematic bibliometric pipeline for mapping occupational risk evidence
#' in any domain — emerging technologies, established hazards, or specific
#' industrial sectors.
#'
#' ## Typical workflow
#'
#' ```r
#' library(orisma)
#'
#' # Simplest possible use: three lines
#' data  <- orm_load("my_references/")
#' result <- orm_run(data)
#' orm_report(result)
#' ```
#'
#' ## Modular workflow (full control)
#'
#' ```r
#' refs    <- orm_load("my_references/")
#' deduped <- orm_dedup(refs)
#' audited <- orm_audit(deduped)
#' matrix  <- orm_extract(audited)
#' result  <- orm_analyse(matrix)
#' orm_report(result, lang = "es", out_dir = "my_outputs/")
#' ```
#'
#' ## Language
#'
#' Set the default language globally:
#' ```r
#' options(orisma.lang = "es")   # Spanish
#' options(orisma.lang = "en")   # English (default)
#' ```
#'
#' @section Original indicators:
#' - **WRDI** Worker-Risk Disconnection Index: proportion of studies that
#'   characterise a risk without measuring real worker exposure.
#' - **RCS** Risk Category Saturation Index: relative dominance of each risk
#'   category compared to a uniform distribution baseline.
#' - **MGP** Material-Gap Profile: ratio between known hazard potential of a
#'   material and its coverage in the literature.
#'
#' @section Citation:
#' Aguilar-Elena, R. (2025). orisma: Occupational Risk Integrated Systematic
#' Mapping and Analysis. R package version 0.1.0.
#' Universidad Internacional de Valencia (VIU).
#' \url{https://github.com/raguilarelena/orisma}
#'
#' @author
#' **Raúl Aguilar-Elena** \email{raul.aguilar@viu.es}
#'
#' Occupational Risk Prevention and Occupational Health Research Group (GPRL),
#' Universidad Internacional de Valencia (VIU), Valencia, Spain.
#'
#' @docType package
#' @name orisma-package
#' @aliases orisma
"_PACKAGE"

# ── Global options ─────────────────────────────────────────────────────────────

#' @importFrom glue glue
#' @importFrom cli cli_h1 cli_h2 cli_alert_success cli_alert_warning
#'   cli_alert_danger cli_alert_info cli_progress_bar cli_progress_update
#'   cli_progress_done
NULL

.onLoad <- function(libname, pkgname) {
  # Load bilingual message system
  source(system.file("i18n/messages.R", package = "orisma"), local = TRUE)

  # Default options (user can override in .Rprofile)
  op <- options()
  op_orisma <- list(
    orisma.lang      = "en",   # "en" or "es"
    orisma.verbose   = TRUE,   # print progress to console
    orisma.out_dir   = "orisma_output",  # default output folder
    orisma.dict      = "iso45001_insst"  # default risk dictionary
  )
  toset <- !(names(op_orisma) %in% names(op))
  if (any(toset)) options(op_orisma[toset])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\norisma v", utils::packageVersion("orisma"),
    " \u2014 Occupational Risk Integrated Systematic Mapping and Analysis\n",
    "Author: Dr. Ra\u00fal Aguilar-Elena \u00b7 GPRL \u00b7 VIU\n",
    "Docs:   https://github.com/raguilarelena/orisma\n",
    "Set language: options(orisma.lang = 'es')  # or 'en'\n"
  )
}
