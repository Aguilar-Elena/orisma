#' orisma: Occupational Risk Integrated Systematic Mapping and Analysis
#'
#' @description
#' A systematic bibliometric pipeline for mapping occupational risk evidence
#' in any domain - emerging technologies, established hazards, or specific
#' industrial sectors.
#'
#' ## Typical workflow
#'
#' ```r
#' library(orisma)
#' data   <- orm_load("my_references/")
#' result <- orm_run(data)
#' orm_report(result)
#' ```
#'
#' @section Original indicators:
#' - **WRDI** Worker-Risk Disconnection Index
#' - **RCS** Risk Category Saturation Index
#' - **MGP** Material-Gap Profile
#'
#' @section Citation:
#' Aguilar-Elena, R. (2025). orisma: Occupational Risk Integrated Systematic
#' Mapping and Analysis. R package version 0.1.0.
#' Universidad Internacional de Valencia (VIU).
#' \url{https://github.com/Aguilar-Elena/orisma}
#'
#' @author
#' Ra\u00fal Aguilar-Elena \email{raguilar@@universidadviu.com}
#'
#' Occupational Risk Prevention and Occupational Health Research Group (GPRL),
#' Universidad Internacional de Valencia (VIU), Valencia, Spain.
#'
#' @docType package
#' @name orisma-package
#' @aliases orisma
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  source(system.file("i18n/messages.R", package = "orisma"), local = TRUE)
  op        <- options()
  op_orisma <- list(
    orisma.lang    = "en",
    orisma.verbose = TRUE,
    orisma.out_dir = "orisma_output",
    orisma.dict    = "iso45001_insst"
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
    "Docs:   https://github.com/Aguilar-Elena/orisma\n",
    "Set language: options(orisma.lang = 'es')  # or 'en'\n"
  )
}

#' Print an orisma_matrix object
#' @param x An `orisma_matrix` object.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.orisma_matrix <- function(x, ...) {
  cat("\n-- ORISMA extraction matrix --\n")
  cat(" Records:    ", x$n_records, "\n")
  cat(" Categories: ", ncol(x$matrix), "\n")
  cat(" Fields used:", paste(x$fields_used, collapse = ", "), "\n")
  cat(" Empty records (no category matched):", x$n_empty, "\n")
  coverage <- colSums(x$matrix)
  pct      <- round(100 * coverage / x$n_records, 1)
  cat_info  <- data.frame(Category = x$categories$label, N = coverage,
                           Pct = pct, check.names = FALSE)
  print(cat_info[order(-cat_info$N), ], row.names = FALSE)
  invisible(x)
}

#' Print an orisma_result object
#' @param x An `orisma_result` object.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.orisma_result <- function(x, ...) {
  cat("\n-- ORISMA Analysis Result --\n")
  cat(" Records analysed:", x$n_records, "\n")
  cat(" Risk categories: ", x$n_categories, "\n\n")
  cat(" WRDI (global):", x$WRDI_global, "\n")
  df <- x$indicators[order(-x$indicators$n_records), ]
  print(df[, c("label", "n_records", "pct_records", "WRDI", "RCS")],
        row.names = FALSE)
  if (!is.null(x$MGP)) {
    cat("\n MGP - Material-Gap Profile (top 5):\n")
    print(utils::head(x$MGP, 5), row.names = FALSE)
  }
  cat("\nRun orm_report() to generate full reports.\n")
  invisible(x)
}
