#' Run the complete ORISMA pipeline in one call
#'
#' @description
#' `orm_run()` is the **single-function entry point** for users who want a
#' complete ORISMA analysis without managing individual pipeline steps.
#'
#' Internally it calls [orm_dedup()], [orm_extract()], [orm_analyse()], and
#' [orm_autodim()] in sequence with sensible defaults. The result contains
#' everything needed for [orm_report()].
#'
#' ## Minimal usage (3 lines)
#'
#' ```r
#' library(orisma)
#' refs   <- orm_load("my_references/")
#' result <- orm_run(refs)
#' orm_report(result, lang = "es")
#' ```
#'
#' @param refs An `orisma_refs` object from [orm_load()].
#' @param dict An `orisma_dict` object. Default: built-in ISO 45001 / INSST /
#'   NIOSH dictionary via [orm_dict()].
#' @param autodim_method Character. Dimension detection method: `"blocks"`
#'   (default, uses normative blocks A-F) or `"text"` (free text extraction).
#' @param material_col Character or `NULL`. Column for MGP computation.
#' @param year_col Character. Column for temporal analysis. Default `"year"`.
#' @param fuzzy_threshold Numeric. Deduplication fuzzy threshold. Default `0.90`.
#' @param fields Character vector. Text fields for risk extraction. Default
#'   `c("title", "abstract", "keywords")`.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical. Default `TRUE`.
#' @param save_report Logical. Auto-call [orm_report()]? Default `FALSE`.
#' @param out_dir Character. Output directory if `save_report = TRUE`.
#'
#' @return An `orisma_result` object with all indicators, analyses, and
#'   auto-detected dimensions in `result$dims`.
#'
#' @export
orm_run <- function(refs,
                    dict             = orm_dict(),
                    autodim_method   = "blocks",
                    material_col     = NULL,
                    year_col         = "year",
                    fuzzy_threshold  = 0.90,
                    fields           = c("title", "abstract", "keywords"),
                    lang             = getOption("orisma.lang", "en"),
                    verbose          = getOption("orisma.verbose", TRUE),
                    save_report      = FALSE,
                    out_dir          = getOption("orisma.out_dir", "orisma_output")) {

  .check_lang(lang)

  if (!inherits(refs, "orisma_refs")) {
    stop("'refs' must be an orisma_refs object. Run orm_load() first.",
         call. = FALSE)
  }

  t_start <- proc.time()

  # ── Step 1: Deduplication ───────────────────────────────────────────────────
  deduped <- orm_dedup(refs,
                       fuzzy_threshold = fuzzy_threshold,
                       lang            = lang,
                       verbose         = verbose)

  # ── Step 2: Extraction ──────────────────────────────────────────────────────
  mx <- orm_extract(deduped,
                    dict    = dict,
                    fields  = fields,
                    lang    = lang,
                    verbose = verbose)

  # ── Step 3: Analysis ────────────────────────────────────────────────────────
  result <- orm_analyse(mx,
                        material_col = material_col,
                        year_col     = year_col,
                        lang         = lang,
                        verbose      = verbose)

  # ── Step 4: Auto-dimension detection ────────────────────────────────────────
  if (verbose) cli::cli_h2(
    if (lang == "es") "Deteccion automatica de dimensiones"
    else "Automatic dimension detection"
  )

  dims <- tryCatch(
    orm_autodim(mx, method = autodim_method, lang = lang, verbose = verbose),
    error = function(e) {
      cli::cli_alert_warning(paste0("orm_autodim failed: ", e$message))
      NULL
    }
  )

  result$dims <- dims
  result$mx   <- mx   # store mx for downstream use

  # ── Timing ──────────────────────────────────────────────────────────────────
  elapsed <- round((proc.time() - t_start)["elapsed"], 1)

  attr(result, "pipeline_summary") <- list(
    n_loaded    = nrow(refs),
    n_deduped   = attr(deduped, "dedup_n_unique"),
    n_removed   = attr(deduped, "dedup_n_total"),
    n_analysed  = result$n_records,
    elapsed_sec = elapsed,
    lang        = lang,
    dict_name   = attr(dict, "dict_name")
  )

  if (verbose) {
    cat("\n")
    cli::cli_rule(
      left  = "ORISMA pipeline complete",
      right = paste0(elapsed, " sec")
    )
    cat(
      " Records loaded:  ", nrow(refs), "\n",
      "Records deduped: ", attr(deduped, "dedup_n_unique"),
      paste0("(", attr(deduped, "dedup_n_total"), " removed)\n"),
      "WRDI (global):   ", result$WRDI_global, "\n",
      "Dimensions:      ", if (!is.null(dims)) dims$n_dims else 0, "\n\n",
      sep = ""
    )
    cat("Run orm_report(result) to generate all outputs.\n\n")
  }

  if (save_report) {
    orm_report(result, lang = lang, out_dir = out_dir, verbose = verbose)
  }

  result
}
