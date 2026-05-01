#' Run the complete ORISMA pipeline in one call
#'
#' @description
#' `orm_run()` is the **single-function entry point** for users who want a
#' complete ORISMA analysis without managing individual pipeline steps.
#'
#' Internally it calls [orm_dedup()], [orm_extract()], and [orm_analyse()] in
#' sequence with sensible defaults. Advanced users can call each function
#' separately for full control.
#'
#' ## Minimal usage (3 lines)
#'
#' ```r
#' library(orisma)
#' data   <- orm_load("my_references/")
#' result <- orm_run(data)
#' orm_report(result)
#' ```
#'
#' @param refs An `orisma_refs` object from [orm_load()].
#' @param dict An `orisma_dict` object. Default: built-in ISO 45001 / INSST /
#'   NIOSH dictionary via [orm_dict()].
#' @param material_col Character or `NULL`. Column name containing material
#'   information for MGP computation. Default `NULL` (MGP skipped).
#' @param year_col Character. Column name for publication year. Default `"year"`.
#' @param fuzzy_threshold Numeric. Similarity threshold for fuzzy deduplication.
#'   Default `0.90`.
#' @param fields Character vector. Text fields to search for risk terms. Default
#'   `c("title", "abstract", "keywords")`.
#' @param lang Character. `"en"` or `"es"`. Overrides `orisma.lang` option.
#' @param verbose Logical. Print pipeline progress? Default `TRUE`.
#' @param save_report Logical. Automatically call [orm_report()] at the end?
#'   Default `FALSE` (call separately for full control over output options).
#' @param out_dir Character. Output directory if `save_report = TRUE`.
#'
#' @return An `orisma_result` object. See [orm_analyse()] for full structure.
#'
#' @seealso [orm_load()], [orm_dedup()], [orm_extract()], [orm_analyse()],
#'   [orm_report()]
#'
#' @examples
#' \dontrun{
#' # Simplest possible workflow
#' library(orisma)
#' refs   <- orm_load("my_references/")
#' result <- orm_run(refs)
#' orm_report(result)
#'
#' # Spanish output
#' options(orisma.lang = "es")
#' refs   <- orm_load("mis_referencias/")
#' result <- orm_run(refs)
#' orm_report(result, out_dir = "resultados_orisma/")
#'
#' # With material column and automatic report
#' result <- orm_run(refs,
#'                   material_col  = "material",
#'                   save_report   = TRUE,
#'                   out_dir       = "my_outputs/")
#' }
#'
#' @export
orm_run <- function(refs,
                    dict              = orm_dict(),
                    material_col      = NULL,
                    year_col          = "year",
                    fuzzy_threshold   = 0.90,
                    fields            = c("title", "abstract", "keywords"),
                    lang              = getOption("orisma.lang", "en"),
                    verbose           = getOption("orisma.verbose", TRUE),
                    save_report       = FALSE,
                    out_dir           = getOption("orisma.out_dir", "orisma_output")) {

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

  # ── Timing ──────────────────────────────────────────────────────────────────
  elapsed <- round((proc.time() - t_start)["elapsed"], 1)

  # Attach pipeline summary as attribute
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
      "WRDI (global):   ", result$WRDI_global, "\n\n",
      sep = ""
    )
    cat("Run orm_report(result) to generate all outputs.\n\n")
  }

  # ── Optional auto-report ────────────────────────────────────────────────────
  if (save_report) {
    orm_report(result, lang = lang, out_dir = out_dir, verbose = verbose)
  }

  result
}
