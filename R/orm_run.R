#' Run the complete ORISMA pipeline in one call
#'
#' @description
#' `orm_run()` is the **single-function entry point** for a complete ORISMA
#' analysis. It runs all pipeline steps automatically:
#'
#' 1. Deduplication (3-step: DOI + title + fuzzy)
#' 2. Risk category extraction (dictionary-based)
#' 3. Bibliometric analysis (WRDI, RCS, MGP indicators)
#' 4. Automatic dimension detection (normative blocks)
#' 5. Abstract Sufficiency Score (ASS, 0-5)
#' 6. Bridge article detection and priority ranking
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
#' All intermediate objects are stored in the result for downstream use
#' with [orm_report()], [orm_risk_sheet()], [orm_ranking()], and
#' [orm_extraction_matrix()].
#'
#' @param refs An `orisma_refs` object from [orm_load()].
#' @param dict An `orisma_dict` object. Default: [orm_dict()].
#' @param autodim_method Character. `"blocks"` (default) or `"text"`.
#' @param material_col Character or NULL. Column for MGP. Default NULL.
#' @param year_col Character. Year column. Default `"year"`.
#' @param fuzzy_threshold Numeric. Deduplication threshold. Default `0.90`.
#' @param fields Character vector. Text fields for extraction. Default
#'   `c("title", "abstract", "keywords")`.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical. Default `TRUE`.
#' @param save_report Logical. Auto-call [orm_report()]? Default `FALSE`.
#' @param out_dir Character. Output directory if `save_report = TRUE`.
#'
#' @return An `orisma_result` object containing all indicators, analyses,
#'   dimensions (`result$dims`), extraction matrix (`result$mx`),
#'   ASS scores and bridge classification (in `result$mx$refs`),
#'   and priority ranking (`result$ranking`).
#'
#' @export
orm_run <- function(refs,
                    dict            = orm_dict(),
                    autodim_method  = "blocks",
                    material_col    = NULL,
                    year_col        = "year",
                    fuzzy_threshold = 0.90,
                    fields          = c("title", "abstract", "keywords"),
                    lang            = getOption("orisma.lang", "en"),
                    verbose         = getOption("orisma.verbose", TRUE),
                    save_report     = FALSE,
                    out_dir         = getOption("orisma.out_dir", "orisma_output")) {

  .check_lang(lang)
  if (!inherits(refs, "orisma_refs"))
    stop("'refs' must be an orisma_refs object. Run orm_load() first.", call. = FALSE)

  t_start <- proc.time()

  # ── Step 1: Deduplication ───────────────────────────────────────────────────
  deduped <- orm_dedup(refs,
                       fuzzy_threshold = fuzzy_threshold,
                       lang = lang, verbose = verbose)

  # ── Step 2: Risk extraction ─────────────────────────────────────────────────
  mx <- orm_extract(deduped, dict = dict, fields = fields,
                    lang = lang, verbose = verbose)

  # ── Step 3: Bibliometric analysis ───────────────────────────────────────────
  result <- orm_analyse(mx, material_col = material_col,
                        year_col = year_col, lang = lang, verbose = verbose)

  # ── Step 4: Automatic dimension detection ───────────────────────────────────
  if (verbose) cli::cli_h2(
    if (lang == "es") "Deteccion automatica de dimensiones"
    else "Automatic dimension detection"
  )
  dims <- tryCatch(
    orm_autodim(mx, method = autodim_method, lang = lang, verbose = verbose),
    error = function(e) {
      cli::cli_alert_warning(paste0("orm_autodim: ", e$message))
      NULL
    }
  )

  # ── Step 5: Abstract Sufficiency Score ──────────────────────────────────────
  if (verbose) cli::cli_h2(
    if (lang == "es") "Abstract Sufficiency Score (ASS)"
    else "Abstract Sufficiency Score (ASS)"
  )
  mx <- tryCatch(
    orm_ass(mx, lang = lang, verbose = verbose),
    error = function(e) {
      cli::cli_alert_warning(paste0("orm_ass: ", e$message))
      mx
    }
  )

  # ── Step 6: Bridge article detection ────────────────────────────────────────
  if (verbose) cli::cli_h2(
    if (lang == "es") "Deteccion de articulos puente"
    else "Bridge article detection"
  )
  mx <- tryCatch(
    orm_bridge(mx, lang = lang, verbose = verbose),
    error = function(e) {
      cli::cli_alert_warning(paste0("orm_bridge: ", e$message))
      mx
    }
  )

  # ── Step 7: Priority ranking ─────────────────────────────────────────────────
  ranking <- tryCatch(
    orm_ranking(mx, top_n = 20L, lang = lang),
    error = function(e) {
      cli::cli_alert_warning(paste0("orm_ranking: ", e$message))
      NULL
    }
  )

  # ── Assemble result ──────────────────────────────────────────────────────────
  result$dims    <- dims
  result$mx      <- mx
  result$ranking <- ranking

  # ── Pipeline summary ─────────────────────────────────────────────────────────
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

    # Summary stats
    n_strong <- if (!is.null(result$mx) && "bridge_type" %in% names(result$mx$refs))
      sum(result$mx$refs$bridge_type %in% c("Strong bridge", "Puente fuerte"))
    else 0L

    ass_mean <- if (!is.null(result$mx) && "ass_score" %in% names(result$mx$refs))
      round(mean(result$mx$refs$ass_score, na.rm = TRUE), 2)
    else NA

    cat(
      " Records loaded:     ", nrow(refs), "\n",
      "Records analysed:   ", result$n_records,
      paste0("(", attr(deduped, "dedup_n_total"), " duplicates removed)\n"),
      "WRDI (global):      ", result$WRDI_global, "\n",
      "Dimensions detected:", if(!is.null(dims)) dims$n_dims else 0, "\n",
      "Strong bridges:     ", n_strong, "\n",
      "Mean ASS score:     ", if(!is.na(ass_mean)) ass_mean else "N/A", "/5\n\n",
      sep = ""
    )
    cat("Run orm_report(result) for full report\n")
    cat("Run orm_risk_sheet(result) for practitioner risk sheet\n\n")
  }

  if (save_report) {
    orm_report(result, lang = lang, out_dir = out_dir, verbose = verbose)
  }

  result
}
