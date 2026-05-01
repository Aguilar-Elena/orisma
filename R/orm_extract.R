#' Extract risk categories from bibliographic records
#'
#' @description
#' `orm_extract()` scans the **title**, **abstract**, and **keywords** of each
#' record against the active risk dictionary and builds a **binary presence
#' matrix** (record × risk category). It also detects whether each study
#' contains direct worker exposure data — the key signal for computing the
#' **WRDI** indicator.
#'
#' Matching is case-insensitive and uses whole-word boundary detection to
#' avoid false positives (e.g. "laser" does not match "eyelaser").
#'
#' @param refs An `orisma_refs` object (output of [orm_load()] or [orm_dedup()]).
#' @param dict An `orisma_dict` object. Default: [orm_dict()] (ISO 45001 /
#'   INSST / NIOSH).
#' @param fields Character vector. Which text fields to search. Default
#'   `c("title", "abstract", "keywords")`.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical. Print progress?
#'
#' @return A list (class `orisma_matrix`) containing:
#'   \describe{
#'     \item{`refs`}{Original `orisma_refs` tibble with added columns:
#'       one binary column per risk category (`cat_*`), `n_categories` (total
#'       categories matched), and `has_worker_data` (logical).}
#'     \item{`matrix`}{Pure binary matrix (records × categories) for
#'       downstream analysis.}
#'     \item{`dict`}{The dictionary used.}
#'     \item{`categories`}{Category metadata tibble.}
#'   }
#'
#' @examples
#' \dontrun{
#' refs   <- orm_load("my_references/")
#' deduped <- orm_dedup(refs)
#'
#' # Use default dictionary
#' mx <- orm_extract(deduped)
#'
#' # Use a customised dictionary
#' dict <- orm_dict()
#' dict <- orm_dict_add_terms(dict, "nanoparticles", c("nano-dust", "UFP"))
#' mx   <- orm_extract(deduped, dict = dict)
#'
#' # Restrict to title + abstract only
#' mx <- orm_extract(deduped, fields = c("title", "abstract"))
#' }
#'
#' @export
orm_extract <- function(refs,
                        dict    = orm_dict(),
                        fields  = c("title", "abstract", "keywords"),
                        lang    = getOption("orisma.lang", "en"),
                        verbose = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)

  if (verbose) {
    cli::cli_h1(.msg("phase_extract", lang))
    cli::cli_alert_info(
      .msg("extract_start", lang,
           dict_name    = attr(dict, "dict_name"),
           dict_version = attr(dict, "dict_version"))
    )
  }

  # ── 1. Build searchable text corpus ─────────────────────────────────────────
  available_fields <- intersect(fields, names(refs))
  if (length(available_fields) == 0) {
    stop("None of the specified fields found in the data.", call. = FALSE)
  }

  refs <- refs %>%
    dplyr::mutate(
      .text_corpus = tolower(
        apply(dplyr::select(., dplyr::all_of(available_fields)), 1,
              function(x) paste(x[!is.na(x)], collapse = " "))
      )
    )

  # ── 2. Match each category ───────────────────────────────────────────────────
  cat_names   <- names(dict)
  n_cats      <- length(cat_names)
  n_records   <- nrow(refs)

  binary_mat  <- matrix(0L, nrow = n_records, ncol = n_cats,
                        dimnames = list(refs$record_id, cat_names))
  worker_flag <- logical(n_records)

  if (verbose) {
    cli::cli_progress_bar("Scanning records", total = n_records)
  }

  for (i in seq_len(n_records)) {
    text <- refs$.text_corpus[[i]]
    if (is.na(text) || nchar(text) == 0) next

    for (j in seq_len(n_cats)) {
      cat_key <- cat_names[[j]]
      entry   <- dict[[cat_key]]

      # Build regex: whole-word boundary for each term
      pattern <- paste0(
        "\\b(", paste(stringr::str_escape(entry$terms), collapse = "|"), ")\\b"
      )

      if (grepl(pattern, text, ignore.case = TRUE, perl = TRUE)) {
        binary_mat[i, j] <- 1L
      }

      # Worker exposure detection
      if (!worker_flag[[i]] && length(entry$worker_exposure_terms) > 0) {
        wp <- paste0(
          "\\b(",
          paste(stringr::str_escape(entry$worker_exposure_terms),
                collapse = "|"),
          ")\\b"
        )
        if (grepl(wp, text, ignore.case = TRUE, perl = TRUE)) {
          worker_flag[[i]] <- TRUE
        }
      }
    }

    if (verbose) cli::cli_progress_update()
  }

  if (verbose) cli::cli_progress_done()

  # ── 3. Attach results back to refs tibble ────────────────────────────────────
  cat_df <- as.data.frame(binary_mat)
  names(cat_df) <- paste0("cat_", names(cat_df))

  refs <- dplyr::bind_cols(refs, cat_df) %>%
    dplyr::mutate(
      n_categories    = rowSums(binary_mat),
      has_worker_data = worker_flag
    ) %>%
    dplyr::select(-".text_corpus")

  # ── 4. Warn about empty matches ──────────────────────────────────────────────
  n_empty <- sum(refs$n_categories == 0)
  if (n_empty > 0 && verbose) {
    cli::cli_alert_warning(.msg("extract_empty", lang, n = n_empty))
  }

  if (verbose) {
    cli::cli_alert_success(
      .msg("extract_done", lang,
           n_records = n_records,
           n_cats    = n_cats)
    )
  }

  # ── 5. Assemble output object ─────────────────────────────────────────────────
  result <- list(
    refs       = refs,
    matrix     = binary_mat,
    dict       = dict,
    categories = orm_dict_categories(dict, lang = lang),
    fields_used = available_fields,
    n_records  = n_records,
    n_empty    = n_empty
  )

  class(result) <- c("orisma_matrix", "list")
  attr(result, "orisma_stage")    <- "extracted"
  attr(result, "orisma_lang")     <- lang
  attr(result, "orisma_created")  <- Sys.time()

  result
}


#' Print method for orisma_matrix
#' @export
print.orisma_matrix <- function(x, ...) {
  cat("\n── ORISMA extraction matrix ──────────────────────────────\n")
  cat(" Records:    ", x$n_records, "\n")
  cat(" Categories: ", ncol(x$matrix), "\n")
  cat(" Fields used:", paste(x$fields_used, collapse = ", "), "\n")
  cat(" Empty records (no category matched):", x$n_empty, "\n")
  cat("\n Category coverage:\n")
  coverage <- colSums(x$matrix)
  pct      <- round(100 * coverage / x$n_records, 1)
  cat_info <- data.frame(
    Category = x$categories$label,
    N        = coverage,
    `%`      = pct,
    check.names = FALSE
  )
  cat_info <- cat_info[order(-cat_info$N), ]
  print(cat_info, row.names = FALSE)
  invisible(x)
}
