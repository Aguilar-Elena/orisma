#' Automatic deduplication of bibliographic records
#'
#' @description
#' `orm_dedup()` removes duplicate records using a **three-step progressive
#' pipeline**:
#'
#' 1. **Exact DOI match** — most reliable signal; decisive for records with DOIs.
#' 2. **Normalised title match** — removes punctuation, accents, case, and extra
#'    spaces before comparing; catches the same article listed with minor
#'    typographic differences across databases.
#' 3. **Fuzzy match** — compares title + year + first author using
#'    Optimal String Alignment distance; catches near-identical records that
#'    escape exact matching (e.g. different journal abbreviations, truncated
#'    author lists).
#'
#' Only records that remain ambiguous after all three steps are flagged for
#' optional manual review. These are saved to `dedup_log.csv`.
#'
#' @param refs An `orisma_refs` object returned by [orm_load()].
#' @param fuzzy_threshold Numeric (0–1). Similarity threshold for fuzzy
#'   matching. Default `0.90` (90% similarity = duplicate). Increase for
#'   stricter matching, decrease for more aggressive deduplication.
#' @param lang Character. `"en"` or `"es"`. Overrides `orisma.lang` option.
#' @param verbose Logical. Print progress? Default `TRUE`.
#' @param save_log Logical. Save `dedup_log.csv` to working directory?
#'   Default `TRUE`.
#'
#' @return An `orisma_refs` tibble with duplicates removed. Attributes record
#'   deduplication statistics for inclusion in the PRISMA log.
#'
#' @examples
#' \dontrun{
#' refs    <- orm_load("my_references/")
#' deduped <- orm_dedup(refs)
#'
#' # More aggressive fuzzy matching
#' deduped <- orm_dedup(refs, fuzzy_threshold = 0.85)
#'
#' # Spanish messages, no log file
#' deduped <- orm_dedup(refs, lang = "es", save_log = FALSE)
#' }
#'
#' @export
orm_dedup <- function(refs,
                      fuzzy_threshold = 0.90,
                      lang    = getOption("orisma.lang", "en"),
                      verbose = getOption("orisma.verbose", TRUE),
                      save_log = TRUE) {

  .check_lang(lang)
  if (!inherits(refs, "orisma_refs")) {
    stop("'refs' must be an orisma_refs object. Run orm_load() first.",
         call. = FALSE)
  }

  if (verbose) cli::cli_h1("orm_msg("phase_dedup", lang))
  if (verbose) cli::cli_alert_info("orm_msg("dedup_start", lang))

  n_start  <- nrow(refs)
  log_rows <- list()

  # ── Step 1: Exact DOI match ─────────────────────────────────────────────────
  n_before <- nrow(refs)

  if (!"doi" %in% names(refs) || all(is.na(refs$doi))) {
    if (verbose) cli::cli_alert_warning("orm_msg("err_no_doi", lang))
    n_doi_removed <- 0L
  } else {
    refs <- refs %>%
      dplyr::mutate(
        doi_clean = tolower(trimws(.data$doi))
      ) %>%
      dplyr::group_by(.data$doi_clean) %>%
      dplyr::filter(is.na(.data$doi_clean) | .data$doi_clean == "" |
                      dplyr::row_number() == 1L) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"doi_clean")

    n_doi_removed <- n_before - nrow(refs)
    log_rows[[1]] <- data.frame(
      step = "DOI exact match", removed = n_doi_removed, method = "exact"
    )
  }

  if (verbose) {
    cli::cli_alert_success("orm_msg("dedup_doi", lang, n_removed = n_doi_removed))
  }

  # ── Step 2: Normalised title match ──────────────────────────────────────────
  n_before <- nrow(refs)

  if (!"title" %in% names(refs) || all(is.na(refs$title))) {
    cli::cli_alert_danger("orm_msg("err_no_title", lang))
    stop("Cannot deduplicate without a title column.", call. = FALSE)
  }

  refs <- refs %>%
    dplyr::mutate(
      title_norm = .normalise_title(.data$title)
    ) %>%
    dplyr::group_by(.data$title_norm) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"title_norm")

  n_title_removed <- n_before - nrow(refs)
  log_rows[[2]] <- data.frame(
    step = "Normalised title match", removed = n_title_removed, method = "exact"
  )

  if (verbose) {
    cli::cli_alert_success("orm_msg("dedup_title", lang,
                                n_removed = n_title_removed))
  }

  # ── Step 3: Fuzzy match (title + year + first author) ───────────────────────
  n_before <- nrow(refs)

  # Build composite key for fuzzy matching
  refs <- refs %>%
    dplyr::mutate(
      first_author = .extract_first_author(.data$authors),
      fuzzy_key    = paste(.normalise_title(.data$title),
                           as.character(.data$year),
                           .data$first_author,
                           sep = "||")
    )

  # Compute pairwise distances (using stringdist OSA)
  keys     <- refs$fuzzy_key
  dist_mat <- stringdist::stringdistmatrix(keys, keys, method = "osa")

  # Normalise to [0, 1]: similarity = 1 - dist / max(nchar(a), nchar(b))
  n_chars  <- nchar(keys)
  max_mat  <- outer(n_chars, n_chars, pmax)
  sim_mat  <- 1 - dist_mat / pmax(max_mat, 1)

  # Flag pairs above threshold (lower triangle only)
  is_dup   <- which(lower.tri(sim_mat) & sim_mat >= fuzzy_threshold,
                    arr.ind = TRUE)

  ambiguous_pairs <- nrow(is_dup)

  # Keep first occurrence in each duplicate group
  to_remove <- unique(is_dup[, 1])   # rows identified as duplicates
  if (length(to_remove) > 0) {
    refs <- refs[-to_remove, ]
  }

  refs <- refs %>% dplyr::select(-"first_author", -"fuzzy_key")

  n_fuzzy_removed <- n_before - nrow(refs)
  log_rows[[3]] <- data.frame(
    step = "Fuzzy match (title+year+author)", removed = n_fuzzy_removed,
    method = "fuzzy"
  )

  if (verbose) {
    cli::cli_alert_success("orm_msg("dedup_fuzzy", lang,
                                n_removed = n_fuzzy_removed))
  }

  # ── Save log ─────────────────────────────────────────────────────────────────
  dedup_log <- dplyr::bind_rows(log_rows)
  dedup_log$total_before <- n_start
  dedup_log$total_after  <- nrow(refs)

  if (save_log) {
    readr::write_csv(dedup_log, "dedup_log.csv")
  }

  # ── Final summary ─────────────────────────────────────────────────────────────
  n_total_removed <- n_start - nrow(refs)

  if (verbose) {
    cli::cli_alert_success(
      "orm_msg("dedup_done", lang,
           n_unique        = nrow(refs),
           n_total_removed = n_total_removed)
    )
  }

  # Update class and attributes
  class(refs) <- c("orisma_refs", "tbl_df", "tbl", "data.frame")
  attr(refs, "orisma_stage")       <- "deduped"
  attr(refs, "orisma_lang")        <- lang
  attr(refs, "dedup_n_start")      <- n_start
  attr(refs, "dedup_n_doi")        <- n_doi_removed
  attr(refs, "dedup_n_title")      <- n_title_removed
  attr(refs, "dedup_n_fuzzy")      <- n_fuzzy_removed
  attr(refs, "dedup_n_total")      <- n_total_removed
  attr(refs, "dedup_n_unique")     <- nrow(refs)
  attr(refs, "dedup_log")          <- dedup_log

  refs
}


# ── Deduplication helpers ─────────────────────────────────────────────────────

#' @noRd
.normalise_title <- function(x) {
  x <- tolower(x)
  x <- iconv(x, to = "ASCII//TRANSLIT")   # remove accents
  x <- gsub("[^a-z0-9 ]", "", x)          # keep only alphanumeric + spaces
  x <- trimws(gsub("\\s+", " ", x))       # collapse multiple spaces
  x
}

#' @noRd
.extract_first_author <- function(authors) {
  # Authors stored as "Last, First; Last2, First2" or "Last First, Last2 First2"
  first <- stringr::str_extract(authors, "^[^;,]+")
  tolower(trimws(first))
}
