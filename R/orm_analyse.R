#' Compute ORISMA bibliometric indicators and analyses
#'
#' @description
#' `orm_analyse()` takes an extraction matrix and computes:
#'
#' - **WRDI** - Worker-Risk Disconnection Index: the proportion of studies that
#'   characterise a risk without measuring direct worker exposure. A WRDI of 1
#'   means all studies are purely technical (no worker data); 0 means all
#'   studies include direct worker exposure measurement.
#'
#' - **RCS** - Risk Category Saturation Index: relative dominance of each
#'   risk category compared to a uniform-distribution baseline. RCS > 1 means
#'   the category is over-represented; RCS < 1 means it is under-represented.
#'
#' - **MGP** - Material-Gap Profile: ratio of a material's known hazard
#'   potential (from the literature consensus) to its proportional coverage in
#'   the corpus. Detects hazardous materials that are academically under-studied.
#'
#' It also computes co-occurrence matrices, temporal trends, and author networks
#' for visualisation.
#'
#' @param mx An `orisma_matrix` object returned by [orm_extract()].
#' @param material_col Character. Name of the column containing material
#'   information. If `NULL` (default), MGP is skipped with a warning.
#' @param year_col Character. Column name for publication year. Default `"year"`.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical. Print progress?
#'
#' @return A list (class `orisma_result`) with all indicators and analysis
#'   objects ready for [orm_report()] and visualisation functions.
#'
#' @examples
#' \dontrun{
#' refs    <- orm_load("my_references/")
#' deduped <- orm_dedup(refs)
#' mx      <- orm_extract(deduped)
#' result  <- orm_analyse(mx)
#'
#' # View the three core indicators
#' result$indicators
#'
#' # View WRDI
#' result$WRDI
#' }
#'
#' @export
orm_analyse <- function(mx,
                        material_col = NULL,
                        year_col     = "year",
                        lang         = getOption("orisma.lang", "en"),
                        verbose      = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)
  if (!inherits(mx, "orisma_matrix")) {
    stop("'mx' must be an orisma_matrix object. Run orm_extract() first.",
         call. = FALSE)
  }

  if (verbose) {
    cli::cli_h1(orm_msg("phase_analyse", lang))
    cli::cli_alert_info(orm_msg("analyse_start", lang))
  }

  refs       <- mx$refs
  bin_mat    <- mx$matrix
  n_records  <- nrow(bin_mat)
  n_cats     <- ncol(bin_mat)
  cat_names  <- colnames(bin_mat)

  # -- 1. WRDI - Worker-Risk Disconnection Index --------------------------------
  # Global WRDI
  n_with_workers    <- sum(refs$has_worker_data, na.rm = TRUE)
  n_without_workers <- n_records - n_with_workers
  WRDI_global       <- round(n_without_workers / n_records, 4)

  # WRDI per risk category
  WRDI_by_cat <- vapply(cat_names, function(cat) {
    col      <- paste0("cat_", cat)
    in_cat   <- refs[[col]] == 1L
    n_cat    <- sum(in_cat, na.rm = TRUE)
    if (n_cat == 0) return(NA_real_)
    n_worker <- sum(refs$has_worker_data[in_cat], na.rm = TRUE)
    round((n_cat - n_worker) / n_cat, 4)
  }, numeric(1))

  if (verbose) {
    cli::cli_alert_success(
      orm_msg("analyse_wrdi", lang,
           value = WRDI_global,
           pct   = round(WRDI_global * 100, 1))
    )
  }

  # -- 2. RCS - Risk Category Saturation Index ----------------------------------
  # Observed frequency per category
  cat_counts  <- colSums(bin_mat)
  total_hits  <- sum(cat_counts)
  uniform_exp <- total_hits / n_cats   # expected count if uniform

  # RCS = observed / expected (values > 1 = over-represented)
  RCS <- round(cat_counts / uniform_exp, 4)

  if (verbose) {
    cli::cli_alert_success(orm_msg("analyse_rcs", lang, n_cats = n_cats))
  }

  # -- 3. MGP - Material-Gap Profile --------------------------------------------
  MGP <- NULL

  if (!is.null(material_col) && material_col %in% names(refs)) {
    materials <- unique(na.omit(refs[[material_col]]))
    n_mats    <- length(materials)

    # Coverage: proportion of records mentioning each material
    coverage <- vapply(materials, function(m) {
      sum(grepl(m, refs[[material_col]], ignore.case = TRUE)) / n_records
    }, numeric(1))

    # Hazard proxy: mean number of risk categories co-occurring with this material
    hazard_proxy <- vapply(materials, function(m) {
      idx <- grepl(m, refs[[material_col]], ignore.case = TRUE)
      if (sum(idx) == 0) return(0)
      mean(refs$n_categories[idx], na.rm = TRUE)
    }, numeric(1))

    # MGP = hazard_proxy / coverage (high = high hazard, low coverage = gap)
    MGP <- data.frame(
      material      = materials,
      coverage      = round(coverage, 4),
      hazard_proxy  = round(hazard_proxy, 3),
      MGP           = round(hazard_proxy / pmax(coverage, 0.001), 3),
      stringsAsFactors = FALSE
    )
    MGP <- MGP[order(-MGP$MGP), ]

    if (verbose) {
      cli::cli_alert_success(orm_msg("analyse_mgp", lang, n_mats = n_mats))
    }
  } else if (!is.null(material_col)) {
    cli::cli_alert_warning(paste0(
      "Column '", material_col, "' not found - MGP skipped."
    ))
  }

  # -- 4. Co-occurrence matrix ---------------------------------------------------
  # How many studies address both category A and category B simultaneously
  cooccur_mat <- t(bin_mat) %*% bin_mat
  diag(cooccur_mat) <- 0L   # remove self-co-occurrences

  # -- 5. Temporal trend --------------------------------------------------------
  temporal <- NULL

  if (year_col %in% names(refs)) {
    temporal <- refs %>%
      dplyr::filter(!is.na(.data[[year_col]])) %>%
      dplyr::mutate(year = as.integer(.data[[year_col]])) %>%
      dplyr::group_by(.data$year) %>%
      dplyr::summarise(
        n_records = dplyr::n(),
        dplyr::across(
          dplyr::starts_with("cat_"),
          ~ sum(.x, na.rm = TRUE),
          .names = "{.col}"
        ),
        .groups = "drop"
      )
  }

  # -- 6. Consolidated indicators table ----------------------------------------
  indicators <- data.frame(
    category    = cat_names,
    label       = mx$categories$label,
    n_records   = as.integer(cat_counts),
    pct_records = round(100 * cat_counts / n_records, 1),
    WRDI        = WRDI_by_cat,
    RCS         = RCS,
    stringsAsFactors = FALSE
  )
  rownames(indicators) <- NULL

  if (verbose) cli::cli_alert_success(orm_msg("analyse_done", lang))

  # -- 7. Assemble result --------------------------------------------------------
  result <- list(
    # Core indicators
    WRDI_global = WRDI_global,
    WRDI_by_cat = WRDI_by_cat,
    RCS         = RCS,
    MGP         = MGP,
    indicators  = indicators,

    # Analysis objects
    cooccur_mat = cooccur_mat,
    temporal    = temporal,

    # Metadata
    n_records          = n_records,
    n_with_workers     = n_with_workers,
    n_without_workers  = n_without_workers,
    n_categories       = n_cats,
    dict               = mx$dict,
    categories         = mx$categories,
    refs               = refs,

    # Pass-through for report
    matrix     = bin_mat
  )

  class(result) <- c("orisma_result", "list")
  attr(result, "orisma_stage")   <- "analysed"
  attr(result, "orisma_lang")    <- lang
  attr(result, "orisma_created") <- Sys.time()
  attr(result, "orisma_version") <- as.character(utils::packageVersion("orisma"))
  attr(result, "dict_name")      <- attr(mx$dict, "dict_name")
  attr(result, "dict_version")   <- attr(mx$dict, "dict_version")

  result
}


#' Print method for orisma_result
#' @param x An object to print.
#' @param ... Further arguments passed to or from other methods.
#' @export
print.orisma_result <- function(x, ...) {
  cat("\n-- ORISMA Analysis Result --------------------------------\n")
  cat(" Records analysed:", x$n_records, "\n")
  cat(" Risk categories: ", x$n_categories, "\n\n")

  cat(" WRDI (global):", x$WRDI_global,
      paste0("(", round(x$WRDI_global * 100, 1),
             "% of studies lack direct worker exposure data)\n\n"))

  cat(" Indicators by category:\n")
  df <- x$indicators[order(-x$indicators$n_records), ]
  print(df[, c("label", "n_records", "pct_records", "WRDI", "RCS")],
        row.names = FALSE)

  if (!is.null(x$MGP)) {
    cat("\n MGP - Material-Gap Profile (top 5):\n")
    print(utils::head(x$MGP, 5), row.names = FALSE)
  }

  cat("\nRun orm_report() to generate full reports and visualisations.\n")
  invisible(x)
}
