#' Manual validation assistant with Cohen's Kappa
#'
#' @description
#' `orm_validate()` supports methodological validation of ORISMA's automatic
#' risk extraction by presenting a random sample of classified records for
#' manual review. It then computes **Cohen's Kappa** to measure agreement
#' between automatic and manual classification.
#'
#' This addresses a key peer-review concern: distinguishing between
#' "category detected by dictionary" and "risk actually evaluated in study".
#'
#' The function saves a CSV file pre-filled with automatic classifications
#' that the researcher edits manually, then re-loads for Kappa computation.
#'
#' @param mx An `orisma_matrix` object from [orm_extract()].
#' @param n_sample Integer. Number of records to sample. Default `30`.
#' @param out_dir Character. Directory to save validation files.
#' @param validation_file Character or NULL. Path to a completed validation
#'   CSV (output of a previous `orm_validate()` call) for Kappa computation.
#'   If NULL, creates the file for manual review.
#' @param seed Integer. Random seed for reproducibility. Default `42`.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical.
#'
#' @return If `validation_file` is NULL: invisibly returns the path to the
#'   validation CSV. If `validation_file` is provided: returns a data frame
#'   with Kappa statistics per category.
#'
#' @export
orm_validate <- function(mx,
                          n_sample        = 30L,
                          out_dir         = "orisma_validation",
                          validation_file = NULL,
                          seed            = 42L,
                          lang            = getOption("orisma.lang", "en"),
                          verbose         = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)
  if (!inherits(mx, "orisma_matrix")) {
    stop("'mx' must be an orisma_matrix object from orm_extract().", call. = FALSE)
  }

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # ── Mode 1: Create validation file ──────────────────────────────────────────
  if (is.null(validation_file)) {

    set.seed(seed)
    refs    <- mx$refs
    bin_mat <- mx$matrix
    n_total <- nrow(refs)
    n_samp  <- min(n_sample, n_total)

    # Sample records with at least one category detected (more informative)
    has_cat <- which(rowSums(bin_mat) > 0)
    no_cat  <- which(rowSums(bin_mat) == 0)

    # 80% from records with categories, 20% from those without
    n_with <- min(round(n_samp * 0.8), length(has_cat))
    n_with <- max(n_with, 0)
    n_without <- min(n_samp - n_with, length(no_cat))

    idx <- c(
      if (n_with > 0) sample(has_cat, n_with) else integer(0),
      if (n_without > 0) sample(no_cat, n_without) else integer(0)
    )
    idx <- sort(idx)

    # Build validation dataframe
    sample_refs <- refs[idx, ]
    sample_mat  <- bin_mat[idx, , drop = FALSE]
    cat_names   <- colnames(bin_mat)

    # Active categories only (those with at least 1 detection in sample)
    active_cats <- cat_names[colSums(sample_mat) > 0]

    val_df <- data.frame(
      record_id   = sample_refs$record_id,
      title       = sample_refs$title,
      abstract    = substr(as.character(sample_refs$abstract), 1, 500),
      stringsAsFactors = FALSE
    )

    # Add automatic classification columns
    for (cat in active_cats) {
      col_name <- paste0("auto_", cat)
      val_df[[col_name]] <- as.integer(sample_mat[, cat])
    }

    # Add manual classification columns (pre-filled with NA for researcher)
    for (cat in active_cats) {
      col_name <- paste0("manual_", cat)
      val_df[[col_name]] <- NA_integer_
    }

    # Add notes column
    val_df$notes <- NA_character_

    # Save
    val_path <- file.path(out_dir, "orisma_validation_sample.csv")
    readr::write_csv(val_df, val_path)

    if (verbose) {
      cli::cli_alert_success(paste0(
        if (lang == "es") "Fichero de validacion creado: " else "Validation file created: ",
        val_path
      ))
      cli::cli_alert_info(paste0(
        if (lang == "es")
          paste0("Rellene las columnas 'manual_*' con 0 o 1 para ", n_samp,
                 " articulos y ejecute orm_validate() con validation_file=")
        else
          paste0("Fill in the 'manual_*' columns with 0 or 1 for ", n_samp,
                 " articles and run orm_validate() with validation_file="),
        val_path
      ))
    }

    return(invisible(val_path))
  }

  # ── Mode 2: Compute Kappa from completed file ────────────────────────────────
  val_df <- readr::read_csv(validation_file, show_col_types = FALSE)

  auto_cols   <- names(val_df)[grepl("^auto_",   names(val_df))]
  manual_cols <- names(val_df)[grepl("^manual_", names(val_df))]
  cat_keys    <- gsub("^auto_", "", auto_cols)

  # Filter rows with manual annotations
  has_manual <- rowSums(!is.na(val_df[, manual_cols, drop = FALSE])) > 0
  val_df     <- val_df[has_manual, ]
  n_annotated <- nrow(val_df)

  if (n_annotated == 0) {
    cli::cli_alert_warning(
      if (lang == "es") "No hay anotaciones manuales en el fichero."
      else "No manual annotations found in the file."
    )
    return(invisible(NULL))
  }

  # Compute Kappa per category
  kappa_results <- lapply(seq_along(cat_keys), function(i) {
    cat_key    <- cat_keys[i]
    auto_col   <- paste0("auto_",   cat_key)
    manual_col <- paste0("manual_", cat_key)

    auto   <- as.integer(val_df[[auto_col]])
    manual <- as.integer(val_df[[manual_col]])

    # Only rows where manual annotation exists
    valid <- !is.na(manual)
    if (sum(valid) < 5) return(NULL)

    auto_v   <- auto[valid]
    manual_v <- manual[valid]

    # Cohen's Kappa
    n      <- length(auto_v)
    p_obs  <- sum(auto_v == manual_v) / n

    p_yes_auto   <- mean(auto_v)
    p_yes_manual <- mean(manual_v)
    p_exp <- p_yes_auto * p_yes_manual +
             (1 - p_yes_auto) * (1 - p_yes_manual)

    kappa <- if (p_exp == 1) 1 else (p_obs - p_exp) / (1 - p_exp)

    data.frame(
      category     = cat_key,
      n_annotated  = sum(valid),
      p_agreement  = round(p_obs, 3),
      kappa        = round(kappa, 3),
      kappa_interp = dplyr::case_when(
        kappa >= 0.8 ~ if(lang=="es") "Excelente" else "Excellent",
        kappa >= 0.6 ~ if(lang=="es") "Bueno" else "Good",
        kappa >= 0.4 ~ if(lang=="es") "Moderado" else "Moderate",
        kappa >= 0.2 ~ if(lang=="es") "Debil" else "Fair",
        TRUE         ~ if(lang=="es") "Pobre" else "Poor"
      ),
      n_auto_pos   = sum(auto_v),
      n_manual_pos = sum(manual_v),
      stringsAsFactors = FALSE
    )
  })

  kappa_df <- dplyr::bind_rows(Filter(Negate(is.null), kappa_results))
  kappa_df <- kappa_df[order(-kappa_df$kappa), ]

  # Global Kappa (pooled)
  global_kappa <- round(mean(kappa_df$kappa, na.rm = TRUE), 3)

  if (verbose) {
    cli::cli_alert_success(paste0(
      if (lang == "es") "Kappa global: " else "Global Kappa: ",
      global_kappa,
      " (", n_annotated, " ",
      if (lang == "es") "registros anotados)" else "annotated records)"
    ))
    cli::cli_alert_info(
      if (lang == "es")
        "Kappa >= 0.7 es aceptable para publicacion en revistas de alto impacto"
      else
        "Kappa >= 0.7 is acceptable for publication in high-impact journals"
    )
  }

  # Save kappa results
  kappa_path <- file.path(out_dir, "orisma_kappa_results.csv")
  readr::write_csv(kappa_df, kappa_path)

  attr(kappa_df, "global_kappa")  <- global_kappa
  attr(kappa_df, "n_annotated")   <- n_annotated
  class(kappa_df) <- c("orisma_kappa", "data.frame")
  kappa_df
}


#' Print method for orisma_kappa
#' @param x An `orisma_kappa` object.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.orisma_kappa <- function(x, ...) {
  cat("\n-- ORISMA Validation . Cohen Kappa --\n")
  cat(" Global Kappa:     ", attr(x, "global_kappa"), "\n")
  cat(" Records annotated:", attr(x, "n_annotated"), "\n\n")
  cat(" Kappa by category:\n")
  print(x[, c("category", "n_annotated", "p_agreement", "kappa", "kappa_interp")],
        row.names = FALSE)
  invisible(x)
}
