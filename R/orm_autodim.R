#' Automatic dimension extraction from bibliographic corpus
#'
#' @description
#' `orm_autodim()` automatically discovers the most relevant contextual
#' dimensions of a corpus without requiring any user configuration.
#' Works for any domain: materials in AM, settings in healthcare,
#' tasks in construction, agents in biological studies, etc.
#'
#' @param mx An `orisma_matrix` object from [orm_extract()].
#' @param text_col Character. Text field to analyse. Default `"abstract"`.
#' @param n_dims Integer. Max dimension groups to return. Default `15`.
#' @param min_freq Integer. Min records a term must appear in. Default `2`.
#' @param min_cooccur Numeric (0-1). Min proportion co-occurring with a risk. Default `0.5`.
#' @param max_doc_pct Numeric (0-1). Max proportion of documents a term can
#'   appear in. Terms above this threshold are too generic to be discriminant.
#'   Default `0.6`.
#' @param fuzzy_sim Numeric (0-1). Similarity threshold for grouping terms. Default `0.85`.
#' @param stopwords Character vector. Additional stopwords. Default `NULL`.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical.
#'
#' @return A list (class `orisma_dims`) with detected dimension groups.
#' @seealso [orm_dim_matrix()]
#' @export
orm_autodim <- function(mx,
                        text_col    = "abstract",
                        n_dims      = 15L,
                        min_freq    = 2L,
                        min_cooccur = 0.5,
                        max_doc_pct = 0.6,
                        fuzzy_sim   = 0.85,
                        stopwords   = NULL,
                        lang        = getOption("orisma.lang", "en"),
                        verbose     = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)

  if (!inherits(mx, "orisma_matrix")) {
    stop("'mx' must be an orisma_matrix object from orm_extract().", call. = FALSE)
  }

  refs         <- mx$refs
  bin_mat      <- mx$matrix
  n_total_docs <- nrow(refs)

  if (verbose) cli::cli_h2(
    if (lang == "es") "Extraccion automatica de dimensiones"
    else "Automatic dimension extraction"
  )

  # Select text column
  if (!text_col %in% names(refs) ||
      mean(is.na(refs[[text_col]]) | refs[[text_col]] == "") > 0.5) {
    text_col <- "title"
    if (verbose) cli::cli_alert_warning(
      if (lang == "es") "Abstract no disponible, usando titulo"
      else "Abstract not available, falling back to title"
    )
  }

  # Records with at least one risk detected
  has_risk    <- rowSums(bin_mat) > 0
  refs_risk   <- refs[has_risk, ]
  text_risk   <- tolower(as.character(refs_risk[[text_col]]))
  text_risk[is.na(text_risk)] <- ""
  n_with_risk <- sum(has_risk)

  if (verbose) cli::cli_alert_info(paste0(
    if (lang == "es") "Analizando " else "Analysing ",
    n_with_risk,
    if (lang == "es") " registros con riesgo detectado"
    else " records with at least one risk detected"
  ))

  # Built-in stopwords
  sw_base <- c(
    "the","a","an","and","or","but","in","on","at","to","for","of","with",
    "by","from","as","is","was","are","were","be","been","being","have",
    "has","had","do","does","did","will","would","could","should","may",
    "might","shall","can","not","no","nor","so","yet","both","either",
    "neither","this","that","these","those","it","its","we","our","they",
    "their","he","she","his","her","us","them","which","who","what","when",
    "where","why","how","all","each","every","any","some","such","than",
    "more","most","also","into","over","after","between","through","during",
    "before","under","about","against","while","within","without","among",
    "across","along","above","below","up","down","out","off","then","there",
    "here","only","just","even","well","back","still","way","however","thus",
    "study","studies","results","data","analysis","method","methods",
    "conclusion","background","objective","aim","paper","article","research",
    "review","literature","based","using","used","use","shown","found","show",
    "shows","including","included","include","significantly","significant",
    "associated","compared","comparison","measured","measurement","assessed",
    "assessment","evaluated","evaluation","investigated","performed",
    "conducted","reported","report","presented","present","provide",
    "provided","provides","suggest","suggests","indicate","indicates",
    "demonstrate","demonstrates","high","low","higher","lower","large",
    "small","different","similar","various","several","many","two","three",
    "four","five","first","second","respectively","therefore","furthermore",
    "moreover","although","despite","due","given","known","well","new","one",
    "since","whether","other","further","exposure","risk","risks","hazard",
    "hazards","health","safety","occupational","workers","worker","operator",
    "operators","concentration","level","levels","particle","particles",
    "emission","emissions","sample","samples","control","controls","effect",
    "effects","potential","current","process","processes","surface",
    "material","materials","type","types","size","number","time","work",
    "working","workplace","environment","environmental","also","these",
    "their","been","have","were","with","that","this","from","they","were",
    "been","also","after","than","both","into","very","only","been","when",
    "which","there","could","other","would","about","these","some","their"
  )
  sw_all <- unique(c(sw_base, stopwords))

  # Tokenise: per-document unique words (4+ chars, alpha only)
  tokens_list <- lapply(seq_along(text_risk), function(i) {
    txt   <- text_risk[[i]]
    words <- unlist(regmatches(txt, gregexpr("[a-z]{4,}", txt)))
    words <- words[!words %in% sw_all]
    unique(words)
  })

  # Global term frequency (document frequency, not raw count)
  all_terms <- unlist(tokens_list)
  term_freq <- sort(table(all_terms), decreasing = TRUE)
  term_freq <- term_freq[term_freq >= min_freq]

  if (length(term_freq) == 0) {
    if (verbose) cli::cli_alert_warning(
      if (lang == "es") "Sin terminos frecuentes. Reduzca min_freq."
      else "No frequent terms found. Try reducing min_freq."
    )
    return(.empty_dims())
  }

  # Full text corpus for doc-frequency calculation
  all_text_full <- tolower(as.character(refs[[text_col]]))
  all_text_full[is.na(all_text_full)] <- ""

  # Filter: remove terms appearing in too many documents (too generic)
  candidate_terms <- names(term_freq)
  term_doc_pct <- vapply(candidate_terms, function(term) {
    n_docs <- sum(grepl(paste0("\\b", term, "\\b"), all_text_full, perl = TRUE))
    n_docs / n_total_docs
  }, numeric(1))

  candidate_terms <- candidate_terms[term_doc_pct <= max_doc_pct]
  term_freq       <- term_freq[candidate_terms]

  if (length(candidate_terms) == 0) {
    if (verbose) cli::cli_alert_warning(
      if (lang == "es")
        paste0("Todos los terminos superan max_doc_pct=", max_doc_pct,
               ". Aumente este valor.")
      else
        paste0("All terms exceed max_doc_pct=", max_doc_pct,
               ". Try increasing this value.")
    )
    return(.empty_dims())
  }

  # Co-occurrence filter: proportion of term appearances that co-occur with a risk
  cooccur_scores <- vapply(candidate_terms, function(term) {
    in_risk   <- vapply(tokens_list, function(toks) term %in% toks, logical(1))
    n_in_risk <- sum(in_risk)
    if (n_in_risk == 0) return(0)
    n_total <- sum(grepl(paste0("\\b", term, "\\b"), all_text_full, perl = TRUE))
    if (n_total == 0) return(0)
    n_in_risk / n_total
  }, numeric(1))

  valid_terms <- candidate_terms[cooccur_scores >= min_cooccur]

  if (length(valid_terms) == 0) {
    if (verbose) cli::cli_alert_warning(
      if (lang == "es") "Ningun termino supera el umbral de co-ocurrencia."
      else "No terms pass the co-occurrence threshold."
    )
    return(.empty_dims())
  }

  # Fuzzy grouping: merge near-identical terms
  n_terms <- length(valid_terms)
  groups  <- seq_len(n_terms)

  if (n_terms > 1) {
    dist_mat  <- stringdist::stringdistmatrix(valid_terms, valid_terms, method = "lv")
    max_chars <- outer(nchar(valid_terms), nchar(valid_terms), pmax)
    sim_mat   <- 1 - dist_mat / pmax(max_chars, 1)

    for (i in seq_len(n_terms - 1)) {
      for (j in seq(i + 1, n_terms)) {
        if (sim_mat[i, j] >= fuzzy_sim && groups[j] == j) {
          groups[j] <- groups[i]
        }
      }
    }
  }

  # Build group list
  unique_groups <- unique(groups)
  dim_list <- lapply(unique_groups, function(g) {
    members <- valid_terms[groups == g]
    freqs   <- as.integer(term_freq[members])
    members[order(-freqs)]
  })
  names(dim_list) <- vapply(dim_list, `[[`, character(1), 1)

  # Sort by total group frequency
  group_freqs <- vapply(dim_list, function(g) {
    sum(as.integer(term_freq[g[g %in% names(term_freq)]]), na.rm = TRUE)
  }, numeric(1))
  dim_list <- dim_list[order(-group_freqs)]
  if (length(dim_list) > n_dims) dim_list <- dim_list[seq_len(n_dims)]

  if (verbose) {
    cli::cli_alert_success(paste0(
      length(dim_list),
      if (lang == "es") " dimensiones detectadas automaticamente"
      else " dimensions detected automatically"
    ))
    for (nm in names(dim_list)) {
      syns <- dim_list[[nm]]
      cli::cli_alert_info(paste0(
        "  [", nm, "]",
        if (length(syns) > 1) paste0(" + ", paste(syns[-1], collapse = ", "))
        else ""
      ))
    }
  }

  # Build term frequency table
  tf_df <- data.frame(
    term        = names(dim_list),
    group_freq  = as.integer(group_freqs[seq_along(dim_list)]),
    cooccur_pct = round(cooccur_scores[names(dim_list)] * 100, 1),
    doc_pct     = round(term_doc_pct[names(dim_list)] * 100, 1),
    n_synonyms  = vapply(dim_list, length, integer(1)),
    synonyms    = vapply(dim_list,
                         function(x) paste(x[-1], collapse = "; "),
                         character(1)),
    stringsAsFactors = FALSE
  )

  result <- list(
    dims      = dim_list,
    term_freq = tf_df,
    n_dims    = length(dim_list),
    text_col  = text_col,
    params    = list(
      min_freq    = min_freq,
      min_cooccur = min_cooccur,
      max_doc_pct = max_doc_pct,
      fuzzy_sim   = fuzzy_sim,
      n_dims      = n_dims
    )
  )
  class(result) <- c("orisma_dims", "list")
  attr(result, "orisma_lang") <- lang
  result
}


#' Build a risk x dimension cross-matrix
#'
#' @description
#' Builds a risk category x dimension cross-matrix and saves a hierarchical
#' clustered heatmap. The matrix shows how many studies address each
#' combination of risk category and contextual dimension.
#'
#' @param result An `orisma_result` object.
#' @param dims An `orisma_dims` object from [orm_autodim()].
#' @param min_records Integer. Min records for a risk category to appear. Default `2`.
#' @param out_dir Character or NULL. Directory to save the heatmap PNG.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical.
#'
#' @return Invisibly returns the cross-matrix.
#' @export
orm_dim_matrix <- function(result,
                            dims,
                            min_records = 2L,
                            out_dir     = NULL,
                            lang        = getOption("orisma.lang", "en"),
                            verbose     = getOption("orisma.verbose", TRUE)) {

  if (!inherits(result, "orisma_result"))
    stop("'result' must be an orisma_result object.", call. = FALSE)
  if (!inherits(dims, "orisma_dims"))
    stop("'dims' must be an orisma_dims object from orm_autodim().", call. = FALSE)

  refs     <- result$refs
  bin_mat  <- result$matrix
  text_col <- dims$text_col
  dim_list <- dims$dims

  # Active risk categories
  active_cats <- result$indicators %>%
    dplyr::filter(.data$n_records >= min_records) %>%
    dplyr::pull(.data$category)

  active_labels <- result$indicators %>%
    dplyr::filter(.data$category %in% active_cats) %>%
    dplyr::pull(.data$label)

  bin_active <- bin_mat[, active_cats, drop = FALSE]

  text_all <- tolower(as.character(refs[[text_col]]))
  text_all[is.na(text_all)] <- ""

  dim_names <- names(dim_list)
  cross_mat <- matrix(0L,
    nrow = length(active_cats),
    ncol = length(dim_names),
    dimnames = list(
      stringr::str_wrap(active_labels, 25),
      dim_names
    )
  )

  for (j in seq_along(dim_names)) {
    dim_terms <- dim_list[[j]]
    pattern   <- paste0("\\b(", paste(dim_terms, collapse = "|"), ")\\b")
    has_dim   <- grepl(pattern, text_all, ignore.case = TRUE, perl = TRUE)
    for (i in seq_along(active_cats)) {
      has_risk        <- bin_active[, i] == 1L
      cross_mat[i, j] <- sum(has_risk & has_dim)
    }
  }

  # Plot heatmap
  title_txt <- if (lang == "es") "Focos de Riesgo PRL por Dimension"
               else "OHS Risk Focus by Dimension"

  row_active <- rowSums(cross_mat) > 0
  col_active <- colSums(cross_mat) > 0
  mat_plot   <- cross_mat[row_active, col_active, drop = FALSE]

  if (nrow(mat_plot) >= 2 && ncol(mat_plot) >= 2 && !is.null(out_dir)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    out_path <- file.path(out_dir, "risk_dimension_heatmap.png")

    col_palette <- grDevices::colorRampPalette(
      c("white", "#FFF3E0", "#FFB74D", "#F44336")
    )(50)

    grDevices::png(out_path,
      width  = max(2400, ncol(mat_plot) * 300),
      height = max(2000, nrow(mat_plot) * 200),
      res    = 300)
    pheatmap::pheatmap(
      mat_plot,
      cluster_rows     = nrow(mat_plot) > 2,
      cluster_cols     = ncol(mat_plot) > 2,
      color            = col_palette,
      main             = title_txt,
      fontsize         = 10,
      fontsize_row     = 9,
      fontsize_col     = 9,
      border_color     = "white",
      angle_col        = 45,
      display_numbers  = TRUE,
      number_format    = "%d",
      number_color     = "grey20"
    )
    grDevices::dev.off()

    if (verbose) cli::cli_alert_success(paste0(
      if (lang == "es") "Heatmap guardado en: " else "Heatmap saved to: ",
      out_path
    ))
  }

  invisible(cross_mat)
}


#' Print method for orisma_dims
#' @param x An `orisma_dims` object.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.orisma_dims <- function(x, ...) {
  cat("\n-- ORISMA Auto-detected Dimensions --\n")
  cat(" Dimensions found:", x$n_dims, "\n")
  cat(" Text field used: ", x$text_col, "\n")
  cat(" Parameters: min_freq=", x$params$min_freq,
      " min_cooccur=", x$params$min_cooccur,
      " max_doc_pct=", x$params$max_doc_pct, "\n\n")
  cat(" Dimension groups (by frequency):\n")
  for (i in seq_along(x$dims)) {
    nm  <- names(x$dims)[i]
    tf  <- x$term_freq[x$term_freq$term == nm, ]
    cat(sprintf("  %2d. %-22s  freq=%-4d  doc_pct=%s%%  cooccur=%s%%",
                i, nm,
                if (nrow(tf) > 0) tf$group_freq[1] else 0,
                if (nrow(tf) > 0) tf$doc_pct[1] else "?",
                if (nrow(tf) > 0) tf$cooccur_pct[1] else "?"))
    syns <- x$dims[[nm]]
    if (length(syns) > 1) cat("  [+", paste(syns[-1], collapse = ", "), "]")
    cat("\n")
  }
  invisible(x)
}


#' @noRd
.empty_dims <- function() {
  result <- list(dims = list(), term_freq = data.frame(), n_dims = 0L,
                 text_col = "abstract",
                 params = list())
  class(result) <- c("orisma_dims", "list")
  result
}
