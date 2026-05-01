#' Automatic dimension extraction and risk cross-matrix
#'
#' @description
#' `orm_autodim()` automatically discovers the most relevant contextual
#' dimensions of a corpus using two complementary modes:
#'
#' **Mode 1: Dictionary blocks** (default, `method = "blocks"`)
#' Uses the normative blocks of the ORISMA dictionary (A-Safety, B-Hygiene,
#' C-Ergonomics, D-Psychosociology, E-Biological, F-Emerging) as dimensions.
#' Computes a block x block co-occurrence matrix showing how many studies
#' address combinations of risk blocks simultaneously. Works for any corpus
#' without any configuration.
#'
#' **Mode 2: Free text** (`method = "text"`)
#' Extracts discriminant terms from abstracts using TF-IDF-like filtering.
#' Useful for discovering domain-specific dimensions not covered by the
#' dictionary (e.g. specific materials, sectors, tasks).
#'
#' @param mx An `orisma_matrix` object from [orm_extract()].
#' @param method Character. `"blocks"` (default) or `"text"`.
#' @param text_col Character. Text field for `method = "text"`. Default `"abstract"`.
#' @param n_dims Integer. Max dimensions for `method = "text"`. Default `12`.
#' @param min_freq Integer. Min document frequency for `method = "text"`. Default `3`.
#' @param max_doc_pct Numeric (0-1). Max document proportion for `method = "text"`.
#'   Terms above this are too generic. Default `0.35`.
#' @param min_cooccur Numeric (0-1). Min co-occurrence with a risk. Default `0.5`.
#' @param fuzzy_sim Numeric (0-1). Fuzzy grouping threshold. Default `0.85`.
#' @param stopwords Character vector. Extra stopwords for `method = "text"`.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical.
#'
#' @return A list (class `orisma_dims`) ready for [orm_dim_matrix()].
#' @seealso [orm_dim_matrix()]
#' @export
orm_autodim <- function(mx,
                        method      = "blocks",
                        text_col    = "abstract",
                        n_dims      = 12L,
                        min_freq    = 3L,
                        max_doc_pct = 0.35,
                        min_cooccur = 0.5,
                        fuzzy_sim   = 0.85,
                        stopwords   = NULL,
                        lang        = getOption("orisma.lang", "en"),
                        verbose     = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)
  if (!inherits(mx, "orisma_matrix")) {
    stop("'mx' must be an orisma_matrix object from orm_extract().", call. = FALSE)
  }
  if (!method %in% c("blocks", "text")) {
    stop("'method' must be 'blocks' or 'text'.", call. = FALSE)
  }

  if (verbose) cli::cli_h2(
    if (lang == "es") "Extraccion automatica de dimensiones"
    else "Automatic dimension extraction"
  )

  if (method == "blocks") {
    .autodim_blocks(mx, lang, verbose)
  } else {
    .autodim_text(mx, text_col, n_dims, min_freq, max_doc_pct,
                  min_cooccur, fuzzy_sim, stopwords, lang, verbose)
  }
}


# =============================================================================
# MODE 1: DICTIONARY BLOCKS
# =============================================================================

#' @noRd
.autodim_blocks <- function(mx, lang, verbose) {

  refs    <- mx$refs
  bin_mat <- mx$matrix
  dict    <- mx$dict

  # Extract block for each category
  cat_blocks <- vapply(names(dict), function(k) {
    b <- dict[[k]]$block
    if (is.null(b)) "Unknown" else b
  }, character(1))

  unique_blocks <- unique(cat_blocks)
  unique_blocks <- sort(unique_blocks[!is.na(unique_blocks)])

  if (verbose) cli::cli_alert_info(paste0(
    if (lang == "es") "Modo: bloques normativos · " else "Mode: normative blocks · ",
    length(unique_blocks), " bloques detectados"
  ))

  # For each block, which records have at least one category from that block?
  block_presence <- matrix(0L,
    nrow = nrow(bin_mat),
    ncol = length(unique_blocks),
    dimnames = list(rownames(bin_mat), unique_blocks)
  )

  for (b in unique_blocks) {
    cats_in_block <- names(cat_blocks)[cat_blocks == b]
    cats_present  <- intersect(cats_in_block, colnames(bin_mat))
    if (length(cats_present) > 0) {
      block_presence[, b] <- as.integer(rowSums(bin_mat[, cats_present, drop = FALSE]) > 0)
    }
  }

  # Build dim_list: each block is a dimension
  # Terms = category keys belonging to that block
  dim_list <- lapply(unique_blocks, function(b) {
    cats_in_block <- names(cat_blocks)[cat_blocks == b]
    intersect(cats_in_block, colnames(bin_mat))
  })
  names(dim_list) <- unique_blocks

  # Remove empty blocks
  non_empty <- vapply(dim_list, length, integer(1)) > 0
  dim_list  <- dim_list[non_empty]

  # Frequency: how many records have at least one cat from this block
  group_freqs <- vapply(names(dim_list), function(b) {
    sum(block_presence[, b])
  }, integer(1))

  dim_list <- dim_list[order(-group_freqs)]

  if (verbose) {
    cli::cli_alert_success(paste0(
      length(dim_list),
      if (lang == "es") " bloques como dimensiones"
      else " blocks as dimensions"
    ))
    for (nm in names(dim_list)) {
      cli::cli_alert_info(paste0(
        "  [", nm, "]  n=", group_freqs[nm],
        "  cats=", length(dim_list[[nm]])
      ))
    }
  }

  tf_df <- data.frame(
    term       = names(dim_list),
    group_freq = as.integer(group_freqs[names(dim_list)]),
    n_cats     = vapply(dim_list, length, integer(1)),
    stringsAsFactors = FALSE
  )

  result <- list(
    dims           = dim_list,
    term_freq      = tf_df,
    n_dims         = length(dim_list),
    text_col       = NA_character_,
    method         = "blocks",
    block_presence = block_presence,
    params         = list(method = "blocks")
  )
  class(result) <- c("orisma_dims", "list")
  attr(result, "orisma_lang") <- lang
  result
}


# =============================================================================
# MODE 2: FREE TEXT
# =============================================================================

#' @noRd
.autodim_text <- function(mx, text_col, n_dims, min_freq, max_doc_pct,
                           min_cooccur, fuzzy_sim, stopwords, lang, verbose) {

  refs         <- mx$refs
  bin_mat      <- mx$matrix
  n_total_docs <- nrow(refs)

  # Select text column
  if (!text_col %in% names(refs) ||
      mean(is.na(refs[[text_col]]) | refs[[text_col]] == "") > 0.5) {
    text_col <- "title"
    if (verbose) cli::cli_alert_warning(
      if (lang == "es") "Abstract no disponible, usando titulo"
      else "Abstract not available, falling back to title"
    )
  }

  has_risk  <- rowSums(bin_mat) > 0
  refs_risk <- refs[has_risk, ]
  text_risk <- tolower(as.character(refs_risk[[text_col]]))
  text_risk[is.na(text_risk)] <- ""

  if (verbose) cli::cli_alert_info(paste0(
    if (lang == "es") "Modo: texto libre · analizando "
    else "Mode: free text · analysing ",
    sum(has_risk), " records with risk detected"
  ))

  # Stopwords
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
    "working","workplace","environment","environmental","author","authors",
    "technique","techniques","application","applications","printed","printer",
    "printers","release","released","increase","increased","increases",
    "technology","technologies","industrial","printing","powder","powders",
    "laser","metals","chemical","chemicals","showed","showed","range",
    "related","system","systems","conditions","condition","properties",
    "impact","impacts","human","energy","fusion","melting","metallic",
    "filament","filaments","challenges","challenge","concentrations",
    "additive","additives","manufacturing","metal","process","processes"
  )
  sw_all <- unique(c(sw_base, stopwords))

  # Tokenise
  tokens_list <- lapply(seq_along(text_risk), function(i) {
    txt   <- text_risk[[i]]
    words <- unlist(regmatches(txt, gregexpr("[a-z]{4,}", txt)))
    words <- words[!words %in% sw_all]
    unique(words)
  })

  all_terms <- unlist(tokens_list)
  term_freq <- sort(table(all_terms), decreasing = TRUE)
  term_freq <- term_freq[term_freq >= min_freq]

  if (length(term_freq) == 0) return(.empty_dims())

  # Full corpus for doc-frequency
  all_text_full <- tolower(as.character(refs[[text_col]]))
  all_text_full[is.na(all_text_full)] <- ""

  candidate_terms <- names(term_freq)

  # Filter: max_doc_pct
  term_doc_pct <- vapply(candidate_terms, function(term) {
    n_docs <- sum(grepl(paste0("\\b", term, "\\b"), all_text_full, perl = TRUE))
    n_docs / n_total_docs
  }, numeric(1))
  candidate_terms <- candidate_terms[term_doc_pct <= max_doc_pct]
  term_freq       <- term_freq[candidate_terms]

  if (length(candidate_terms) == 0) return(.empty_dims())

  # Co-occurrence filter
  cooccur_scores <- vapply(candidate_terms, function(term) {
    in_risk   <- vapply(tokens_list, function(toks) term %in% toks, logical(1))
    n_in_risk <- sum(in_risk)
    if (n_in_risk == 0) return(0)
    n_total <- sum(grepl(paste0("\\b", term, "\\b"), all_text_full, perl = TRUE))
    if (n_total == 0) return(0)
    n_in_risk / n_total
  }, numeric(1))

  valid_terms <- candidate_terms[cooccur_scores >= min_cooccur]
  if (length(valid_terms) == 0) return(.empty_dims())

  # Fuzzy grouping
  n_terms <- length(valid_terms)
  groups  <- seq_len(n_terms)

  if (n_terms > 1) {
    dist_mat  <- stringdist::stringdistmatrix(valid_terms, valid_terms, method = "lv")
    max_chars <- outer(nchar(valid_terms), nchar(valid_terms), pmax)
    sim_mat   <- 1 - dist_mat / pmax(max_chars, 1)
    for (i in seq_len(n_terms - 1)) {
      for (j in seq(i + 1, n_terms)) {
        if (sim_mat[i, j] >= fuzzy_sim && groups[j] == j) groups[j] <- groups[i]
      }
    }
  }

  unique_groups <- unique(groups)
  dim_list <- lapply(unique_groups, function(g) {
    members <- valid_terms[groups == g]
    freqs   <- as.integer(term_freq[members])
    members[order(-freqs)]
  })
  names(dim_list) <- vapply(dim_list, `[[`, character(1), 1)

  group_freqs <- vapply(dim_list, function(g) {
    sum(as.integer(term_freq[g[g %in% names(term_freq)]]), na.rm = TRUE)
  }, numeric(1))
  dim_list <- dim_list[order(-group_freqs)]
  if (length(dim_list) > n_dims) dim_list <- dim_list[seq_len(n_dims)]

  if (verbose) {
    cli::cli_alert_success(paste0(length(dim_list), " dimensions detected"))
    for (nm in names(dim_list)) {
      syns <- dim_list[[nm]]
      cli::cli_alert_info(paste0(
        "  [", nm, "]",
        if (length(syns) > 1) paste0(" + ", paste(syns[-1], collapse = ", ")) else ""
      ))
    }
  }

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
    method    = "text",
    params    = list(min_freq = min_freq, min_cooccur = min_cooccur,
                     max_doc_pct = max_doc_pct, fuzzy_sim = fuzzy_sim)
  )
  class(result) <- c("orisma_dims", "list")
  attr(result, "orisma_lang") <- lang
  result
}


# =============================================================================
# orm_dim_matrix
# =============================================================================

#' Build a risk category x dimension cross-matrix
#'
#' @description
#' Builds a risk category x dimension cross-matrix and saves a hierarchical
#' clustered heatmap with dendrograms and numeric values in each cell.
#'
#' When `dims` was built with `method = "blocks"`, the matrix shows
#' risk categories x normative blocks (A-Safety, B-Hygiene, etc.).
#' When `dims` was built with `method = "text"`, the matrix shows
#' risk categories x discovered text dimensions.
#'
#' @param result An `orisma_result` object from [orm_analyse()] or [orm_run()].
#' @param dims An `orisma_dims` object from [orm_autodim()].
#' @param min_records Integer. Min records for a risk category row. Default `2`.
#' @param out_dir Character or NULL. Directory to save the heatmap PNG.
#' @param filename Character. Output filename. Default `"risk_dimension_heatmap.png"`.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical.
#'
#' @return Invisibly returns the cross-matrix (risk categories x dimensions).
#' @export
orm_dim_matrix <- function(result,
                            dims,
                            min_records = 2L,
                            out_dir     = NULL,
                            filename    = "risk_dimension_heatmap.png",
                            lang        = getOption("orisma.lang", "en"),
                            verbose     = getOption("orisma.verbose", TRUE)) {

  if (!inherits(result, "orisma_result"))
    stop("'result' must be an orisma_result object.", call. = FALSE)
  if (!inherits(dims, "orisma_dims"))
    stop("'dims' must be an orisma_dims object from orm_autodim().", call. = FALSE)

  refs     <- result$refs
  bin_mat  <- result$matrix
  dim_list <- dims$dims
  method   <- dims$method

  # Active risk categories (rows)
  active_cats <- result$indicators %>%
    dplyr::filter(.data$n_records >= min_records) %>%
    dplyr::arrange(dplyr::desc(.data$n_records)) %>%
    dplyr::pull(.data$category)

  active_labels <- result$indicators %>%
    dplyr::filter(.data$category %in% active_cats) %>%
    dplyr::arrange(dplyr::desc(.data$n_records)) %>%
    dplyr::pull(.data$label)

  bin_active <- bin_mat[, active_cats, drop = FALSE]
  dim_names  <- names(dim_list)

  cross_mat <- matrix(0L,
    nrow = length(active_cats),
    ncol = length(dim_names),
    dimnames = list(
      stringr::str_wrap(active_labels, 28),
      dim_names
    )
  )

  if (method == "blocks" && !is.null(dims$block_presence)) {
    # Mode 1: use pre-computed block presence matrix
    bp <- dims$block_presence
    for (j in seq_along(dim_names)) {
      blk     <- dim_names[[j]]
      has_dim <- if (blk %in% colnames(bp)) bp[, blk] == 1L
                 else rep(FALSE, nrow(bp))
      for (i in seq_along(active_cats)) {
        has_risk        <- bin_active[, i] == 1L
        cross_mat[i, j] <- sum(has_risk & has_dim)
      }
    }
  } else {
    # Mode 2: search terms in text
    text_col  <- dims$text_col
    text_all  <- tolower(as.character(refs[[text_col]]))
    text_all[is.na(text_all)] <- ""

    for (j in seq_along(dim_names)) {
      dim_terms <- dim_list[[j]]
      pattern   <- paste0("\\b(", paste(dim_terms, collapse = "|"), ")\\b")
      has_dim   <- grepl(pattern, text_all, ignore.case = TRUE, perl = TRUE)
      for (i in seq_along(active_cats)) {
        has_risk        <- bin_active[, i] == 1L
        cross_mat[i, j] <- sum(has_risk & has_dim)
      }
    }
  }

  # Remove zero rows/cols
  row_active <- rowSums(cross_mat) > 0
  col_active <- colSums(cross_mat) > 0
  mat_plot   <- cross_mat[row_active, col_active, drop = FALSE]

  if (verbose) {
    cli::cli_alert_success(paste0(
      if (lang == "es") "Matriz: " else "Matrix: ",
      nrow(mat_plot), " x ", ncol(mat_plot)
    ))
  }

  # Heatmap
  if (nrow(mat_plot) >= 2 && ncol(mat_plot) >= 2 && !is.null(out_dir)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    out_path <- file.path(out_dir, filename)

    title_txt <- if (lang == "es") "Focos de Riesgo PRL por Bloque Normativo"
                 else "OHS Risk Focus by Normative Block"

    col_palette <- grDevices::colorRampPalette(
      c("white", "#FFF3E0", "#FFB74D", "#F44336")
    )(50)

    grDevices::png(out_path,
      width  = max(2800, ncol(mat_plot) * 400),
      height = max(2400, nrow(mat_plot) * 220),
      res    = 300)

    pheatmap::pheatmap(
      mat_plot,
      cluster_rows    = nrow(mat_plot) > 2,
      cluster_cols    = ncol(mat_plot) > 2,
      color           = col_palette,
      main            = title_txt,
      fontsize        = 11,
      fontsize_row    = 9,
      fontsize_col    = 10,
      border_color    = "white",
      angle_col       = 45,
      display_numbers = TRUE,
      number_format   = "%d",
      number_color    = "grey20"
    )
    grDevices::dev.off()

    if (verbose) cli::cli_alert_success(paste0(
      if (lang == "es") "Heatmap guardado en: " else "Heatmap saved to: ",
      out_path
    ))
  }

  invisible(cross_mat)
}


# =============================================================================
# PRINT METHODS
# =============================================================================

#' Print method for orisma_dims
#' @param x An `orisma_dims` object.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.orisma_dims <- function(x, ...) {
  cat("\n-- ORISMA Dimensions --\n")
  cat(" Method:          ", x$method, "\n")
  cat(" Dimensions found:", x$n_dims, "\n\n")

  if (x$method == "blocks") {
    cat(" Normative blocks (dimensions):\n")
    for (i in seq_along(x$dims)) {
      nm <- names(x$dims)[i]
      tf <- x$term_freq[x$term_freq$term == nm, ]
      cat(sprintf("  %2d. %-40s  n_records=%-4d  n_cats=%d\n",
                  i, nm,
                  if (nrow(tf) > 0) tf$group_freq[1] else 0,
                  if (nrow(tf) > 0) tf$n_cats[1] else 0))
    }
  } else {
    cat(" Text dimensions (by frequency):\n")
    for (i in seq_along(x$dims)) {
      nm   <- names(x$dims)[i]
      tf   <- x$term_freq[x$term_freq$term == nm, ]
      syns <- x$dims[[nm]]
      cat(sprintf("  %2d. %-22s  freq=%-4d  doc_pct=%s%%",
                  i, nm,
                  if (nrow(tf) > 0) tf$group_freq[1] else 0,
                  if (nrow(tf) > 0) tf$doc_pct[1] else "?"))
      if (length(syns) > 1) cat("  [+", paste(syns[-1], collapse = ", "), "]")
      cat("\n")
    }
  }
  cat("\nUse orm_dim_matrix(result, dims) to build the risk x dimension heatmap.\n")
  invisible(x)
}


#' @noRd
.empty_dims <- function() {
  result <- list(dims = list(), term_freq = data.frame(), n_dims = 0L,
                 text_col = NA_character_, method = "text",
                 params = list())
  class(result) <- c("orisma_dims", "list")
  result
}
