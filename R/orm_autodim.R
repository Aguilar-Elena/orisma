#' Automatic dimension extraction from bibliographic corpus
#'
#' @description
#' `orm_autodim()` automatically discovers the most relevant **contextual
#' dimensions** of a corpus — materials, sectors, technologies, agents, or
#' any other recurring concept — without requiring any user configuration.
#'
#' It works by:
#' 1. Extracting significant terms from abstracts of records where at least
#'    one risk category was detected.
#' 2. Filtering terms that co-occur consistently with risk categories.
#' 3. Grouping near-identical terms (fuzzy deduplication).
#' 4. Returning a named list of term groups that can be used to build a
#'    risk x dimension cross-matrix via [orm_dim_matrix()].
#'
#' The result is domain-agnostic: for metal AM corpora it will discover
#' materials (titanium, steel, nickel...); for healthcare corpora it will
#' discover settings (ICU, surgery, laboratory...); for construction corpora
#' it will discover tasks (demolition, welding, painting...).
#'
#' @param mx An `orisma_matrix` object from [orm_extract()].
#' @param text_col Character. Column in `mx$refs` containing the text to
#'   analyse. Default `"abstract"`. Falls back to `"title"` if abstract
#'   is mostly empty.
#' @param n_dims Integer. Maximum number of dimension groups to return.
#'   Default `15`.
#' @param min_freq Integer. Minimum number of records a term must appear in
#'   to be considered. Default `2`.
#' @param min_cooccur Numeric (0-1). Minimum proportion of a term's
#'   appearances that must co-occur with a detected risk category. Default
#'   `0.5`.
#' @param fuzzy_sim Numeric (0-1). Similarity threshold for grouping
#'   near-identical terms. Default `0.85`.
#' @param stopwords Character vector. Additional stopwords beyond the built-in
#'   English list. Default `NULL`.
#' @param lang Character. `"en"` or `"es"` for console messages.
#' @param verbose Logical. Print progress?
#'
#' @return A list (class `orisma_dims`) with:
#'   \describe{
#'     \item{`dims`}{Named list: each element is a character vector of terms
#'       belonging to that dimension group.}
#'     \item{`term_freq`}{Data frame with term frequencies and co-occurrence
#'       scores.}
#'     \item{`n_dims`}{Number of dimension groups found.}
#'   }
#'
#' @seealso [orm_dim_matrix()] to build the risk x dimension cross-matrix.
#'
#' @examples
#' \dontrun{
#' refs   <- orm_load("my_references/")
#' result <- orm_run(refs)
#'
#' # Automatically discover dimensions
#' dims <- orm_autodim(result$mx)
#' print(dims)
#'
#' # Build risk x dimension matrix
#' mat <- orm_dim_matrix(result, dims)
#' }
#'
#' @export
orm_autodim <- function(mx,
                        text_col    = "abstract",
                        n_dims      = 15L,
                        min_freq    = 2L,
                        min_cooccur = 0.5,
                        fuzzy_sim   = 0.85,
                        stopwords   = NULL,
                        lang        = getOption("orisma.lang", "en"),
                        verbose     = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)

  if (!inherits(mx, "orisma_matrix")) {
    stop("'mx' must be an orisma_matrix object from orm_extract().",
         call. = FALSE)
  }

  refs     <- mx$refs
  bin_mat  <- mx$matrix

  if (verbose) {
    cli::cli_h2(if (lang == "es") "Extraccion automatica de dimensiones"
                else "Automatic dimension extraction")
  }

  # ── 1. Select text column ────────────────────────────────────────────────────
  if (!text_col %in% names(refs) ||
      mean(is.na(refs[[text_col]]) | refs[[text_col]] == "") > 0.5) {
    text_col <- "title"
    if (verbose) cli::cli_alert_warning(
      if (lang == "es") "Abstract no disponible, usando titulo"
      else "Abstract not available, falling back to title"
    )
  }

  # ── 2. Filter records with at least one risk detected ─────────────────────────
  has_risk <- rowSums(bin_mat) > 0
  refs_risk <- refs[has_risk, ]
  text_risk <- tolower(as.character(refs_risk[[text_col]]))
  text_risk[is.na(text_risk)] <- ""

  n_with_risk <- sum(has_risk)
  if (verbose) {
    cli::cli_alert_info(
      if (lang == "es")
        paste0("Analizando ", n_with_risk, " registros con al menos un riesgo detectado")
      else
        paste0("Analysing ", n_with_risk, " records with at least one risk detected")
    )
  }

  # ── 3. Built-in stopwords (English + common scientific) ───────────────────────
  sw_base <- c(
    "the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for",
    "of", "with", "by", "from", "as", "is", "was", "are", "were", "be",
    "been", "being", "have", "has", "had", "do", "does", "did", "will",
    "would", "could", "should", "may", "might", "shall", "can", "not",
    "no", "nor", "so", "yet", "both", "either", "neither", "this", "that",
    "these", "those", "it", "its", "we", "our", "they", "their", "he",
    "she", "his", "her", "us", "them", "which", "who", "whom", "what",
    "when", "where", "why", "how", "all", "each", "every", "any", "some",
    "such", "than", "more", "most", "also", "into", "over", "after",
    "between", "through", "during", "before", "under", "about", "against",
    "while", "within", "without", "among", "across", "along", "above",
    "below", "up", "down", "out", "off", "then", "there", "here", "only",
    "just", "even", "well", "back", "still", "way", "however", "thus",
    # Scientific boilerplate
    "study", "studies", "results", "data", "analysis", "method", "methods",
    "conclusion", "conclusions", "background", "objective", "objectives",
    "aim", "aims", "purpose", "approach", "paper", "article", "research",
    "review", "literature", "based", "using", "used", "use", "shown",
    "found", "show", "shows", "including", "included", "include",
    "significantly", "significant", "associated", "compared", "comparison",
    "measured", "measurement", "measurements", "assessed", "assessment",
    "evaluated", "evaluation", "investigated", "investigation",
    "performed", "conducted", "reported", "report", "presented",
    "present", "provide", "provided", "provides", "suggest", "suggests",
    "indicate", "indicates", "demonstrate", "demonstrates", "high", "low",
    "higher", "lower", "large", "small", "different", "similar", "various",
    "several", "many", "two", "three", "four", "five", "first", "second",
    "respectively", "however", "therefore", "furthermore", "moreover",
    "although", "despite", "due", "given", "known", "well", "new", "one",
    "since", "while", "whether", "both", "other", "further",
    # OHS boilerplate (too generic to be a useful dimension)
    "exposure", "risk", "risks", "hazard", "hazards", "health", "safety",
    "occupational", "workers", "worker", "operator", "operators",
    "concentration", "level", "levels", "particle", "particles",
    "emission", "emissions", "sample", "samples", "control", "controls",
    "effect", "effects", "potential", "current", "process", "processes",
    "surface", "material", "materials", "type", "types", "size", "number",
    "time", "work", "working", "workplace", "environment", "environmental"
  )

  sw_all <- unique(c(sw_base, stopwords))

  # ── 4. Tokenise and count term frequencies ───────────────────────────────────
  # Split each abstract into words
  tokens_list <- lapply(seq_along(text_risk), function(i) {
    txt   <- text_risk[[i]]
    # Keep only alphabetic sequences of 4+ chars
    words <- unlist(regmatches(txt, gregexpr("[a-z]{4,}", txt)))
    words <- words[!words %in% sw_all]
    unique(words)  # per-document presence (not raw count)
  })

  # Global frequency: in how many documents does each term appear?
  all_terms  <- unlist(tokens_list)
  term_freq  <- sort(table(all_terms), decreasing = TRUE)
  term_freq  <- term_freq[term_freq >= min_freq]

  if (length(term_freq) == 0) {
    if (verbose) cli::cli_alert_warning(
      if (lang == "es") "No se encontraron terminos frecuentes. Reduzca min_freq."
      else "No frequent terms found. Try reducing min_freq."
    )
    return(.empty_dims())
  }

  candidate_terms <- names(term_freq)

  # ── 5. Co-occurrence filter ───────────────────────────────────────────────────
  # For each term, what proportion of its appearances co-occur with a risk?
  risk_record_ids <- refs_risk$record_id

  cooccur_scores <- vapply(candidate_terms, function(term) {
    # Which risk records contain this term?
    in_risk <- vapply(tokens_list, function(toks) term %in% toks, logical(1))
    n_in_risk <- sum(in_risk)
    if (n_in_risk == 0) return(0)

    # All records containing this term (including non-risk ones)
    all_text <- tolower(as.character(refs[[text_col]]))
    all_text[is.na(all_text)] <- ""
    n_total <- sum(grepl(paste0("\\b", term, "\\b"), all_text, perl = TRUE))
    if (n_total == 0) return(0)

    n_in_risk / n_total
  }, numeric(1))

  # Keep terms above co-occurrence threshold
  valid_terms <- candidate_terms[cooccur_scores >= min_cooccur]

  if (length(valid_terms) == 0) {
    if (verbose) cli::cli_alert_warning(
      if (lang == "es") "Ningún término supera el umbral de co-ocurrencia. Reduzca min_cooccur."
      else "No terms pass the co-occurrence threshold. Try reducing min_cooccur."
    )
    return(.empty_dims())
  }

  # ── 6. Fuzzy deduplication and grouping ──────────────────────────────────────
  # Group near-identical terms (e.g. "titanium" and "titania" should be separate,
  # but "nanoparticle" and "nanoparticles" should merge)
  n_terms    <- length(valid_terms)
  groups     <- seq_len(n_terms)           # each term starts in its own group
  group_repr <- valid_terms                # representative for each group

  if (n_terms > 1) {
    dist_mat <- stringdist::stringdistmatrix(valid_terms, valid_terms,
                                             method = "lv")
    max_chars <- outer(nchar(valid_terms), nchar(valid_terms), pmax)
    sim_mat   <- 1 - dist_mat / pmax(max_chars, 1)

    for (i in seq_len(n_terms - 1)) {
      for (j in seq(i + 1, n_terms)) {
        if (sim_mat[i, j] >= fuzzy_sim && groups[j] == j) {
          groups[j] <- groups[i]   # merge j into i's group
        }
      }
    }
  }

  # Build group list: key = representative term, value = all synonyms
  unique_groups <- unique(groups)
  dim_list      <- lapply(unique_groups, function(g) {
    members  <- valid_terms[groups == g]
    # Representative: highest frequency member
    freqs    <- as.integer(term_freq[members])
    members[order(-freqs)]
  })

  # Name each group by its most frequent member
  names(dim_list) <- vapply(dim_list, `[[`, character(1), 1)

  # Sort groups by total frequency (sum of member frequencies)
  group_freqs <- vapply(dim_list, function(g) {
    sum(as.integer(term_freq[g[g %in% names(term_freq)]]), na.rm = TRUE)
  }, numeric(1))

  dim_list <- dim_list[order(-group_freqs)]

  # Trim to n_dims
  if (length(dim_list) > n_dims) dim_list <- dim_list[seq_len(n_dims)]

  if (verbose) {
    cli::cli_alert_success(
      if (lang == "es")
        paste0(length(dim_list), " dimensiones detectadas automaticamente")
      else
        paste0(length(dim_list), " dimensions detected automatically")
    )
    for (nm in names(dim_list)) {
      syns <- dim_list[[nm]]
      cli::cli_alert_info(paste0(
        "  [", nm, "]",
        if (length(syns) > 1) paste0(" + ", paste(syns[-1], collapse = ", "))
        else ""
      ))
    }
  }

  # ── 7. Build term frequency table ────────────────────────────────────────────
  tf_df <- data.frame(
    term        = names(dim_list),
    group_freq  = as.integer(group_freqs[seq_along(dim_list)]),
    cooccur_pct = round(cooccur_scores[names(dim_list)] * 100, 1),
    n_synonyms  = vapply(dim_list, length, integer(1)),
    synonyms    = vapply(dim_list, function(x)
                    paste(x[-1], collapse = "; "), character(1)),
    stringsAsFactors = FALSE
  )

  # ── 8. Return ─────────────────────────────────────────────────────────────────
  result <- list(
    dims      = dim_list,
    term_freq = tf_df,
    n_dims    = length(dim_list),
    text_col  = text_col,
    params    = list(
      min_freq    = min_freq,
      min_cooccur = min_cooccur,
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
#' `orm_dim_matrix()` takes an ORISMA analysis result and an `orisma_dims`
#' object (from [orm_autodim()]) and builds a **risk category x dimension**
#' cross-matrix, similar to the material x risk heatmaps common in
#' occupational health bibliometric reviews.
#'
#' Each cell contains the number of studies that mention both a given risk
#' category and a given dimension term. The matrix is visualised as a
#' hierarchical clustered heatmap with dendrograms.
#'
#' @param result An `orisma_result` object from [orm_analyse()] or [orm_run()].
#' @param dims An `orisma_dims` object from [orm_autodim()].
#' @param min_records Integer. Minimum records for a risk category to appear.
#'   Default `2`.
#' @param out_dir Character or NULL. If provided, saves the heatmap PNG here.
#' @param lang Character. `"en"` or `"es"`.
#' @param verbose Logical.
#'
#' @return A matrix (risk categories x dimensions) with co-occurrence counts.
#'   Invisibly also saves a heatmap PNG if `out_dir` is provided.
#'
#' @export
orm_dim_matrix <- function(result,
                            dims,
                            min_records = 2L,
                            out_dir     = NULL,
                            lang        = getOption("orisma.lang", "en"),
                            verbose     = getOption("orisma.verbose", TRUE)) {

  if (!inherits(result, "orisma_result")) {
    stop("'result' must be an orisma_result object.", call. = FALSE)
  }
  if (!inherits(dims, "orisma_dims")) {
    stop("'dims' must be an orisma_dims object from orm_autodim().", call. = FALSE)
  }

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

  # Text corpus
  text_all <- tolower(as.character(refs[[text_col]]))
  text_all[is.na(text_all)] <- ""

  # Build cross-matrix: rows = risk cats, cols = dimensions
  dim_names  <- names(dim_list)
  cross_mat  <- matrix(0L,
                       nrow = length(active_cats),
                       ncol = length(dim_names),
                       dimnames = list(
                         stringr::str_wrap(active_labels, 25),
                         dim_names
                       ))

  for (j in seq_along(dim_names)) {
    dim_terms <- dim_list[[j]]
    # Which records mention any term from this dimension?
    pattern   <- paste0("\\b(", paste(dim_terms, collapse = "|"), ")\\b")
    has_dim   <- grepl(pattern, text_all, ignore.case = TRUE, perl = TRUE)

    for (i in seq_along(active_cats)) {
      has_risk        <- bin_active[, i] == 1L
      cross_mat[i, j] <- sum(has_risk & has_dim)
    }
  }

  # ── Plot heatmap ──────────────────────────────────────────────────────────────
  title_txt <- if (lang == "es") "Focos de Riesgo PRL por Dimension"
               else "OHS Risk Focus by Dimension"

  # Only plot rows/cols with at least one non-zero cell
  row_active <- rowSums(cross_mat) > 0
  col_active <- colSums(cross_mat) > 0
  mat_plot   <- cross_mat[row_active, col_active, drop = FALSE]

  if (nrow(mat_plot) >= 2 && ncol(mat_plot) >= 2) {
    # Color palette: white -> light green -> dark green
    col_palette <- grDevices::colorRampPalette(
      c("white", "#FFF3E0", "#FFB74D", "#F44336")
    )(50)

    if (!is.null(out_dir)) {
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      out_path <- file.path(out_dir, "risk_dimension_heatmap.png")

      grDevices::png(out_path,
                     width  = max(2400, ncol(mat_plot) * 300),
                     height = max(2000, nrow(mat_plot) * 200),
                     res    = 300)
      pheatmap::pheatmap(
        mat_plot,
        cluster_rows  = nrow(mat_plot) > 2,
        cluster_cols  = ncol(mat_plot) > 2,
        color         = col_palette,
        main          = title_txt,
        fontsize      = 10,
        fontsize_row  = 9,
        fontsize_col  = 9,
        border_color  = "white",
        angle_col     = 45,
        display_numbers = TRUE,
        number_format = "%d",
        number_color  = "grey20"
      )
      grDevices::dev.off()

      if (verbose) cli::cli_alert_success(
        paste0(if (lang == "es") "Heatmap guardado en: " else "Heatmap saved to: ",
               out_path)
      )
    }
  } else {
    if (verbose) cli::cli_alert_warning(
      if (lang == "es")
        "Matriz demasiado pequena para visualizar. Reduzca min_records o min_freq."
      else
        "Matrix too small to visualise. Try reducing min_records or min_freq."
    )
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
  cat(" Text field used: ", x$text_col, "\n\n")
  cat(" Dimension groups (by frequency):\n")
  for (i in seq_along(x$dims)) {
    nm   <- names(x$dims)[i]
    syns <- x$dims[[nm]]
    tf   <- x$term_freq[x$term_freq$term == nm, ]
    cat(sprintf("  %2d. %-20s  freq=%-4d  cooccur=%s%%",
                i, nm,
                if (nrow(tf) > 0) tf$group_freq[1] else 0,
                if (nrow(tf) > 0) tf$cooccur_pct[1] else "?"))
    if (length(syns) > 1) cat("  [+", paste(syns[-1], collapse = ", "), "]")
    cat("\n")
  }
  invisible(x)
}


# ── Internal helper ────────────────────────────────────────────────────────────
#' @noRd
.empty_dims <- function() {
  result <- list(dims = list(), term_freq = data.frame(), n_dims = 0L,
                 text_col = "abstract",
                 params = list())
  class(result) <- c("orisma_dims", "list")
  result
}
