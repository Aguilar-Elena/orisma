#' Load bibliographic references from one or multiple files / folders
#'
#' @description
#' `orm_load()` is the entry point of every ORISMA analysis. It reads
#' bibliographic files in **RIS**, **BibTeX**, or **CSV** format from a folder
#' (or a vector of individual file paths), detects the format of each file
#' automatically, combines all records into a single tidy data frame, and
#' records the source database for each record.
#'
#' All major bibliographic databases export to at least one supported format:
#'
#' | Database | Recommended format | Notes |
#' |---|---|---|
#' | Web of Science | RIS / Plain text | Max 1 000 records per batch |
#' | Scopus | RIS or CSV | Max 2 000 records per batch |
#' | PubMed | RIS | No limit |
#' | Dimensions | CSV or RIS | Max 2 500 per batch |
#' | EBSCO (CINAHL, BSC) | RIS | Up to 25 000 |
#' | ProQuest | RIS or BibTeX | Max 100 per batch |
#' | Cochrane Library | RIS | No limit |
#' | Ovid / MEDLINE | RIS | Max 1 000 per batch |
#' | ScienceDirect | RIS | No limit |
#' | The Lens (free) | RIS or CSV | No limit |
#'
#' @param path Character. Path to a **folder** containing reference files, or a
#'   **character vector** of individual file paths.
#' @param lang Character. Language for console messages: `"en"` (default) or
#'   `"es"`. Overrides `getOption("orisma.lang")`.
#' @param verbose Logical. Print progress messages? Default `TRUE`.
#'
#' @return A tibble (class `orisma_refs`) with standardised columns:
#'   \describe{
#'     \item{`record_id`}{Internal unique identifier assigned by ORISMA}
#'     \item{`source_file`}{Name of the original file}
#'     \item{`source_db`}{Database inferred from file name or format}
#'     \item{`title`}{Article title}
#'     \item{`authors`}{Authors (semicolon-separated)}
#'     \item{`year`}{Publication year}
#'     \item{`doi`}{Digital Object Identifier (if available)}
#'     \item{`abstract`}{Abstract text}
#'     \item{`keywords`}{Author keywords}
#'     \item{`journal`}{Journal name}
#'     \item{`volume`, `issue`, `pages`}{Bibliographic location}
#'     \item{`document_type`}{Article, review, conference paper, etc.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Load all .ris and .bib files from a folder
#' refs <- orm_load("my_references/")
#'
#' # Load specific files
#' refs <- orm_load(c("wos_results.ris", "scopus_results.csv"))
#'
#' # Spanish messages
#' refs <- orm_load("mis_referencias/", lang = "es")
#' }
#'
#' @export
orm_load <- function(path, lang = getOption("orisma.lang", "en"),
                     verbose = getOption("orisma.verbose", TRUE)) {

  .check_lang(lang)

  if (verbose) cli::cli_h1(.msg("phase_load", lang))

  # ── 1. Resolve file list ────────────────────────────────────────────────────
  files <- .resolve_files(path, verbose, lang)
  if (length(files) == 0) {
    cli::cli_alert_danger(.msg("load_no_files", lang, path = path))
    cli::cli_alert_info(.msg("load_fmt_hint", lang))
    return(invisible(NULL))
  }

  formats <- unique(tools::file_ext(files))
  if (verbose) {
    cli::cli_alert_info(.msg("load_files", lang,
                             n = length(files),
                             formats = paste(formats, collapse = ", ")))
  }

  # ── 2. Read each file ───────────────────────────────────────────────────────
  records_list <- lapply(seq_along(files), function(i) {
    f   <- files[[i]]
    ext <- tolower(tools::file_ext(f))
    db  <- .infer_database(f)

    raw <- tryCatch(
      switch(ext,
        ris = synthesisr::read_refs(f),
        bib = synthesisr::read_refs(f),
        csv = .read_csv_refs(f),
        {
          cli::cli_alert_warning(paste0("Skipping unsupported file: ", basename(f)))
          return(NULL)
        }
      ),
      error = function(e) {
        cli::cli_alert_warning(paste0("Could not read: ", basename(f), " — ", e$message))
        NULL
      }
    )

    if (is.null(raw) || nrow(raw) == 0) return(NULL)

    # Standardise column names
    raw <- .standardise_columns(raw)
    raw$source_file <- basename(f)
    raw$source_db   <- db
    raw
  })

  # ── 3. Combine ──────────────────────────────────────────────────────────────
  records_list <- Filter(Negate(is.null), records_list)
  if (length(records_list) == 0) {
    cli::cli_alert_danger("No records could be parsed from the provided files.")
    return(invisible(NULL))
  }

  combined <- dplyr::bind_rows(records_list)

  # Assign internal IDs
  combined$record_id <- paste0("ORM", sprintf("%05d", seq_len(nrow(combined))))

  # Reorder columns
  combined <- .reorder_columns(combined)

  # Set class
  class(combined) <- c("orisma_refs", "tbl_df", "tbl", "data.frame")
  attr(combined, "orisma_stage")   <- "loaded"
  attr(combined, "orisma_lang")    <- lang
  attr(combined, "orisma_created") <- Sys.time()
  attr(combined, "n_sources")      <- length(files)

  if (verbose) {
    cli::cli_alert_success(.msg("load_done", lang,
                               n_total   = nrow(combined),
                               n_sources = length(files)))
  }

  combined
}


# ── Internal helpers ──────────────────────────────────────────────────────────

#' @noRd
.resolve_files <- function(path, verbose, lang) {
  supported_ext <- c("ris", "bib", "csv")

  if (length(path) == 1 && dir.exists(path)) {
    all_files <- list.files(path, full.names = TRUE, recursive = FALSE)
    files <- all_files[tolower(tools::file_ext(all_files)) %in% supported_ext]
  } else {
    files <- path[file.exists(path) &
                    tolower(tools::file_ext(path)) %in% supported_ext]
    missing <- path[!file.exists(path)]
    if (length(missing) > 0) {
      cli::cli_alert_warning(paste0("Files not found: ",
                                    paste(basename(missing), collapse = ", ")))
    }
  }
  files
}

#' @noRd
.infer_database <- function(filepath) {
  fname <- tolower(basename(filepath))
  dplyr::case_when(
    grepl("wos|web.of.science|savedrecs", fname) ~ "Web of Science",
    grepl("scopus",                        fname) ~ "Scopus",
    grepl("pubmed|medline",                fname) ~ "PubMed",
    grepl("dimensions",                    fname) ~ "Dimensions",
    grepl("cochrane",                      fname) ~ "Cochrane",
    grepl("proquest",                      fname) ~ "ProQuest",
    grepl("ebsco|cinahl",                  fname) ~ "EBSCO",
    grepl("lens",                          fname) ~ "The Lens",
    grepl("ovid",                          fname) ~ "Ovid",
    TRUE                                          ~ "Unknown"
  )
}

#' @noRd
.read_csv_refs <- function(filepath) {
  raw <- readr::read_csv(filepath, show_col_types = FALSE, progress = FALSE)
  # Scopus CSV uses specific column names; normalise to synthesisr-like names
  names(raw) <- tolower(gsub("\\s+", "_", names(raw)))
  raw
}

#' @noRd
.standardise_columns <- function(df) {
  # Map common column name variants to ORISMA standard names
  col_map <- c(
    title        = "title|TI|article.title|Article.Title",
    authors      = "author|AU|authors|Authors",
    year         = "year|PY|publication.year|Year",
    doi          = "doi|DO|DOI",
    abstract     = "abstract|AB|Abstract",
    keywords     = "keywords|DE|author.keywords|Author.Keywords",
    journal      = "journal|SO|source.title|Source.Title",
    volume       = "volume|VL|Volume",
    issue        = "issue|IS|Issue",
    pages        = "pages|BP|EP|Page.Start",
    document_type = "document.type|DT|Document.Type"
  )

  df_names <- names(df)
  for (std_name in names(col_map)) {
    pattern  <- col_map[[std_name]]
    matched  <- grep(paste0("^(", pattern, ")$"), df_names,
                     ignore.case = TRUE, value = TRUE)
    if (length(matched) > 0 && !std_name %in% df_names) {
      names(df)[names(df) == matched[[1]]] <- std_name
    }
  }

  # Ensure all standard columns exist (fill with NA if missing)
  std_cols <- names(col_map)
  for (col in std_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  dplyr::as_tibble(df)
}

#' @noRd
.reorder_columns <- function(df) {
  priority <- c("record_id", "source_db", "source_file",
                "title", "authors", "year", "doi",
                "abstract", "keywords", "journal",
                "volume", "issue", "pages", "document_type")
  rest     <- setdiff(names(df), priority)
  present  <- intersect(priority, names(df))
  df[, c(present, rest)]
}

#' @noRd
.check_lang <- function(lang) {
  if (!lang %in% c("en", "es")) {
    stop(.msg("err_lang", lang = "en", lang = lang), call. = FALSE)
  }
}
