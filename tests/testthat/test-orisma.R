library(testthat)
library(orisma)

# ── Helpers: minimal synthetic data ──────────────────────────────────────────

.make_refs <- function(n = 10) {
  refs <- data.frame(
    record_id     = paste0("ORM", sprintf("%05d", seq_len(n))),
    source_db     = rep("Test DB", n),
    source_file   = rep("test.ris", n),
    title = c(
      "Nanoparticle exposure in laser powder bed fusion",
      "Occupational health risks of metal additive manufacturing",
      "Laser radiation hazards in LPBF processes",
      "Worker exposure to ultrafine particles in 3D printing",
      "Dust explosion risk in titanium powder handling",
      "Ergonomic assessment of additive manufacturing operators",
      "Inert gas asphyxiation risk in EBM chambers",
      "Metal fume emissions in directed energy deposition",
      "Review of occupational hazards in metal AM",
      "Electrical safety in additive manufacturing equipment"
    ),
    abstract = c(
      "This study measured nanoparticle concentrations during metal AM operations.",
      "Worker exposure to occupational hazards was assessed across 15 facilities.",
      "Laser radiation levels were characterised during LPBF printing.",
      "Breathing zone measurements of ultrafine particles were performed.",
      "Titanium dust explosion risk was evaluated using minimum ignition energy.",
      "RULA assessment was conducted for 20 AM operators.",
      "Oxygen depletion in confined build chambers was monitored.",
      "Metal fume and VOC emissions were measured during DED operations.",
      "A systematic review of occupational exposure in metal printing.",
      "Lockout tagout procedures for electrical systems in AM were reviewed."
    ),
    keywords    = rep("additive manufacturing; occupational health", n),
    year        = rep(2022L, n),
    doi         = paste0("10.1234/test.", seq_len(n)),
    authors     = paste0("Author", seq_len(n), ", A"),
    journal     = rep("Test Journal", n),
    volume      = rep("1", n),
    issue       = rep("1", n),
    pages       = rep("1-10", n),
    document_type = rep("Article", n),
    stringsAsFactors = FALSE
  )
  class(refs) <- c("orisma_refs", "tbl_df", "tbl", "data.frame")
  attr(refs, "orisma_stage")   <- "loaded"
  attr(refs, "orisma_lang")    <- "en"
  attr(refs, "orisma_created") <- Sys.time()
  attr(refs, "n_sources")      <- 1L
  dplyr::as_tibble(refs)
}


# ── Tests: orm_dict ───────────────────────────────────────────────────────────

test_that("orm_dict returns correct class", {
  d <- orm_dict()
  expect_s3_class(d, "orisma_dict")
})

test_that("orm_dict has expected categories", {
  d <- orm_dict()
  expect_true("nanoparticles" %in% names(d))
  expect_true("laser_radiation" %in% names(d))
  expect_true("explosion" %in% names(d))
})

test_that("orm_dict_categories returns a data frame", {
  d  <- orm_dict()
  df <- orm_dict_categories(d)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("category", "label", "n_terms") %in% names(df)))
})

test_that("orm_dict_add_terms adds new terms", {
  d  <- orm_dict()
  d2 <- orm_dict_add_terms(d, "nanoparticles", c("nano-cloud", "nano-fog"))
  expect_true("nano-cloud" %in% d2$nanoparticles$terms)
  expect_true("nano-fog"   %in% d2$nanoparticles$terms)
})

test_that("orm_dict_add_category creates new entry", {
  d  <- orm_dict()
  d2 <- orm_dict_add_category(
    d, "vibration",
    label_en = "Vibration exposure",
    label_es = "Exposición a vibraciones",
    terms    = c("hand-arm vibration", "whole-body vibration", "HAV")
  )
  expect_true("vibration" %in% names(d2))
  expect_equal(d2$vibration$label_en, "Vibration exposure")
})


# ── Tests: orm_extract ────────────────────────────────────────────────────────

test_that("orm_extract returns orisma_matrix", {
  refs <- .make_refs()
  mx   <- orm_extract(refs, verbose = FALSE)
  expect_s3_class(mx, "orisma_matrix")
})

test_that("orm_extract produces binary matrix with correct dimensions", {
  refs  <- .make_refs(10)
  dict  <- orm_dict()
  mx    <- orm_extract(refs, dict = dict, verbose = FALSE)
  n_cats <- length(dict)
  expect_equal(nrow(mx$matrix), 10)
  expect_equal(ncol(mx$matrix), n_cats)
  expect_true(all(mx$matrix %in% c(0L, 1L)))
})

test_that("orm_extract detects nanoparticle category correctly", {
  refs <- .make_refs()
  mx   <- orm_extract(refs, verbose = FALSE)
  # First record is about nanoparticles — should be flagged
  expect_equal(mx$matrix[1, "nanoparticles"], 1L)
})

test_that("orm_extract detects worker exposure data", {
  refs <- .make_refs()
  mx   <- orm_extract(refs, verbose = FALSE)
  # Records 2 and 4 explicitly mention "worker exposure" / "breathing zone"
  expect_true(any(mx$refs$has_worker_data))
})


# ── Tests: orm_analyse ────────────────────────────────────────────────────────

test_that("orm_analyse returns orisma_result", {
  refs   <- .make_refs()
  mx     <- orm_extract(refs, verbose = FALSE)
  result <- orm_analyse(mx, verbose = FALSE)
  expect_s3_class(result, "orisma_result")
})

test_that("WRDI is between 0 and 1", {
  refs   <- .make_refs()
  mx     <- orm_extract(refs, verbose = FALSE)
  result <- orm_analyse(mx, verbose = FALSE)
  expect_gte(result$WRDI_global, 0)
  expect_lte(result$WRDI_global, 1)
})

test_that("RCS values are positive", {
  refs   <- .make_refs()
  mx     <- orm_extract(refs, verbose = FALSE)
  result <- orm_analyse(mx, verbose = FALSE)
  expect_true(all(result$RCS[!is.na(result$RCS)] > 0))
})

test_that("indicators table has expected columns", {
  refs   <- .make_refs()
  mx     <- orm_extract(refs, verbose = FALSE)
  result <- orm_analyse(mx, verbose = FALSE)
  expect_true(all(c("category", "n_records", "WRDI", "RCS") %in%
                    names(result$indicators)))
})

test_that("cooccurrence matrix is symmetric", {
  refs   <- .make_refs()
  mx     <- orm_extract(refs, verbose = FALSE)
  result <- orm_analyse(mx, verbose = FALSE)
  expect_equal(result$cooccur_mat, t(result$cooccur_mat))
})


# ── Tests: orm_run ────────────────────────────────────────────────────────────

test_that("orm_run completes without error", {
  refs   <- .make_refs()
  result <- orm_run(refs, verbose = FALSE)
  expect_s3_class(result, "orisma_result")
})

test_that("orm_run result has expected structure", {
  refs   <- .make_refs()
  result <- orm_run(refs, verbose = FALSE)
  expect_true(!is.null(result$WRDI_global))
  expect_true(!is.null(result$indicators))
  expect_true(!is.null(result$cooccur_mat))
})


# ── Tests: orm_report ─────────────────────────────────────────────────────────

test_that("orm_report creates output directory and files", {
  refs    <- .make_refs()
  result  <- orm_run(refs, verbose = FALSE)
  tmp_dir <- tempfile("orisma_test_")

  orm_report(result,
             out_dir = tmp_dir,
             formats = c("csv", "certificate"),
             verbose = FALSE)

  expect_true(dir.exists(tmp_dir))
  expect_true(file.exists(file.path(tmp_dir, "orisma_indicators.csv")))
  expect_true(file.exists(file.path(tmp_dir, "analysis.orisma")))

  unlink(tmp_dir, recursive = TRUE)
})

test_that("reproducibility certificate contains required fields", {
  refs    <- .make_refs()
  result  <- orm_run(refs, verbose = FALSE)
  tmp_dir <- tempfile("orisma_cert_")

  orm_report(result,
             out_dir = tmp_dir,
             formats = "certificate",
             verbose = FALSE)

  cert <- jsonlite::read_json(file.path(tmp_dir, "analysis.orisma"))
  expect_true("orisma_version"  %in% names(cert))
  expect_true("WRDI_global"     %in% names(cert))
  expect_true("digest_matrix"   %in% names(cert))

  unlink(tmp_dir, recursive = TRUE)
})
