#' Sample bibliographic records for ORISMA
#'
#' Twenty bibliographic records on occupational health in metal
#' additive manufacturing (2015-2026) from Web of Science and Scopus,
#' pre-processed with ORISMA to illustrate the full pipeline.
#'
#' @format A data frame with 20 rows and 9 variables:
#' \describe{
#'   \item{record_id}{Character. Unique record identifier.}
#'   \item{title}{Character. Article title.}
#'   \item{abstract}{Character. Abstract (max 800 characters).}
#'   \item{year}{Integer. Publication year.}
#'   \item{doi}{Character. Digital Object Identifier.}
#'   \item{source_db}{Character. Source database.}
#'   \item{bridge_type}{Character. Bridge classification.}
#'   \item{bridge_score}{Integer. Bridge score (0-5).}
#'   \item{ass_score}{Integer. Abstract Sufficiency Score (0-5).}
#' }
#' @source Web of Science and Scopus (2015-2026).
#' @usage data(orisma_sample)
#' @keywords datasets
"orisma_sample"
