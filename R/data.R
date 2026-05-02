#' Sample bibliographic records for ORISMA examples and testing
#'
#' A dataset of 20 bibliographic records representative of the
#' occupational health and safety literature on metal additive
#' manufacturing (2015-2026). Records were retrieved from Web of
#' Science and Scopus and pre-processed with ORISMA.
#'
#' @format A data frame with 20 rows and 9 variables:
#' \describe{
#'   \item{record_id}{Character. Unique record identifier.}
#'   \item{title}{Character. Article title.}
#'   \item{abstract}{Character. Abstract (truncated to 800 characters).}
#'   \item{year}{Integer. Publication year.}
#'   \item{doi}{Character. Digital Object Identifier.}
#'   \item{source_db}{Character. Source database.}
#'   \item{bridge_type}{Character. Bridge classification.}
#'   \item{bridge_score}{Integer. Bridge score (0-5).}
#'   \item{ass_score}{Integer. Abstract Sufficiency Score (0-5).}
#' }
#' @source Web of Science and Scopus (2015-2026).
#' @docType data
#' @keywords datasets
#' @examples
#' data(orisma_sample)
#' head(orisma_sample)
#' table(orisma_sample$bridge_type)
"orisma_sample"
