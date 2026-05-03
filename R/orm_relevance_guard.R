#' Relevance guard for occupational risk evidence mapping
#'
#' @description
#' Adds a relevance-control layer before ORISMA analysis. The function identifies
#' whether each record is relevant to the target topic, whether it contains an
#' occupational context, whether it is likely to be biomedical or clinical noise,
#' and whether it should be excluded from the main occupational analysis.
#'
#' @param data A data frame of bibliographic records.
#' @param topic Optional topic label used to derive a topic-specific regular expression.
#' @param topic_regex Optional regular expression defining the target technology/topic.
#' @param occupational_regex Optional regular expression defining occupational relevance.
#' @param noise_regex Optional regular expression defining likely off-topic biomedical/clinical noise.
#' @param title_col Optional title column name. If `NULL`, it is detected automatically.
#' @param abstract_col Optional abstract column name. If `NULL`, it is detected automatically.
#' @param keywords_col Optional keywords column name. If `NULL`, it is detected automatically.
#'
#' @return The input data frame with additional relevance-control columns.
#' @export
orm_relevance_guard <- function(data,
                                topic = NULL,
                                topic_regex = NULL,
                                occupational_regex = NULL,
                                noise_regex = NULL,
                                title_col = NULL,
                                abstract_col = NULL,
                                keywords_col = NULL) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  nms <- names(data)
  nms_low <- tolower(nms)

  detect_col <- function(candidates) {
    idx <- match(tolower(candidates), nms_low)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) return(NULL)
    nms[idx[1]]
  }

  if (is.null(title_col)) {
    title_col <- detect_col(c("title", "ti", "article_title", "document_title"))
  }

  if (is.null(abstract_col)) {
    abstract_col <- detect_col(c("abstract", "ab", "summary", "description"))
  }

  if (is.null(keywords_col)) {
    keywords_col <- detect_col(c("keywords", "author_keywords", "de", "id", "kw", "keyword"))
  }

  safe_text <- function(col) {
    if (is.null(col) || !col %in% names(data)) {
      return(rep("", nrow(data)))
    }
    x <- data[[col]]
    x[is.na(x)] <- ""
    as.character(x)
  }

  txt <- paste(
    safe_text(title_col),
    safe_text(abstract_col),
    safe_text(keywords_col),
    sep = " "
  )

  txt_low <- tolower(txt)

  if (is.null(topic_regex)) {
    topic_text <- tolower(ifelse(is.null(topic), "", topic))

    if (grepl("robot|cobot|automation|human.robot|collaborative", topic_text)) {
      topic_regex <- paste(
        c(
          "cobot[s]?",
          "collaborative robot[s]?",
          "collaborative robotics",
          "human[- ]?robot",
          "human robot collaboration",
          "human.robot collaboration",
          "human.robot interaction",
          "\\bhrc\\b",
          "industrial robot[s]?",
          "robotic automation",
          "autonomous mobile robot[s]?",
          "\\bamr\\b",
          "automation"
        ),
        collapse = "|"
      )
    } else if (grepl("additive|3d|powder bed|metal", topic_text)) {
      topic_regex <- paste(
        c(
          "additive manufacturing",
          "3d printing",
          "three-dimensional printing",
          "metal printing",
          "laser powder bed fusion",
          "powder bed fusion",
          "\\bl-pbf\\b",
          "\\blpbf\\b",
          "selective laser melting",
          "\\bslm\\b",
          "directed energy deposition",
          "\\bded\\b"
        ),
        collapse = "|"
      )
    } else {
      words <- unlist(strsplit(topic_text, "[^a-z0-9]+"))
      words <- words[nchar(words) >= 4]
      if (length(words) == 0) {
        topic_regex <- "."
      } else {
        topic_regex <- paste(unique(words), collapse = "|")
      }
    }
  }

  if (is.null(occupational_regex)) {
    occupational_regex <- paste(
      c(
        "occupational",
        "workplace",
        "work place",
        "worker[s]?",
        "employee[s]?",
        "operator[s]?",
        "personnel",
        "job",
        "task",
        "industrial",
        "factory",
        "manufacturing",
        "assembly",
        "production line",
        "safety",
        "health and safety",
        "risk assessment",
        "hazard",
        "exposure",
        "ergonomic",
        "ergonomics",
        "musculoskeletal",
        "psychosocial",
        "mental workload",
        "workload",
        "fatigue",
        "human factors",
        "personal sampling",
        "breathing zone",
        "field study",
        "on-site",
        "onsite",
        "work environment"
      ),
      collapse = "|"
    )
  }

  if (is.null(noise_regex)) {
    noise_regex <- paste(
      c(
        "patient[s]?",
        "clinical",
        "surgery",
        "surgical",
        "hospital infection",
        "infection control",
        "disease treatment",
        "antimicrobial resistance",
        "antibiotic",
        "vaccine[s]?",
        "renal transplant",
        "transplant",
        "aquaculture",
        "aquamedicine",
        "manure",
        "livestock",
        "animal vaccine",
        "genomic surveillance",
        "pathogenic bacteria",
        "public health surveillance"
      ),
      collapse = "|"
    )
  }

  topic_relevant <- grepl(topic_regex, txt_low, perl = TRUE)
  occupational_relevant <- grepl(occupational_regex, txt_low, perl = TRUE)
  biomedical_noise <- grepl(noise_regex, txt_low, perl = TRUE)

  # Strong occupational override: if title/abstract clearly contains workplace terms,
  # do not exclude only because of biomedical vocabulary.
  strong_occupational_regex <- paste(
    c(
      "occupational exposure",
      "worker exposure",
      "workplace exposure",
      "personal sampling",
      "breathing zone",
      "operators?",
      "workers?",
      "industrial workplace",
      "manufacturing worker",
      "assembly worker",
      "occupational safety",
      "occupational health"
    ),
    collapse = "|"
  )

  strong_occupational <- grepl(strong_occupational_regex, txt_low, perl = TRUE)

  # Conservative exclusion logic:
  # - Records outside the target topic are excluded.
  # - Topic-related records with no occupational signal are excluded.
  # - Biomedical/clinical records are excluded only when they lack occupational relevance.
  # - Records with both topic and occupational relevance are retained, even if they contain
  #   biomedical terms, because robotics, rehabilitation, healthcare and wearable-sensor
  #   studies may still be relevant for occupational ergonomics or prevention.
  exclusion_flag <- (!topic_relevant) |
    (topic_relevant & !occupational_relevant) |
    (biomedical_noise & !occupational_relevant & !strong_occupational)

  review_flag <- biomedical_noise & occupational_relevant & topic_relevant

  exclusion_reason <- rep("included", length(txt_low))
  exclusion_reason[!topic_relevant] <- "not related to target topic"
  exclusion_reason[topic_relevant & !occupational_relevant] <- "topic-related but no clear occupational context"
  exclusion_reason[biomedical_noise & !occupational_relevant & !strong_occupational] <- "likely biomedical/clinical/non-occupational noise"
  exclusion_reason[review_flag] <- "included but flagged for biomedical/clinical review"
  exclusion_reason[topic_relevant & occupational_relevant & !biomedical_noise] <- "included"

  data$topic_relevant <- topic_relevant
  data$occupational_relevant <- occupational_relevant
  data$biomedical_noise <- biomedical_noise
  data$strong_occupational_signal <- strong_occupational
  data$review_flag <- review_flag
  data$exclusion_flag <- exclusion_flag
  data$exclusion_reason <- exclusion_reason

  data
}
