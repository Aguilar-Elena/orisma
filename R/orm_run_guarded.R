#' Run ORISMA with a relevance-control layer
#'
#' @description
#' Runs ORISMA after applying `orm_relevance_guard()`. This is useful for
#' real-world bibliographic searches where broad database queries may retrieve
#' technically related but non-occupational or off-topic records.
#'
#' @param refs A data frame of references, usually produced by `orm_load()`.
#' @param topic Topic label passed to `orm_relevance_guard()` and `orm_run()`.
#' @param exclude_non_relevant Logical. If `TRUE`, records flagged as non-relevant
#' are excluded before running the main ORISMA pipeline.
#' @param min_records Minimum number of records required after filtering. If the
#' filter leaves fewer records, the function stops to avoid accidental over-filtering.
#' @param topic_regex Optional topic regex.
#' @param occupational_regex Optional occupational relevance regex.
#' @param noise_regex Optional noise regex.
#' @param ... Additional arguments passed to `orm_run()`.
#'
#' @return An ORISMA result object with an added `relevance_guard` component.
#' @export
orm_run_guarded <- function(refs,
                            topic = NULL,
                            exclude_non_relevant = TRUE,
                            min_records = 50,
                            topic_regex = NULL,
                            occupational_regex = NULL,
                            noise_regex = NULL,
                            ...) {

  if (!is.data.frame(refs)) {
    stop("`refs` must be a data frame. Use orm_load() first.", call. = FALSE)
  }

  guarded <- orm_relevance_guard(
    refs,
    topic = topic,
    topic_regex = topic_regex,
    occupational_regex = occupational_regex,
    noise_regex = noise_regex
  )

  relevance_summary <- data.frame(
    metric = c(
      "records_before_guard",
      "topic_relevant",
      "occupational_relevant",
      "biomedical_noise",
      "excluded_by_guard",
      "records_after_guard"
    ),
    value = c(
      nrow(guarded),
      sum(guarded$topic_relevant, na.rm = TRUE),
      sum(guarded$occupational_relevant, na.rm = TRUE),
      sum(guarded$biomedical_noise, na.rm = TRUE),
      sum(guarded$exclusion_flag, na.rm = TRUE),
      sum(!guarded$exclusion_flag, na.rm = TRUE)
    )
  )

  message("ORISMA relevance guard")
  message("Records before guard: ", nrow(guarded))
  message("Records excluded: ", sum(guarded$exclusion_flag, na.rm = TRUE))
  message("Records retained: ", sum(!guarded$exclusion_flag, na.rm = TRUE))

  refs_for_run <- guarded

  if (isTRUE(exclude_non_relevant)) {
    refs_for_run <- guarded[!guarded$exclusion_flag, , drop = FALSE]
  }

  if (nrow(refs_for_run) < min_records) {
    stop(
      "The relevance guard retained only ", nrow(refs_for_run),
      " records. This is below `min_records = ", min_records, "`. ",
      "Review the regex rules or set `exclude_non_relevant = FALSE`.",
      call. = FALSE
    )
  }

  result <- orm_run(
    refs_for_run,
    topic = topic,
    ...
  )

  result$relevance_guard <- list(
    summary = relevance_summary,
    all_records = guarded,
    analysed_records = refs_for_run,
    exclude_non_relevant = exclude_non_relevant,
    topic_regex = topic_regex,
    occupational_regex = occupational_regex,
    noise_regex = noise_regex
  )

  result
}
