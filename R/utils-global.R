# utils-global.R
# Resolves R CMD check NOTEs about global variables and missing imports
# This file is NOT exported - internal use only

# ── Suppress R CMD check NOTEs for ggplot2 aes() variables ───────────────────
# These are column names used inside aes() that R CMD check cannot resolve
# statically. Declaring them here suppresses the "no visible binding" NOTE.

utils::globalVariables(c(
  # ggplot2 aes variables used in orm_report.R plot functions
  "WRDI", "RCS", "label", "n_records", "pct_records",
  "year", "n", "category",
  # dplyr .data pronoun and dot
  ".data", ".",
  # Internal helper
  ".msg"
))

# ── importFrom declarations for functions used via :: in code ─────────────────
# These tell R CMD check that we are intentionally importing these functions

#' @importFrom dplyr mutate filter select group_by ungroup summarise
#'   bind_rows bind_cols across starts_with all_of as_tibble n
#'   row_number case_when
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv write_csv
#' @importFrom stringr str_extract str_escape
#' @importFrom ggplot2 ggplot aes geom_col geom_point geom_line geom_vline
#'   geom_hline geom_text scale_fill_gradient scale_colour_manual
#'   scale_colour_gradient scale_size_continuous annotate labs
#'   theme_minimal theme element_blank guides guide_legend ggsave
#' @importFrom ggrepel geom_text_repel
#' @importFrom grDevices colorRampPalette dev.off png
#' @importFrom stats reorder na.omit
#' @importFrom utils packageVersion head
#' @importFrom tools file_ext
#' @importFrom jsonlite write_json read_json
#' @importFrom digest digest
#' @importFrom glue glue
#' @importFrom cli cli_h1 cli_h2 cli_alert_success cli_alert_warning
#'   cli_alert_danger cli_alert_info cli_progress_bar cli_progress_update
#'   cli_progress_done cli_rule
NULL
