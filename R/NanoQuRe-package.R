#' @keywords internal
"_PACKAGE"
## usethis namespace: start
#' @import dplyr
#' @import rmarkdown
#' @importFrom data.table data.table
#' @importFrom DT datatable
#' @importFrom plotly plot_ly add_bars add_lines add_trace layout subplot
#' @importFrom stats quantile
## usethis namespace: end
NULL
################################################################################
# NSE variable declarations for R CMD check
################################################################################
# These variables are used in dplyr/data.table NSE contexts (mutate/filter/
# group_by/summarise/count/:=) throughout the package and would otherwise
# generate "no visible binding/function definition" NOTEs.
utils::globalVariables(c(
  # Core sequencing summary columns
  ##############################################################################
  "sample_id",
  "run_id",
  "channel",
  "start_time",
  "duration",
  "sequence_length_template",
  "mean_qscore_template",
  "passes_filtering",
  # Channel activity decay (plot_active_channels)
  ##############################################################################
  "last_activity",
  "channel_no_start",
  "inactive_channels",
  "active_channels",
  # Cumulative yield binning (plot_cumulative_yield)
  ##############################################################################
  "h_start_time",
  "bases_gb",
  "time_bin",
  # Quality distribution binning (plot_quality_distribution)
  ##############################################################################
  "bin",
  "bin_mid",
  # Length / Q-score over time (plot_double)
  ##############################################################################
  "hour",
  "min_length",
  # Pore activity heatmap (pore_activity_heatmap)
  ##############################################################################
  "template_unix",
  "bin_idx",
  # Generic dplyr::count() / aggregation output
  ##############################################################################
  "n",
  # magrittr placeholder (used in conditional pipe branches)
  ##############################################################################
  ".",
  # data.table's in-place assignment operator (tab[, col := value]) - not a
  # real function/variable, but codetools flags it the same way; this is
  # data.table's own documented fix for this exact NOTE
  ##############################################################################
  ":="
))
