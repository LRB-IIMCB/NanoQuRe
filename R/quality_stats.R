#' Quality Stats
#'
#' Generates a table containing the most important quality metrics from a
#' sequencing summary: number of all reads, number of passed reads, N50 value,
#' mean Q-score, longest read length, and mean length of passed reads.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#'
#' @returns A dataframe with quality metrics for the sequencing run
#' @import dplyr
#' @export
#'
#' @examples
#' quality_stats(sample_data)
quality_stats <- function(seq_summary) {
  
  # --- Validation ---
  if (nrow(seq_summary) == 0)
    stop("The input data frame is empty")
  if (!("sample_id" %in% names(seq_summary)))
    stop("The data frame is missing the 'sample_id' column")
  if (!("run_id" %in% names(seq_summary)))
    stop("The data frame is missing the 'run_id' column")
  if (!("passes_filtering" %in% names(seq_summary)))
    stop("The data frame is missing the 'passes_filtering' column")
  if (!("mean_qscore_template" %in% names(seq_summary)))
    stop("The data frame is missing the 'mean_qscore_template' column")
  if (!("sequence_length_template" %in% names(seq_summary)))
    stop("The data frame is missing the 'sequence_length_template' column")
  if (!is.numeric(seq_summary$mean_qscore_template))
    stop("Column 'mean_qscore_template' must be numeric")
  if (!is.numeric(seq_summary$sequence_length_template))
    stop("Column 'sequence_length_template' must be numeric")
  
  # --- Compute ---
  n50_value <- calculate_n50(seq_summary)
  
  qtab1 <- seq_summary %>%
    dplyr::summarise(
      "sample id"          = dplyr::first(sample_id),
      "all reads"          = dplyr::n(),
      "passed reads"       = sum(passes_filtering),
      "n50 value"          = round(n50_value, 2),
      "mean qscore"        = round(mean(mean_qscore_template), 2),
      "longest read"       = max(sequence_length_template),
      "passed mean length" = round(mean(
        sequence_length_template[passes_filtering == TRUE],
        na.rm = TRUE), 2)
    )
  
  return(qtab1)
}