#' Quality Stats
#'Generates a joined table from sequencing summaries that contains most import quality metrics - number of all reads, number of passed reads, n50 value, mean q-score, longest read length, mean length of passed reads.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#'
#' @return joined table with quality data for all sequencing summaries in database
#' @import dplyr
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' NULL
quality_stats <- function(seq_summary){

  assertthat::assert_that(nrow(seq_summary) > 0, msg = "The input data frame is empty")

  assertthat::assert_that(is.numeric(seq_summary$mean_qscore_template), msg = "Column 'mean_qscore_template' must be numeric")
  assertthat::assert_that(is.numeric(seq_summary$sequence_length_template), msg = "Column 'sequence_length_template' must be numeric")


  assertthat::assert_that(assertthat::has_name(seq_summary, "sample_id"), msg = "The data frame is missing the 'sample_id' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "run_id"), msg = "The data frame is missing the 'run_id' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "passes_filtering"), msg = "The data frame is missing the 'passes_filtering' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "mean_qscore_template"), msg = "The data frame is missing the 'mean_qscore_template' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sequence_length_template"), msg = "The data frame is missing the 'sequence_length_template' column")

  n50_value <- calculate_n50(seq_summary)

  qtab1 <- seq_summary %>%
    dplyr::summarise(
      "sample id" = dplyr::first(sample_id),
      "all reads" = dplyr::n(),
      "passed reads" = sum(passes_filtering),
      "n50 value" = round(n50_value, 2),
      "mean qscore" = round(mean(mean_qscore_template), 2),
      "longest read" = max(sequence_length_template),
      "passed mean length" = round(mean(
        sequence_length_template[passes_filtering == TRUE],
        na.rm = TRUE), 2)
    )

  return(qtab1)
}
