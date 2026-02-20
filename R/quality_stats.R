#' Quality Stats
#'Generates a joined table from sequencing summaries that contains most import quality metrics - number of all reads, number of passed reads, n50 value, mean q-score, longest read length, mean length of passed reads.
#'
#' @param my_data A dataframe containing the sequencing summary
#'
#' @return joined table with quality data for all sequencing summaries in database
#' @import dplyr
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' NULL
quality_stats <- function(my_data){

  assertthat::assert_that(my_data %has_name% "sample_id", msg = "The data frame is missing the 'sample_id' column")
  assertthat::assert_that(my_data %has_name% "run_id", msg = "The data frame is missing the 'run_id' column")
  assertthat::assert_that(my_data %has_name% "passes_filtering", msg = "The data frame is missing the 'passes_filtering' column")
  assertthat::assert_that(my_data %has_name% "mean_qscore_template", msg = "The data frame is missing the 'mean_qscore_template' column")
  assertthat::assert_that(my_data %has_name% "sequence_length_template", msg = "The data frame is missing the 'sequence_length_template' column")

  n50_value <- calculate_n50(my_data)

  qtab1 <- my_data %>%
    dplyr::summarise(
      "sample id" = dplyr::first(sample_id),
      "all reads" = nrow(my_data),
      "passed reads" = sum(passes_filtering),
      #"failed reads" = "all reads" - "passed reads",
      "n50 value" = n50_value,
      "mean qscore" = mean(mean_qscore_template),
      "longest read" = max(sequence_length_template),
      "passed meanlength" = mean(
        sequence_length_template[passes_filtering == TRUE],
        na.rm = TRUE)
    )

  return(qtab1)
}
