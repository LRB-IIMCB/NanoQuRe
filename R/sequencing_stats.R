#' Sequencing Stats
#'
#' From sequencing summary's database creates a joined table containing general metrics regarding each run - each sequencing summary. Information in the table contains: sample id, run id, duration in hours, number of reads, total bases sequenced in GB gigabases and passed_reads in percentage.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#'
#' @returns returns joined table with rudimentary data for all sequencing summaries in database
#' @import dplyr
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' NULL
sequencing_stats <- function(seq_summary){

  assertthat::assert_that(nrow(seq_summary) > 0, msg = "The input data frame is empty")



  assertthat::assert_that(assertthat::has_name(seq_summary, "sample_id"), msg = "The data frame is missing the 'sample_id' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "run_id"), msg = "The data frame is missing the 'run_id' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "start_time"), msg = "The data frame is missing the 'start_time' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "duration"), msg = "The data frame is missing the 'duration' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sequence_length_template"), msg = "The data frame is missing the 'sequence_length_template' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "passes_filtering"), msg = "The data frame is missing the 'passes_filtering' column")

  assertthat::assert_that(is.numeric(seq_summary$sequence_length_template), msg = "Column 'sequence_length_template' must be numeric")

  tab1 <- seq_summary %>%
    dplyr::summarise(
      "sample id" = dplyr::first(sample_id),
      "duration [h]" = round(max(start_time + duration, na.rm = TRUE) / 3600, 2),
      "number of reads" = dplyr::n(),
      #1e9 1 billion = 1 gigabases
      "total bases sequenced [Gb]" = round(sum(sequence_length_template/1e9), 3),
      "passed reads [%]" = round(sum(passes_filtering)/n()*100, 2),
      "average speed [bp/s]" = round(sum(sequence_length_template)/sum(duration), 2)
    )

  return(tab1)

}

