
#' Sequencing Stats
#'
#' From sequencing summary's database creates a joined table containing general metrics regarding each run - each sequencing summary. Information in the table contains: sample id, run id, duration in hours, number of reads, total bases sequenced in GB gigabases and passed_reads in percentage.
#'
#' @param my_data A dataframe containing the sequencing summary
#'
#' @returns returns joined table with rudimentary data for all sequencing summaries in database
#' @import dplyr
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' NULL
sequencing_stats <- function(my_data){
  assertthat::assert_that(my_data %has_name% "sample_id", msg = "The data frame is missing the 'sample_id' column")
  assertthat::assert_that(my_data %has_name% "run_id", msg = "The data frame is missing the 'run_id' column")
  assertthat::assert_that(my_data %has_name% "start_time", msg = "The data frame is missing the 'start_time' column")
  assertthat::assert_that(my_data %has_name% "duration", msg = "The data frame is missing the 'duration' column")
  assertthat::assert_that(my_data %has_name% "sequence_length_template", msg = "The data frame is missing the 'sequence_length_template' column")
  assertthat::assert_that(my_data %has_name% "passes_filtering", msg = "The data frame is missing the 'passes_filtering' column")


  tab1 <- my_data %>%
    dplyr::summarise(
      "sample id" = first(sample_id),
      "duration [h]" = round(max(start_time + duration, na.rm = TRUE) / 3600, 2),
      "number of reads" = n(),
      #1e9 1 billion = 1 gigabases
      "total bases sequenced [Gb]" = round(sum(sequence_length_template/1e9), 3),
      "passed reads [%]" = round(sum(passes_filtering)/n()*100, 2),
      "average speed [bp/s]" = round(sum(sequence_length_template)/sum(duration), 2)
    )

  return(tab1)

}


