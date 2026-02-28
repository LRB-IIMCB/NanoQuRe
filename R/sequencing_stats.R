
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

  assertthat::assert_that(nrow(my_data) > 0, msg = "The input data frame is empty")

  assertthat::assert_that(is.numeric(my_data$sequence_length_template), msg = "Column 'sequence_length_template' must be numeric")

  assertthat::assert_that(assertthat::has_name(my_data, "sample_id"), msg = "The data frame is missing the 'sample_id' column")
  assertthat::assert_that(assertthat::has_name(my_data, "run_id"), msg = "The data frame is missing the 'run_id' column")
  assertthat::assert_that(assertthat::has_name(my_data, "start_time"), msg = "The data frame is missing the 'start_time' column")
  assertthat::assert_that(assertthat::has_name(my_data, "duration"), msg = "The data frame is missing the 'duration' column")
  assertthat::assert_that(assertthat::has_name(my_data, "sequence_length_template"), msg = "The data frame is missing the 'sequence_length_template' column")
  assertthat::assert_that(assertthat::has_name(my_data, "passes_filtering"), msg = "The data frame is missing the 'passes_filtering' column")


  tab1 <- my_data %>%
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

#tab1 <- sequencing_stats(sample_data)
#tab1
