
#' Sequencing Stats
#'
#' From sequencing summary's database creates a joined table containing general metrics regarding each run - each sequencing summary.
#'
#' @param my_data
#'
#' @returns returns joined table with rudimentary data for all sequencing summaries in database
#' @import
#' @export
#'
#' @examples
#' \dontrun {
#'  tab1 <- lapply(seq_sum_database, basic_table)
#' }
#'
#'
seq_stats <- function(my_data){
  ssertthat::assert_that(my_data %has_name% "sample_id", msg = "The data frame is missing the 'sample_id' column")
  ssertthat::assert_that(my_data %has_name% "run_id", msg = "The data frame is missing the 'run_id' column")
  ssertthat::assert_that(my_data %has_name% "start_time", msg = "The data frame is missing the 'start_time' column")
  ssertthat::assert_that(my_data %has_name% "duration", msg = "The data frame is missing the 'duration' column")
  ssertthat::assert_that(my_data %has_name% "sequence_length_template", msg = "The data frame is missing the 'sequence_length_template' column")
  ssertthat::assert_that(my_data %has_name% "passes_filtering", msg = "The data frame is missing the 'passes_filtering' column")


  tab1 <- my_data %>%
    dplyr::summarise(
      "sample id" = dplyr::first(sample_id),
      "run id" = dplyr::first(run_id),
      "duration [h]" = max(start_time + duration, na.rm = TRUE) / 3600,
      "number of reads" = dplyr::n(),
      #1e9 1 billion = 1 gigabases
      "total bases sequenced [Gb]" = sum(sequence_length_template/1e9),
      "passed_reads [%]" = sum(passes_filtering)/dplyr::n()*100

    )
  tab1_database <- lapply(seqsum_database, seq_stats)
  joinedtab <- dplyr::bind_rows(tab1_database)
  print(joinedtab)

}


