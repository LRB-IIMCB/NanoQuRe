#' Shorter Seq sum
#'
#'Function takes user's sequencing summary and makes a shorter version of sequencing_summary.txt
#' @param my_data path to a file
#'
#' @returns Sequencing summary data frame
#' @import readr
#' @import dplyr
#' @export
#'
#' @examples
#' NULL
shorter_seqsum <- function(my_data){

  assertthat::assert_that(nrow(my_data) > 0, msg = "The input data frame is empty.")

  my_data <- my_data %>% dplyr::select(-filename_fastq,-filename_fast5, -filename_pod5, -parent_read_id, -pore_type, -end_reason)
  return(my_data)
  }




