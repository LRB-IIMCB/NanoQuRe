#' Shorter Seq sum
#'
#'Function takes user's sequencing summary and makes a shorter version of sequencing_summary.txt
#' @param seq_summary path to a file
#'
#' @returns Sequencing summary data frame
#' @import readr
#' @import dplyr
#' @export
#'
#' @examples
#' NULL
shorter_seqsum <- function(seq_summary){

  assertthat::assert_that(nrow(seq_summary) > 0, msg = "The input data frame is empty.")

  seq_summary <- seq_summary %>% dplyr::select(-filename_fastq,-filename_fast5, -filename_pod5, -parent_read_id, -pore_type, -end_reason)
  return(seq_summary)
  }




