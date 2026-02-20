#' Shorter Seq sum
#'
#'Function takes user's sequencing summary and makes a shorter version of sequencing_summary.txt
#' @param my_data
#'
#' @returns Sequencing summary data frame
#' @import readr
#' @import dplyr
#' @export
#'
#' @examples
#' NULL
shorter_seqsum <- function(my_data){
  my_data <- readr::read_tsv("/mnt/nanopore/raw_data/libraries_tests/250320_ligaseEla_SuperScript/20250320_1344_MN39398_FBA13144_c7450e82/sequencing_summary_FBA13144_c7450e82_7c2ba15e.txt", header = TRUE)
  my_data <- my_data %>% dplyr::select(-filename_fastq,-filename_fast5, -filename_pod5, -parent_read_id, -pore_type, -read_id, -run_id, -end_reason)
  return(my_data)
  }




