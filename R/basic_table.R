
#' Basic Table
#' From sequencing summary's database creates a joined table containing general metrics regarding each run - each sequencing summary.
#'
#' @param my_data
#'
#' @returns
#' @export
#'
#' @examples
#' /dontrun
#' tab1 <- lapply(seq_sum_database, basic_table)
basic_table <- function(my_data){
  tab1 <- my_data %>%
    dplyr::summarise(
      "sample id" = first(sample_id),
      "run id" = first(run_id),
      "duration [h]" = max(start_time + duration, na.rm = TRUE) / 3600,
      "number of reads" = n(),
      #1e9 1 billion = 1 gigabases
      "total bases sequenced [Gb]" = sum(sequence_length_template/1e9),
      "passed_reads [%]" = sum(passes_filtering)/n()*100

    )

}

tab1_database <- lapply(seqsum_database, basic_table)
joinedtab <- dplyr::bind_rows(tab1_database)
print(joinedtab)

