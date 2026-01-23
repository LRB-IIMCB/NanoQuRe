#' Quality Stats
#'Generates a joined table from sequencing summaries that contains most import quality metrics - number of all reads, number of passed reads, n50 values, mean q-score, longest read length, mean length of passed reads.
#'
#' @param my_data
#'
#' @return joined table with quality data for all sequencing summaries in database
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun {
#'  tab2 <- lapply(seq_sum_database, quality_table)
#' }
#'
quality_stats <- function(my_data){
  qtab1 <- my_data %>%
    dplyr::summarise(
      "sample id" = dplyr::first(sample_id),
      "all reads" = nrow(my_data),
      "passed reads" = sum(passes_filtering),
      #"failed reads" = "all reads" - "passed reads",
      "n50 value" = Biostrings::N50(sequence_length_template),
      "mean qscore" = mean(mean_qscore_template),
      "longest read" = max(sequence_length_template),
      "passed meanlength" = mean(
        sequence_length_template[passes_filtering == TRUE],
        na.rm = TRUE)
    )

  qtab1_database <- lapply(seqsum_database, quality_stats)
  joinedqtab <- dplyr::bind_rows(qtab1_database)
  print(joinedqtab)

}
