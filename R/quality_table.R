
seqsum_database = list(ligase_ela, ligase_induro)
plotlytab_database = list()



plotlytab_database = list()


#' Quality table
#'collects and summarizes most important quality metrics from sequencing summary files including: sample id, all reads, passed reads, n50 value, mean qscore, longest read, passed mean length
#' @param my_data
#'
#' @returns
#' @export
#'
#' @examples
quality_table <- function(my_data){
  for (n in 1:length(seqsum_database)){

    # creating a tab1 containg data from all sequencing summaries
    qtab1 <- seqsum_database[[n]] %>%
      summarise(
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
# adding all qtab1 to a plotlytab databse
    plotlytab_database <- c(plotlytab_database, list(qtab1))
    print(plotlytab_database)
  }
  # binding all qtab1 and converting it to one plotly table
  qjoinedtab <- dplyr::bind_rows(plotlytab_database)
  print(qjoinedtab)

  # converting qjoinedtable into plotly table
  qtab2 <- plot_ly(
    type = 'table',
    columnwidth = c(200, 50),
    columnorder = c(0, 1),
    header = list(
      values = c("sample id", "all reads", "passed reads", "n50 value", "mean qscore", "longest read", "passed meanlength"),
      #"failed reads"
      line = list(width = 1, color = 'black'),
      fill = list(color = c("mediumpurple", "mediumpurple")),
      font = list(family = "Arial", size = 14, color = "white")),
    cells = list(
      values = t(qjoinedtab),
      line = list(width = 1, color = 'black'))
  )
}


qplotly_database <- quality_table(seqsum_database)

print(qplotly_database)


ela_passed_meanlength = mean(
  ligase_ela$sequence_length_template[ligase_ela$passes_filtering == TRUE],
  na.rm = TRUE)





