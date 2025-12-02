
seqsum_database = list(ligase_ela, ligase_induro)
plotlytab_database = list()



all_reads = nrow(ligase_ela)
passed_reads = sum(ligase_ela$passes_filtering)
failed_reads = all_reads - passed_reads
n50_value <- Biostrings::N50(ligase_ela$sequence_length_template)
qscore = mean(ligase_ela$mean_qscore_template)
longest_read = max(ligase_ela$sequence_length_template)

all_passed = ligase_ela %>% dplyr::filter(passes_filtering == TRUE)
passed_meanlength = mean(all_passed$sequence_length_template)






plotlytab_database = list()


quality_table <- function(my_data){
  for (n in 1:length(seqsum_database)){

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

    plotlytab_database <- c(plotlytab_database, list(qtab1))
    print(plotlytab_database)
  }
  qjoinedtab <- dplyr::bind_rows(plotlytab_database)
  print(qjoinedtab)

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


