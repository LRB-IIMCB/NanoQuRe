#install.packages("BiocManager")
#BiocManager::install("ShortRead")
#library(ShortRead)


mean_length_ela = mean(ligase_ela$sequence_length_template)

n50_value <- Biostrings::N50(ligase_ela$sequence_length_template)



count_seq <- length_ascd %>% dplyr::count(sequence_length_template)


chart_ela <- ggplot(data = count_seq, aes(x=sequence_length_template, y=n))








