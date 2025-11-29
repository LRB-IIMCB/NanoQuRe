#install.packages("BiocManager")
#BiocManager::install("ShortRead")
#library(ShortRead)

# calculating mean length of reads
mean_length_ela = mean(ligase_ela$sequence_length_template)

# calculating n50 - 50% of the total length of the genome assembly
n50_value <- Biostrings::N50(ligase_ela$sequence_length_template)


# counting occurrence of every read's length
count_seq <- length_ascd %>% dplyr::count(sequence_length_template)

# creating a plot comparing reads length and their occurrence
chart_ela <- ggplot(data = count_seq, aes(x=sequence_length_template, y=n))








