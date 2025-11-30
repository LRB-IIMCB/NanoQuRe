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
#chart_ela_draft <- ggplot(data = count_seq, aes(x=sequence_length_template, y=log(n)))
#chart_ela <- chart_ela_draft + geom_col(color = "royalblue") + geom_vline(xintercept=n50_value, color ="firebrick4", title="N50")

#print(chart_ela)


plotly_draft <- plot_ly( x=count_seq$sequence_length_template, y=count_seq$n ,type = "bar")
plotly_ela <- plotly_draft %>% layout(xaxis =list(title = "sequence length", range = c(0, 1000), ),  yaxis = list(title="n"))
print(plotly_ela)



