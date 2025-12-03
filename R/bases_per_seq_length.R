#install.packages("BiocManager")
#BiocManager::install("ShortRead")
#library(ShortRead)

# calculating mean length of reads
mean_length_ela = mean(ligase_ela$sequence_length_template)

# calculating n50 - 50% of the total length of the genome assembly
n50_ela <- Biostrings::N50(ligase_ela$sequence_length_template)


# counting occurrence of every read's length
count_seq <- ligase_ela %>% dplyr::count(sequence_length_template)

# adding line
xline_plotly <- function(plot, x_val) {
  plot %>%
    layout(
      shapes = list(
        list(type = "line", x0 = x_val, x1 = x_val, y0 = 0, y1 = 1, yref = "paper",
             line = list(color = "firebrick4", line_dash = "solid", width = 0.5))
      )
    )
}


plotly_draft <- plot_ly(x=count_seq$sequence_length_template, y=count_seq$n ,type = "bar", color = "skyblue3")
plotly_ela <- plotly_draft %>% layout(title = "Bases per sequence length", xaxis =list(title = "read length [nt]", range = c(0, 1000)),  yaxis = list(title="number of bases sequenced"), range = c(0, 10000))

plotly_ela <- plotly_ela %>% xline_plotly(x_val = mean_length_ela)

print(plotly_ela)



