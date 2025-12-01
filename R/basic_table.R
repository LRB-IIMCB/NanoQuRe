#sample data - ligase
ligase_ela <- read_tsv("/mnt/nanopore/raw_data/libraries_tests/250320_ligaseEla_SuperScript/20250320_1344_MN39398_FBA13144_c7450e82/sequencing_summary_FBA13144_c7450e82_7c2ba15e.txt")

ligase_induro <- read_tsv("/mnt/nanopore/raw_data/libraries_tests/250320_ligaseT4_Induro/20250320_1337_MN40838_FBA13273_4849cee3/sequencing_summary_FBA13273_4849cee3_5ab4411b.txt")


# creating list of sequencing summary files
seqsum_database = list(ligase_ela, ligase_induro)

#assertthat::`%has_name%`()

#' Basic Table
#' From a list of provided sequencing summaries it generates a database with basic table (contains data such as: run id, duration, number of reads) for each sequencing summary
#' @param my_data
#'
#' @returns
#' @export
#'
#' @examples
basic_table <- function(my_data){
  #checking data
  assertthat::not_empty(my_data$run_id)
  assertthat::not_empty(my_data$start_time)
  assertthat::not_empty(my_data$duration)

  # creating table with sequencing parameters
  tab1 <- my_data %>%
  summarise(
  "run id" = first(run_id),
  "duration" = max(start_time + duration, na.rm = TRUE) / 3600,
  "number of reads" = n()
  )
  tab2 <- plot_ly(
    type = 'table',
    header = list(
      values = tab1))

}


# applying basic_table function to every element in the seqencing summary database


plotlytab_database = list()

plotly_table <- function(my_data){
  for (n in 1:length(seqsum_database)){
    #print(n)
    #print(seqsum_database[n])
    #new_tab <- as.data.frame(seqsum_database[n])
    tab1 <- seqsum_database[[n]] %>%
      summarise(
        "run id" = first(run_id),
        "duration" = max(start_time + duration, na.rm = TRUE) / 3600,
        "number of reads" = n())
    #plotlytab_database <- append(tab1, plotlytab_database)
    plotlytab_database <- c(plotlytab_database, list(tab1))
    print(plotlytab_database)
  }
  joinedtab <- dplyr::bind_rows(plotlytab_database)

  print(joinedtab)


  joinedtab <-  dplyr::bind_rows(plotlytab_database)
  print(joinedtab)

  tab2 <- plot_ly(
    type = 'table',
    columnwidth = c(200, 50),
    columnorder = c(0, 1),
    header = list(
      values = c("run id", "duration", "number of reads"),
      line = list(width = 1, color = 'black'),
      fill = list(color = c("royalblue", "royalblue")),
      font = list(family = "Arial", size = 14, color = "white")),
    cells = list(
      values = t(joinedtab),
      line = list(width = 1, color = 'black'))
  )
}


plotly_database_lapply <- lapply(seqsum_database, plotly_table)

print(plotly_database_lapply)


for (n in 1:length(seqsum_database)){
  #print(n)
  #print(seqsum_database[n])
  #new_tab <- as.data.frame(seqsum_database[n])
  tab1 <- seqsum_database[[n]] %>%
    summarise(
      "run id" = first(run_id),
      "duration" = max(start_time + duration, na.rm = TRUE) / 3600,
      "number of reads" = n())
  #plotlytab_database <- append(tab1, plotlytab_database)
  plotlytab_database <- c(plotlytab_database, list(tab1))
  print(plotlytab_database)
}
joinedtab <- dplyr::bind_rows(plotlytab_database)

print(joinedtab)


