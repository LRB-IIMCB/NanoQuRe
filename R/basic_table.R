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

# creating a list of tab1 that would be then binded and form into plotly table
plotlytab_database = list()

plotly_table <- function(my_data){
  # iteration over every element in seqsum database
  for (n in 1:length(seqsum_database)){
    # checking for required input
    assertthat::not_empty(my_data$run_id)
    assertthat::not_empty(my_data$start_time)
    assertthat::not_empty(my_data$duration)
    assertthat::not_empty(my_data$sample_id)
    # n as integrer, seqsum database indexing
    tab1 <- seqsum_database[[n]] %>%
      summarise(
        "run id" = first(run_id),
        "duration" = max(start_time + duration, na.rm = TRUE) / 3600,
        "number of reads" = n(),
        "sample id" = first(sample_id))
    # adding tab1 to plotly database
    plotlytab_database <- c(plotlytab_database, list(tab1))
    print(plotlytab_database)
  }
  #binding all tab1 into one joined table that would be coverted into plotly table
  joinedtab <- dplyr::bind_rows(plotlytab_database)
  #print(joinedtab)

  # creating a plotly table
  tab2 <- plot_ly(
    type = 'table',
    columnwidth = c(200, 50),
    columnorder = c(0, 1),
    header = list(
      values = c("run id", "duration", "number of reads", "sample id"),
      line = list(width = 1, color = 'black'),
      fill = list(color = c("royalblue", "royalblue")),
      font = list(family = "Arial", size = 14, color = "white")),
    cells = list(
      values = t(joinedtab),
      line = list(width = 1, color = 'black'))
  )
}

# applying plotly table to all all in database
plotly_database_lapply <- plotly_table(seqsum_database)

print(plotly_database_lapply)



