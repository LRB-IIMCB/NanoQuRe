#' Generate Report
#'
#' Generates an HTML QC report from one or more sequencing summary dataframes.
#' Automatically selects a single-sample or multi-sample template
#' based on the number of unique sample IDs present in the data.
#'
#' @param ... One or more dataframes containing sequencing summaries
#' @param output_file Name of the output HTML file, default is "NanoQuRe_Report.html"
#'
#' @returns Path to the rendered HTML report
#' @import rmarkdown
#' @export
#'
#' @examples
#' NULL
generate_report <- function(..., output_file = "NanoQuRe_Report.html") {
  
  seq_summary <- dplyr::bind_rows(...)
  
  assertthat::assert_that(nrow(seq_summary) > 0, msg = "The input data frame is empty")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sample_id"), msg = "The data frame is missing the 'sample_id' column")
  
  n_samples <- dplyr::n_distinct(seq_summary$sample_id)
  
  template <- if (n_samples == 1) {
    system.file("rmd", "report_single.Rmd", package = "NanoQuRe")
  } else {
    system.file("rmd", "report_multiple.Rmd", package = "NanoQuRe")
  }
  
  assertthat::assert_that(nchar(template) > 0, msg = "Report template not found. Make sure the package is installed correctly.")
  
  output <- rmarkdown::render(
    input = template,
    output_file = output_file,
    params = list(data = seq_summary),
    envir = new.env(parent = globalenv())
  )
  
  return(output)
}