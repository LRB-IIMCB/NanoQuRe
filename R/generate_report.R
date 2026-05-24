#' Generate Report
#'
#' Generates an HTML QC report from one or more sequencing summary dataframes.
#' Automatically selects a single-sample or multi-sample template based on
#' the number of unique sample IDs present in the data.
#'
#' @param ... One or more dataframes containing sequencing summaries
#' @param output_file Name of the output HTML file,
#'   default is "NanoQuRe_Report.html"
#'
#' @returns Path to the rendered HTML report
#' @import rmarkdown
#' @export
#'
#' @examples
#' \dontrun{
#' generate_report(sample_data, output_file = "QC_Report.html")
#' }
generate_report <- function(..., output_file = "NanoQuRe_Report.html") {
  
  seq_summary <- dplyr::bind_rows(...)
  
  if (nrow(seq_summary) == 0)
    stop("The input data frame is empty")
  if (!("sample_id" %in% names(seq_summary)))
    stop("The data frame is missing the 'sample_id' column")
  
  n_samples <- dplyr::n_distinct(seq_summary$sample_id)
  
  template <- if (n_samples == 1) {
    system.file("rmd", "report_single.Rmd", package = "NanoQuRe")
  } else {
    system.file("rmd", "report_multiple.Rmd", package = "NanoQuRe")
  }
  
  if (nchar(template) == 0)
    stop("Report template not found. Make sure the package is installed correctly.")
  
  output <- rmarkdown::render(
    input       = template,
    output_file = output_file,
    params      = list(data = seq_summary),
    envir       = new.env(parent = globalenv())
  )
  
  return(output)
}