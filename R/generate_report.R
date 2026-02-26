#' Generate Nanopore QC Report
#'
#' @param my_data seq summary file
#' @param output_file generated file html
#' @export
generate_report <- function(my_data, output_file = "QC_Report.html") {

  template <- system.file("rmd", "qc_report_template.Rmd", package = "NanoQuRe")

  rmarkdown::render(input = template, output_file = output_file, params = list(data = my_data))

  message("Report created: ", output_file)
}

