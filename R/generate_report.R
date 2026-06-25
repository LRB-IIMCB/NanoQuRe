#' Generate Report
#'
#' Generates an HTML QC report from one or more sequencing summary dataframes.
#' Automatically selects a single-sample or multi-sample template based on
#' the number of unique sample IDs present in the data.
#'
#' @param ... One or more dataframes containing sequencing summaries
#' @param output_file Name of the output HTML file,
#'   default is "NanoQuRe_Report.html"
#' @param output_dir Directory the rendered report is written to. Defaults
#'   to the current working directory.
#' @param platform Flowcell platform forwarded to
#'   \code{\link{pore_activity_heatmap}}: \code{"minion"} or
#'   \code{"promethion"}. Defaults to \code{"minion"}.
#'
#' @returns Path to the rendered HTML report
#' @export
#'
#' @examples
#' \dontrun{
#' generate_report(sample_data, output_file = "QC_Report.html")
#' }
generate_report <- function(...,
                            output_file = "NanoQuRe_Report.html",
                            output_dir = getwd(),
                            platform = "minion") {

  #Assertions
  if (!(platform %in% c("minion", "promethion")))
    stop("platform must be either 'minion' or 'promethion'")

  inputs <- list(...)

  # Accept either a file path (string) or an in-memory data frame
  parsed <- lapply(inputs, function(x) {
    if (is.character(x) && length(x) == 1) {
      if (!file.exists(x)) {
        stop("File not found: ", x)
      }
      readr::read_tsv(x, show_col_types = FALSE)
    } else if (is.data.frame(x)) {
      x
    } else {
      stop("Each input must be either a file path (string) or a data frame.")
    }
  })

  seq_summary <- dplyr::bind_rows(parsed)

  if (nrow(seq_summary) == 0) {
    stop("The input data frame is empty.")
  }

  if (!"sample_id" %in% names(seq_summary)) {
    stop("The data frame is missing the 'sample_id' column.")
  }

  n_samples <- dplyr::n_distinct(seq_summary$sample_id)

  template <- if (n_samples == 1) {
    system.file("rmd", "report_single.Rmd", package = "NanoQuRe")
  } else {
    system.file("rmd", "report_multiple.Rmd", package = "NanoQuRe")
  }

  if (nchar(template) == 0) {
    stop("Report template not found. Make sure the package is installed correctly.")
  }

  output <- rmarkdown::render(
    input = template,
    output_file = output_file,
    output_dir = output_dir,
    params = list(data = seq_summary, platform = platform),
    envir = new.env(parent = globalenv())
  )

  return(output)
}
