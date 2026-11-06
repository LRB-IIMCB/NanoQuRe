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
      # ids are forced to character: readr guesses per file, so a numeric-looking
      # sample_id in one file made bind_rows() fail on multi-file input
      readr::read_tsv(x, show_col_types = FALSE,
                      col_types = readr::cols(sample_id = "c", run_id = "c",
                                              .default = readr::col_guess()))
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

  # NA ids were counted as an extra sample by n_distinct() and then silently
  # dropped by split() in the multi-sample template
  if (any(is.na(seq_summary$sample_id))) {
    stop("Column 'sample_id' contains missing values.")
  }

  # NA pass/fail flags propagate as NA into every summary table
  if ("passes_filtering" %in% names(seq_summary) &&
      any(is.na(seq_summary$passes_filtering))) {
    stop("Column 'passes_filtering' contains missing values.")
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

  # knit intermediates go to a private temp dir: by default rmarkdown writes
  # them next to the installed template, which fails in read-only (system)
  # libraries and collides when two reports render at the same time
  work_dir <- tempfile("nanoqure_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  output <- rmarkdown::render(
    input = template,
    output_file = output_file,
    output_dir = output_dir,
    intermediates_dir = work_dir,
    knit_root_dir = work_dir,
    params = list(data = seq_summary, platform = platform),
    envir = new.env(parent = globalenv())
  )

  return(output)
}
