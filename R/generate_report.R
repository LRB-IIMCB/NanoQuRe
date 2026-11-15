#' Generate Report
#'
#' Generates an HTML QC report from one or more sequencing summaries.
#' Automatically selects a single-sample or multi-sample template based on
#' the number of unique sample IDs present in the data.
#'
#' @details Every input must contain the columns \code{run_id},
#'   \code{channel}, \code{start_time}, \code{duration},
#'   \code{sequence_length_template}, \code{mean_qscore_template}, and
#'   \code{passes_filtering}. Only these columns and \code{sample_id} are
#'   read from a file. Raw Guppy or Dorado sequencing summaries do not
#'   contain \code{sample_id}: for a file without this column the file name
#'   (without directory and extension) is used as the sample identifier. A
#'   data frame input must contain \code{sample_id}, as there is no file name
#'   to fall back on. The \code{passes_filtering} column may be
#'   logical, TRUE/FALSE text or 1/0. A sample spanning several
#'   \code{run_id} values triggers a warning, because reads from different
#'   runs share one time axis in the plots.
#'
#' @param ... One or more data frames containing sequencing summaries, or
#'   paths to tab-separated sequencing summary files
#' @param output_file Name of the output HTML file (a file name without a
#'   directory component), default is "NanoQuRe_Report.html"
#' @param output_dir Directory the rendered report is written to. Defaults
#'   to the current working directory.
#' @param platform Flow cell platform forwarded to
#'   \code{\link{pore_activity_heatmap}}: \code{"minion"} or
#'   \code{"promethion"}. Defaults to \code{"minion"}.
#'
#' @return Path to the rendered HTML report
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

  #assertions
  if (...length() == 0) {
    stop("Sequencing summary is missing. Please provide at least one data frame or file path.",
         call. = FALSE)
  }

  assert_condition(is_string(platform),
                   "Platform must be a single character string. Please provide a valid argument.")
  assert_condition(is_string(output_file),
                   "Output_file must be a single character string. Please provide a valid argument.")
  assert_condition(is_string(output_dir),
                   "Output_dir must be a single character string. Please provide a valid argument.")

  if (!(platform %in% c("minion", "promethion"))) {
    stop("Platform must be either 'minion' or 'promethion'. Please provide a valid argument.",
         call. = FALSE)
  }

  # a directory component would be silently dropped by rmarkdown::render()
  if (basename(output_file) != output_file) {
    stop("Output_file must be a file name without a directory; use output_dir instead.",
         call. = FALSE)
  }

  inputs <- list(...)

  # columns used by the report; nothing else is read from a file
  report_cols <- c("sample_id", "run_id", "channel", "start_time", "duration",
                   "sequence_length_template", "mean_qscore_template",
                   "passes_filtering")

  # accept either a file path (string) or an in-memory data frame
  parsed <- lapply(inputs, function(x) {
    if (is_string(x)) {
      if (!file.exists(x)) {
        stop("File not found: ", x, call. = FALSE)
      }
      # the header is read first, so that only the report columns are loaded
      header <- colnames(vroom::vroom(x, delim = "\t", n_max = 0,
                                      show_col_types = FALSE))
      keep_cols <- intersect(report_cols, header)
      # ids are forced to character: types are guessed per file, so a
      # numeric-looking sample_id in one file made bind_rows() fail on
      # multi-file input
      id_cols <- intersect(c("sample_id", "run_id"), keep_cols)
      id_types <- stats::setNames(rep(list("c"), length(id_cols)), id_cols)
      tab <- vroom::vroom(x, delim = "\t",
                          col_select = dplyr::all_of(keep_cols),
                          col_types = id_types,
                          show_col_types = FALSE)
      tab <- data.frame(tab)
      # raw guppy/dorado summaries carry no sample_id; the file name (without
      # directory, .gz and extension) is used instead
      if (!("sample_id" %in% keep_cols)) {
        tab$sample_id <- rep(sub("\\.[^.]*$", "", sub("\\.gz$", "", basename(x))),
                             nrow(tab))
      }
      tab
    } else if (is.data.frame(x)) {
      x
    } else {
      stop("Each input must be either a file path (string) or a data frame.",
           call. = FALSE)
    }
  })

  seq_summary <- dplyr::bind_rows(parsed)
  rm(parsed)
  gc()

  if (nrow(seq_summary) == 0) {
    stop("Empty data frame provided as an input. Please provide a valid input.",
         call. = FALSE)
  }

  # required columns
  required_cols <- c("sample_id")
  missing_cols <- setdiff(required_cols, colnames(seq_summary))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }

  # NA ids were counted as an extra sample by n_distinct() and then silently
  # dropped by split() in the multi-sample template
  assert_condition(!anyNA(seq_summary$sample_id),
                   "Column sample_id contains missing values. Please provide a valid input.")

  # NA pass/fail flags propagate as NA into every summary table
  if ("passes_filtering" %in% names(seq_summary)) {
    assert_condition(!anyNA(seq_summary$passes_filtering),
                     "Column passes_filtering contains missing values. Please provide a valid input.")
  }

  # pass/fail flags stored as text or 0/1 are converted once here, so that
  # every plot and table receives a logical column
  if ("passes_filtering" %in% names(seq_summary) &&
      !is.logical(seq_summary$passes_filtering)) {
    flag <- toupper(as.character(seq_summary$passes_filtering))
    if (!all(flag %in% c("TRUE", "FALSE", "1", "0"))) {
      stop("Column passes_filtering must contain TRUE/FALSE or 1/0 values. Please provide a valid input.",
           call. = FALSE)
    }
    seq_summary$passes_filtering <- flag %in% c("TRUE", "1")
  }

  # reads of one sample from several runs are plotted on one time axis
  if ("run_id" %in% names(seq_summary)) {
    runs_per_sample <- tapply(as.character(seq_summary$run_id),
                              as.character(seq_summary$sample_id),
                              function(x) length(unique(x)))
    mixed <- names(runs_per_sample)[runs_per_sample > 1]
    if (length(mixed) > 0) {
      warning("Sample(s) ", paste(mixed, collapse = ", "),
              " contain more than one run_id: reads from different runs",
              " share one time axis in the report.", call. = FALSE)
    }
  }

  n_samples <- dplyr::n_distinct(seq_summary$sample_id)

  template <- if (n_samples == 1) {
    system.file("rmd", "report_single.Rmd", package = "NanoQuRe")
  } else {
    system.file("rmd", "report_multiple.Rmd", package = "NanoQuRe")
  }

  if (nchar(template) == 0) {
    stop("Report template not found. Make sure the package is installed correctly.",
         call. = FALSE)
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
