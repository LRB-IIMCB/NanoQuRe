#' Sequencing Stats
#'
#' Generates a table containing general run metrics from a sequencing summary:
#' sample ID, duration in hours, number of reads, total bases sequenced in Gb,
#' percentage of passed reads, and average sequencing speed in nt/s. Missing values are removed from every sum; the speed is
#' \code{NA} when the total read duration is zero.
#'
#' @param seq_summary A data frame containing the sequencing summary
#'
#' @return A data frame with general run metrics
#' @export
#'
#' @examples
#' \dontrun{
#' sequencing_stats(sample_data)
#' }
sequencing_stats <- function(seq_summary) {

  #assertions
  if (missing(seq_summary)) {
    stop("Sequencing summary is missing. Please provide a valid seq_summary argument.",
         call. = FALSE)
  }

  if (!is.data.frame(seq_summary) || nrow(seq_summary) == 0) {
    stop("Empty data frame provided as an input (seq_summary). Please provide a valid input.",
         call. = FALSE)
  }

  # required columns
  required_cols <- c("sample_id", "run_id", "start_time", "duration", "sequence_length_template", "passes_filtering")
  missing_cols <- setdiff(required_cols, colnames(seq_summary))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }

  assert_condition(is.numeric(seq_summary$sequence_length_template),
                   "Column sequence_length_template must be numeric. Please provide a valid argument.")
  assert_condition(is.logical(seq_summary$passes_filtering),
                   "Column passes_filtering must be logical. Please provide a valid argument.")

  # Compute
  # na.rm = TRUE throughout (same policy as quality_stats); a zero total
  # duration gives NA instead of Inf/NaN
  total_duration <- sum(seq_summary$duration, na.rm = TRUE)

  tab1 <- seq_summary %>%
    dplyr::summarise(
      "sample id" = dplyr::first(sample_id),
      "duration [h]" = round(max(start_time + duration, na.rm = TRUE) / 3600, 2),
      "number of reads" = dplyr::n(),
      # 1e9 = 1 billion bases = 1 gigabase
      "total bases sequenced [Gb]" = round(sum(sequence_length_template / 1e9, na.rm = TRUE), 3),
      "passed reads [%]" = round(sum(passes_filtering, na.rm = TRUE) / dplyr::n() * 100, 2),
      "average speed [nt/s]" = if (total_duration > 0) {
        round(sum(sequence_length_template, na.rm = TRUE) / total_duration, 2)
      } else {
        NA_real_
      }
    )

  return(tab1)
}
