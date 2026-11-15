#' Quality Stats
#'
#' Generates a table containing the most important quality metrics from a
#' sequencing summary: number of all reads, number of passed reads, N50 value,
#' mean Q-score, longest read length, and mean length of passed reads. Missing
#' values in read lengths and Q-scores are removed from every summary; the
#' mean length of passed reads is \code{NA} when no read passes the filter.
#'
#' @param seq_summary A data frame containing the sequencing summary
#'
#' @return A data frame with quality metrics for the sequencing run
#' @export
#'
#' @examples
#' \dontrun{
#' quality_stats(sample_data)
#' }
quality_stats <- function(seq_summary) {

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
  required_cols <- c("sample_id", "run_id", "passes_filtering", "mean_qscore_template", "sequence_length_template")
  missing_cols <- setdiff(required_cols, colnames(seq_summary))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }

  assert_condition(is.numeric(seq_summary$mean_qscore_template),
                   "Column mean_qscore_template must be numeric. Please provide a valid argument.")
  assert_condition(is.numeric(seq_summary$sequence_length_template),
                   "Column sequence_length_template must be numeric. Please provide a valid argument.")
  assert_condition(is.logical(seq_summary$passes_filtering),
                   "Column passes_filtering must be logical. Please provide a valid argument.")

  # Compute
  n50_value <- calculate_n50(seq_summary)

  # na.rm = TRUE throughout, so that one NA read does not turn every metric
  # into NA while the N50 is computed without it
  pass_lengths <- seq_summary$sequence_length_template[
    seq_summary$passes_filtering %in% TRUE]

  qtab1 <- seq_summary %>%
    dplyr::summarise(
      "sample id" = dplyr::first(sample_id),
      "all reads" = dplyr::n(),
      "passed reads" = sum(passes_filtering, na.rm = TRUE),
      "n50 value" = round(n50_value, 2),
      "mean Q-score" = round(mean(mean_qscore_template, na.rm = TRUE), 2),
      "longest read" = max(sequence_length_template, na.rm = TRUE),
      # mean() of an empty vector is NaN, NA is reported instead
      "passed mean length" = if (sum(!is.na(pass_lengths)) == 0) {
        NA_real_
      } else {
        round(mean(pass_lengths, na.rm = TRUE), 2)
      }
    )

  return(qtab1)
}
