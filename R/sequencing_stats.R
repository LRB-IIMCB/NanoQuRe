#' Sequencing Stats
#'
#' Generates a table containing general run metrics from a sequencing summary:
#' sample ID, duration in hours, number of reads, total bases sequenced in Gb,
#' percentage of passed reads, and average sequencing speed in bp/s.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#'
#' @returns A dataframe with general run metrics
#' @import dplyr
#' @export
#'
#' @examples
#' sequencing_stats(sample_data)
sequencing_stats <- function(seq_summary) {
  
  # --- Validation ---
  if (nrow(seq_summary) == 0)
    stop("The input data frame is empty")
  if (!("sample_id" %in% names(seq_summary)))
    stop("The data frame is missing the 'sample_id' column")
  if (!("run_id" %in% names(seq_summary)))
    stop("The data frame is missing the 'run_id' column")
  if (!("start_time" %in% names(seq_summary)))
    stop("The data frame is missing the 'start_time' column")
  if (!("duration" %in% names(seq_summary)))
    stop("The data frame is missing the 'duration' column")
  if (!("sequence_length_template" %in% names(seq_summary)))
    stop("The data frame is missing the 'sequence_length_template' column")
  if (!("passes_filtering" %in% names(seq_summary)))
    stop("The data frame is missing the 'passes_filtering' column")
  if (!is.numeric(seq_summary$sequence_length_template))
    stop("Column 'sequence_length_template' must be numeric")
  
  # --- Compute ---
  tab1 <- seq_summary %>%
    dplyr::summarise(
      "sample id"                  = dplyr::first(sample_id),
      "duration [h]"               = round(max(start_time + duration, na.rm = TRUE) / 3600, 2),
      "number of reads"            = dplyr::n(),
      # 1e9 = 1 billion bases = 1 gigabase
      "total bases sequenced [Gb]" = round(sum(sequence_length_template / 1e9), 3),
      "passed reads [%]"           = round(sum(passes_filtering) / n() * 100, 2),
      "average speed [bp/s]"       = round(sum(sequence_length_template) / sum(duration), 2)
    )
  
  return(tab1)
}