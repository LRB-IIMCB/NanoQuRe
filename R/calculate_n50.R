#' Calculate N50
#'
#' Calculates the N50 value: the read length at which 50% of all sequenced
#' bases are contained in reads of that length or longer.
#'
#' @param seq_summary A data frame containing the sequencing summary
#'
#' @return Numeric N50 value in bases
#' @export
#'
#' @examples
#' \dontrun{
#'  seq_summary <- data.frame(
#'   sequence_length_template = c(1, 2, 3, 4, 10)
#' )
#' calculate_n50(seq_summary)
#' }
calculate_n50 <- function(seq_summary) {

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
  required_cols <- c("sequence_length_template")
  missing_cols <- setdiff(required_cols, colnames(seq_summary))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }

  assert_condition(is.numeric(seq_summary$sequence_length_template),
                   "Column sequence_length_template must be numeric. Please provide a valid argument.")

  total_bases <- sum(seq_summary$sequence_length_template, na.rm = TRUE)
  half_data <- total_bases / 2
  # sort the vector directly instead of copying the whole data frame
  descend_seq <- sort(seq_summary$sequence_length_template,
                      decreasing = TRUE, na.last = NA)
  cumulative_sum <- cumsum(descend_seq)
  n50_index <- which(cumulative_sum >= half_data)[1]
  n50_value <- descend_seq[n50_index]

  return(n50_value)
}
