#' Calculate N50
#'
#' Calculates the N50 value: the read length at which 50% of all sequenced
#' bases are contained in reads of that length or longer.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#'
#' @returns Numeric N50 value in bases
#' @import dplyr
#' @export
#'
#' @examples
#' seq_summary <- data.frame(
#'   sequence_length_template = c(1, 2, 3, 4, 10)
#' )
#' calculate_n50(seq_summary)
calculate_n50 <- function(seq_summary) {
  
  if (!("sequence_length_template" %in% names(seq_summary)))
    stop("The data frame is missing the 'sequence_length_template' column")
  if (nrow(seq_summary) == 0)
    stop("The input data frame is empty")
  if (!is.numeric(seq_summary$sequence_length_template))
    stop("Column 'sequence_length_template' must be numeric")
  
  total_bases <- sum(seq_summary$sequence_length_template, na.rm = TRUE)
  half_data   <- total_bases / 2
  descend_seq <- seq_summary %>%
    dplyr::select(sequence_length_template) %>%
    dplyr::arrange(dplyr::desc(sequence_length_template)) %>%
    dplyr::pull(sequence_length_template)
  cumulative_sum <- cumsum(descend_seq)
  n50_index      <- which(cumulative_sum >= half_data)[1]
  n50_value      <- descend_seq[n50_index]
  
  return(n50_value)
}