
#' Calculate N50
#' Calculates N50 value is the length which 50% of all reads are the same length or longer than its value
#'
#' @param my_data A dataframe containing the sequencing summary
#'
#' @returns n50 value
#' @import dplyr
#' @import ggplot2
#' @importFrom assertthat assert_that %has_name%
#' @export
#'
#' @examples
#'
calculate_n50 <- function(my_data){

  assertthat::assert_that(my_data %has_name% "sequence_length_template", msg = "The data frame is missing the 'sequence_length_template' column")

  total_bases <- sum(my_data$sequence_length_template)
  half_data <- total_bases/2
  descend_seq <- my_data %>%
    dplyr::select(sequence_length_template) %>%
    dplyr::arrange(dplyr::desc(sequence_length_template)) %>%
    dplyr::pull(sequence_length_template)
  cumulative_sum <- cumsum(descend_seq)
  n50_index <- which(cumulative_sum >= half_data)[1]
  n50_value <- descend_seq[n50_index]

  return(n_50_value)
}
