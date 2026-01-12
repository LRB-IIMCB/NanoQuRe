
#' n50
#'
#' @param my_data
#'
#' @returns
#' @export
#'
#' @examples
n50_calc <- function(my_data){
  total_bases <- sum(my_data$sequence_length_template)
  half_data <- total_bases/2
  descend_seq <- ligase_ela %>% select(sequence_length_template) %>% arrange(desc(sequence_length_template)) %>% pull(sequence_length_template)
  cumulative_sum <- cumsum(descend_seq)
  n50_index <- which(cumulative_sum >= half_data)[1]
  n50_value <- descend_seq[n50_index]

  n50_database <- lapply(seqsum_database, n50_calc)
}
