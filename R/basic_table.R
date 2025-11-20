


#' Title
#'
#' @param data
#'
#' @returns
#' @export
#'
#' @examples
#'
basic_table <- function(data){

  b_table <- data %>%  data$run_id + select(max(data$start_time + data$duration)) + nrow(data)

  return(b_table)
}

basic_table(ligase_data)
