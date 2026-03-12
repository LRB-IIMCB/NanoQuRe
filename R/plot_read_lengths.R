#'Plot Read Lengths
#'
#'Generates a plot with number of bases sequenced of each length in bases sorted by pass/fail filtering status.Two vertical lines represent calculated mean read length of the reads in bases and N 50 value calculated with the function calculate_n50.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#'
#' @returns ggplot2 object
#' @import ggplot2
#' @import dplyr
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' NULL
plot_read_lengths <- function(seq_summary){


  assertthat::assert_that(assertthat::has_name(seq_summary, "sequence_length_template"), msg = "The data frame is missing the 'sequence_length_template' column")
assertthat::assert_that(assertthat::has_name(seq_summary, "sample_id"), msg = "The data frame is missing the 'sample_id' column")
assertthat::assert_that(is.numeric(seq_summary$sequence_length_template), msg = "Column 'sequence_length_template' must be numeric")


  count_seq <- seq_summary %>% dplyr::count(sequence_length_template)
  max_y_mean <- max(count_seq$n, na.rm = TRUE)
  mean_length = mean(seq_summary$sequence_length_template)
  n50_SR <- calculate_n50(seq_summary)
  max_y_n50 <- max(count_seq$n, na.rm = TRUE)
  sample_name <-  dplyr::first(seq_summary$sample_id)



  plot_data <- ggplot2::ggplot(data = count_seq) + ggplot2::aes(x = sequence_length_template, y = n)
bar_color <- ggplot2::geom_col(color = "#56B4E9")
  
  
mean_length_line <- ggplot2::geom_vline(ggplot2::aes(xintercept = mean_length, color = "Mean Length"), linewidth = 1)
  
n50_line <- ggplot2::geom_vline(ggplot2::aes(xintercept = n50_SR, color = "N50"), linewidth = 1)
  
legend_colors <- ggplot2::scale_color_manual(name = NULL, values = c("Mean Length" = "#CC79A7", "N50" = "#009E73"))
  
 

  axis_limit <- ggplot2::coord_cartesian(xlim = c(0, 2000), ylim = c(0, max_y_mean*1.1))
  plot_label <-  ggplot2::labs(title =paste0("Length plot  ", sample_name), x = "Sequence length [nt]", y = "Number of bases sequenced")


length_plot <- plot_data + mean_length_line + bar_color + n50_line + legend_colors + axis_limit + plot_label + nanoqure_theme()


  return(length_plot)

}

