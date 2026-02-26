#'Plot Read Lengths
#'
#'Generates a plot with number of bases sequenced of each length in bases sorted by pass/fail filtering status.Two vertical lines represent calculated mean read length of the reads in bases and N 50 value calculated with the function calculate_n50.
#'
#' @param my_data A dataframe containing the sequencing summary
#'
#' @returns ggplot2 object
#' @import ggplot2
#' @import dplyr
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' NULL
plot_read_lengths <- function(my_data){



  assertthat::assert_that(assertthat::has_name(my_data, "sequence_length_template"),

                          msg = "The data frame is missing the 'sequence_length_template' column")



  assertthat::assert_that(assertthat::has_name(my_data, "sample_id"),

                          msg = "The data frame is missing the 'sample_id' column")



  count_seq <- my_data %>% dplyr::count(sequence_length_template)
  max_y_mean <- max(count_seq$n, na.rm = TRUE)
  mean_length = mean(my_data$sequence_length_template)
  n50_SR <- calculate_n50(my_data)
  max_y_n50 <- max(count_seq$n, na.rm = TRUE)
  sample_name <-  dplyr::first(my_data$sample_id)



  data <- ggplot2::ggplot(data = count_seq) + ggplot2::aes(x = sequence_length_template, y = n)

  bar_color <- ggplot2::geom_col(color = "skyblue")

  mean_length_line <- ggplot2::geom_vline(ggplot2::aes(xintercept = mean_length), color = "purple")

  n50_line <- ggplot2::geom_vline(ggplot2::aes(xintercept = n50_SR), color = "darkgreen")

  #y_axis <- max_y_mean*1.2
  axis_limit <- ggplot2::coord_cartesian(xlim = c(0, 4000), ylim = c(0, max_y_mean*1.1))

  plot_lable <-  ggplot2::labs(title =paste0("Length plot  ", sample_name), x = "Sequence length [nt]", y = "Number of bases sequenced")



  mean_label <- ggplot2::annotate(geom = "text", x = mean_length* 2.5, y = max_y_mean * 0.65, label = paste("Mean Length"), color = "purple", fontface = "bold", size = 4)

  n50_label <- ggplot2::annotate(geom = "text", x = n50_SR* 2.5, y = max_y_n50 * 0.65, label = paste("N50"), color = "darkgreen", fontface = "bold", size = 4)



  length_plot <- data + bar_color + mean_length_line + n50_line + axis_limit + plot_lable + mean_label + n50_label

  return(length_plot)

}


#plot1 <- plot_read_lengths(ligase_ela)
#plot1

#plot2 <- plot_read_lengths(sample_data)
#plot2
