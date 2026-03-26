#'Plot Read Lengths
#'
#'Generates a plot with number of bases sequenced of each length in bases sorted by pass/fail filtering status.Two vertical lines represent calculated mean read length of the reads in bases and N 50 value calculated with the function calculate_n50.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#' @param upper_limit limit of x axis, default option is 4000
#'
#' @returns ggplot2 object
#' @import ggplot2
#' @import dplyr
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' NULL
plot_read_lengths <- function(seq_summary, upper_limit = 4000) {
  
  # --- Validation ---
  assertthat::assert_that(assertthat::has_name(seq_summary, "sequence_length_template"),
                          msg = "The data frame is missing the 'sequence_length_template' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sample_id"),
                          msg = "The data frame is missing the 'sample_id' column")
  assertthat::assert_that(is.numeric(seq_summary$sequence_length_template),
                          msg = "Column 'sequence_length_template' must be numeric")
  
  upper_limit <- as.numeric(upper_limit)
  assertthat::assert_that(!is.na(upper_limit), msg = "upper_limit must be a number")
  
  # --- Data prep ---
  count_seq   <- seq_summary %>% dplyr::count(sequence_length_template)
  mean_length <- mean(seq_summary$sequence_length_template, na.rm = TRUE)
  n50_SR      <- calculate_n50(seq_summary)
  sample_name <- dplyr::first(seq_summary$sample_id)
  max_y       <- max(count_seq$n, na.rm = TRUE)
  
  # --- Plot ---
  length_plot <- plotly::plot_ly() %>%
    # Bars
    plotly::add_bars(
      data          = count_seq,
      x             = ~sequence_length_template,
      y             = ~n,
      name          = "Read counts",
      marker        = list(color = "#404040", opacity = 0.85,
                           line = list(color = "#404040", width = 0.5)),
      hovertemplate = "Length: %{x} nt<br>Count: %{y}<extra></extra>"
    ) %>%
    # Mean line — two points within y range, locked by layout range below
    plotly::add_lines(
      x             = c(mean_length, mean_length),
      y             = c(0, max_y),
      name          = paste0("Mean: ", round(mean_length, 0), " nt"),
      line          = list(color = "#CC79A7", width = 2.5),
      hovertemplate = paste0("Mean: ", round(mean_length, 0), " nt<extra></extra>")
    ) %>%
    # N50 line
    plotly::add_lines(
      x             = c(n50_SR, n50_SR),
      y             = c(0, max_y),
      name          = paste0("N50: ", round(n50_SR, 0), " nt"),
      line          = list(color = "#009E73", width = 2.5),
      hovertemplate = paste0("N50: ", round(n50_SR, 0), " nt<extra></extra>")
    ) %>%
    plotly::layout(
      title = list(
        text = paste0("<b>", sample_name, "</b>"),
        x    = 0.5,
        font = list(size = 15, color = "#333333", family = "Arial")
      ),
      xaxis = list(
        title     = list(text = "<b>Sequence length [nt]</b>",
                         font = list(size = 13, family = "Arial")),
        range     = c(0, upper_limit),
        showgrid  = TRUE,
        gridcolor = "#e0e0e0",
        tickfont  = list(size = 11, family = "Arial", color = "#333333")
      ),
      yaxis = list(
        title     = list(text = "<b>Number of reads</b>",
                         font = list(size = 13, family = "Arial")),
        range     = c(0, max_y * 1.1),  # fixed range — lines can't expand it
        showgrid  = TRUE,
        gridcolor = "#e0e0e0",
        tickfont  = list(size = 11, family = "Arial", color = "#333333")
      ),
      bargap        = 0,
      plot_bgcolor  = "#f9f9f9",
      paper_bgcolor = "#f9f9f9",
      legend = list(
        bgcolor     = "#ffffff",
        bordercolor = "#cccccc",
        borderwidth = 1,
        font        = list(size = 11, family = "Arial"),
        x           = 0.75,
        y           = 0.95
      )
    )
  
  return(length_plot)
}

