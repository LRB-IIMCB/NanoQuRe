#' Plot Read Lengths
#'
#' Generates an interactive plot with number of reads of each length in bases.
#' Two vertical lines represent the mean read length and the N50 value
#' calculated with \code{\link{calculate_n50}}.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#' @param upper_limit Upper limit of the x axis. Defaults to 4000.
#' @param y_limit Upper limit of the y axis. If NULL (default), automatically
#'   set to the 99.9th percentile of read counts multiplied by 1.1.
#'
#' @returns plotly object
#' @import dplyr
#' @importFrom plotly plot_ly add_bars add_lines layout
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' plot_read_lengths(sample_data)
plot_read_lengths <- function(seq_summary, upper_limit = 4000, y_limit = NULL) {
  
  # --- Validation ---
  if (!("sequence_length_template" %in% names(seq_summary)))
    stop("The data frame is missing the 'sequence_length_template' column")
  if (!("sample_id" %in% names(seq_summary)))
    stop("The data frame is missing the 'sample_id' column")
  if (!is.numeric(seq_summary$sequence_length_template))
    stop("Column 'sequence_length_template' must be numeric")
  
  upper_limit <- suppressWarnings(as.numeric(upper_limit))
  if (is.na(upper_limit))
    stop("upper_limit must be a number")
  
  # --- Data prep ---
  lengths     <- seq_summary$sequence_length_template
  sample_name <- seq_summary$sample_id[[1]]
  mean_length <- mean(lengths, na.rm = TRUE)
  n50_SR      <- calculate_n50(seq_summary)
  
  count_seq         <- as.data.frame(table(lengths))
  count_seq$lengths <- as.numeric(as.character(count_seq$lengths))
  names(count_seq)  <- c("sequence_length_template", "n")
  count_seq$n       <- as.integer(count_seq$n)
  max_y             <- max(count_seq$n, na.rm = TRUE)
  
  if (is.null(y_limit))
    y_limit <- quantile(count_seq$n, 0.999, na.rm = TRUE) * 1.1
  
  # --- Plot ---
  length_plot <- plotly::plot_ly() %>%
    plotly::add_bars(
      data          = count_seq,
      x             = ~sequence_length_template,
      y             = ~n,
      name          = "Read counts",
      marker        = list(color = "#404040", opacity = 0.85,
                           line = list(color = "#404040", width = 0.5)),
      hovertemplate = "Length: %{x} nt<br>Count: %{y}<extra></extra>"
    ) %>%
    plotly::add_lines(
      x             = c(mean_length, mean_length),
      y             = c(0, max_y),
      name          = paste0("Mean: ", round(mean_length, 0), " nt"),
      line          = list(color = "#CC79A7", width = 2.5),
      hovertemplate = paste0("Mean: ", round(mean_length, 0), " nt<extra></extra>")
    ) %>%
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
        range     = c(0, y_limit),
        showgrid  = TRUE,
        gridcolor = "#e0e0e0",
        tickfont  = list(size = 11, family = "Arial", color = "#333333")
      ),
      bargap        = 0,
      plot_bgcolor  = "#f9f9f9",
      paper_bgcolor = "#f9f9f9",
      legend = list(
        x           = 1.02,
        y           = 1,
        xanchor     = "left",
        bgcolor     = "#ffffff",
        bordercolor = "#cccccc",
        borderwidth = 1,
        font        = list(size = 11, family = "Arial")
      )
    )
  
  return(length_plot)
}