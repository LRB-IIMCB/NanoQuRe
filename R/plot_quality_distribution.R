#' Plot Quality Distribution
#' Generates a plot with number of reads of each length and their average quality score Q score sorted by pass/fail filtering status. Vertical line represents Q score cut-off which is by default equal to 7.
#'
#'
#' @param seq_summary A dataframe containing the sequencing summary
#' @param qscore_cutoff Numeric parameter of Qscore cut-off
#'
#' @returns ggplot2 object
#' @import ggplot2
#' @import dplyr
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' NULL
#' Plot Quality Distribution
#'
#' Generates an interactive plot with number of reads at each Q score sorted
#' by pass/fail filtering status. Vertical line represents Q score cut-off
#' which is by default equal to 7.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#' @param qscore_cutoff Numeric parameter of Qscore cut-off
#'
#' @returns plotly object
#' @import dplyr
#' @importFrom plotly plot_ly add_bars add_lines layout
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' NULL
plot_quality_distribution <- function(seq_summary, qscore_cutoff = 7) {
  
  # --- Validation ---
  qscore_cutoff <- as.numeric(qscore_cutoff)
  assertthat::assert_that(nrow(seq_summary) > 0,
                          msg = "The input data frame is empty.")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sequence_length_template"),
                          msg = "The data frame is missing the 'sequence_length_template' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sample_id"),
                          msg = "Missing 'sample_id' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "mean_qscore_template"),
                          msg = "The data frame is missing the 'mean_qscore_template' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "passes_filtering"),
                          msg = "The data frame is missing the 'passes_filtering' column")
  assertthat::assert_that(is.numeric(seq_summary$mean_qscore_template),
                          msg = "Q-score column must be numeric")
  assertthat::assert_that(!is.na(qscore_cutoff),
                          msg = "qscore_cutoff must be a number")
  
  # --- Data prep ---
  sample_name <- dplyr::first(seq_summary$sample_id)
  binwidth    <- 0.15
  max_y       <- nrow(seq_summary) * 0.05
  
  bins <- seq(-0.1, 15.1, by = binwidth)
  
  pass_data <- seq_summary %>%
    dplyr::filter(passes_filtering == TRUE) %>%
    dplyr::mutate(bin = cut(mean_qscore_template, breaks = bins,
                            right = FALSE, include.lowest = TRUE)) %>%
    dplyr::count(bin) %>%
    dplyr::mutate(bin_mid = bins[as.integer(bin)] + binwidth / 2)
  
  fail_data <- seq_summary %>%
    dplyr::filter(passes_filtering == FALSE) %>%
    dplyr::mutate(bin = cut(mean_qscore_template, breaks = bins,
                            right = FALSE, include.lowest = TRUE)) %>%
    dplyr::count(bin) %>%
    dplyr::mutate(bin_mid = bins[as.integer(bin)] + binwidth / 2)
  
  # --- Plot ---
  qual_plot <- plotly::plot_ly() %>%
    plotly::add_bars(
      data          = fail_data,
      x             = ~bin_mid,
      y             = ~n,
      name          = "Fail",
      marker        = list(
        color = "#D62728",          # brick red — colorblind friendly
        line  = list(width = 0)     # fix: no border — removes uneven edge rendering
      ),
      hovertemplate = "Q score: %{x:.2f}<br>Count: %{y}<extra>Fail</extra>"
    ) %>%
    plotly::add_bars(
      data          = pass_data,
      x             = ~bin_mid,
      y             = ~n,
      name          = "Pass",
      marker        = list(
        color = "#0072B2",
        line  = list(width = 0)     # fix: no border
      ),
      hovertemplate = "Q score: %{x:.2f}<br>Count: %{y}<extra>Pass</extra>"
    ) %>%
    plotly::add_lines(
      x             = c(qscore_cutoff, qscore_cutoff),
      y             = c(0, max_y),
      name          = paste0("Q score cut-off: ", qscore_cutoff),
      line          = list(color = "#E69F00", width = 2.5),
      hovertemplate = paste0("Cut-off: ", qscore_cutoff, "<extra></extra>")
    ) %>%
    plotly::layout(
      barmode = "stack",
      title = list(
        text = paste0("<b>", sample_name, "</b>"),
        x    = 0.5,
        font = list(size = 15, color = "#333333", family = "Arial")
      ),
      xaxis = list(
        title     = list(text = "<b>Mean Q score of read</b>",
                         font = list(size = 13, family = "Arial")),
        range     = c(0, 15),
        showgrid  = TRUE,
        gridcolor = "#e0e0e0",
        tickfont  = list(size = 11, family = "Arial", color = "#333333")
      ),
      yaxis = list(
        title     = list(text = "<b>Number of reads</b>",
                         font = list(size = 13, family = "Arial")),
        range     = c(0, max_y),
        showgrid  = TRUE,
        gridcolor = "#e0e0e0",
        tickfont  = list(size = 11, family = "Arial", color = "#333333")
      ),
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
    ) %>%
    # Toolbar appears only on hover — disappears when mouse leaves the plot
    plotly::config(displayModeBar = "hover")
  
  return(qual_plot)
}