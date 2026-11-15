#' Plot Quality Distribution
#'
#' Generates an interactive plot with number of reads at each Q-score sorted
#' by pass/fail filtering status. Two vertical lines are drawn: the
#' user-defined Q-score cut-off (\code{qscore_cutoff}, by default 7) and the
#' basecaller threshold, i.e. the lowest Q-score among passing reads. The
#' basecaller threshold line is only drawn when both passing and failing reads
#' are present in the data.
#'
#' @param seq_summary A data frame containing the sequencing summary
#' @param qscore_cutoff Numeric Q-score cut-off drawn as a vertical line.
#'   Defaults to 7.
#'
#' @return plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_quality_distribution(sample_data)
#' }
plot_quality_distribution <- function(seq_summary, qscore_cutoff = 7) {

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
  required_cols <- c("sample_id", "sequence_length_template", "mean_qscore_template",
                     "passes_filtering")
  missing_cols <- setdiff(required_cols, colnames(seq_summary))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }

  assert_condition(is.logical(seq_summary$passes_filtering),
                   "Column passes_filtering must be logical. Please provide a valid argument.")

  assert_condition(is.numeric(seq_summary$mean_qscore_template),
                   "Column mean_qscore_template must be numeric. Please provide a valid argument.")

  # numeric strings are accepted
  qscore_cutoff <- suppressWarnings(as.numeric(qscore_cutoff))
  assert_condition(length(qscore_cutoff) == 1 && !is.na(qscore_cutoff),
                   "Qscore_cutoff must be a number. Please provide a valid argument.")

  # Data prep
  sample_name <- dplyr::first(seq_summary$sample_id)
  binwidth <- 0.15
  # bins span the observed Q range; the former fixed -0.1..15.1 grid hid every
  # read above ~Q15 (i.e. most reads of current R10.4.1 / sup basecalling)
  q_range <- range(seq_summary$mean_qscore_template, na.rm = TRUE)
  bins <- seq(floor(q_range[1]), ceiling(q_range[2]) + binwidth, by = binwidth)

  binned <- seq_summary %>%
    dplyr::mutate(
      bin = cut(mean_qscore_template, breaks = bins,
                right = FALSE, include.lowest = TRUE),
      bin_mid = bins[as.integer(bin)] + binwidth / 2
    )

  # the reference lines span the tallest stacked (pass + fail) bar
  max_y <- max(table(binned$bin))

  pass_data <- binned %>%
    dplyr::filter(passes_filtering == TRUE) %>%
    dplyr::count(bin, bin_mid)

  fail_data <- binned %>%
    dplyr::filter(passes_filtering == FALSE) %>%
    dplyr::count(bin, bin_mid)

  # Basecaller threshold - the minimum Q-score among reads that passed the
  # basecaller filter. This approximates the actual pass/fail boundary.
  # Only drawn when both pass and fail reads are present.
  pass_scores <- seq_summary$mean_qscore_template[seq_summary$passes_filtering == TRUE]
  fail_scores <- seq_summary$mean_qscore_template[seq_summary$passes_filtering == FALSE]
  basecaller_boundary <- if (length(pass_scores) > 0 && length(fail_scores) > 0)
    min(pass_scores, na.rm = TRUE) else NULL

  # Plot
  # suppressWarnings() silences a plotly barmode="stack" warning that fires
  # when one of the pass/fail traces contains no data (all-pass or all-fail run).
  qual_plot <- suppressWarnings(plotly::plot_ly() %>%
                                  plotly::add_bars(
                                    data = fail_data,
                                    x = ~bin_mid,
                                    y = ~n,
                                    name = "Fail",
                                    marker = list(color = "#D62728", line = list(width = 0)),
                                    hovertemplate = "Q-score: %{x:.2f}<br>Count: %{y}<extra>Fail</extra>"
                                  ) %>%
                                  plotly::add_bars(
                                    data = pass_data,
                                    x = ~bin_mid,
                                    y = ~n,
                                    name = "Pass",
                                    marker = list(color = "#0072B2", line = list(width = 0)),
                                    hovertemplate = "Q-score: %{x:.2f}<br>Count: %{y}<extra>Pass</extra>"
                                  ) %>%
                                  plotly::add_lines(
                                    x = c(qscore_cutoff, qscore_cutoff),
                                    y = c(0, max_y),
                                    name = paste0("User-defined cut-off: Q", qscore_cutoff),
                                    line = list(color = "#E69F00", width = 2.5, dash = "dash"),
                                    hovertemplate = paste0(
                                      "User-defined cut-off: Q", qscore_cutoff,
                                      "<br>Reads below basecaller threshold but above this line<br>",
                                      "may be recoverable for analysis.<extra></extra>"
                                    )
                                  ) %>%
                                  { if (!is.null(basecaller_boundary))
                                    plotly::add_lines(.,
                                                      x = c(basecaller_boundary, basecaller_boundary),
                                                      y = c(0, max_y),
                                                      name = paste0("Basecaller threshold: Q", round(basecaller_boundary, 2)),
                                                      line = list(color = "#612a78", width = 2.5),
                                                      hovertemplate = paste0(
                                                        "Basecaller pass/fail boundary: Q", round(basecaller_boundary, 2),
                                                        "<extra></extra>"
                                                      )
                                    )
                                    else . } %>%
                                  plotly::layout(
                                    barmode = "stack",
                                    title = list(
                                      text = paste0("<b>", sample_name, "</b>"),
                                      x = 0.5,
                                      font = list(size = 15, color = "#333333", family = "Arial")
                                    ),
                                    xaxis = list(
                                      title = list(text = "<b>Mean Q-score of read</b>",
                                                   font = list(size = 13, family = "Arial")),
                                      showgrid = TRUE,
                                      gridcolor = "#e0e0e0",
                                      tickfont = list(size = 11, family = "Arial", color = "#333333")
                                    ),
                                    yaxis = list(
                                      title = list(text = "<b>Number of reads</b>",
                                                   font = list(size = 13, family = "Arial")),
                                      showgrid = TRUE,
                                      gridcolor = "#e0e0e0",
                                      tickfont = list(size = 11, family = "Arial", color = "#333333")
                                    ),
                                    plot_bgcolor = "#f9f9f9",
                                    paper_bgcolor = "#f9f9f9",
                                    legend = list(
                                      x = 1.02,
                                      y = 1,
                                      xanchor = "left",
                                      bgcolor = "#ffffff",
                                      bordercolor = "#cccccc",
                                      borderwidth = 1,
                                      font = list(size = 11, family = "Arial")
                                    )
                                  ) %>%
                                  plotly::config(displayModeBar = "hover"))

  return(qual_plot)
}
