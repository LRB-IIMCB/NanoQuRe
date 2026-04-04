testthat::test_that("plot_double returns a list of two valid plotly objects", {
  
  test_df <- data.frame(
    sample_id                = "Double_Sample",
    start_time               = c(0, 3600, 7200),
    duration                 = c(10, 10, 10),
    passes_filtering         = c(TRUE, TRUE, TRUE),
    sequence_length_template = c(100, 500, 1000),
    mean_qscore_template     = c(8, 10, 12)
  )
  
  result <- plot_double(test_df)
  
  # returns a named list with two plotly objects
  testthat::expect_type(result, "list")
  testthat::expect_named(result, c("length_plot", "qscore_plot"))
  testthat::expect_s3_class(result$length_plot, "plotly")
  testthat::expect_s3_class(result$qscore_plot, "plotly")
})

testthat::test_that("plot_double has correct titles and axis labels", {
  
  test_df <- data.frame(
    sample_id                = "Double_Sample",
    start_time               = c(0, 3600, 7200),
    duration                 = c(10, 10, 10),
    passes_filtering         = c(TRUE, TRUE, TRUE),
    sequence_length_template = c(100, 500, 1000),
    mean_qscore_template     = c(8, 10, 12)
  )
  
  result <- plot_double(test_df)
  
  length_layout <- plotly::plotly_build(result$length_plot)$x$layout
  qscore_layout <- plotly::plotly_build(result$qscore_plot)$x$layout
  
  # length plot
  testthat::expect_equal(length_layout$title$text,       "<b>Length Double_Sample</b>")
  testthat::expect_equal(length_layout$xaxis$title$text, "<b>Time [h]</b>")
  testthat::expect_equal(length_layout$yaxis$title$text, "<b>Length [bp]</b>")
  
  # qscore plot
  testthat::expect_equal(qscore_layout$title$text,       "<b>Q-score Double_Sample</b>")
  testthat::expect_equal(qscore_layout$xaxis$title$text, "<b>Time [h]</b>")
  testthat::expect_equal(qscore_layout$yaxis$title$text, "<b>Q-score</b>")
})

testthat::test_that("plot_double calculates length stats correctly", {
  
  # hour 0: lengths 100, 500  → max=500, min=100, avg=300
  # hour 1: lengths 1000      → max=1000, min=1000, avg=1000
  test_df <- data.frame(
    sample_id                = "Double_Sample",
    start_time               = c(0, 0, 3600),
    duration                 = c(10, 10, 10),
    passes_filtering         = c(TRUE, TRUE, TRUE),
    sequence_length_template = c(100, 500, 1000),
    mean_qscore_template     = c(8, 10, 12)
  )
  
  result <- plot_double(test_df)
  built  <- plotly::plotly_build(result$length_plot)
  
  # trace order: Max, Average, Min
  max_trace <- built$x$data[[1]]
  av_trace  <- built$x$data[[2]]
  min_trace <- built$x$data[[3]]
  
  testthat::expect_equal(max_trace$name, "Max")
  testthat::expect_equal(av_trace$name,  "Average")
  testthat::expect_equal(min_trace$name, "Min")
  
  # hour 0 max = 500, hour 1 max = 1000
  testthat::expect_equal(max_trace$y, c(500, 1000), ignore_attr = TRUE)
  
  # hour 0 avg = 300, hour 1 avg = 1000
  testthat::expect_equal(av_trace$y,  c(300, 1000), ignore_attr = TRUE)
})

testthat::test_that("plot_double calculates qscore stats correctly", {
  
  # hour 0: qscores 8, 10  → max=10, min=8, avg=9
  # hour 1: qscores 12     → max=12, min=12, avg=12
  test_df <- data.frame(
    sample_id                = "Double_Sample",
    start_time               = c(0, 0, 3600),
    duration                 = c(10, 10, 10),
    passes_filtering         = c(TRUE, TRUE, TRUE),
    sequence_length_template = c(100, 500, 1000),
    mean_qscore_template     = c(8, 10, 12)
  )
  
  result <- plot_double(test_df)
  built  <- plotly::plotly_build(result$qscore_plot)
  
  max_trace <- built$x$data[[1]]
  av_trace  <- built$x$data[[2]]
  min_trace <- built$x$data[[3]]
  
  # hour 0 max = 10, hour 1 max = 12
  testthat::expect_equal(max_trace$y, c(10, 12), ignore_attr = TRUE)
  
  # hour 0 avg = 9, hour 1 avg = 12
  testthat::expect_equal(av_trace$y,  c(9, 12),  ignore_attr = TRUE)
  
  # hour 0 min = 8, hour 1 min = 12
  testthat::expect_equal(min_trace$y, c(8, 12),  ignore_attr = TRUE)
})

testthat::test_that("plot_double throws correct error messages", {
  
  # Empty data frame
  testthat::expect_error(
    plot_double(data.frame()),
    "The input data frame is empty"
  )
  
  # passes_filtering is wrong type (character instead of logical)
  testthat::expect_error(
    plot_double(data.frame(
      sample_id = "S1", start_time = 0, duration = 10,
      sequence_length_template = 100, mean_qscore_template = 10,
      passes_filtering = "TRUE"
    )),
    "Column 'passes_filtering' must be logical"
  )
  
  # start_time is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    plot_double(data.frame(
      sample_id = "S1", start_time = factor("0"), duration = 10,
      sequence_length_template = 100, mean_qscore_template = 10,
      passes_filtering = TRUE
    )),
    "Column 'start_time' must be numeric"
  )
  
  # Missing 'duration' column
  testthat::expect_error(
    plot_double(data.frame(
      sample_id = "S1", start_time = 0,
      sequence_length_template = 100, mean_qscore_template = 10,
      passes_filtering = TRUE
    )),
    "The data frame is missing the 'duration' column"
  )
})