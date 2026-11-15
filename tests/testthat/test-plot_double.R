testthat::test_that("plot_double returns a list of two valid plotly objects", {

  test_df <- data.frame(
    sample_id = "Double_Sample",
    start_time = c(0, 3600, 7200),
    duration = c(10, 10, 10),
    passes_filtering = c(TRUE, TRUE, TRUE),
    sequence_length_template = c(100, 500, 1000),
    mean_qscore_template = c(8, 10, 12)
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
    sample_id = "Double_Sample",
    start_time = c(0, 3600, 7200),
    duration = c(10, 10, 10),
    passes_filtering = c(TRUE, TRUE, TRUE),
    sequence_length_template = c(100, 500, 1000),
    mean_qscore_template = c(8, 10, 12)
  )

  result <- plot_double(test_df)

  length_layout <- plotly::plotly_build(result$length_plot)$x$layout
  qscore_layout <- plotly::plotly_build(result$qscore_plot)$x$layout

  # length plot
  testthat::expect_equal(length_layout$title$text, "<b>Length Double_Sample</b>")
  testthat::expect_equal(length_layout$xaxis$title$text, "<b>Time [h]</b>")
  testthat::expect_equal(length_layout$yaxis$title$text, "<b>Length [nt]</b>")

  # qscore plot
  testthat::expect_equal(qscore_layout$title$text, "<b>Q-score Double_Sample</b>")
  testthat::expect_equal(qscore_layout$xaxis$title$text, "<b>Time [h]</b>")
  testthat::expect_equal(qscore_layout$yaxis$title$text, "<b>Q-score</b>")
})

testthat::test_that("plot_double calculates length stats correctly", {

  # hour 0: lengths 100, 500  -> max=500, min=100, avg=300
  # hour 1: lengths 1000      -> max=1000, min=1000, avg=1000
  test_df <- data.frame(
    sample_id = "Double_Sample",
    start_time = c(0, 0, 3600),
    duration = c(10, 10, 10),
    passes_filtering = c(TRUE, TRUE, TRUE),
    sequence_length_template = c(100, 500, 1000),
    mean_qscore_template = c(8, 10, 12)
  )

  result <- plot_double(test_df)
  built <- plotly::plotly_build(result$length_plot)

  # trace order: Max, Average, Min
  max_trace <- built$x$data[[1]]
  av_trace <- built$x$data[[2]]
  min_trace <- built$x$data[[3]]

  testthat::expect_equal(max_trace$name, "Max")
  testthat::expect_equal(av_trace$name, "Average")
  testthat::expect_equal(min_trace$name, "Min")

  # hour 0 max = 500, hour 1 max = 1000
  testthat::expect_equal(max_trace$y, c(500, 1000), ignore_attr = TRUE)

  # hour 0 avg = 300, hour 1 avg = 1000
  testthat::expect_equal(av_trace$y, c(300, 1000), ignore_attr = TRUE)
})

testthat::test_that("plot_double calculates qscore stats correctly", {

  # hour 0: qscores 8, 10  -> max=10, min=8, avg=9
  # hour 1: qscores 12     -> max=12, min=12, avg=12
  test_df <- data.frame(
    sample_id = "Double_Sample",
    start_time = c(0, 0, 3600),
    duration = c(10, 10, 10),
    passes_filtering = c(TRUE, TRUE, TRUE),
    sequence_length_template = c(100, 500, 1000),
    mean_qscore_template = c(8, 10, 12)
  )

  result <- plot_double(test_df)
  built <- plotly::plotly_build(result$qscore_plot)

  max_trace <- built$x$data[[1]]
  av_trace <- built$x$data[[2]]
  min_trace <- built$x$data[[3]]

  # hour 0 max = 10, hour 1 max = 12
  testthat::expect_equal(max_trace$y, c(10, 12), ignore_attr = TRUE)

  # hour 0 avg = 9, hour 1 avg = 12
  testthat::expect_equal(av_trace$y, c(9, 12), ignore_attr = TRUE)

  # hour 0 min = 8, hour 1 min = 12
  testthat::expect_equal(min_trace$y, c(8, 12), ignore_attr = TRUE)
})

testthat::test_that("plot_double throws correct error messages", {

  # missing argument
  testthat::expect_error(plot_double(), "Sequencing summary is missing")

  # Empty data frame
  testthat::expect_error(
    plot_double(data.frame()),
    "Empty data frame provided"
  )

  # missing required column
  testthat::expect_error(
    plot_double(data.frame(sample_id = "S1", start_time = 0)),
    "Required columns missing: sequence_length_template, mean_qscore_template"
  )

  # start_time is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    plot_double(data.frame(
      sample_id = "S1", start_time = factor("0"), duration = 10,
      sequence_length_template = 100, mean_qscore_template = 10,
      passes_filtering = TRUE
    )),
    "Column start_time must be numeric"
  )

  # mean_qscore_template is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    plot_double(data.frame(
      sample_id = "S1", start_time = 0,
      sequence_length_template = 100, mean_qscore_template = factor("high")
    )),
    "Column mean_qscore_template must be numeric"
  )
})

testthat::test_that("plot_double reports the true minimum length", {

  test_df <- data.frame(
    sample_id = "Double_Sample",
    start_time = c(0, 10, 20),
    duration = c(1, 1, 1),
    passes_filtering = c(TRUE, TRUE, TRUE),
    sequence_length_template = c(10, 20, 30),
    mean_qscore_template = c(10, 11, 12)
  )

  built <- plotly::plotly_build(plot_double(test_df)$length_plot)
  traces <- stats::setNames(lapply(built$x$data, function(tr) tr$y),
                            unlist(lapply(built$x$data, function(tr) tr$name)))

  testthat::expect_equal(traces[["Min"]], 10, ignore_attr = TRUE)
  testthat::expect_true(traces[["Min"]] <= traces[["Max"]])
})

testthat::test_that("plot_double no longer requires duration or passes_filtering", {

  test_df <- data.frame(
    sample_id = "S1", start_time = c(0, 3600),
    sequence_length_template = c(100, 200), mean_qscore_template = c(10, 12)
  )

  testthat::expect_no_error(plot_double(test_df))
})

testthat::test_that("plot_double shows hours without reads as gaps", {

  # reads in hours 0 and 2 only; hour 1 must be NA, not interpolated
  test_df <- data.frame(
    sample_id = "S1", start_time = c(0, 7200, 7300),
    sequence_length_template = c(100, 200, 400), mean_qscore_template = c(10, 12, 14)
  )

  res <- plot_double(test_df)
  len_av <- plotly::plotly_build(res$length_plot)$x$data[[2]]
  q_av <- plotly::plotly_build(res$qscore_plot)$x$data[[2]]

  testthat::expect_equal(len_av$y, c(100, NA, 300), ignore_attr = TRUE)
  testthat::expect_equal(q_av$y, c(10, NA, 13), ignore_attr = TRUE)
})

testthat::test_that("plot_double tolerates NA values and ignores zero-length reads in the minimum", {

  test_df <- data.frame(
    sample_id = "S1", start_time = c(0, 10, 20, 30),
    sequence_length_template = c(0, 50, NA, 150),
    mean_qscore_template = c(10, NA, 12, 14)
  )

  res <- plot_double(test_df)
  len <- plotly::plotly_build(res$length_plot)$x$data
  q <- plotly::plotly_build(res$qscore_plot)$x$data

  # zero length would break the log axis, so the minimum is 50
  testthat::expect_equal(len[[3]]$y, 50, ignore_attr = TRUE)
  testthat::expect_equal(len[[1]]$y, 150, ignore_attr = TRUE)
  testthat::expect_equal(q[[3]]$y, 10, ignore_attr = TRUE)
  testthat::expect_equal(q[[2]]$y, 12, ignore_attr = TRUE)
})
