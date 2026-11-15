testthat::test_that("plot_average_speed returns a valid plotly object", {

  test_df <- data.frame(
    sample_id = "Speed_Sample",
    start_time = c(0, 3600, 0),
    duration = c(2, 4, 10),
    passes_filtering = c(TRUE, TRUE, FALSE),
    sequence_length_template = c(1000, 2000, 500)
  )

  plt <- plot_average_speed(test_df)

  testthat::expect_s3_class(plt, "plotly")
})

testthat::test_that("plot_average_speed has correct title and axis labels", {

  test_df <- data.frame(
    sample_id = "Speed_Sample",
    start_time = c(0, 3600, 0),
    duration = c(2, 4, 10),
    passes_filtering = c(TRUE, TRUE, FALSE),
    sequence_length_template = c(1000, 2000, 500)
  )

  plt <- plot_average_speed(test_df)
  built <- plotly::plotly_build(plt)
  layout <- built$x$layout

  testthat::expect_equal(layout$title$text, "<b>Speed_Sample</b>")
  testthat::expect_equal(layout$xaxis$title$text, "<b>Time [h]</b>")
  testthat::expect_equal(layout$yaxis$title$text, "<b>Speed [nt/s]</b>")
})

testthat::test_that("plot_average_speed calculates speed correctly", {

  # Pass reads: hour 0 -> 1000 nt / 2s = 500 nt/s
  #             hour 1 -> 2000 nt / 4s = 500 nt/s
  # Fail reads: hour 0 -> 500 nt  / 10s = 50 nt/s
  test_df <- data.frame(
    sample_id = "Speed_Sample",
    start_time = c(0, 3600, 0),
    duration = c(2, 4, 10),
    passes_filtering = c(TRUE, TRUE, FALSE),
    sequence_length_template = c(1000, 2000, 500)
  )

  plt <- plot_average_speed(test_df)
  built <- plotly::plotly_build(plt)

  # trace 1 = Pass, trace 2 = Fail
  pass_trace <- built$x$data[[1]]
  fail_trace <- built$x$data[[2]]

  testthat::expect_equal(pass_trace$name, "Pass")
  testthat::expect_equal(fail_trace$name, "Fail")

  # ignore_attr = TRUE skips plotly's internal apiSrc attribute
  testthat::expect_equal(pass_trace$y, c(500, 500), ignore_attr = TRUE)
  testthat::expect_equal(fail_trace$y, 50, ignore_attr = TRUE)
})

testthat::test_that("plot_average_speed throws correct error messages", {

  # Empty data frame
  testthat::expect_error(
    plot_average_speed(data.frame()),
    "Empty data frame provided"
  )

  # Missing 'sample_id'
  testthat::expect_error(
    plot_average_speed(data.frame(
      start_time = 0, duration = 10,
      passes_filtering = TRUE, sequence_length_template = 100
    )),
    "Required columns missing: sample_id"
  )

  # Missing 'start_time'
  testthat::expect_error(
    plot_average_speed(data.frame(
      sample_id = "S1", duration = 10,
      passes_filtering = TRUE, sequence_length_template = 100
    )),
    "Required columns missing: start_time"
  )

  # passes_filtering is wrong type (character instead of logical)
  testthat::expect_error(
    plot_average_speed(data.frame(
      sample_id = "S1", start_time = 0, duration = 10,
      passes_filtering = "TRUE", sequence_length_template = 100
    )),
    "Column passes_filtering must be logical"
  )

  # duration is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    plot_average_speed(data.frame(
      sample_id = "S1", start_time = 0, duration = factor("10s"),
      passes_filtering = TRUE, sequence_length_template = 100
    )),
    "Column duration must be numeric"
  )
})

testthat::test_that("plot_average_speed gives NA for an hour with zero total duration", {

  # hour 1 holds only a zero-duration read; gap in the middle so that
  # plotly_build() keeps the NA point
  test_df <- data.frame(
    sample_id = "S1",
    start_time = c(0, 3600, 7200),
    duration = c(2, 0, 4),
    passes_filtering = c(TRUE, TRUE, TRUE),
    sequence_length_template = c(1000, 500, 2000)
  )

  pass_trace <- plotly::plotly_build(plot_average_speed(test_df))$x$data[[1]]

  testthat::expect_equal(pass_trace$y, c(500, NA, 500), ignore_attr = TRUE)
})

testthat::test_that("plot_average_speed ignores NA lengths in the hourly sum", {

  test_df <- data.frame(
    sample_id = "S1",
    start_time = c(0, 10),
    duration = c(2, 2),
    passes_filtering = c(TRUE, TRUE),
    sequence_length_template = c(1000, NA)
  )

  pass_trace <- plotly::plotly_build(plot_average_speed(test_df))$x$data[[1]]

  testthat::expect_true(is.finite(pass_trace$y))
})

testthat::test_that("plot_average_speed errors when seq_summary is missing", {
  testthat::expect_error(plot_average_speed(), "Sequencing summary is missing")
})

testthat::test_that("plot_average_speed errors on an empty data frame", {
  testthat::expect_error(plot_average_speed(data.frame()), "Empty data frame provided")
})
