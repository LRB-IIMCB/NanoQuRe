testthat::test_that("plot_average_speed returns a valid plotly object", {
  
  test_df <- data.frame(
    sample_id                = "Speed_Sample",
    start_time               = c(0, 3600, 0),
    duration                 = c(2, 4, 10),
    passes_filtering         = c(TRUE, TRUE, FALSE),
    sequence_length_template = c(1000, 2000, 500)
  )
  
  plt <- plot_average_speed(test_df)
  
  testthat::expect_s3_class(plt, "plotly")
})

testthat::test_that("plot_average_speed has correct title and axis labels", {
  
  test_df <- data.frame(
    sample_id                = "Speed_Sample",
    start_time               = c(0, 3600, 0),
    duration                 = c(2, 4, 10),
    passes_filtering         = c(TRUE, TRUE, FALSE),
    sequence_length_template = c(1000, 2000, 500)
  )
  
  plt    <- plot_average_speed(test_df)
  built  <- plotly::plotly_build(plt)
  layout <- built$x$layout
  
  testthat::expect_equal(layout$title$text,       "<b>Speed_Sample</b>")
  testthat::expect_equal(layout$xaxis$title$text, "<b>Time [h]</b>")
  testthat::expect_equal(layout$yaxis$title$text, "<b>Speed [bp/s]</b>")
})

testthat::test_that("plot_average_speed calculates speed correctly", {
  
  # Pass reads: hour 0 → 1000bp / 2s = 500 bp/s
  #             hour 1 → 2000bp / 4s = 500 bp/s
  # Fail reads: hour 0 → 500bp  / 10s = 50 bp/s
  test_df <- data.frame(
    sample_id                = "Speed_Sample",
    start_time               = c(0, 3600, 0),
    duration                 = c(2, 4, 10),
    passes_filtering         = c(TRUE, TRUE, FALSE),
    sequence_length_template = c(1000, 2000, 500)
  )
  
  plt   <- plot_average_speed(test_df)
  built <- plotly::plotly_build(plt)
  
  # trace 1 = Pass, trace 2 = Fail
  pass_trace <- built$x$data[[1]]
  fail_trace <- built$x$data[[2]]
  
  testthat::expect_equal(pass_trace$name, "Pass")
  testthat::expect_equal(fail_trace$name, "Fail")
  
  # ignore_attr = TRUE skips plotly's internal apiSrc attribute
  testthat::expect_equal(pass_trace$y, c(500, 500), ignore_attr = TRUE)
  testthat::expect_equal(fail_trace$y, 50,           ignore_attr = TRUE)
})

testthat::test_that("plot_average_speed throws correct error messages", {
  
  # Empty data frame
  testthat::expect_error(
    plot_average_speed(data.frame()),
    "The input data frame is empty"
  )
  
  # Missing 'sample_id'
  testthat::expect_error(
    plot_average_speed(data.frame(
      start_time = 0, duration = 10,
      passes_filtering = TRUE, sequence_length_template = 100
    )),
    "The data frame is missing the 'sample_id' column"
  )
  
  # Missing 'start_time'
  testthat::expect_error(
    plot_average_speed(data.frame(
      sample_id = "S1", duration = 10,
      passes_filtering = TRUE, sequence_length_template = 100
    )),
    "The data frame is missing the 'start_time' column"
  )
  
  # passes_filtering is wrong type (character instead of logical)
  testthat::expect_error(
    plot_average_speed(data.frame(
      sample_id = "S1", start_time = 0, duration = 10,
      passes_filtering = "TRUE", sequence_length_template = 100
    )),
    "Column 'passes_filtering' must be logical"
  )
  
  # duration is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    plot_average_speed(data.frame(
      sample_id = "S1", start_time = 0, duration = factor("10s"),
      passes_filtering = TRUE, sequence_length_template = 100
    )),
    "Column 'duration' must be numeric"
  )
})