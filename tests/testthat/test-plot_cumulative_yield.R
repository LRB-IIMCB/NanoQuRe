testthat::test_that("plot_cumulative_yield returns a valid plotly object", {
  
  test_df <- data.frame(
    sample_id                = "Cum_Sample",
    start_time               = c(0, 3600, 7200, 0),
    duration                 = c(10, 10, 10, 10),
    passes_filtering         = c(TRUE, TRUE, TRUE, FALSE),
    sequence_length_template = c(1e9, 1e9, 1e9, 0.5e9)
  )
  
  plt <- plot_cumulative_yield(test_df)
  
  testthat::expect_s3_class(plt, "plotly")
})

testthat::test_that("plot_cumulative_yield has correct title and axis labels", {
  
  test_df <- data.frame(
    sample_id                = "Cum_Sample",
    start_time               = c(0, 3600, 7200, 0),
    duration                 = c(10, 10, 10, 10),
    passes_filtering         = c(TRUE, TRUE, TRUE, FALSE),
    sequence_length_template = c(1e9, 1e9, 1e9, 0.5e9)
  )
  
  plt    <- plot_cumulative_yield(test_df)
  built  <- plotly::plotly_build(plt)
  layout <- built$x$layout
  
  testthat::expect_equal(layout$title$text,       "<b>Cum_Sample</b>")
  testthat::expect_equal(layout$xaxis$title$text, "<b>Time [h]</b>")
  testthat::expect_equal(layout$yaxis$title$text, "<b>Yield [Gb]</b>")
})

testthat::test_that("plot_cumulative_yield calculates cumulative yield correctly", {
  
  # Pass reads: 1e9 + 1e9 + 1e9 = 3 Gb cumulative
  # Fail reads: 0.5e9 = 0.5 Gb cumulative
  test_df <- data.frame(
    sample_id                = "Cum_Sample",
    start_time               = c(0, 3600, 7200, 0),
    duration                 = c(10, 10, 10, 10),
    passes_filtering         = c(TRUE, TRUE, TRUE, FALSE),
    sequence_length_template = c(1e9, 1e9, 1e9, 0.5e9)
  )
  
  plt   <- plot_cumulative_yield(test_df)
  built <- plotly::plotly_build(plt)
  
  # trace 1 = Pass, trace 2 = Fail
  pass_trace <- built$x$data[[1]]
  fail_trace <- built$x$data[[2]]
  
  testthat::expect_equal(pass_trace$name, "Pass")
  testthat::expect_equal(fail_trace$name, "Fail")
  
  # final cumulative pass yield should be 3 Gb
  testthat::expect_equal(max(pass_trace$y), 3,   ignore_attr = TRUE)
  
  # final cumulative fail yield should be 0.5 Gb
  testthat::expect_equal(max(fail_trace$y), 0.5, ignore_attr = TRUE)
})

testthat::test_that("plot_cumulative_yield throws correct error messages", {
  
  # Missing 'sample_id'
  testthat::expect_error(
    plot_cumulative_yield(data.frame(wrong_col = 1)),
    "The data frame is missing the 'sample_id' column"
  )
  
  # sequence_length_template is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    plot_cumulative_yield(data.frame(
      sample_id = "S1", start_time = 0, duration = 10,
      passes_filtering = TRUE, sequence_length_template = factor("100")
    )),
    "Column 'sequence_length_template' must be numeric"
  )
  
  # Empty data frame (correct column names but 0 rows)
  testthat::expect_error(
    plot_cumulative_yield(data.frame(
      sample_id = character(), start_time = numeric(),
      duration = numeric(), passes_filtering = logical(),
      sequence_length_template = numeric()
    )),
    "The input data frame is empty"
  )
  
  # passes_filtering is wrong type (numeric instead of logical)
  testthat::expect_error(
    plot_cumulative_yield(data.frame(
      sample_id = "S1", start_time = 10, duration = 10,
      passes_filtering = 1, sequence_length_template = 100
    )),
    "Column 'passes_filtering' must be logical"
  )
})