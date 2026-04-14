testthat::test_that("plot_read_lengths returns a valid plotly object", {
  
  test_df <- data.frame(
    sample_id                = "Sample_A",
    sequence_length_template = c(100, 200, 300, 400, 500)
  )
  
  plt <- plot_read_lengths(test_df)
  
  testthat::expect_s3_class(plt, "plotly")
})

testthat::test_that("plot_read_lengths has correct title and axis labels", {
  
  test_df <- data.frame(
    sample_id                = "Sample_A",
    sequence_length_template = c(100, 200, 300, 400, 500)
  )
  
  plt    <- plot_read_lengths(test_df, upper_limit = 1000)
  built  <- plotly::plotly_build(plt)
  layout <- built$x$layout
  
  testthat::expect_equal(layout$title$text,       "<b>Sample_A</b>")
  testthat::expect_equal(layout$xaxis$title$text, "<b>Sequence length [nt]</b>")
  testthat::expect_equal(layout$yaxis$title$text, "<b>Number of reads</b>")
})

testthat::test_that("plot_read_lengths applies upper_limit to x axis", {
  
  test_df <- data.frame(
    sample_id                = "Sample_A",
    sequence_length_template = c(100, 200, 300, 400, 500)
  )
  
  plt    <- plot_read_lengths(test_df, upper_limit = 1000)
  built  <- plotly::plotly_build(plt)
  layout <- built$x$layout
  
  # x axis range should be c(0, upper_limit)
  testthat::expect_equal(layout$xaxis$range, c(0, 1000), ignore_attr = TRUE)
})

testthat::test_that("plot_read_lengths has correct trace names and vertical lines", {
  
  # Lengths: 10, 4, 3, 2, 1 → mean = 4, N50 = 10
  test_df <- data.frame(
    sample_id                = "Sample_A",
    sequence_length_template = c(1, 2, 3, 4, 10)
  )
  
  plt   <- plot_read_lengths(test_df)
  built <- plotly::plotly_build(plt)
  
  # trace 1 = bars, trace 2 = mean line, trace 3 = N50 line
  bars_trace <- built$x$data[[1]]
  mean_trace <- built$x$data[[2]]
  n50_trace  <- built$x$data[[3]]
  
  testthat::expect_equal(bars_trace$name, "Read counts")
  
  # mean of c(1,2,3,4,10) = 4
  testthat::expect_equal(mean_trace$x, c(4, 4), ignore_attr = TRUE)
  
  # N50 of c(1,2,3,4,10) = 10
  testthat::expect_equal(n50_trace$x,  c(10, 10), ignore_attr = TRUE)
})

testthat::test_that("plot_read_lengths accepts numeric string for upper_limit", {
  
  # as.numeric("5000") should work — no error expected
  test_df <- data.frame(
    sample_id                = "S1",
    sequence_length_template = 100
  )
  
  testthat::expect_no_error(
    plot_read_lengths(test_df, upper_limit = "5000")
  )
})

testthat::test_that("plot_read_lengths throws correct error messages", {
  
  # Missing 'sequence_length_template'
  testthat::expect_error(
    plot_read_lengths(data.frame(x = 1)),
    "The data frame is missing the 'sequence_length_template' column"
  )
  
  # Missing 'sample_id'
  testthat::expect_error(
    plot_read_lengths(data.frame(sequence_length_template = 100)),
    "The data frame is missing the 'sample_id' column"
  )
  
  # sequence_length_template is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    plot_read_lengths(data.frame(
      sample_id = "S1", sequence_length_template = factor("100")
    )),
    "Column 'sequence_length_template' must be numeric"
  )
  
  # Invalid upper_limit (word that cannot be coerced to numeric)
  testthat::expect_error(
    plot_read_lengths(
      data.frame(sample_id = "S1", sequence_length_template = 100),
      upper_limit = "abc"
    ),
    "upper_limit must be a number"
  )
})