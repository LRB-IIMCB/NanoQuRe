testthat::test_that("plot_seq_throughput returns a valid plotly object", {
  
  test_df <- data.frame(
    sample_id                = "Sample_123",
    start_time               = c(0, 3600, 7200, 0),
    duration                 = c(10, 10, 10, 10),
    passes_filtering         = c(TRUE, TRUE, TRUE, FALSE),
    sequence_length_template = c(1e9, 1e9, 1e9, 1e9)
  )
  
  plt <- plot_seq_throughput(test_df)
  
  testthat::expect_s3_class(plt, "plotly")
})

testthat::test_that("plot_seq_throughput has correct title and axis labels", {
  
  test_df <- data.frame(
    sample_id                = "Sample_123",
    start_time               = c(0, 3600, 7200, 0),
    duration                 = c(10, 10, 10, 10),
    passes_filtering         = c(TRUE, TRUE, TRUE, FALSE),
    sequence_length_template = c(1e9, 1e9, 1e9, 1e9)
  )
  
  plt    <- plot_seq_throughput(test_df)
  built  <- plotly::plotly_build(plt)
  layout <- built$x$layout
  
  testthat::expect_equal(layout$title$text,       "<b>Sample_123</b>")
  testthat::expect_equal(layout$xaxis$title$text, "<b>Time [h]</b>")
  testthat::expect_equal(layout$yaxis$title$text, "<b>Yield per hour [Gb]</b>")
})

testthat::test_that("plot_seq_throughput calculates yield per hour correctly", {
  
  # Pass reads: hour 0 → 1e9 bp = 1 Gb, hour 1 → 1e9 bp = 1 Gb, hour 2 → 1e9 bp = 1 Gb
  # Fail reads: hour 0 → 1e9 bp = 1 Gb
  test_df <- data.frame(
    sample_id                = "Sample_123",
    start_time               = c(0, 3600, 7200, 0),
    duration                 = c(10, 10, 10, 10),
    passes_filtering         = c(TRUE, TRUE, TRUE, FALSE),
    sequence_length_template = c(1e9, 1e9, 1e9, 1e9)
  )
  
  plt   <- plot_seq_throughput(test_df)
  built <- plotly::plotly_build(plt)
  
  # trace 1 = Pass, trace 2 = Fail
  pass_trace <- built$x$data[[1]]
  fail_trace <- built$x$data[[2]]
  
  testthat::expect_equal(pass_trace$name, "Pass")
  testthat::expect_equal(fail_trace$name, "Fail")
  
  # all 3 pass hours should each have 1 Gb yield
  testthat::expect_equal(pass_trace$y, c(1, 1, 1), ignore_attr = TRUE)
  
  # single fail hour should have 1 Gb yield
  testthat::expect_equal(fail_trace$y, 1, ignore_attr = TRUE)
})

testthat::test_that("plot_seq_throughput throws correct error messages", {
  
  # Missing 'sample_id'
  testthat::expect_error(
    plot_seq_throughput(data.frame(wrong_col = 1)),
    "The data frame is missing the 'sample_id' column"
  )
  
  # Empty data frame (correct column names but 0 rows)
  testthat::expect_error(
    plot_seq_throughput(data.frame(
      sample_id = character(), start_time = numeric(),
      duration = numeric(), passes_filtering = logical(),
      sequence_length_template = numeric()
    )),
    "The input data frame is empty"
  )
  
  # passes_filtering is wrong type (character instead of logical)
  testthat::expect_error(
    plot_seq_throughput(data.frame(
      sample_id = "S1", start_time = 0, duration = 10,
      passes_filtering = "TRUE", sequence_length_template = 100
    )),
    "Column 'passes_filtering' must be logical"
  )
  
  # start_time is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    plot_seq_throughput(data.frame(
      sample_id = "S1", start_time = factor("zero"), duration = 10,
      passes_filtering = TRUE, sequence_length_template = 100
    )),
    "Column 'start_time' must be numeric"
  )
})