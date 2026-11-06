testthat::test_that("plot_read_lengths returns a valid plotly object", {
  
  test_df <- data.frame(
    sample_id = "Sample_A",
    sequence_length_template = c(100, 200, 300, 400, 500)
  )
  
  plt <- plot_read_lengths(test_df)
  
  testthat::expect_s3_class(plt, "plotly")
})

testthat::test_that("plot_read_lengths has correct title and axis labels", {
  
  test_df <- data.frame(
    sample_id = "Sample_A",
    sequence_length_template = c(100, 200, 300, 400, 500)
  )
  
  plt <- plot_read_lengths(test_df, upper_limit = 1000)
  built <- plotly::plotly_build(plt)
  layout <- built$x$layout
  
  testthat::expect_equal(layout$title$text, "<b>Sample_A</b>")
  testthat::expect_equal(layout$xaxis$title$text, "<b>Sequence length [nt]</b>")
  testthat::expect_equal(layout$yaxis$title$text, "<b>Number of reads</b>")
})

testthat::test_that("plot_read_lengths applies upper_limit to x axis", {
  
  test_df <- data.frame(
    sample_id = "Sample_A",
    sequence_length_template = c(100, 200, 300, 400, 500)
  )
  
  plt <- plot_read_lengths(test_df, upper_limit = 1000, log_x = FALSE)
  built <- plotly::plotly_build(plt)
  layout <- built$x$layout
  
  # x axis range should be c(0, upper_limit)
  testthat::expect_equal(layout$xaxis$range, c(0, 1000), ignore_attr = TRUE)
  
  # on the default log axis the range is given in log10 units
  built_log <- plotly::plotly_build(plot_read_lengths(test_df, upper_limit = 1000))
  testthat::expect_equal(built_log$x$layout$xaxis$type, "log")
  testthat::expect_equal(built_log$x$layout$xaxis$range[2], 3)
})

testthat::test_that("plot_read_lengths has correct trace names and vertical lines", {
  
  # Lengths: 10, 4, 3, 2, 1 → mean = 4, N50 = 10
  test_df <- data.frame(
    sample_id = "Sample_A",
    sequence_length_template = c(1, 2, 3, 4, 10)
  )
  
  plt <- plot_read_lengths(test_df)
  built <- plotly::plotly_build(plt)
  
  # trace 1 = bars, trace 2 = mean line, trace 3 = N50 line
  bars_trace <- built$x$data[[1]]
  mean_trace <- built$x$data[[2]]
  n50_trace <- built$x$data[[3]]
  
  testthat::expect_equal(bars_trace$name, "Read counts")
  
  # mean of c(1,2,3,4,10) = 4
  testthat::expect_equal(mean_trace$x, c(4, 4), ignore_attr = TRUE)
  
  # N50 of c(1,2,3,4,10) = 10
  testthat::expect_equal(n50_trace$x, c(10, 10), ignore_attr = TRUE)
})

testthat::test_that("plot_read_lengths accepts numeric string for upper_limit", {
  
  # as.numeric("5000") should work — no error expected
  test_df <- data.frame(
    sample_id = "S1",
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


testthat::test_that("plot_read_lengths counts every read and every base", {
  
  test_df <- data.frame(
    sample_id = "Sample_A",
    sequence_length_template = c(150, 900, 900, 2500, 43000)
  )
  
  # the last point of the step line repeats the last bin, so it is dropped
  shown_total <- function(...) {
    trace <- plotly::plotly_build(plot_read_lengths(test_df, ...))$x$data[[1]]
    sum(utils::head(trace$y, -1))
  }
  
  testthat::expect_equal(shown_total(), 5)
  testthat::expect_equal(shown_total(log_x = FALSE), 5)
  testthat::expect_equal(shown_total(weight = "bases"), 47450)
})

testthat::test_that("plot_read_lengths keeps the plot size independent of read number", {
  
  set.seed(123) # reproducible simulated lengths
  test_df <- data.frame(
    sample_id = "Sample_A",
    sequence_length_template = round(stats::rlnorm(1e5, 8, 1)) + 1
  )
  
  trace <- plotly::plotly_build(plot_read_lengths(test_df))$x$data[[1]]
  
  # 50 bins per order of magnitude over at most ~5 orders
  testthat::expect_lt(length(trace$x), 300)
})

testthat::test_that("plot_read_lengths notes reads hidden by upper_limit", {
  
  test_df <- data.frame(
    sample_id = "Sample_A",
    sequence_length_template = c(100, 200, 5000, 6000)
  )
  
  built <- plotly::plotly_build(plot_read_lengths(test_df, upper_limit = 4000))
  
  testthat::expect_match(built$x$layout$annotations[[1]]$text,
                         "2 reads longer than 4000 nt")
})

testthat::test_that("plot_read_lengths errors on an invalid weight", {
  
  testthat::expect_error(
    plot_read_lengths(data.frame(sample_id = "S1", sequence_length_template = 100),
                      weight = "length"),
    "weight must be either 'reads' or 'bases'"
  )
})
