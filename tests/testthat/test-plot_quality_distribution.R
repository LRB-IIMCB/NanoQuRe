testthat::test_that("plot_quality_distribution returns a valid plotly object", {
  
  test_df <- data.frame(
    sample_id                = "Quality_Sample",
    sequence_length_template = c(100, 200, 300, 400),
    mean_qscore_template     = c(6, 8, 10, 12),
    passes_filtering         = c(FALSE, TRUE, TRUE, TRUE)
  )
  
  plt <- plot_quality_distribution(test_df)
  
  testthat::expect_s3_class(plt, "plotly")
})

testthat::test_that("plot_quality_distribution has correct title and axis labels", {
  
  test_df <- data.frame(
    sample_id                = "Quality_Sample",
    sequence_length_template = c(100, 200, 300, 400),
    mean_qscore_template     = c(6, 8, 10, 12),
    passes_filtering         = c(FALSE, TRUE, TRUE, TRUE)
  )
  
  plt    <- plot_quality_distribution(test_df)
  built  <- plotly::plotly_build(plt)
  layout <- built$x$layout
  
  testthat::expect_equal(layout$title$text,       "<b>Quality_Sample</b>")
  testthat::expect_equal(layout$xaxis$title$text, "<b>Mean Q score of read</b>")
  testthat::expect_equal(layout$yaxis$title$text, "<b>Number of reads</b>")
})

testthat::test_that("plot_quality_distribution has correct trace names and cutoff line", {
  
  test_df <- data.frame(
    sample_id                = "Quality_Sample",
    sequence_length_template = c(100, 200, 300, 400),
    mean_qscore_template     = c(6, 8, 10, 12),
    passes_filtering         = c(FALSE, TRUE, TRUE, TRUE)
  )
  
  plt   <- plot_quality_distribution(test_df, qscore_cutoff = 7)
  built <- plotly::plotly_build(plt)
  
  # trace 1 = Fail bars, trace 2 = Pass bars, trace 3 = cutoff line
  fail_trace   <- built$x$data[[1]]
  pass_trace   <- built$x$data[[2]]
  cutoff_trace <- built$x$data[[3]]
  
  testthat::expect_equal(fail_trace$name,  "Fail")
  testthat::expect_equal(pass_trace$name,  "Pass")
  
  # cutoff line x values should both equal the cutoff
  testthat::expect_equal(cutoff_trace$x, c(7, 7), ignore_attr = TRUE)
})

testthat::test_that("plot_quality_distribution accepts numeric string for qscore_cutoff", {
  
  # as.numeric("7.5") should work — no error expected
  test_df <- data.frame(
    sample_id                = "S1",
    sequence_length_template = 100,
    mean_qscore_template     = 10,
    passes_filtering         = TRUE
  )
  
  testthat::expect_no_error(
    plot_quality_distribution(test_df, qscore_cutoff = "7.5")
  )
})

testthat::test_that("plot_quality_distribution throws correct error messages", {
  
  # Empty data frame
  testthat::expect_error(
    plot_quality_distribution(data.frame()),
    "The input data frame is empty."
  )
  
  # Missing 'sequence_length_template'
  testthat::expect_error(
    plot_quality_distribution(data.frame(
      sample_id = "S1", mean_qscore_template = 10, passes_filtering = TRUE
    )),
    "The data frame is missing the 'sequence_length_template' column"
  )
  
  # Missing 'sample_id'
  testthat::expect_error(
    plot_quality_distribution(data.frame(
      sequence_length_template = 100, mean_qscore_template = 10,
      passes_filtering = TRUE
    )),
    "Missing 'sample_id' column"
  )
  
  # mean_qscore_template is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    plot_quality_distribution(data.frame(
      sample_id = "S1", sequence_length_template = 100,
      mean_qscore_template = factor("High"), passes_filtering = TRUE
    )),
    "Q-score column must be numeric"
  )
  
  # Invalid qscore_cutoff (NA cannot be coerced to numeric)
  testthat::expect_error(
    plot_quality_distribution(
      data.frame(
        sample_id = "S1", sequence_length_template = 100,
        mean_qscore_template = 10, passes_filtering = TRUE
      ),
      qscore_cutoff = NA
    ),
    "qscore_cutoff must be a number"
  )
})