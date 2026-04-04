testthat::test_that("quality_stats returns correct table structure", {
  
  test_df <- data.frame(
    sample_id                = "SampleA",
    run_id                   = "Run1",
    passes_filtering         = c(TRUE, FALSE),
    mean_qscore_template     = c(10, 12),
    sequence_length_template = c(100, 200)
  )
  
  output <- quality_stats(test_df)
  
  # returns a data frame with all expected columns
  testthat::expect_s3_class(output, "data.frame")
  testthat::expect_named(output, c("sample id", "all reads", "passed reads",
                                   "n50 value", "mean qscore", "longest read",
                                   "passed mean length"))
})

testthat::test_that("quality_stats calculates all metrics correctly", {
  
  # lengths: 100 (pass), 200 (fail)
  # all reads    = 2
  # passed reads = 1
  # mean qscore  = (10 + 12) / 2 = 11
  # longest read = 200
  # passed mean length = 100 (only pass read)
  # N50: total = 300, half = 150 → sorted desc: 200, cumsum 200 >= 150 → N50 = 200
  test_df <- data.frame(
    sample_id                = "SampleA",
    run_id                   = "Run1",
    passes_filtering         = c(TRUE, FALSE),
    mean_qscore_template     = c(10, 12),
    sequence_length_template = c(100, 200)
  )
  
  output <- quality_stats(test_df)
  
  testthat::expect_equal(output$`sample id`,          "SampleA")
  testthat::expect_equal(output$`all reads`,          2)
  testthat::expect_equal(output$`passed reads`,       1)
  testthat::expect_equal(output$`mean qscore`,        11)
  testthat::expect_equal(output$`longest read`,       200)
  testthat::expect_equal(output$`passed mean length`, 100)
  testthat::expect_equal(output$`n50 value`,          200)
})

testthat::test_that("quality_stats throws correct error messages", {
  
  # Empty data frame
  testthat::expect_error(
    quality_stats(data.frame()),
    "The input data frame is empty"
  )
  
  # Missing 'sample_id'
  testthat::expect_error(
    quality_stats(data.frame(
      run_id = "R1", passes_filtering = TRUE,
      mean_qscore_template = 10, sequence_length_template = 100
    )),
    "The data frame is missing the 'sample_id' column"
  )
  
  # Missing 'run_id'
  testthat::expect_error(
    quality_stats(data.frame(
      sample_id = "S1", passes_filtering = TRUE,
      mean_qscore_template = 10, sequence_length_template = 100
    )),
    "The data frame is missing the 'run_id' column"
  )
  
  # mean_qscore_template is wrong type (character triggers the numeric check)
  testthat::expect_error(
    quality_stats(data.frame(
      sample_id = "S1", run_id = "R1", passes_filtering = TRUE,
      mean_qscore_template = "High", sequence_length_template = 100
    )),
    "Column 'mean_qscore_template' must be numeric"
  )
  
  # sequence_length_template is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    quality_stats(data.frame(
      sample_id = "S1", run_id = "R1", passes_filtering = TRUE,
      mean_qscore_template = 10, sequence_length_template = factor("100")
    )),
    "Column 'sequence_length_template' must be numeric"
  )
})