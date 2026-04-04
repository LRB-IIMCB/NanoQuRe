testthat::test_that("sequencing_stats returns correct table structure", {
  
  test_df <- data.frame(
    sample_id                = "SampleA",
    run_id                   = "Run1",
    start_time               = c(0, 3600),
    duration                 = c(10, 10),
    sequence_length_template = c(1e9, 1e9),
    passes_filtering         = c(TRUE, FALSE)
  )
  
  output <- sequencing_stats(test_df)
  
  # returns a data frame with all expected columns
  testthat::expect_s3_class(output, "data.frame")
  testthat::expect_named(output, c("sample id", "duration [h]", "number of reads",
                                   "total bases sequenced [Gb]", "passed reads [%]",
                                   "average speed [bp/s]"))
})

testthat::test_that("sequencing_stats calculates all metrics correctly", {
  
  # 2 reads, both in Run1
  # duration [h]             : max(0+10, 3600+10) / 3600 = 3610 / 3600 = 1.00 h
  # number of reads          : 2
  # total bases sequenced    : (1e9 + 1e9) / 1e9 = 2.000 Gb
  # passed reads [%]         : 1 pass / 2 total = 50.00 %
  # average speed [bp/s]     : (1e9 + 1e9) / (10 + 10) = 1e8 bp/s
  test_df <- data.frame(
    sample_id                = "SampleA",
    run_id                   = "Run1",
    start_time               = c(0, 3600),
    duration                 = c(10, 10),
    sequence_length_template = c(1e9, 1e9),
    passes_filtering         = c(TRUE, FALSE)
  )
  
  output <- sequencing_stats(test_df)
  
  testthat::expect_equal(output$`sample id`,                  "SampleA")
  testthat::expect_equal(output$`duration [h]`,               1.00)
  testthat::expect_equal(output$`number of reads`,            2)
  testthat::expect_equal(output$`total bases sequenced [Gb]`, 2.000)
  testthat::expect_equal(output$`passed reads [%]`,           50.00)
  testthat::expect_equal(output$`average speed [bp/s]`,       1e8)
})

testthat::test_that("sequencing_stats handles NA in sequence_length_template", {
  
  # sum() without na.rm = TRUE will return NA when NAs are present
  test_df <- data.frame(
    sample_id                = "SampleA",
    run_id                   = "Run1",
    start_time               = 0,
    duration                 = 10,
    sequence_length_template = NA_real_,
    passes_filtering         = TRUE
  )
  
  output <- sequencing_stats(test_df)
  
  # NA input → NA output for bases sequenced
  testthat::expect_true(is.na(output$`total bases sequenced [Gb]`))
})

testthat::test_that("sequencing_stats throws correct error messages", {
  
  # Empty data frame
  testthat::expect_error(
    sequencing_stats(data.frame()),
    "The input data frame is empty"
  )
  
  # Missing 'sample_id'
  testthat::expect_error(
    sequencing_stats(data.frame(wrong_col = 1)),
    "The data frame is missing the 'sample_id' column"
  )
  
  # Missing 'run_id'
  testthat::expect_error(
    sequencing_stats(data.frame(
      sample_id = "S1", start_time = 0, duration = 10,
      sequence_length_template = 100, passes_filtering = TRUE
    )),
    "The data frame is missing the 'run_id' column"
  )
  
  # Missing 'duration'
  testthat::expect_error(
    sequencing_stats(data.frame(
      sample_id = "S1", run_id = "R1", start_time = 0,
      sequence_length_template = 100, passes_filtering = TRUE
    )),
    "The data frame is missing the 'duration' column"
  )
  
  # sequence_length_template is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    sequencing_stats(data.frame(
      sample_id = "S1", run_id = "R1", start_time = 0,
      duration = 10, sequence_length_template = factor("100"),
      passes_filtering = TRUE
    )),
    "Column 'sequence_length_template' must be numeric"
  )
})