testthat::test_that("sequencing_stats returns correct table structure", {

  test_df <- data.frame(
    sample_id = "SampleA",
    run_id = "Run1",
    start_time = c(0, 3600),
    duration = c(10, 10),
    sequence_length_template = c(1e9, 1e9),
    passes_filtering = c(TRUE, FALSE)
  )

  output <- sequencing_stats(test_df)

  # returns a data frame with all expected columns
  testthat::expect_s3_class(output, "data.frame")
  testthat::expect_named(output, c("sample id", "duration [h]", "number of reads",
                                   "total bases sequenced [Gb]", "passed reads [%]",
                                   "average speed [nt/s]"))
})

testthat::test_that("sequencing_stats calculates all metrics correctly", {

  # 2 reads, both in Run1
  # duration [h]             : max(0+10, 3600+10) / 3600 = 3610 / 3600 = 1.00 h
  # number of reads          : 2
  # total bases sequenced    : (1e9 + 1e9) / 1e9 = 2.000 Gb
  # passed reads [%]         : 1 pass / 2 total = 50.00 %
  # average speed [nt/s]     : (1e9 + 1e9) / (10 + 10) = 1e8 nt/s
  test_df <- data.frame(
    sample_id = "SampleA",
    run_id = "Run1",
    start_time = c(0, 3600),
    duration = c(10, 10),
    sequence_length_template = c(1e9, 1e9),
    passes_filtering = c(TRUE, FALSE)
  )

  output <- sequencing_stats(test_df)

  testthat::expect_equal(output$`sample id`, "SampleA")
  testthat::expect_equal(output$`duration [h]`, 1.00)
  testthat::expect_equal(output$`number of reads`, 2)
  testthat::expect_equal(output$`total bases sequenced [Gb]`, 2.000)
  testthat::expect_equal(output$`passed reads [%]`, 50.00)
  testthat::expect_equal(output$`average speed [nt/s]`, 1e8)
})

testthat::test_that("sequencing_stats handles NA in sequence_length_template", {

  # NA read lengths are removed from every sum, so totals are still computed
  test_df <- data.frame(
    sample_id = "SampleA",
    run_id = "Run1",
    start_time = c(0, 10),
    duration = c(10, 10),
    sequence_length_template = c(NA, 2e9),
    passes_filtering = c(TRUE, TRUE)
  )

  output <- sequencing_stats(test_df)

  testthat::expect_equal(output$`total bases sequenced [Gb]`, 2)
  testthat::expect_equal(output$`average speed [nt/s]`, 2e9 / 20)
  testthat::expect_equal(output$`number of reads`, 2)
})

testthat::test_that("sequencing_stats returns NA speed for a zero total duration", {

  test_df <- data.frame(
    sample_id = "SampleA", run_id = "Run1",
    start_time = 0, duration = 0,
    sequence_length_template = 100, passes_filtering = TRUE
  )

  testthat::expect_true(is.na(sequencing_stats(test_df)$`average speed [nt/s]`))
})

testthat::test_that("sequencing_stats throws correct error messages", {

  # Empty data frame
  testthat::expect_error(
    sequencing_stats(data.frame()),
    "Empty data frame provided"
  )

  # Missing 'sample_id'
  testthat::expect_error(
    sequencing_stats(data.frame(wrong_col = 1)),
    "Required columns missing: sample_id"
  )

  # Missing 'run_id'
  testthat::expect_error(
    sequencing_stats(data.frame(
      sample_id = "S1", start_time = 0, duration = 10,
      sequence_length_template = 100, passes_filtering = TRUE
    )),
    "Required columns missing: run_id"
  )

  # Missing 'duration'
  testthat::expect_error(
    sequencing_stats(data.frame(
      sample_id = "S1", run_id = "R1", start_time = 0,
      sequence_length_template = 100, passes_filtering = TRUE
    )),
    "Required columns missing: duration"
  )

  # sequence_length_template is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    sequencing_stats(data.frame(
      sample_id = "S1", run_id = "R1", start_time = 0,
      duration = 10, sequence_length_template = factor("100"),
      passes_filtering = TRUE
    )),
    "Column sequence_length_template must be numeric"
  )

  # Missing 'start_time'
  testthat::expect_error(
    sequencing_stats(data.frame(
      sample_id = "S1", run_id = "R1", duration = 10,
      sequence_length_template = 100, passes_filtering = TRUE
    )),
    "Required columns missing: start_time"
  )

  # Missing 'passes_filtering'
  testthat::expect_error(
    sequencing_stats(data.frame(
      sample_id = "S1", run_id = "R1", start_time = 0, duration = 10,
      sequence_length_template = 100
    )),
    "Required columns missing: passes_filtering"
  )

  # passes_filtering is not logical
  testthat::expect_error(
    sequencing_stats(data.frame(
      sample_id = "S1", run_id = "R1", start_time = 0, duration = 10,
      sequence_length_template = 100, passes_filtering = 1
    )),
    "Column passes_filtering must be logical"
  )
})

testthat::test_that("sequencing_stats errors when seq_summary is missing", {
  testthat::expect_error(sequencing_stats(), "Sequencing summary is missing")
})

testthat::test_that("sequencing_stats errors on an empty data frame", {
  testthat::expect_error(sequencing_stats(data.frame()), "Empty data frame provided")
})
