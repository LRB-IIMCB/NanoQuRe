testthat::test_that("quality_stats returns correct table structure", {

  test_df <- data.frame(
    sample_id = "SampleA",
    run_id = "Run1",
    passes_filtering = c(TRUE, FALSE),
    mean_qscore_template = c(10, 12),
    sequence_length_template = c(100, 200)
  )

  output <- quality_stats(test_df)

  # returns a data frame with all expected columns
  testthat::expect_s3_class(output, "data.frame")
  testthat::expect_named(output, c("sample id", "all reads", "passed reads",
                                   "n50 value", "mean Q-score", "longest read",
                                   "passed mean length"))
})

testthat::test_that("quality_stats calculates all metrics correctly", {

  # lengths: 100 (pass), 200 (fail)
  # all reads    = 2
  # passed reads = 1
  # mean Q-score  = (10 + 12) / 2 = 11
  # longest read = 200
  # passed mean length = 100 (only pass read)
  # N50: total = 300, half = 150 -> sorted desc: 200, cumsum 200 >= 150 -> N50 = 200
  test_df <- data.frame(
    sample_id = "SampleA",
    run_id = "Run1",
    passes_filtering = c(TRUE, FALSE),
    mean_qscore_template = c(10, 12),
    sequence_length_template = c(100, 200)
  )

  output <- quality_stats(test_df)

  testthat::expect_equal(output$`sample id`, "SampleA")
  testthat::expect_equal(output$`all reads`, 2)
  testthat::expect_equal(output$`passed reads`, 1)
  testthat::expect_equal(output$`mean Q-score`, 11)
  testthat::expect_equal(output$`longest read`, 200)
  testthat::expect_equal(output$`passed mean length`, 100)
  testthat::expect_equal(output$`n50 value`, 200)
})

testthat::test_that("quality_stats throws correct error messages", {

  # Empty data frame
  testthat::expect_error(
    quality_stats(data.frame()),
    "Empty data frame provided"
  )

  # Missing 'sample_id'
  testthat::expect_error(
    quality_stats(data.frame(
      run_id = "R1", passes_filtering = TRUE,
      mean_qscore_template = 10, sequence_length_template = 100
    )),
    "Required columns missing: sample_id"
  )

  # Missing 'run_id'
  testthat::expect_error(
    quality_stats(data.frame(
      sample_id = "S1", passes_filtering = TRUE,
      mean_qscore_template = 10, sequence_length_template = 100
    )),
    "Required columns missing: run_id"
  )

  # mean_qscore_template is wrong type (character triggers the numeric check)
  testthat::expect_error(
    quality_stats(data.frame(
      sample_id = "S1", run_id = "R1", passes_filtering = TRUE,
      mean_qscore_template = "High", sequence_length_template = 100
    )),
    "Column mean_qscore_template must be numeric"
  )

  # sequence_length_template is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    quality_stats(data.frame(
      sample_id = "S1", run_id = "R1", passes_filtering = TRUE,
      mean_qscore_template = 10, sequence_length_template = factor("100")
    )),
    "Column sequence_length_template must be numeric"
  )

  # Missing 'passes_filtering'
  testthat::expect_error(
    quality_stats(data.frame(
      sample_id = "S1", run_id = "R1",
      mean_qscore_template = 10, sequence_length_template = 100
    )),
    "Required columns missing: passes_filtering"
  )

  # Missing 'mean_qscore_template'
  testthat::expect_error(
    quality_stats(data.frame(
      sample_id = "S1", run_id = "R1", passes_filtering = TRUE,
      sequence_length_template = 100
    )),
    "Required columns missing: mean_qscore_template"
  )

  # Missing 'sequence_length_template'
  testthat::expect_error(
    quality_stats(data.frame(
      sample_id = "S1", run_id = "R1", passes_filtering = TRUE,
      mean_qscore_template = 10
    )),
    "Required columns missing: sequence_length_template"
  )

  # passes_filtering is not logical
  testthat::expect_error(
    quality_stats(data.frame(
      sample_id = "S1", run_id = "R1", passes_filtering = "TRUE",
      mean_qscore_template = 10, sequence_length_template = 100
    )),
    "Column passes_filtering must be logical"
  )
})

testthat::test_that("quality_stats reports NA, not NaN, for the passed mean length when no read passes", {

  test_df <- data.frame(
    sample_id = "SampleA", run_id = "Run1",
    passes_filtering = c(FALSE, FALSE),
    mean_qscore_template = c(5, 6),
    sequence_length_template = c(100, 200)
  )

  output <- quality_stats(test_df)

  testthat::expect_identical(output$`passed mean length`, NA_real_)
  testthat::expect_equal(output$`passed reads`, 0)
})

testthat::test_that("quality_stats removes NA values consistently", {

  # one NA read length and one NA Q-score must not turn the other metrics into NA
  test_df <- data.frame(
    sample_id = "SampleA", run_id = "Run1",
    passes_filtering = c(TRUE, TRUE, FALSE),
    mean_qscore_template = c(10, NA, 12),
    sequence_length_template = c(100, NA, 300)
  )

  output <- quality_stats(test_df)

  testthat::expect_equal(output$`mean Q-score`, 11)
  testthat::expect_equal(output$`longest read`, 300)
  testthat::expect_equal(output$`passed mean length`, 100)
  testthat::expect_equal(output$`all reads`, 3)
})

testthat::test_that("quality_stats errors when seq_summary is missing", {
  testthat::expect_error(quality_stats(), "Sequencing summary is missing")
})

testthat::test_that("quality_stats errors on an empty data frame", {
  testthat::expect_error(quality_stats(data.frame()), "Empty data frame provided")
})
