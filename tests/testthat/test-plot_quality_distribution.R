testthat::test_that("plot_quality_distribution returns a valid plotly object", {

  test_df <- data.frame(
    sample_id = "Quality_Sample",
    sequence_length_template = c(100, 200, 300, 400),
    mean_qscore_template = c(6, 8, 10, 12),
    passes_filtering = c(FALSE, TRUE, TRUE, TRUE)
  )

  plt <- plot_quality_distribution(test_df)

  testthat::expect_s3_class(plt, "plotly")
})

testthat::test_that("plot_quality_distribution has correct title and axis labels", {

  test_df <- data.frame(
    sample_id = "Quality_Sample",
    sequence_length_template = c(100, 200, 300, 400),
    mean_qscore_template = c(6, 8, 10, 12),
    passes_filtering = c(FALSE, TRUE, TRUE, TRUE)
  )

  plt <- plot_quality_distribution(test_df)
  built <- plotly::plotly_build(plt)
  layout <- built$x$layout

  testthat::expect_equal(layout$title$text, "<b>Quality_Sample</b>")
  testthat::expect_equal(layout$xaxis$title$text, "<b>Mean Q-score of read</b>")
  testthat::expect_equal(layout$yaxis$title$text, "<b>Number of reads</b>")
})

testthat::test_that("plot_quality_distribution has correct trace names and cutoff line", {

  test_df <- data.frame(
    sample_id = "Quality_Sample",
    sequence_length_template = c(100, 200, 300, 400),
    mean_qscore_template = c(6, 8, 10, 12),
    passes_filtering = c(FALSE, TRUE, TRUE, TRUE)
  )

  plt <- plot_quality_distribution(test_df, qscore_cutoff = 7)
  built <- plotly::plotly_build(plt)

  # trace 1 = Fail bars, trace 2 = Pass bars, trace 3 = cutoff line
  fail_trace <- built$x$data[[1]]
  pass_trace <- built$x$data[[2]]
  cutoff_trace <- built$x$data[[3]]

  testthat::expect_equal(fail_trace$name, "Fail")
  testthat::expect_equal(pass_trace$name, "Pass")

  # cutoff line x values should both equal the cutoff
  testthat::expect_equal(cutoff_trace$x, c(7, 7), ignore_attr = TRUE)
})

testthat::test_that("plot_quality_distribution accepts numeric string for qscore_cutoff", {

  # as.numeric("7.5") should work - no error expected
  test_df <- data.frame(
    sample_id = "S1",
    sequence_length_template = 100,
    mean_qscore_template = 10,
    passes_filtering = TRUE
  )

  testthat::expect_no_error(
    plot_quality_distribution(test_df, qscore_cutoff = "7.5")
  )
})

testthat::test_that("plot_quality_distribution throws correct error messages", {

  # Empty data frame
  testthat::expect_error(
    plot_quality_distribution(data.frame()),
    "Empty data frame provided"
  )

  # missing argument
  testthat::expect_error(plot_quality_distribution(),
                         "Sequencing summary is missing")

  # Missing 'sequence_length_template'
  testthat::expect_error(
    plot_quality_distribution(data.frame(
      sample_id = "S1", mean_qscore_template = 10, passes_filtering = TRUE
    )),
    "Required columns missing: sequence_length_template"
  )

  # Missing 'sample_id'
  testthat::expect_error(
    plot_quality_distribution(data.frame(
      sequence_length_template = 100, mean_qscore_template = 10,
      passes_filtering = TRUE
    )),
    "Required columns missing: sample_id"
  )

  # passes_filtering is wrong type (text flags used to give an empty plot)
  testthat::expect_error(
    plot_quality_distribution(data.frame(
      sample_id = "S1", sequence_length_template = 100,
      mean_qscore_template = 10, passes_filtering = "True"
    )),
    "Column passes_filtering must be logical"
  )

  # mean_qscore_template is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    plot_quality_distribution(data.frame(
      sample_id = "S1", sequence_length_template = 100,
      mean_qscore_template = factor("High"), passes_filtering = TRUE
    )),
    "Column mean_qscore_template must be numeric"
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
    "Qscore_cutoff must be a number"
  )
})

testthat::test_that("plot_quality_distribution shows every read, including Q > 15", {

  test_df <- data.frame(
    sample_id = "Quality_Sample",
    sequence_length_template = c(100, 200, 300, 400),
    mean_qscore_template = c(6, 10, 20, 28.5),
    passes_filtering = c(FALSE, TRUE, TRUE, TRUE)
  )

  built <- plotly::plotly_build(plot_quality_distribution(test_df))
  bars <- Filter(function(tr) identical(tr$type, "bar"), built$x$data)
  shown <- sum(unlist(lapply(bars, function(tr) tr$y[!is.na(tr$x)])))

  testthat::expect_equal(shown, 4)
})

testthat::test_that("plot_quality_distribution draws the basecaller threshold only for mixed data", {

  mixed <- data.frame(
    sample_id = "S1", sequence_length_template = c(100, 200, 300, 400),
    mean_qscore_template = c(6, 8, 10, 12),
    passes_filtering = c(FALSE, TRUE, TRUE, TRUE)
  )
  all_pass <- mixed
  all_pass$passes_filtering <- TRUE

  names_of <- function(df) {
    built <- plotly::plotly_build(plot_quality_distribution(df))
    unlist(lapply(built$x$data, function(tr) tr$name))
  }

  testthat::expect_true(any(grepl("^Basecaller threshold", names_of(mixed))))
  testthat::expect_false(any(grepl("^Basecaller threshold", names_of(all_pass))))
})

testthat::test_that("plot_quality_distribution reference lines reach the tallest bar", {

  # 30 reads in one narrow Q range: the tallest stacked bar is far above 5%
  # of the read count
  set.seed(123) # reproducible simulated Q-scores
  test_df <- data.frame(
    sample_id = "S1", sequence_length_template = 1000,
    mean_qscore_template = c(rep(10.05, 20), rep(10.06, 10), 5),
    passes_filtering = c(rep(TRUE, 30), FALSE)
  )

  built <- plotly::plotly_build(plot_quality_distribution(test_df))
  bars <- Filter(function(tr) identical(tr$type, "bar"), built$x$data)
  stacked <- tapply(unlist(lapply(bars, function(tr) tr$y)),
                    unlist(lapply(bars, function(tr) tr$x)), sum)
  lines <- Filter(function(tr) identical(tr$mode, "lines"), built$x$data)

  testthat::expect_gte(min(unlist(lapply(lines, function(tr) max(tr$y)))),
                       max(stacked))
})
