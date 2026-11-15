# Minimal helper - 3 channels, 2 hours, one run
make_heatmap_df <- function(platform = "minion") {
  data.frame(
    sample_id = "Heatmap_Sample",
    run_id = "Run1",
    channel = c(1L, 2L, 3L),
    start_time = c(0, 3600, 7200),
    duration = c(10, 10, 10),
    sequence_length_template = c(1000L, 2000L, 3000L)
  )
}

# -- Return type ---------------------------------------------------------------

testthat::test_that("pore_activity_heatmap returns a plotly object", {

  plt <- pore_activity_heatmap(make_heatmap_df(), platform = "minion")

  testthat::expect_s3_class(plt, "plotly")
})

# -- Platform routing ----------------------------------------------------------

testthat::test_that("pore_activity_heatmap accepts 'minion' platform without error", {

  testthat::expect_no_error(
    pore_activity_heatmap(make_heatmap_df(), platform = "minion")
  )
})

testthat::test_that("pore_activity_heatmap accepts 'promethion' platform without error", {

  testthat::expect_no_error(
    pore_activity_heatmap(make_heatmap_df(), platform = "promethion")
  )
})

# -- Title ---------------------------------------------------------------------

testthat::test_that("pore_activity_heatmap uses sample_id as title", {

  plt <- pore_activity_heatmap(make_heatmap_df(), platform = "minion")
  built <- plotly::plotly_build(plt)

  # subplot wraps two traces; title lives on the first sub-plot layout
  title_text <- built$x$layout$title$text

  testthat::expect_equal(title_text, "<b>Heatmap_Sample</b>")
})

# -- Subplot structure ---------------------------------------------------------

testthat::test_that("pore_activity_heatmap returns a subplot with two heatmap traces", {

  plt <- pore_activity_heatmap(make_heatmap_df(), platform = "minion")
  built <- plotly::plotly_build(plt)

  trace_types <- unlist(lapply(built$x$data, function(tr) tr$type))

  # both p1 and p2 are heatmap traces
  testthat::expect_equal(length(trace_types), 2L)
  testthat::expect_true(all(trace_types == "heatmap"))
})

# -- Validation errors ---------------------------------------------------------

testthat::test_that("pore_activity_heatmap errors on empty data frame", {

  testthat::expect_error(
    pore_activity_heatmap(data.frame()),
    "Empty data frame provided as an input"
  )
})

testthat::test_that("pore_activity_heatmap errors on missing 'channel' column", {

  df <- make_heatmap_df()
  df$channel <- NULL

  testthat::expect_error(
    pore_activity_heatmap(df),
    "Required columns missing: channel"
  )
})

testthat::test_that("pore_activity_heatmap errors on missing 'run_id' column", {

  df <- make_heatmap_df()
  df$run_id <- NULL

  testthat::expect_error(
    pore_activity_heatmap(df),
    "Required columns missing: run_id"
  )
})

testthat::test_that("pore_activity_heatmap errors on missing 'sample_id' column", {

  df <- make_heatmap_df()
  df$sample_id <- NULL

  testthat::expect_error(
    pore_activity_heatmap(df),
    "Required columns missing: sample_id"
  )
})

testthat::test_that("pore_activity_heatmap errors on missing 'sequence_length_template' column", {

  df <- make_heatmap_df()
  df$sequence_length_template <- NULL

  testthat::expect_error(
    pore_activity_heatmap(df),
    "Required columns missing: sequence_length_template"
  )
})

testthat::test_that("pore_activity_heatmap errors on invalid platform string", {

  testthat::expect_error(
    pore_activity_heatmap(make_heatmap_df(), platform = "gridion"),
    "Wrong platform specified"
  )
})

testthat::test_that("pore_activity_heatmap errors when multiple run_ids are present", {

  df <- make_heatmap_df()
  df$run_id <- c("Run1", "Run2", "Run1") # two distinct run_ids

  testthat::expect_error(
    pore_activity_heatmap(df),
    "Sequencing summary must contain only one run_id"
  )
})

# -- Data integrity ------------------------------------------------------------

testthat::test_that("pore_activity_heatmap keeps every base in both panels", {

  built <- plotly::plotly_build(pore_activity_heatmap(make_heatmap_df()))
  totals <- unlist(lapply(built$x$data, function(tr) sum(tr$z)))

  # 1000 + 2000 + 3000 bases, including the earliest read
  testthat::expect_equal(totals, c(6000, 6000))
})

testthat::test_that("pore_activity_heatmap keeps bases of runs shorter than 30 min", {

  df <- make_heatmap_df()
  df$start_time <- c(0, 10, 20)

  built <- plotly::plotly_build(pore_activity_heatmap(df))

  testthat::expect_equal(sum(built$x$data[[1]]$z), 6000)
})

testthat::test_that("pore_activity_heatmap places MinION channels at their layout cell", {

  df <- make_heatmap_df()
  df$channel <- c(1L, 33L, 1L)

  z <- plotly::plotly_build(pore_activity_heatmap(df))$x$data[[2]]$z

  # layout row 1 reads 33..40 followed by 8..1, so channel 33 is at (1, 1)
  # and channel 1 at (1, 16)
  testthat::expect_equal(z[1, 1], 2000)
  testthat::expect_equal(z[1, 16], 4000)
})

testthat::test_that("pore_activity_heatmap errors on channels outside the platform range", {

  df <- make_heatmap_df()
  df$channel <- c(1L, 2L, 2500L)

  testthat::expect_error(
    pore_activity_heatmap(df, platform = "minion"),
    "Channel ids must be between 1 and 512"
  )
})

testthat::test_that("pore_activity_heatmap places PromethION channels at their layout cell", {

  df <- make_heatmap_df()
  df$channel <- c(1L, 2500L, 3000L)

  z <- plotly::plotly_build(pore_activity_heatmap(df, platform = "promethion"))$x$data[[2]]$z

  # every base lands somewhere on the 3000-channel map, none is dropped
  testthat::expect_equal(dim(z), c(25, 120))
  testthat::expect_equal(sum(z), 6000)
  # channel 3000 is the last cell of the last chunk
  testthat::expect_equal(z[25, 120], 3000)
})

testthat::test_that("pore_activity_heatmap validates bin_width", {

  for (bad in list(0, -1, c(1, 2), "1", NA_real_)) {
    testthat::expect_error(
      pore_activity_heatmap(make_heatmap_df(), bin_width = bad),
      "Bin_width must be a single positive number of hours"
    )
  }
})

testthat::test_that("pore_activity_heatmap errors on NA or 0 channel ids", {

  df <- make_heatmap_df()
  df$channel <- c(1L, NA, 3L)
  testthat::expect_error(pore_activity_heatmap(df), "Channel ids must be between")

  df$channel <- c(1L, 0L, 3L)
  testthat::expect_error(pore_activity_heatmap(df), "Channel ids must be between")
})

testthat::test_that("pore_activity_heatmap errors on a missing, non-numeric or NA duration", {

  df <- make_heatmap_df()
  df$duration <- NULL
  testthat::expect_error(pore_activity_heatmap(df),
                         "Required columns missing: duration")

  df <- make_heatmap_df()
  df$duration <- factor(c("a", "b", "c"))
  testthat::expect_error(pore_activity_heatmap(df),
                         "Column duration must be numeric")

  df <- make_heatmap_df()
  df$duration[2] <- NA
  testthat::expect_error(pore_activity_heatmap(df),
                         "must not contain NA values")

  df <- make_heatmap_df()
  df$start_time[1] <- NA
  testthat::expect_error(pore_activity_heatmap(df),
                         "must not contain NA values")
})

testthat::test_that("pore_activity_heatmap errors when seq_summary is missing", {

  testthat::expect_error(
    pore_activity_heatmap(),
    "Sequencing summary is missing"
  )
})
