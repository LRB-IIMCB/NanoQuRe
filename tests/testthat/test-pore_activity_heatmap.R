# Minimal helper — 3 channels, 2 hours, one run
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

# ── Return type ───────────────────────────────────────────────────────────────

testthat::test_that("pore_activity_heatmap returns a plotly object", {
  
  plt <- pore_activity_heatmap(make_heatmap_df(), platform = "minion")
  
  testthat::expect_s3_class(plt, "plotly")
})

# ── Platform routing ──────────────────────────────────────────────────────────

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

# ── Title ─────────────────────────────────────────────────────────────────────

testthat::test_that("pore_activity_heatmap uses sample_id as title", {
  
  plt <- pore_activity_heatmap(make_heatmap_df(), platform = "minion")
  built <- plotly::plotly_build(plt)
  
  # subplot wraps two traces; title lives on the first sub-plot layout
  title_text <- built$x$layout$title$text
  
  testthat::expect_equal(title_text, "<b>Heatmap_Sample</b>")
})

# ── Subplot structure ─────────────────────────────────────────────────────────

testthat::test_that("pore_activity_heatmap returns a subplot with two heatmap traces", {
  
  plt <- pore_activity_heatmap(make_heatmap_df(), platform = "minion")
  built <- plotly::plotly_build(plt)
  
  trace_types <- vapply(built$x$data, function(tr) tr$type, character(1))
  
  # both p1 and p2 are heatmap traces
  testthat::expect_equal(length(trace_types), 2L)
  testthat::expect_true(all(trace_types == "heatmap"))
})

# ── Validation errors ─────────────────────────────────────────────────────────

testthat::test_that("pore_activity_heatmap errors on empty data frame", {
  
  testthat::expect_error(
    pore_activity_heatmap(data.frame()),
    "The input data frame is empty"
  )
})

testthat::test_that("pore_activity_heatmap errors on missing 'channel' column", {
  
  df <- make_heatmap_df()
  df$channel <- NULL
  
  testthat::expect_error(
    pore_activity_heatmap(df),
    "The data frame is missing the 'channel' column"
  )
})

testthat::test_that("pore_activity_heatmap errors on missing 'run_id' column", {
  
  df <- make_heatmap_df()
  df$run_id <- NULL
  
  testthat::expect_error(
    pore_activity_heatmap(df),
    "The data frame is missing the 'run_id' column"
  )
})

testthat::test_that("pore_activity_heatmap errors on missing 'sample_id' column", {
  
  df <- make_heatmap_df()
  df$sample_id <- NULL
  
  testthat::expect_error(
    pore_activity_heatmap(df),
    "The data frame is missing the 'sample_id' column"
  )
})

testthat::test_that("pore_activity_heatmap errors on missing 'sequence_length_template' column", {
  
  df <- make_heatmap_df()
  df$sequence_length_template <- NULL
  
  testthat::expect_error(
    pore_activity_heatmap(df),
    "The data frame is missing the 'sequence_length_template' column"
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