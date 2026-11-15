# Minimal sample: 2 pass reads, 1 fail read, 2 channels, 2 hours
make_caption_df <- function() {
  data.frame(
    sample_id = "Caption_Sample",
    channel = c(1L, 2L, 1L),
    start_time = c(0, 100, 3700),
    duration = c(10, 10, 10),
    sequence_length_template = c(1000, 3000, 2000),
    mean_qscore_template = c(12, 14, 6),
    passes_filtering = c(TRUE, TRUE, FALSE)
  )
}

testthat::test_that("caption_numbers reports values computed from the data", {

  df <- make_caption_df()

  testthat::expect_equal(
    NanoQuRe:::caption_numbers("read_lengths", df),
    "3 reads; mean length 2,000 nt; N50 3,000 nt; longest read 3,000 nt."
  )
  testthat::expect_match(
    NanoQuRe:::caption_numbers("cumulative_yield", df),
    "(66.7%) in pass reads", fixed = TRUE
  )
  testthat::expect_equal(
    NanoQuRe:::caption_numbers("channels", df),
    "2 of 512 channels produced at least one read during the run."
  )
  testthat::expect_match(
    NanoQuRe:::caption_numbers("heatmap", df, platform = "promethion"),
    "2,998 of 3,000 channels", fixed = TRUE
  )
})

testthat::test_that("caption_numbers prints round values without scientific notation", {

  # 100000 reads used to be printed as "1e+05"
  df <- data.frame(
    sample_id = "Round_Sample",
    channel = 1L,
    start_time = 0,
    duration = 1,
    sequence_length_template = rep(1000, 100000),
    mean_qscore_template = 10,
    passes_filtering = TRUE
  )

  testthat::expect_match(
    NanoQuRe:::caption_numbers("read_lengths", df),
    "100,000 reads", fixed = TRUE
  )
})

testthat::test_that("caption_numbers handles a sample without fail reads", {

  df <- make_caption_df()
  df$passes_filtering <- TRUE

  speed <- NanoQuRe:::caption_numbers("speed", df)

  testthat::expect_match(speed, "fail reads none", fixed = TRUE)
  testthat::expect_false(grepl("NaN|Inf", speed))
})

testthat::test_that("caption_numbers pins the text of the data-derived sections", {

  df <- make_caption_df()

  # pass reads 1000 + 3000 nt over 20 s, fail read 2000 nt over 10 s
  testthat::expect_equal(
    NanoQuRe:::caption_numbers("quality", df),
    "Median read Q-score: pass 13.0, fail 6.0; lowest Q-score among pass reads: 12.0."
  )
  testthat::expect_equal(
    NanoQuRe:::caption_numbers("length_qscore", df),
    "Median read length 2,000 nt; median read Q-score 12.0."
  )
  # hour 0 holds 1000 + 3000 nt, hour 1 holds 2000 nt
  testthat::expect_equal(
    NanoQuRe:::caption_numbers("throughput", df),
    "Highest hourly yield 0.000004 Gb in hour 0; last hour (1) 0.000002 Gb (pass and fail together)."
  )
  testthat::expect_equal(
    NanoQuRe:::caption_numbers("speed", df),
    "Over the whole run: pass reads 200 nt/s, fail reads 200 nt/s."
  )
})

testthat::test_that("caption_numbers speed is robust to a zero total duration", {

  df <- make_caption_df()
  df$duration <- 0

  testthat::expect_equal(
    NanoQuRe:::caption_numbers("speed", df),
    "Over the whole run: pass reads none, fail reads none."
  )
})

testthat::test_that("every report section has a description and values", {

  sections <- c("read_lengths", "cumulative_yield", "quality", "speed",
                "throughput", "channels", "length_qscore", "heatmap")

  for (section in sections) {
    testthat::expect_true(nchar(NanoQuRe:::caption_description(section)) > 0)
    testthat::expect_true(nchar(NanoQuRe:::caption_numbers(section, make_caption_df())) > 0)
  }
})

testthat::test_that("caption functions error on an unknown section", {

  testthat::expect_error(NanoQuRe:::caption_description("nope"), "Unknown report section")
  testthat::expect_error(NanoQuRe:::caption_numbers("nope", make_caption_df()), "Unknown report section")
})
