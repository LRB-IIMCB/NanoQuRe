testthat::test_that("assert_condition stops with the given message", {

  testthat::expect_error(
    NanoQuRe:::assert_condition(FALSE, "Value must be positive"),
    "Value must be positive"
  )

  # default message
  testthat::expect_error(
    NanoQuRe:::assert_condition(FALSE),
    "Assertion failed"
  )
})

testthat::test_that("assert_condition stops on anything that is not a single TRUE", {

  # NA, NULL, vectors and non-logical values are not treated as a pass
  for (bad in list(NA, NULL, c(TRUE, TRUE), logical(0), "TRUE", 1)) {
    testthat::expect_error(
      NanoQuRe:::assert_condition(bad, "Condition not met"),
      "Condition not met"
    )
  }
})

testthat::test_that("assert_condition reports the error without the call", {

  err <- tryCatch(NanoQuRe:::assert_condition(FALSE, "No call expected"),
                  error = function(e) e)

  testthat::expect_null(conditionCall(err))
})

testthat::test_that("assert_condition returns TRUE invisibly when the condition holds", {

  testthat::expect_invisible(NanoQuRe:::assert_condition(TRUE))
  testthat::expect_true(NanoQuRe:::assert_condition(TRUE))
  testthat::expect_true(NanoQuRe:::assert_condition(1 + 1 == 2, "Not reached"))
})

testthat::test_that("is_string accepts a single non-empty character string", {

  testthat::expect_true(NanoQuRe:::is_string("minion"))
  testthat::expect_true(NanoQuRe:::is_string("a"))
})

testthat::test_that("is_string rejects non-strings, vectors, NA and empty strings", {

  testthat::expect_false(NanoQuRe:::is_string(1))
  testthat::expect_false(NanoQuRe:::is_string(TRUE))
  testthat::expect_false(NanoQuRe:::is_string(factor("minion")))
  testthat::expect_false(NanoQuRe:::is_string(c("minion", "promethion")))
  testthat::expect_false(NanoQuRe:::is_string(character(0)))
  testthat::expect_false(NanoQuRe:::is_string(NA_character_))
  testthat::expect_false(NanoQuRe:::is_string(NA))
  testthat::expect_false(NanoQuRe:::is_string(""))
  testthat::expect_false(NanoQuRe:::is_string(list("minion")))
})

testthat::test_that("is_string respects min.chars", {

  testthat::expect_true(NanoQuRe:::is_string("", min.chars = 0))
  testthat::expect_true(NanoQuRe:::is_string("abc", min.chars = 3))
  testthat::expect_false(NanoQuRe:::is_string("ab", min.chars = 3))
})

testthat::test_that("is_string handles NULL according to null.ok", {

  testthat::expect_false(NanoQuRe:::is_string(NULL))
  testthat::expect_true(NanoQuRe:::is_string(NULL, null.ok = TRUE))
})
