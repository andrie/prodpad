library(testthat)

expect_tbl <- function(x, nrow = NULL, ncol = NULL) {
  expect_s3_class(x, "tbl")
  expect_true(ncol(x) > 0)
  if (!is.null(nrow)) expect_equal(nrow(x), nrow)
  if (!is.null(ncol)) expect_equal(ncol(x), ncol)
}

test_that("get", {
  expect_tbl(
    pp_get_contacts(email = "andrie@rstudio.com"), nrow = 1
  )

  expect_tbl(
    pp_get_feedbacks(size = 7), nrow = 7, ncol = 23
  )

  expect_tbl(
    pp_get_ideas(size = 9), nrow = 9, ncol = 20
  )

  expect_tbl(
    pp_get_products(), ncol = 13
  )

  expect_tbl(
    pp_get_ideas(size = 6), nrow = 6, ncol = 20
  )

  expect_tbl(
    pp_get_idea_feedback("633051"), ncol = 23
  )

})
