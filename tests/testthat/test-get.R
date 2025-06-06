library(testthat)

test_that("get", {
  expect_tbl(
    pp_get_contacts(email = "andrie@rstudio.com"),
    nrow = 1
  )

  expect_tbl(
    pp_get_feedbacks(size = 7),
    nrow = 7,
    ncol = 17
  )

  expect_tbl(
    pp_get_ideas(.limit = 9, size = 100),
    nrow = 9,
    ncol = 20
  )

  expect_tbl(
    pp_get_products(),
    ncol = 11
  )

  expect_true(
    pp_get_products() |> attr("count") > 0
  )

  expect_tbl(
    pp_get_idea_feedback("633051"),
    ncol = 17
  )

  expect_tbl(
    pp_get_companies(size = 9),
    nrow = 9,
    ncol = 11
  )

  expect_tbl(
    pp_get_contacts(size = 3),
    nrow = 3,
    ncol = 14
  )

  expect_tbl(
    pp_get_contacts(size = 3, .limit = 6),
    nrow = 6,
    ncol = 14
  )

  expect_tbl(
    pp_get_personas(),
    ncol = 8
  )

  expect_tbl(
    pp_get_users(),
    ncol = 9
  )

  expect_type(
    pp_get_user(90438),
    "list"
  )
})
