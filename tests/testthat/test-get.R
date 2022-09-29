library(testthat)

test_that("get", {
  expect_tbl(
    pp_get_contacts(email = "andrie@rstudio.com"), nrow = 1
  )

  expect_tbl(
    pp_get_feedbacks(size = 7), nrow = 7, ncol = 25
  )

  expect_tbl(
    pp_get_ideas(.limit = 9, size = 100), nrow = 9, ncol = 21
  )

  expect_tbl(
    pp_get_products(), ncol = 14
  )

  expect_true(
    pp_get_products() %>% attr("count") > 0
  )


  expect_tbl(
    pp_get_idea_feedback("633051"), ncol = 24
  )

  expect_tbl(
    pp_get_companies(size = 11), nrow = 11, ncol = 11
  )

  expect_tbl(
    pp_get_contacts(size = 3), nrow = 3, ncol = 14
  )

  expect_tbl(
    pp_get_personas(), ncol = 8
  )

  expect_tbl(
    pp_get_users(), ncol = 8
  )

  expect_type(
    pp_get_user(90438), "list"
  )


})
