if (require("R6", quietly = TRUE)) {
  Pager <- R6::R6Class(
    "Pager",
    public = list(
      n = 0,
      orig = 0,
      pointer = 1,
      initialize = function(n) {
        self$orig <- n
        self$n <- n
      },
      call = function(n = 0) {
        self$n <- self$n - n
        self$n <- max(0, self$n)
        self$n
      },
      call_df = function(n = 0) {
        size = min(n, self$n)
        self$n <- self$n - size
        series <- seq(from = self$pointer, length.out = size)
        self$pointer <- self$pointer + size
        data.frame(
          i = series
        )
      }
    )
  )
}


test_that("pager class works", {
  skip_if_not_installed("R6")

  p <- Pager$new(10)
  expect_equal(p$call(), 10)
  expect_equal(p$call(4), 6)
  expect_equal(p$call(7), 0)

  p <- Pager$new(10)
  expect_equal(nrow(p$call_df(3)), 3)
  expect_equal(nrow(p$call_df(4)), 4)
  expect_equal(nrow(p$call_df(4)), 3)
  expect_equal(nrow(p$call_df(4)), 0)
})


test_that("page_all_requests works", {
  skip_if_not_installed("R6")

  p <- Pager$new(10)

  get_one <- function(page, size) {
    p$call_df(size)
  }

  expect_equal(
    nrow(page_all_requests(get_one, page = 4, size = 10, .limit = 7)),
    7
  )
  expect_equal(
    nrow(page_all_requests(get_one, page = 4, size = 10, .limit = 7)),
    3
  )
})


test_that("paging works on ideas", {
  skip_if_not_installed("R6")
  expect_tbl(pp_get_ideas(size = 11, .limit = 20), nrow = 20)
})

test_that("paging works on companies", {
  skip_if_not_installed("R6")
  expect_tbl(pp_get_companies(size = 10, .limit = 20), nrow = 20)
})
