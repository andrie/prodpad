
expect_tbl <- function(x, nrow = NULL, ncol = NULL) {
  expect_s3_class(x, "tbl")
  expect_true(ncol(x) > 0)
  if (!is.null(nrow)) expect_equal(nrow(x), nrow)
  if (!is.null(ncol)) expect_equal(ncol(x), ncol)
}
