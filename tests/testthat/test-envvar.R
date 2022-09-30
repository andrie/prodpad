test_that("error message if PRODPAD_API_KEY env var is not set", {
  old <- Sys.getenv("PRODPAD_API_KEY", unset = NA)
  on.exit(Sys.setenv("PRODPAD_API_KEY" = old))
  Sys.unsetenv("PRODPAD_API_KEY")
  expect_error(
    pp_get_products()
  )
})

