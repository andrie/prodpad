library(tidyverse)
test_that("unnest works", {
  fb <- pp_get_feedbacks(.limit = 200)
  expect_equal(nrow(fb), 200)

  ufb <-
    fb %>%
    select(id, tags) %>%
    unnest_tibble(tags)
  expect_tbl(ufb, ncol = 5)

  ufb <-
    fb %>%
    select(id, ideas) %>%
    unnest_tibble(ideas)
  expect_tbl(ufb, ncol = 4)

  ufb <-
    fb %>%
    select(id, products) %>%
    unnest_tibble(products)
  expect_tbl(ufb, ncol = 5)


})
