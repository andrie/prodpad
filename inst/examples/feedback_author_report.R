# remotes::install_github("andrie/prodpad")

library(tidyverse)
library(prodpad)
fb <- pp_get_feedbacks(.limit = 10000) # change .limit to a larger number, including Inf
fb |>
  filter(created_at >= Sys.Date() - 365) |>
  group_by(added_by_username) |>
  tally() |>
  arrange(-n) |>
  view("feedbacks") |>
  identity()
