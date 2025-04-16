#' @importFrom tidyr unnest_wider
#' @importFrom tidyselect all_of
pp_unnest <- function(x, ...) {
  x |>
    dplyr::tibble() |>
    unnest_wider(1)
}


# old_pp_unnest <- function(x, names_sep = "_", .unnest_dont_unlist = NULL, ...) {
#   # unnest initial data frame
#   df <- tibble::tibble(x)
#   list_cols <- setdiff(names(x), .unnest_dont_unlist)
#   z <- unnest_wider(df, x, ...)

#   # browser()
#   # unnest list columns
#   list_cols <- z |> purrr::keep(is.list) |> names()
#   list_cols <- setdiff(list_cols, .unnest_dont_unlist)
#   if (length(list_cols) > 0) {
#     unnest_wider(z, all_of(list_cols), names_sep = names_sep)
#   } else {
#     z
#   }
# }

unnull_and_tibble <- function(x) {
  null_to_na <- function(x) if (length(x)) x else NA
  map(x, ~ map(., null_to_na)) |>
    map(as_tibble) |>
    list_rbind()
}


#' Unnest a list column of tibbles.
#'
#' This is frequently necessary to unnest `tags`, `products`, `feedbacks`, `personas`
#' and other columns embedded in prodpad data.
#'
#' This function converts the list column into a list of tibbles, then calls
#' `[tidyr::unnest_longer()]` followed by `[tidyr::unnest_wider()]`.
#' The effect is to hoist the entire list into the top level data.
#'
#' @param .data Tibble
#' @param .col Column name
#'
#' @export
#'
#' @return A modified tibble
#'
#' @importFrom tidyr unnest_longer unnest_wider
#' @importFrom rlang `:=`
#'
unnest_tibble <- function(.data, .col) {
  .data |>
    mutate({{ .col }} := {{ .col }} |> map(unnull_and_tibble)) |>
    unnest_longer({{ .col }}) |>
    unnest_wider({{ .col }}, names_sep = "_") |>
    identity()
}
