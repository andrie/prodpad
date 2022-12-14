unnull_and_tibble <- function(x) {
  null_to_na <- function(x) if(length(x)) x else NA
  map(x, ~map(., null_to_na)) %>% map_dfr(as_tibble)
}


#' Unnest a list column of tibbles.
#'
#' This is frequently necessary to unnest `tags`, `products`, `feedbacks`, `personas` and other columns embedded in prodpad data.
#'
#' This function converts the list column into a list of tibbles, then calls `[tidyr::unnest_longer()]` followed by `[tidyr::unnest_wider()]`.  The effect is to hoist the entire list into the top level data.
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
unnest_tibble <- function(.data, .col) {
  .data %>%
    mutate({{ .col }} := {{ .col }} %>%  map(unnull_and_tibble)) %>%
    unnest_longer({{ .col }}) %>%
    unnest_wider({{ .col }}, names_sep = "_") %>%
    identity()
}


