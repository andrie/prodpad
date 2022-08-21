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
unnest_tibble <- function(.data, .col) {
  .data %>%
    mutate({{ .col }} := {{ .col }} %>%  map(~map_dfr(., as_tibble))) %>%
    unnest_longer({{ .col }}) %>%
    unnest_wider({{ .col }}, names_sep = "_")
}
