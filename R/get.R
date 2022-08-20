#' Get tibble of products.
#'
#' @export
#' @importFrom dplyr bind_cols select rename select_if
#' @importFrom purrr map_dfr
pp_get_products <- function() {
  res <- pp("/products", group = FALSE)
  p <- res$productlines

 p %>% map_dfr(~.$products %>% pp_unnest())

}

#' Get tibble of tags.
#'
#' @export
pp_get_tags <- function() {
  pp("/tags", .unnest = TRUE)
}

#' Get vector of tags
#'
#' @export
pp_get_tags_vector <- function() {
  all_tags <- pp_get_tags()
  as.list(rlang::set_names(all_tags[["tag_id"]], all_tags[["tag"]]))
}


#' Get list of ideas.
#'
#' @param tags One or more tag names to filter the ideas by. These act as an OR not AND
#'
#' @param product Name of a product to filter the ideas by.
#'
#' @param persona Name of a persona to filter the ideas by.
#'
#' @param status Name of a workflow status to filter the ideas by.
#'
#' @param state Filters the returned ideas based on their state. Active Public is same as portal in the UI. If not included then the endpoint returns all active and active_public the same as the UI.
#'
#' @param size The number of results per page
#'
#' @param ... Other arguments passed to [pp()]
#'
#' @export
#'
#' @importFrom tidyr hoist
pp_get_ideas <- function(
    tags = NULL,
    product = NULL,
    persona = NULL,
    status = NULL,
    state = c("active", "active_public", "archived"),
    size = 20,
    ...
) {
  state <- match.arg(state)

  pp("/ideas",
     tags = tags,
     product = product,
     persona = persona,
     status = status,
     state = state,
     size = size,
     ...,
     .unnest_element = "ideas"
  )
}


# in progress -------------------------------------------------------------


#' #' @export
#' get_idea_feedback <- function(client, id) {
#'   url <- glue::glue("/ideas/", as.character(id), "/feedback")
#'   rawdat <- client$GET(url)
#'
#'   return(rawdat)
# }




