#' Browse the ProdPad API docs at swaggerhub.
#'
#' @param version Version number
#'
#' @export
browse_api_docs <- function(version = "1.0") {
  glue("https://app.swaggerhub.com/apis-docs/ProdPad/prodpad/{version}/") %>%
  utils::browseURL()
}

#' Browse ProdPad and open the API key page
#'
#' @export
browse_api_key <- function() {
  utils::browseURL("https://app.prodpad.com/me/apikeys")
}

