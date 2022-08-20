
#' Get tibble of feedback.
#'
#' @param state Set to value of active for active, archived for archived feedback, unsorted for unsorted feedback and all for all feedback. Default is active
#'
#' @param product Filter results by the product associated to feedback. Can either be the product UUID or product ID
#'
#' @param customer Can be either numeric ID, UUID of a contact or contact email
#'
#' @param company Set to filter the feedback results based on whether the feedback was entered for a contact linked to the company.
#'
#' @param ... Other arguments passed to [pp()]
#'
#' @export
pp_get_feedback <- function(
  state = c("active", "archived", "unsorted", "all"),
  product = NULL,
  customer = NULL,
  company = NULL,
  ...

) {
  arguments <- call_args(current_call())
  arguments["state"] <- match.arg(state)
  inject(
    pp("/feedbacks", !!!arguments, .unnest = TRUE,
       .unnest_dont_unlist = c("tags", "ideas", "products", "personas"))
  )
}
