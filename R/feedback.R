
#' Get tibble of feedbacks.
#'
#' Returns a tibble of feedback which can be a straight list or grouped by contact using the group_by parameter.  The list can also be filtered by state query parameter for active, archived, unsorted or all.Note that by default only active feedback is returned.
#'
#'
#' @param group_by Setting to customer will group the feedback by customer
#'
#' @param state Set to value of `active` for active, `archived` for archived feedback, `unsorted` for unsorted feedback and `all` for all feedback. Default is `active`.
#'
#' @param page Set to page through the results. Default is 1.
#'
#' @param size For setting the number of results per page. Default is 100.
#'
#' @param company Set to filter the feedback results based on whether the feedback was entered for a contact linked to the company.
#'
#' @param company_country Set to filter the feedback results based on the country set for the associated company.
#'
#' @param company_size Set to filter the feedback results based on the size of the company for the associated company.
#'
#' @param company_value Set to filter the feedback results based on the value of the company for the company assocaited to the feedback.
#'
#' @param customer Can be either numeric ID, UUID of a contac or contact email.
#'
#' @param product Filter results by the product associated to feedback. Can either be the product UUID or product ID.
#'
#' @param persona Filter results by the persona associated to feedback. Can either be the persona UUID or persona ID.
#'
#' @param job_role Filter results by the job role of the contact associated to the feedback. Use the JobRole UUID.
#'
#' @param tags Filter feedback results by the tags associated to the feedback. Mulitple tags can be specified and acts as an OR. Use the tag ID or UUID.
#'
#' @param has_ideas Whether the feedback is associated to one or more ideas. Default is either.
#'
#' @param external_id Filter feedback to return the feedback associated with a specific External ID. An example of an external ID is the ID of a record in a CRM or ID of a ticket in a customer support application.
#'
#' @param external_url Filter feedback to return the feedback associated with a specific external url. An example of an external url is that of a record in a CRM or a ticket in a customer support application
#'
#' @param ... Other arguments passed to [pp()]
#'
#' @note GET /feedbacks
#'
#' @return tibble
#'
#' @export
pp_get_feedbacks <- function(
    state = c("active", "archived", "unsorted", "all"),
    company = NULL,
    company_country = NULL,
    company_size = NULL,
    company_value = NULL,
    customer = NULL,
    product = NULL,
    persona = NULL,
    job_role = NULL,
    tags = NULL,
    has_ideas = NULL,
    external_id = NULL,
    external_url = NULL,
    group_by = FALSE,
    page = 1,
    size = 100,
    ...
) {
  "state" <- match.arg(state)
  pp("get /feedbacks",
     group_by = group_by,
     state = state,
     page = page,
     size = size,
     company = company,
     company_country = company_country,
     company_size = company_size,
     company_value = company_value,
     customer = customer,
     product = product,
     persona = persona,
     job_role = job_role,
     tags = tags,
     has_ideas = has_ideas,
     external_id = external_id,
     external_url = external_url,
     ... = ...,
     .unnest = TRUE,
     .unnest_dont_unlist = c("tags", "ideas", "products", "personas")
  )
}

