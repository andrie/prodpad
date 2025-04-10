#' Get tibble of feedbacks.
#'
#' Returns a tibble of feedback which can be a straight list or grouped by
#' contact using the group_by parameter.  The list can also be filtered by state
#' query parameter for active, archived, unsorted or all.Note that by default
#' only active feedback is returned.
#'
#'
#' @param group_by Setting to customer will group the feedback by customer
#'
#' @param state Set to value of `active` for active, `archived` for archived
#'   feedback, `unsorted` for unsorted feedback and `all` for all feedback.
#'   Default is `all`.
#'
#' @param page Set to page through the results. Default is 1.
#'
#' @param size For setting the number of results per page. Default is 100.
#'
#' @param company Set to filter the feedback results based on whether the
#'   feedback was entered for a contact linked to the company.
#'
#' @param company_country Set to filter the feedback results based on the
#'   country set for the associated company.
#'
#' @param company_size Set to filter the feedback results based on the size of
#'   the company for the associated company.
#'
#' @param company_value Set to filter the feedback results based on the value of
#'   the company for the company associated to the feedback.
#'
#' @param customer Can be either numeric ID, UUID of a contact or contact email.
#'
#' @param product Filter results by the product associated to feedback. Can
#'   either be the product UUID or product ID.
#'
#' @param persona Filter results by the persona associated to feedback. Can
#'   either be the persona UUID or persona ID.
#'
#' @param job_role Filter results by the job role of the contact associated to
#'   the feedback. Use the JobRole UUID.
#'
#' @param tags Filter feedback results by the tags associated to the feedback.
#'   Multiple tags can be specified and acts as an OR. Use the tag ID or UUID.
#'
#' @param has_ideas Whether the feedback is associated to one or more ideas.
#'   Default is either.
#'
#' @param external_id Filter feedback to return the feedback associated with a
#'   specific External ID. An example of an external ID is the ID of a record in
#'   a CRM or ID of a ticket in a customer support application.
#'
#' @param external_url Filter feedback to return the feedback associated with a
#'   specific external url. An example of an external url is that of a record in
#'   a CRM or a ticket in a customer support application
#'
#' @param .limit The maximum number of elements to return
#'
#' @param ... Other arguments passed to [.pp()]()]
#'
#' @note GET /feedbacks
#'
#' @return tibble
#'
#' @export
pp_get_feedbacks <- function(
  state = c("all", "active", "archived", "unsorted"),
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
  .limit = NULL,
  ...
) {
  "state" <- match.arg(state)

  get_one <- function(page, size) {
    .pp(
      "get /feedbacks",
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

  page_all_requests(get_one, page = page, size = size, .limit = .limit)
}


#' Get a piece of feedback.
#'
#' Return an individual piece of feedback. The feedback is returned with the
#' details of the contact that provided the piece of feedback.
#'
#'
#' @param id Feedback ID.
#'
#' @param ... Other arguments passed to [.pp()]()]
#'
#' @note GET /feedbacks/{id}
#'
#' @export
pp_get_feedback <- function(
  id = NULL,
  ...
) {
  .pp("get /feedbacks/{id}", id = id, ... = ..., .unnest_element = NULL) |>
    c()
}


# post --------------------------------------------------------------------

#' Create a new feedback.
#'
#' Create a new feedback in the account. The feedback has to have either a name
#' or an email and the actual feedback. The email is used to add the feedback to
#' an existing contact.
#'
#' Fields to create a feedback. Note either the contact ID or contact name is
#' required. Otherwise a 400 error will be returned. If a name and email field
#' are used, the value in the email field will be used to see if the contact
#' already exists and if it does the feedback will be automatically added to the
#' existing contact.
#'
#'
#' @param contact_id	`string($uuid)` ID of the contact providing the feedback.
#'   Either Contact ID or Contact name is required.
#'
#' @param contact_name	`string` Name of the contact providing the feedback. Either
#'   Contact ID or Contact name is required.
#'
# @param company_id	`string($uuid)` UUID of the company to link the contact to.
#   The UUID can be determined using /GET companies endpoint.
#
#' @param product_id Product id. See [pp_get_products()]
#'
#' @param product_name Product name. See [pp_get_products()]
#'
#' @param feedback	`string` (Required) The feedback. This field accepts HTML and
#'   is stored as UTF-8.
#'
# @param email	`string` The email of the contact. This is used to avoid
#   duplication of contacts. This can be any unique ID for each contact.
#
# @param about	`string` Text field about the contact. This will overwrite the
#   existing about if the contact already exists. This field accepts HTML and
#   is stored as UTF-8.
#'
#' @param ... Other arguments passed to [.pp()]()]
#'
#' @note POST /feedbacks
#'
#' @export
pp_post_feedback <- function(
  contact_id,
  contact_name = NULL,
  product_id,
  product_name = NA,
  feedback,
  # company_id = NULL,
  ...
) {
  feedback <- as.character(feedback)
  products = data.frame(id = product_id, name = product_name)

  .pp(
    "POST /feedbacks",
    contact_id = contact_id,
    name = contact_name,
    # company_id = company_id,
    feedback = feedback,
    # email = email,
    # about = about,
    products = products,
    ... = ...,
    .send_headers = c(`Content-Type` = "application/json")
  ) |>
    c()
}

#' Archive an existing piece of feedback.
#'
#' Use this endpoint to edit the details of an existing piece of feedback
#' including the feedback, the status and external links to add (for example
#' link to a record in the CRM, other 3rd party application or video of the
#' customer interview).
#'
#'
#' @param id Feedback ID
#'
#' @param ... Other arguments passed to [.pp()]()]
#'
#' @note PUT /feedbacks/{id}
#'
#' @export
pp_archive_feedback <- function(
  id = NULL,
  ...
) {
  .pp(
    "PUT /feedbacks/{id}",
    id = id,
    state = "archived",
    ... = ...,
    .send_headers = c(`Content-Type` = "application/json")
  )
}
