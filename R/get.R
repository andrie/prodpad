# products ----------------------------------------------------------------

#' Get tibble of products.
#'
#' @family Products
#' @export
#' @importFrom dplyr bind_cols select rename select_if
#' @importFrom purrr map list_rbind
#' @importFrom magrittr set_attr
pp_get_products <- function() {
  res <- .pp("/products", group = FALSE)
  count <- attr(res, "count")
  res$productlines |>
    map(~ .$products |> pp_unnest()) |>
    list_rbind() |>
    set_attr("count", count)
}


# tags --------------------------------------------------------------------

#' Get tibble of products.
#'
#' @family Products
#' @export
pp_get_products_vector <- function() {
  pp_get_products() |>
    get_id_vector()
}

#' Get tibble of tags.
#'
#' @family Tags
#' @export
pp_get_tags <- function() {
  .pp("/tags", .unnest = TRUE)
}

#' Get vector of tags
#'
#' @family Tags
#' @export
pp_get_tags_vector <- function() {
  all_tags <- pp_get_tags()
  as.list(rlang::set_names(all_tags[["tag_id"]], all_tags[["tag"]]))
}


# ideas -------------------------------------------------------------------

#' Get list of ideas.
#'
#' @param tags One or more tag names to filter the ideas by. These act as an OR
#'   not AND
#'
#' @param product Name of a product to filter the ideas by.
#'
#' @param persona Name of a persona to filter the ideas by.
#'
#' @param status Name of a workflow status to filter the ideas by.
#'
#' @param state Filters the returned ideas based on their state. Active Public
#'   is same as portal in the UI. If not included then the endpoint returns all
#'   active and active_public the same as the UI. "all" returns all unsorted and
#'   active ideas.
#'
#' @param page Page of results to return
#'
#' @param size The number of results per page
#'
#' @param .limit The maximum number of elements to return.
#'
#' @param ... Other arguments passed to [.pp()]()]
#'
#' @export
#' @family Ideas
#'
#' @importFrom tidyr hoist
pp_get_ideas <- function(
  tags = NULL,
  product = NULL,
  persona = NULL,
  status = NULL,
  state = c("all", "active", "active public", "archived", "unsorted"),
  page = 1,
  size = 100,
  .limit = 100,
  ...
) {
  if (!is.null(state)) state <- match.arg(state)
  if (size > .limit) size <- .limit

  get_one <- function(page, size) {
    .pp(
      "/ideas",
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

  # get_one(page = page, size = size, .limit = .limit)
  page_all_requests(get_one, page = page, size = size, .limit = .limit, ...)
}


#' Get an idea.
#'
#' Use the numeric ID to return an idea. You can get a simple version of the idea or an expanded one using the expanded parameter which returns the business case, functional specs, notes, user stories, comments and other linked data.
#'
#' If you have the project_id (the numerical number from the ProdPad UI in the idea canvas url) you can use that to fetch the idea by using the query parameter by_project_id, otherwise you need to use the global numerical ID.
#'
#'
#' @param id Numeric ID of the idea unless using the by_project_id then it is the numeric project ID (the Idea number seen in the UI).
#'
#' @param expand Whether to return the expanded version of the idea object. This adds in business case, user stories, comments, etc.
#'
#' @param by_project_id Whether the ID represents the project_id instead of the numeric ID.
#'
#' @param ... Other arguments passed to [.pp()]()]
#'
#' @note `GET /ideas/{id}`
#'
#' @export
pp_get_idea <- function(
  id = NULL,
  expand = FALSE,
  by_project_id = FALSE,
  ...
) {
  .pp(
    "/ideas/{id}",
    id = id,
    expand = isTRUE(expand),
    by_project_id = isTRUE(by_project_id),
    ... = ...,
    .unnest = FALSE
  ) |>
    c() # drops attributes
}


#' Get feedback associated to an idea.
#'
#' Returns a tibble of all the feedback that is associated to the idea. This allows you to display the feedback in other apps for a particular idea or sync feedback between ProdPad and other applications.
#'
#'
#' @param id Numeric ID of the idea.
#'
#' @inherit pp_get_ideas
#'
#' @note `GET /ideas/{id}/feedback`
#' @family Ideas
#'
#' @export
pp_get_idea_feedback <- function(
  id,
  ...
) {
  .pp(
    "/ideas/{id}/feedback",
    id = id,
    ... = ...,
    .unnest = TRUE,
    .unnest_dont_unlist = c("tags", "products", "personas")
  )
}


# companies ---------------------------------------------------------------

#' Get a tibble of companies.
#'
#' Return a list of companies. Companies are a collection of contacts that are providing feedback. Using companies allows you to group feedback from the same organisational source.
#'
#' While very useful for business to busines,s companies can be used to create a any sort grouping of contacts for example by a conference or a cohort.
#'
#' Using parameters allows you to also select whether to return feedback associated to the company via the contacts connected to the company. This is only available to accounts with an Advanced+ subscription.
#'
#'
#' @param country Set to filter the companies based on the country. Use ISO Alpha-2 country codes. Only one country can be filtered at a time.
#'
#' @param company_size Set to filter the companies based on their size.
#'
#' @param value Set to filter the companies based on their value.
#'
#' @param city Set to filter the companies based on city.
#'
#' @param tags Filter companies by the tags associated to the feedback. Mulitple tags can be specified and acts as an OR. Use the tag ID or UUID.
#'
#' @param name Filter the companies by the name or partial name of the companies.
#'
#' @param external_id Filter the companies by an ID from a 3rd party application associated to the companies in Prodpad
#'
#' @param external_url Filter the companies by a URL that is associated to a company.
#'
#' @param contacts Whether to include contacts associated with each company in the results. Default is true.
#'
#' @param feedbacks Whether to include the feedback for each contact associated to a company in the results. Default is true. Note if this is set to true then contacts will be returned whether or not contacts is set to true or false.
#'
#' @param page The page of results to return (size is always 100).
#'
#' @param .debug If TRUE, prints debug messages
#'
#' @inherit pp_get_ideas
#'
#' @note `GET /companies`
#' @family Companies
#'
#' @export
pp_get_companies <- function(
  name = NULL,
  country = NULL,
  company_size = NULL,
  value = NULL,
  city = NULL,
  tags = NULL,
  external_id = NULL,
  external_url = NULL,
  contacts = FALSE,
  feedbacks = FALSE,
  page = 1,
  size = 100,
  .limit = size,
  ...
) {
  if (size > .limit) size <- .limit
  if (isTRUE(contacts)) contacts <- "true"
  if (isTRUE(feedbacks)) feedbacks <- "true"
  get_page <- function(page, size) {
    .pp(
      "/companies",
      country = country,
      company_size = company_size,
      value = value,
      city = city,
      tags = tags,
      name = name,
      external_id = external_id,
      external_url = external_url,
      contacts = contacts,
      feedbacks = feedbacks,
      page = page,
      size = size,
      ... = ...,
      .unnest_element = "companies",
      .debug = FALSE
    )
  }
  page_all_requests(
    get_page,
    page = page,
    size = size,
    .limit = .limit,
    .debug = .debug
  )
}


pp_get_company <- function(id) {
  id <- URLencode(id)
  .pp(
    "GET /companies/{id}",
    id = id
  ) |>
    c()
}


#' Get named vector of companies.
#'
#' @export
#' @family Companies
#'
#' @return Named vector
pp_get_companies_vector <- function() {
  pp_get_companies() |>
    get_id_vector()
}

# contacts ----------------------------------------------------------------

#' Get a tibble of contacts.
#'
#' Get a tibble of all the contacts in the account. This can be used to sync contacts between ProdPad and CRMs.
#'
#'
#' @param company UUID of a company to filter contacts by.
#'
#' @param persona ID of a persona to filter contacts by.
#'
#' @param job_role UUID of a job role to filter contacts by.
#'
#' @param tags ID, UUID or name of one or more tags to filter the contacts by.
#'
#' @param name Name of contact or partial name of contacts to filter the list by
#'
#' @param external_id Filter the contacts by an ID from another application such as a CRM
#'
#' @param external_url Filter the contacts by a URL from another application such as a CRM
#'
#' @param email Filter the contacts by an email.
#'
#' @param feedbacks Wheter to include the feedback for each contact in the results. Default is false.
#'
#' @inherit pp_get_ideas
#'
#' @family Contacts
#' @note `GET /contacts`
#'
#' @export
pp_get_contacts <- function(
  name = NULL,
  email = NULL,
  company = NULL,
  persona = NULL,
  job_role = NULL,
  tags = NULL,
  external_id = NULL,
  external_url = NULL,
  feedbacks = NULL,
  page = 1,
  size = 100,
  .limit = size,
  ...
) {
  if (size > .limit) size <- .limit
  get_page <- function(page, size) {
    .pp(
      "/contacts",
      company = company,
      persona = persona,
      job_role = job_role,
      tags = tags,
      name = name,
      external_id = external_id,
      external_url = external_url,
      email = email,
      feedbacks = feedbacks,
      page = page,
      size = size,
      ... = ...,
      .unnest_element = "contacts"
    )
  }
  page_all_requests(get_page, page = page, size = size, .limit = .limit)
}


#' Get named vector of contacts
#'
#' @export
#' @family Contacts
#'
#' @return Named vector
pp_get_contacts_vector <- function() {
  pp_get_contacts() |>
    get_id_vector()
}


#' Get a contact.
#'
#' Returns the details of a contact. This will include a contacts PII (such as name and email) so be aware of the privacy of the individual when using this endpoint.
#'
#' You can choose to include the feedback provided by the contact or not.
#'
#'
#' @param id Contact ID to fetch.
#'
#' @param feedbacks Whether to include the feedback provided by the contact in the response or not.
#'
#' @param ... Other arguments passed to [.pp()]()]
#'
#' @note `GET /contacts/{id}`
#'
#' @export
pp_get_contact <- function(
  id = NULL,
  feedbacks = NULL,
  ...
) {
  .pp(
    "get /contacts/{id}",
    id = id,
    feedbacks = feedbacks,
    ... = ...,
    .unnest_element = NULL
  ) |>
    c()
}


# personas ----------------------------------------------------------------

#' Get tibble of personas.
#'
#' @param ... Other arguments passed to [.pp()]()]
#'
#' @note `GET /personas`
#'
#' @family Persona
#' @export
pp_get_personas <- function(
  ...
) {
  .pp("get /personas", ... = ..., .unnest = TRUE)
}

#' Get named vector of personas.
#'
#' @export
#'
#' @family Persona
#' @return Named vector
pp_get_personas_vector <- function() {
  pp_get_personas() |>
    get_id_vector()
}

# users -------------------------------------------------------------------

#' Get a list of users.
#'
#' This endpoint returns a list of users with roles in the account.
#'
#' @param ... Other arguments passed to [.pp()]()]
#'
#' @note `GET /users`
#'
#' @family User
#' @export
#' @return Tibble with user information
pp_get_users <- function(
  ...
) {
  .pp("get /users", ... = ..., .unnest = TRUE)
}


#' Get a user.
#'
#' Return the details on the user and their role.
#'
#'
#' @param id Numeric ID of the user.
#'
#' @param ... Other arguments passed to [.pp()]()]
#'
#' @note `GET /user/{id}`
#'
#' @family User
#' @export
pp_get_user <- function(
  id = NULL,
  ...
) {
  .pp(
    "/users/{id}",
    id = id,
    ... = ...
    # .unnest_element = NULL
  ) |>
    c()
}
