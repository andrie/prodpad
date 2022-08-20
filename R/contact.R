
# @export
# pp_contact_create <- function(client, name) {
#   client$POST("/contacts", body = list(name = name))
# }

# @export
pp_contact_url <- function(id) {
  glue("https://app.prodpad.com/contacts/{id}/about")
}



#' Get a list of all the contacts in the account.
#'
#' @param company UUID of a company to filter contacts by
#'
#' @param persona ID of a persona to filter contacts by
#'
#' @param job_role UUID of a job role to filter contacts by.
#'
#' @param name Name of contact or partial name of contacts to filter the list by
#'
#' @param email Filter the contacts by an email
#'
#' @param ... Other arguments passed to [pp()]
#'
#' @export
pp_get_contacts <- function(
    company = NULL,
    persona = NULL,
    job_role = NULL,
    name = NULL,
    email = NULL,
    ...
) {
  arguments <- call_args(current_call())

    pp(
      "/contacts",
      company = company,
      persona = persona,
      job_role = job_role,
      name = name,
      email = email,
      ...,
      .unnest_element = "contacts"
  )
}
