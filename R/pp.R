#' @importFrom tidyr unnest_wider
#' @importFrom tidyselect all_of
pp_unnest <- function(x, names_sep = "_", .unnest_dont_unlist = NULL, ...) {
  # unnest initial data frame
  df <- tibble::tibble(x)
  list_cols <- setdiff(names(x), .unnest_dont_unlist)
  z <- unnest_wider(df, x, ...)

  # browser()
  # unnest list columns
  list_cols <- z |> purrr::keep(is.list) |> names()
  list_cols <- setdiff(list_cols, .unnest_dont_unlist)
  if (length(list_cols) > 0) {
    unnest_wider(z, all_of(list_cols), names_sep = names_sep)
  } else {
    z
  }
}

#' Query the Prodpad API
#'
#' @description
#'
#' This is an extremely minimal client. You need to know the API
#' to be able to use this client. All this function does is:
#'
#' * Try to substitute each listed parameter into `endpoint`, using the
#'   `{parameter}` notation.
#'
#' * If a GET request (the default), then add all other listed parameters
#'   as query parameters.
#'
#' * If not a GET request, then send the other parameters in the request
#'   body, as JSON.
#'
#' * Convert the response to an R list using [jsonlite::fromJSON()].
#'
#' @param endpoint Prodpad API endpoint. Must be one of the following forms:
#'    * `METHOD path`, e.g. `GET /feedbacks`,
#'    * `path`, e.g. `/feedbacks`,
#'    * `METHOD url`, e.g. `GET https://api.prodpad.com/v1/feedbacks`,
#'    * `url`, e.g. `https://api.prodpad.com/v1/feedbacks`.
#'
#'    If the method is not supplied, will use `.method`, which defaults
#'    to `"GET"`.
#'
#' @param ... Name-value pairs giving API parameters. Will be matched into
#'   `endpoint` placeholders, sent as query parameters in GET requests, and as a
#'   JSON body of POST requests. If there is only one unnamed parameter, and it
#'   is a raw vector, then it will not be JSON encoded, but sent as raw data, as
#'   is. This can be used for example to add assets to releases. Named `NULL`
#'   values are silently dropped. For GET requests, named `NA` values trigger an
#'   error. For other methods, named `NA` values are included in the body of the
#'   request, as JSON `null`.
#'
#' @param per_page Number of items to return per page. If omitted,
#'   will be substituted by `max(.limit, 100)` if `.limit` is set,
#'   otherwise determined by the API (never greater than 100).
#'
#' @param .destfile Path to write response to disk. If `NULL` (default),
#'   response will be processed and returned as an object. If path is given,
#'   response will be written to disk in the form sent.
#'
#' @param .overwrite If `.destfile` is provided, whether to overwrite an
#'   existing file.  Defaults to `FALSE`.
#'
#' @param .api_url Prodpad API url (default: <https://api.prodpad.com/v1>). Used
#'   if `endpoint` just contains a path. Defaults to `PRODPAD_API_URL`
#'   environment variable if set.
#'
#' @param .method HTTP method to use if not explicitly supplied in the
#'    `endpoint`.
#'
#' @param .limit Number of records to return. This can be used
#'   instead of manual pagination. By default it is `NULL`,
#'   which means that the defaults of the Prodpad API are used.
#'   You can set it to a number to request more (or less)
#'   records, and also to `Inf` to request all records.
#'   Note, that if you request many records, then multiple Prodpad
#'   API calls are used to get them, and this can take a potentially
#'   long time.
#'
#' @param .accept The value of the `Accept` HTTP header. Defaults to
#'   `"application/json` . If `Accept` is given in
#'   `.send_headers`, then that will be used. This parameter can be used to
#'   provide a custom media type, in order to access a preview feature of
#'   the API.
#'
#' @param .send_headers Named character vector of header field values
#'   (except `Authorization`, which is handled via `.token`). This can be
#'   used to override or augment the default `User-Agent` header:
#'   `"https://github.com/andrie/prodpad"`.
#'
#' @param .progress Whether to show a progress indicator for calls that
#'   need more than one HTTP request.
#'
#' @param .params Additional list of parameters to append to `...`.
#'   It is easier to use this than `...` if you have your parameters in
#'   a list already.
#'
#' @param .api_version This must be `1`.
#'
#' @param .unnest If TRUE, unnests the result object
#'
#' @param .unnest_element Name of element to extract before unnesting the result object
#'
#' @param .unnest_dont_unlist Vector of columns name to ignore during the unnesting.
#'
#' @return Answer from the API as a `pp_response` object, which is also a
#'   `list`. Failed requests will generate an R error. Requests that
#'   generate a raw response will return a raw vector.
#'
#'
#'
#' @export
.pp <- function(
  endpoint,
  ...,
  per_page = NULL,
  .destfile = NULL,
  .overwrite = FALSE,
  .api_url = NULL,
  .method = "GET",
  .limit = NULL,
  .accept = "application/json",
  .send_headers = NULL,
  .progress = TRUE,
  .params = list(),
  .api_version = 1,
  .unnest = !is.null(.unnest_element),
  .unnest_element = NULL,
  .unnest_dont_unlist = NULL
) {
  # browser()

  if (is.null(.api_url)) {
    .api_url <- switch(
      as.character(.api_version),
      "1" = "https://api.prodpad.com/v1",
      "2" = "https://api.prodpad.com/api/v2",
      stop(".api_version must be either 1 or 2")
    )
  }

  if (.api_version == 1 && Sys.getenv("PRODPAD_API_KEY") == "") {
    stop(
      "You must set the PRODPAD_API_KEY env var for authentication. ",
      "Use browse_api_key() to find your API key.",
      call. = FALSE
    )
  }

  params <- c(list(...), .params)
  params <- drop_named_nulls(params)

  if (is.null(per_page)) {
    if (!is.null(.limit)) {
      per_page <- max(min(.limit, 100), 1)
    }
  }

  if (!is.null(per_page)) {
    params <- c(params, list(per_page = per_page))
  }

  req <- pp_build_request(
    endpoint = endpoint,
    params = params,
    token = NULL,
    destfile = .destfile,
    overwrite = .overwrite,
    accept = .accept,
    send_headers = .send_headers,
    api_url = .api_url,
    method = .method
  )

  if (req$method == "GET") check_named_nas(params)

  # if (.progress) prbr <- make_progress_bar(req)

  raw <- pp_make_request(req)

  res <- pp_process_response(raw)
  len <- pp_response_length(res)

  # while (!is.null(.limit) && len < .limit && pp_has_next(res)) {
  #   if (.progress) update_progress_bar(prbr, res)
  #   res2 <- pp_next(res)
  #
  #   if (!is.null(names(res2)) && identical(names(res), names(res2))) {
  #     res3 <- mapply( # Handle named array case
  #       function(x, y, n) { # e.g. GET /search/repositories
  #         z <- c(x, y)
  #         atm <- is.atomic(z)
  #         if (atm && n %in% c("total_count", "incomplete_results")) {
  #           y
  #         } else if (atm) {
  #           unique(z)
  #         } else {
  #           z
  #         }
  #       },
  #       res, res2, names(res),
  #       SIMPLIFY = FALSE
  #     )
  #   } else { # Handle unnamed array case
  #     res3 <- c(res, res2) # e.g. GET /orgs/:org/invitations
  #   }
  #
  #   len <- len + pp_response_length(res2)
  #
  #   attributes(res3) <- attributes(res2)
  #   res <- res3
  # }

  # We only subset for a non-named response.
  if (
    !is.null(.limit) &&
      len > .limit &&
      !"total_count" %in% names(res) &&
      length(res) == len
  ) {
    res_attr <- attributes(res)
    res <- res[seq_len(.limit)]
    attributes(res) <- res_attr
  }

  # extract _count element, if it exists
  idx <- which(grepl("_count", names(res)))
  count <-
    if (length(idx)) res[[idx]] else NA_integer_

  # unnest if necessary
  if (isTRUE(.unnest)) {
    z <-
      if (!is.null(.unnest_element)) {
        res[[.unnest_element]] |>
          pp_unnest(.unnest_dont_unlist = .unnest_dont_unlist)
      } else {
        res |>
          pp_unnest(.unnest_dont_unlist = .unnest_dont_unlist)
      }
    res <-
      z |>
      mutate(dplyr::across(
        dplyr::starts_with("created_at"),
        parse_prodpad_date
      )) |>
      mutate(dplyr::across(
        dplyr::starts_with("updated_at"),
        parse_prodpad_date
      ))
  }

  attr(res, "count") <- count
  res
}

pp_response_length <- function(res) {
  if (
    !is.null(names(res)) && length(res) > 1 && names(res)[1] == "total_count"
  ) {
    # Ignore total_count, incomplete_results, repository_selection
    # and take the first list element to get the length
    lst <- vapply(res, is.list, logical(1))
    nm <- setdiff(
      names(res),
      c("total_count", "incomplete_results", "repository_selection")
    )
    tgt <- which(lst[nm])[1]
    if (is.na(tgt)) length(res) else length(res[[nm[tgt]]])
  } else {
    length(res)
  }
}

pp_make_request <- function(x) {
  method_fun <- list(
    "GET" = httr::GET,
    "POST" = httr::POST,
    "PATCH" = httr::PATCH,
    "PUT" = httr::PUT,
    "DELETE" = httr::DELETE
  )[[x$method]]
  if (is.null(method_fun)) {
    cli::cli_abort("Unknown HTTP verb: {.val {x$method}}")
  }

  raw <- do.call(
    method_fun,
    compact(list(
      url = x$url,
      query = x$query,
      body = x$body,
      add_headers(x$headers),
      x$dest
    ))
  )
  raw
}
