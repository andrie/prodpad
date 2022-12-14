## Main API URL
default_api_url <- function() {
  Sys.getenv("PRODPAD_URL", unset = "https://api.prodpad.com/v1")
}

## Headers to send with each API request
default_send_headers <- c("User-Agent" = "https://github.com/andrie/prodpad")

pp_build_request <- function(endpoint = "/user", params = list(),
                             token = NULL, destfile = NULL, overwrite = NULL,
                             accept = NULL, send_headers = NULL,
                             api_url = NULL, method = "GET") {
  working <- list(
    method = method, url = character(), headers = NULL,
    query = NULL, body = NULL,
    endpoint = endpoint, params = params,
    token = token, accept = c(Accept = accept),
    send_headers = send_headers, api_url = api_url,
    dest = destfile, overwrite = overwrite
  )

  working <- pp_set_verb(working)
  working <- pp_set_endpoint(working)
  working <- pp_set_query(working)
  working <- pp_set_body(working)
  working <- pp_set_url(working)
  working <- pp_set_headers(working)
  working <- pp_set_dest(working)
  working[c("method", "url", "headers", "query", "body", "dest")]
}


## pp_set_*(x)
## x = a list in which we build up an httr request
## x goes in, x comes out, possibly modified

pp_set_verb <- function(x) {
  if (!nzchar(x$endpoint)) {
    return(x)
  }

  # No method defined, so use default
  if (grepl("^/", x$endpoint) || grepl("^http", x$endpoint)) {
    return(x)
  }

  # Method can be lower-case (e.g. copy-pasting from API docs in Firefox)
  method <- gsub("^([^/ ]+)\\s+.*$", "\\1", x$endpoint)
  x$endpoint <- gsub(sprintf("^%s+ ", method), "", x$endpoint)
  # Now switch method to upper-case
  x$method <- toupper(method)
  x
}

pp_set_endpoint <- function(x) {
  params <- x$params
  if (!is_template(x$endpoint) || length(params) == 0L || has_no_names(params)) {
    return(x)
  }

  named_params <- which(has_name(params))
  done <- rep_len(FALSE, length(params))
  endpoint <- endpoint2 <- x$endpoint

  for (i in named_params) {
    endpoint2 <- expand_variable(
      varname  = names(params)[i],
      value    = params[[i]][1],
      template = endpoint
    )
    if (is.na(endpoint2)) {
      cli::cli_abort(
        "Named NA parameters are not allowed: {names(params)[i]}"
      )
    }
    if (endpoint2 != endpoint) {
      endpoint <- endpoint2
      done[i] <- TRUE
    }
    if (!is_template(endpoint)) {
      break
    }
  }

  x$endpoint <- endpoint
  x$params <- x$params[!done]
  x$params <- cleanse_names(x$params)
  x
}

pp_set_query <- function(x) {
  params <- x$params
  if (x$method != "GET" || length(params) == 0L) {
    return(x)
  }
  stopifnot(all(has_name(params)))
  x$query <- params
  x$params <- NULL
  x
}

pp_set_body <- function(x) {
  if (length(x$params) == 0L) {
    return(x)
  }
  if (x$method == "GET") {
    warning("This is a 'GET' request and unnamed parameters are being ignored.")
    return(x)
  }
  if (length(x$params) == 1 && is.raw(x$params[[1]])) {
    x$body <- x$params[[1]]
  } else {
    x$body <- toJSON(x$params, auto_unbox = TRUE)
  }
  x
}

pp_set_url <- function(x) {
  if (grepl("^https?://", x$endpoint)) {
    x$url <- URLencode(x$endpoint)
    x$api_url <- get_baseurl(x$url)
  } else {
    x$api_url <- x$api_url %||% default_api_url()
    x$url <- URLencode(paste0(x$api_url, x$endpoint))
  }

  x
}

get_baseurl <- function(url) { # https://github.uni.edu/api/v3/
  if (!any(grepl("^https?://", url))) {
    stop("Only works with HTTP(S) protocols")
  }
  prot <- sub("^(https?://).*$", "\\1", url) # https://
  rest <- sub("^https?://(.*)$", "\\1", url) #         github.uni.edu/api/v3/
  host <- sub("/.*$", "", rest) #         github.uni.edu
  paste0(prot, host) # https://github.uni.edu
}

# https://api.github.com --> https://github.com
# api.github.com --> github.com
normalize_host <- function(x) {
  sub("api[.]prodpad[.]com", "prodpad.com", x)
}

get_hosturl <- function(url) {
  url <- get_baseurl(url)
  normalize_host(url)
}


is_prodpad_dot_com <- function(url) {
  url <- get_baseurl(url)
  url <- normalize_host(url)
  grepl("^https?://.*prodpad.com", url)
}

pp_set_headers <- function(x) {
  auth <- c(Authorization = paste0('Bearer ', Sys.getenv("PRODPAD_API_KEY", NA_character_)))
  send_headers <- pp_send_headers(x$accept, x$send_headers)
  x$headers <- c(send_headers, auth)
  x
}

pp_send_headers <- function(accept_header = NULL, headers = NULL) {
  modify_vector(
    modify_vector(default_send_headers, accept_header),
    headers
  )
}

#' @importFrom httr write_disk write_memory add_headers http_type
pp_set_dest <- function(x) {
  if (is.null(x$dest)) {
    x$dest <- write_memory()
  } else {
    x$dest <- write_disk(x$dest, overwrite = x$overwrite)
  }
  x
}

# helpers ----
# https://tools.ietf.org/html/rfc6570
# we support what the RFC calls "Level 1 templates", which only require
# simple string expansion of a placeholder consisting of [A-Za-z0-9_]
is_template <- function(x) {
  is_colon_template(x) || is_uri_template(x)
}

is_colon_template <- function(x) grepl(":", x)

is_uri_template <- function(x) grepl("[{]\\w+?[}]", x)

template_type <- function(x) {
  if (is_uri_template(x)) {
    return("uri")
  }
  if (is_colon_template(x)) {
    return("colon")
  }
}

expand_variable <- function(varname, value, template) {
  type <- template_type(template)
  if (is.null(type)) {
    return(template)
  }
  pattern <- switch(type,
                    uri   = paste0("[{]", varname, "[}]"),
                    colon = paste0(":", varname, "\\b"),
                    stop("Internal error: unrecognized template type")
  )
  gsub(pattern, value, template)
}
