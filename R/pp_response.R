#' @importFrom httr status_code headers content
pp_process_response <- function(response) {
  stopifnot(inherits(response, "response"))
  if (status_code(response) >= 300) {
    pp_error(response)
  }

  content_type <- http_type(response)
  pp_media_type <- headers(response)[["x-github-media-type"]]
  is_raw <- content_type == "application/octet-stream" ||
    isTRUE(grepl("param=raw$", pp_media_type, ignore.case = TRUE))
  is_ondisk <- inherits(response$content, "path")
  if (is_ondisk) {
    res <- response$content
  } else if (grepl("^application/json", content_type, ignore.case = TRUE)) {
    res <- fromJSON(content(response, as = "text"), simplifyVector = FALSE)
  } else if (is_raw) {
    res <- content(response, as = "raw")
  } else if (content_type == "application/octet-stream" &&
             length(content(response, as = "raw")) == 0) {
    res <- NULL
  } else {
    if (grepl("^text/html", content_type, ignore.case = TRUE)) {
      warning("Response came back as html :(", call. = FALSE)
    }
    res <- list(message = content(response, as = "text"))
  }

  attr(res, "method") <- response$request$method
  attr(res, "response") <- headers(response)
  attr(res, ".send_headers") <- response$request$headers
  if (is_ondisk) {
    class(res) <- c("pp_response", "path")
  } else if (is_raw) {
    class(res) <- c("pp_response", "raw")
  } else {
    class(res) <- c("pp_response", "list")
  }
  res
}

# https://docs.github.com/v3/#client-errors
pp_error <- function(response, call = rlang::caller_env()) {
  heads <- headers(response)
  res <- content(response)
  status <- status_code(response)

  msg <- "Prodpad API error ({status}): {heads$status %||% ''} {res$error$message}"

  if (status == 400) {
    msg <- c(msg, x = c("Access denied: {.url {response$request$url}}"))
  }

  if (status == 401) {
    msg <- c(msg, x = c("The call was not authenticated: {.url {response$request$url}}"))
  }

  if (status == 404) {
    msg <- c(msg, x = c("URL not found: {.url {response$request$url}}"))
  }

  doc_url <- res$documentation_url
  if (!is.null(doc_url)) {
    msg <- c(msg, c("i" = "Read more at {.url {doc_url}}"))
  }

  errors <- res$error
  if (is.null(errors)) {
    errors <- res
  }
  if (!is.null(errors)) {
    errors <- as.data.frame(do.call(rbind, errors))
    # nms <- c("resource", "field", "code", "message")
    # nms <- c("code", "message")
    # nms <- nms[nms %in% names(errors)]
    nms <- names(errors)
    msg <- c(
      msg,
      capture.output(print(errors[nms], row.names = FALSE))
    )
  }

  cli::cli_abort(
    msg,
    class = c("github_error", paste0("http_error_", status)),
    call = call,
    response_headers = heads,
    response_content = res
  )
}
