## from devtools, among other places
compact <- function(x) {
  is_empty <- vapply(x, function(x) length(x) == 0, logical(1))
  x[!is_empty]
}

check_named_nas <- function(x) {
  if (has_no_names(x)) {
    return(x)
  }
  named <- has_name(x)
  na <- vapply(x, FUN.VALUE = logical(1), function(v) {
    is.atomic(v) && anyNA(v)
  })
  bad <- which(named & na)
  if (length(bad)) {
    str <- paste0("`", names(x)[bad], "`", collapse = ", ")
    stop("Named NA parameters are not allowed: ", str)
  }
}

## to process HTTP headers, i.e. combine defaults w/ user-specified headers
## in the spirit of modifyList(), except
## x and y are vectors (not lists)
## name comparison is case insensitive
## http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.2
## x will be default headers, y will be user-specified
modify_vector <- function(x, y = NULL) {
  if (length(y) == 0L) {
    return(x)
  }
  lnames <- function(x) tolower(names(x))
  c(x[!(lnames(x) %in% lnames(y))], y)
}

## from purrr, among other places
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}


## if all names are "", strip completely
cleanse_names <- function(x) {
  if (has_no_names(x)) {
    names(x) <- NULL
  }
  x
}

## as seen in purrr, with the name `has_names()`
has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep_len(FALSE, length(x))
  } else {
    !(is.na(nms) | nms == "")
  }
}

has_no_names <- function(x) all(!has_name(x))

drop_named_nulls <- function(x) {
  if (has_no_names(x)) {
    return(x)
  }
  named <- has_name(x)
  null <- vapply(x, is.null, logical(1))
  cleanse_names(x[!named | !null])
}

#' Parse prodpad dates using lubridate.
#'
#' This is useful for columns such as `created_at` and `updated_at`.
#'
#' @param x Character vector to parse.  Assumes `datetimes` are written as `2022-01-01T20-00-00Z`
parse_prodpad_date <- function(x) {
  lubridate::parse_date_time(x, orders="%Y-%m-%d %H%:M%:%S")
}

get_id_vector <- function(x, id = "id", name = "name") {
  setNames(x[[id]], x[[name]])
}

