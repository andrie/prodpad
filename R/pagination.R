page_all_requests <- function(f, page, size, .limit) {
  # browser()

  if (is.null(.limit)) .limit <- size
  if (.limit < size) size <- .limit

  # get the first tibble
  z <- match.fun(f)(page = page, size = size)
  # print(glue::glue("> Page: {page}, nrow(z): {nrow(z)})"))

  if (nrow(z) == 0) return(z)
  returned <- nrow(z)
  gcz <- get_count(z)
  if (!is.na(gcz) && gcz < .limit) { .limit <- get_count(z) }
  returned <- nrow(z)
  count <- min(get_count(z), .limit, na.rm = TRUE)

  # get all subsequent tibbles
  while (nrow(z) < max(.limit, returned) && returned == size) {
    page <- page + 1
    # print(glue::glue("> Page: {page}, nrow(z): {nrow(z)})"))
    if(.limit < Inf) size <- min(size, .limit - nrow(z))
    z2 <- match.fun(f)(page = page, size = size)
    returned <- nrow(z2)
    if (nrow(z2) == 0) return(z)
    z <- dplyr::bind_rows(z, z2)
  }
  z
}

get_count <- function(x) {
  attr(x, "count") %||% NA
}


