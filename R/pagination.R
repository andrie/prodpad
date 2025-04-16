page_all_requests <- function(
  f,
  page = 1,
  size,
  .limit = size,
  .debug = FALSE
) {
  print_debug <- function() {
    if (.debug) {
      message(glue::glue("> Page: {page}, nrow(z): {nrow(z)}"))
    }
  }

  size <- min(size, .limit)
  first <- TRUE
  done <- FALSE
  # browser()
  while (!done) {
    z2 <- match.fun(f)(page = page, size = size)
    returned <- nrow(z2)
    if (first) {
      # get the first tibble
      first <- FALSE
      z <- z2
      if (returned == 0) return(z)
      gcz <- get_count(z)
      if (!is.na(gcz) && gcz < .limit) {
        .limit <- get_count(z)
      }
      count <- min(get_count(z), .limit, na.rm = TRUE)
    } else {
      # get all subsequent tibbles
      if (returned == 0) return(z)
      z <- dplyr::bind_rows(z, z2)
    }
    print_debug()
    remaining <- .limit - nrow(z)
    size <- min(size, remaining)
    if (nrow(z) >= max(.limit, returned) || returned < size) done <- TRUE
    page <- page + 1
  }
  # print_debug()
  z
}

get_count <- function(x) {
  attr(x, "count") %||% NA
}
