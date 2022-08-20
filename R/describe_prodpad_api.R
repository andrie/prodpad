# library(tidyverse)
# library(glue)

# pad_spaces <- function(x, n) {
#   suffix <- sapply(x, function(x) paste(rep(" ", nchar(x)), collapse = ""))
#   paste0(x, suffix)
# }
#
#
#
# x <- c("foo", "barbaz")
# pad_spaces(c("foo", "barbaz"), 7)

#' Describe prodpad API.
#'
#' @keywords Internal
#' @examples
#' describe_api()
describe_api <- function(max.level = 4) {
  prodpad_api_v1 %>% str(max.level = max.level)
}


#' Describe prodpad API paths.
#'
#' @keywords Internal
#' @examples
#' describe_api_paths()
describe_api_paths <- function() {
  prodpad_api_v1$paths %>%
    purrr::map_dfr(~tibble(verb = names(.)), .id = "path")
}


#' Loads prodpad api from internal data, then construct roxygen and code
#'
#' @keywords Internal
#' @examples
#' describe("/feedbacks")
#' describe("/ideas")
#' describe("/ideas/{id}")
describe <- function(path = "/feedbacks", verb = "get", element = "parameters") {
  api <- prodpad_api_v1
  this <- api[["paths"]][[path]][[verb]]
  collapse <- function(..., collapse = "\n", sep = "") {
    paste0(..., collapse = collapse, sep = sep)
  }

  title <- this$summary %>% gsub("\\.*\n*$", "", .)
  z1 <- collapse(glue::glue("#' {title}.\n#'\n"), sep = "\n")

  # browser()
  desc <- this$description %>%
    strsplit("\n") %>%
    .[[1]] %>%
    gsub("\\.*\n$", "", .)
  z2 <- collapse(c(
    glue::glue("#' {desc}\n#'\n"),
    "#'\n"
  ))
  z2


  params <- this[[element]] %>% as_tibble() %>% select(name, description)
  z3 <- paste0(
    glue("#' @param {params$name} {params$description}"),
    collapse = "\n#'\n",
    sep = ""
  )

  z4 <- collapse(c(
      "#'",
      glue("\n#'\n#' @note {toupper(verb)} {path}")
    ))

  z5 <- collapse(c(
    "#'",
    "#' @export"
  ))


  fn_name <- glue::glue("pp_{verb}_{gsub('^//*', '', path)}")
  func <- collapse(
    c(
      glue("{{fn_name}} <- function(", .open = "{{", .close = "}}"),
      glue("  {params$name}, "),
      "  ...",
      ") {",
      glue("  pp(\"{verb} {path}\""),
      glue("    {params$name} = {params$name},"),
      "    ...,",
      "    .unnest_element = NULL",
      "  )",
      "}"
    )
  )
  z <- collapse(z1, z2, z3, z4, "\n", z5, "\n", func, sep = "\n")
  writeClipboard(z)
  cat(z)
  message("Copied to clipboard")
  invisible(z)
}

# api %>% describe() %>% as_tibble() %>% select(name, description)

