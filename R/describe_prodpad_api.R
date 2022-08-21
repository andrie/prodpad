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
#' @param max.level passed to [str()]
#'
#' @keywords internal
#' @examples
#' describe_api()
describe_api <- function(max.level = 4) {
  prodpad_api_v1 %>% str(max.level = max.level)
}


#' Describe prodpad API paths.
#'
#' @keywords internal
#' @examples
#' describe_api_paths()
describe_api_paths <- function() {
  prodpad_api_v1$paths %>%
    purrr::map_dfr(~tibble(verb = names(.)), .id = "path")
}


#' Loads prodpad api from internal data, then construct roxygen and code.
#'
#' Prints result to the console, and copies to the clipboard.
#'
#' @param path Prodpad API path, e.g. `/feedbacks`
#'
#' @param verb `get`, `post`, etc.
#'
#' @keywords internal
#' @examples
#' describe_api_path("/feedbacks")
#' describe_api_path("/ideas")
#' describe_api_path("/ideas/{id}")
#'
#' @importFrom tibble as_tibble
describe_api_path <- function(path = "/feedbacks", verb = "get") {
  api <- prodpad_api_v1
  this <- api[["paths"]][[path]][[verb]]
  collapse <- function(..., collapse = "\n", sep = "") {
    paste0(..., collapse = collapse, sep = sep)
  }

  # title

  title <- this$summary %>% gsub("\\.*\n*$", "", .)
  z1 <- collapse(glue::glue("#' {title}.\n#'\n"), sep = "\n")

  # description

  desc <- this$description %>%
    strsplit("\n") %>%
    .[[1]] %>%
    gsub("\\.*\n$", "", .)
  z2 <- collapse(c(
    glue::glue("#' {desc}\n#'\n"),
    "#'\n"
  ))
  z2

  # @params

  params <- if (is.null(this[["parameters"]])) {
    NULL
  } else {
    this[["parameters"]] %>% as_tibble() %>% select(name, description)
  }

  z3 <- if (is.null(params)) {
    NULL
  } else {
    paste0(
      glue("#' @param {params$name} {params$description}"),
      collapse = "\n#'\n",
      sep = ""
    )
  }


  dots <- collapse(c(
    "#'",
    "#' @param ... Other arguments passed to [pp()]"
  ))

  # @note

  z4 <- collapse(c(
      "#'",
      glue("#' @note {toupper(verb)} {path}")
    ))

  # @export

  z5 <- collapse(c(
    "#'",
    "#' @export"
  ))

  # function definition

  fn_name <- glue::glue("pp_{verb}_{gsub('^//*', '', path)}")
  func <- collapse(
    c(
      glue("{{fn_name}} <- function(", .open = "{{", .close = "}}"),
      glue("  {params$name} = NULL, "),
      "  ...",
      ") {",
      glue("  pp(\"{verb} {path}\","),
      glue("    {params$name} = {params$name},"),
      "    ... = ...,",
      "    .unnest_element = NULL",
      "  )",
      "}"
    )
  )
  z <- collapse(z1, z2, z3, "\n", dots, "\n", z4, "\n", z5, "\n", func, sep = "\n")
  writeClipboard(z)
  cat(z)
  message("Copied to clipboard")
  invisible(z)
}

# api %>% describe() %>% as_tibble() %>% select(name, description)

