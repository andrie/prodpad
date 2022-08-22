
feedback_url <- function(id) {
  glue::glue("https://app.prodpad.com/feedback/{id}/canvas")
}


reactable_feedbacks <- function(.data) {
  .data %>%
    arrange(desc(created_at)) %>%
    mutate(
      fburl = feedback_url(id),
      url = map_chr(fburl, ~ as.character(htmltools::a(.x, href = .x, target = "_blank")))
    ) %>%
    select(created_at, added_by_username, feedback, url) %>%
    reactable(
      filterable = TRUE,
      searchable = TRUE,
      columns = list(
        feedback = colDef(html = TRUE),
        url = colDef(html = TRUE),
        created_at = colDef(format = colFormat(datetime = TRUE, ))
      ))
}

reactable_ideas <- function(.data) {
  .data %>%
  arrange(desc(created_at)) %>%
  mutate(
    url = purrr::map_chr(web_url, ~ as.character(htmltools::a(.x, href = .x)))
  ) %>%
  select(created_at, creator_username, title, description, url) %>%
  reactable(
    filterable = TRUE,
    searchable = TRUE,
    columns = list(
      title = colDef(html = TRUE),
      description = colDef(html = TRUE),
      url = colDef(html = TRUE),
      created_at = colDef(format = colFormat(datetime = TRUE, ))
    )
  )
}

