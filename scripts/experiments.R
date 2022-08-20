library(tidyverse)

pp_build_request("/tags", params = list(size=10))
pp("/tags", size=10)
pp("/tags", size = 10) %>% length()
pp("/tags", size = 10, .unnest = TRUE)


pp("/products", .unnest = TRUE)
pp_get_products()

pp("/contacts", size = 10, .unnest_element = "contacts")

pp_get_contacts(size = 20)
pp_get_contacts(email = "andrie@rstudio.com")


tags <- pp("/tags", .unnest = TRUE)
tags

pp("/ideas", size = 10)$ideas %>% pp_unnest()
ideas <- pp("/ideas", size = 10, product = 56138, .unnest = TRUE, .unnest_element = "ideas")
ideas

pp("/ideas/159", by_project_id = FALSE) %>% str()


feedback <- pp("/feedbacks", size = 10, product = 56138, .unnest = TRUE)
feedback


idea <- pp("/ideas/{id}", id = 159, by_project_id = FALSE, expand = TRUE)
idea %>% str()


pp("/users")
pp("/users/{id}", id = 90722)
pp("/contacts/{id}", id = "8493d880-f70d-11ec-9958-1bf415b4c6e6")$company$id
idea$account$id

convert_idea_to_feedback <- function(idea, contact_id) {
  contact <- pp("/contacts/{id}", id = contact_id)
  # company_id <- contact$company$id
  # email      <- contact$email
  # contact$name
  pp(
    "POST /feedbacks",
    contact_id     = contact_id,
    # name = "",
    # name           = contact$name,
    # company_id     = contact_id,
    feedback       = idea$description,
    # email          = contact$email,
    # about          = NA_character_,
    tags           = idea$tags,
    personas       = idea$personas %>% unnest() %>% .[, -3],
    products       = idea$products %>% unnest() %>% .[, -3],
    source         = idea$source,
    external_links = idea$external_links,
    # ideas = data.frame(id = 1:2, ideas = c("idea 1", "idea 2")),
    .send_headers = c(`Content-Type` = "application/json")
  )
}

convert_idea_to_feedback(idea = idea, contact_id = "8493d880-f70d-11ec-9958-1bf415b4c6e6")


fromJSON(
'
{
  "contact_id": "3fa85f64-5717-4562-b3fc-2c963f66afa6",
  "name": "string",
  "company_id": "3fa85f64-5717-4562-b3fc-2c963f66afa6",
  "feedback": "string",
  "email": "string",
  "about": "string",
  "ideas": [
    {
      "id": "string"
    }
  ],
  "tags": [
    {
      "id": "string"
    },
    {
      "name": "string"
    }
  ],
  "personas": [
    {
      "id": 0
    },
    {
      "name": "string"
    }
  ],
  "products": [
    {
      "id": 0
    },
    {
      "name": "string"
    }
  ],
  "source": "api",
  "external_links": [
    {
      "name": "string",
      "url": "string",
      "external_id": "string"
    }
  ]
}
') %>% dput()
