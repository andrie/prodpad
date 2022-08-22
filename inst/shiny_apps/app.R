library(shiny)
library(shinycssloaders)
library(reactable)
library(tidyverse)


# library(prodpad)
devtools::load_all(here::here())

ui <- fluidPage(

    titlePanel("ProdPad Review"),


    fluidRow(
      column(12, div(actionButton("interrupt", "Interrupt")))
    ),
    tabsetPanel(
      tabPanel(
        "Feedbacks",
        fluidRow(
          column(12, div(actionButton("refresh_feedback", "Refresh Feedbacks")))
        ),
        # TODO: Add filters on these items
        #fluidRow(
        #  column(6, div(withSpinner(uiOutput("select_contact")))),
        #  column(6, div(withSpinner(uiOutput("select_product"))))
        #),
        #fluidRow(
        #  column(6, withSpinner(uiOutput("select_personas"))),
        #  column(6, withSpinner(uiOutput("select_tags")))
        #),
        fluidRow(
          column(
            12,
            withSpinner(reactable::reactableOutput("feedback_global"))
          )
        )
      ),
      tabPanel(
        "Ideas",
        fluidRow(
          column(12, div(actionButton("refresh_ideas", "Refresh Ideas")))
        ),
        fluidRow(
          column(
            12,
            withSpinner(reactable::reactableOutput("ideas"))
          )
        )
      )
    )
)

server <- function(input, output, session) {
  all_contacts <- pp_get_contacts_vector()
  output$select_contact <- renderUI({
     selectizeInput(
          "contact",
          "Contact",
          choices = c("Select a Contact" = "", all_contacts),
          options = list(create = TRUE) # TODO: handle creation better
          )
  })

  all_products <- pp_get_products_vector()
  output$select_product <- renderUI({
      selectizeInput(
          "products",
          "Products",
          choices = all_products,
          multiple = TRUE
          )
  })

  all_personas <- pp_get_personas_vector()
  output$select_personas <- renderUI({
      selectizeInput(
          "personas",
          "Personas",
          choices = all_personas,
          multiple = TRUE
          )
  })

  all_tags <- pp_get_tags_vector()
  output$select_tags <- renderUI({
      selectizeInput(
          "tags",
          "Tags",
          choices = all_tags,
          multiple = TRUE
          )
  })

  # View feedbacks
  feedbacks <- reactiveVal(pp_get_feedbacks())
  ideas <- reactiveVal(pp_get_ideas())

  observeEvent(input$refresh_feedback, {
    showNotification("Feedbacks: fetching... please wait")
    feedbacks(pp_get_feedbacks())
    showNotification("Feedbacks: Done!")
  })

  observeEvent(input$refresh_ideas, {
    showNotification("Ideas: fetching... please wait")
    ideas(pp_get_ideas())
    showNotification("Ideas: Done!")
  })

  observeEvent(input$interrupt, {
    browser()
  })

  output$feedback_global <- reactable::renderReactable({
    req(ncol(feedbacks()) > 0)
    feedbacks() %>%
      reactable_feedbacks()
  })

  output$ideas <- reactable::renderReactable({
    req(ncol(ideas()) > 0)
    ideas() %>%
      reactable_ideas()
  })
}

shinyApp(ui = ui, server = server)
