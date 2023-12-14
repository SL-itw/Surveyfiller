#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(DT)
library(flexdashboard)
library(tidyverse)
library(rentrez)
library(lubridate)

source("../getfunctions.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "PubMed Search/Google Scholar Cited"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Search", tabName = "search"),
      fluidRow(
        box(
          title = "Enter Details",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          textInput("full_name", "Full Name",placeholder = "Type Full Name"),
          textInput("full_name2", "Full Name",placeholder = "Type Other Name (If applicable)"),
          textInput("hire_date", "Hire Date",placeholder = "01/01/2023"),
          textInput("affiliation_1", "Affiliation 1", placeholder = "Mount Sinai"),
          textInput("affiliation_2", "Affiliation 2", placeholder = "University of Florida"),
          textInput("affiliation_3", "Affiliation 3", placeholder = "University of Washington"),
          actionButton("submit_button", "Submit")
        )
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "search",
        fluidRow(
          box(
            title = "Total Publications",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            valueBoxOutput("total_publications_box")
          ),
          box(
            title = "Total Citations",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            valueBoxOutput("total_cite_box")
          ),
          box(
            title = "H Index",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            valueBoxOutput("h_index_box")
          )
        ),


        br(),
        fluidRow(
          box(
            title = "Search Results",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("output_table") # Placeholder for the output table
          )
        )
      )
    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$submit_button, {

    output_data = article_data(
    name1=input$full_name,
    hiredate=input$hire_date,
    affiliation1 = input$affiliation_1,
    name2 = input$full_name2,
    affiliation2 = input$affiliation_2,
    affiliation3 = input$affiliation_3
  )

  h_index = output_data %>%
    mutate(n = length(titles),
           ind = if_else(citations >= n, 1,0)) %>%
    summarize(h_index = sum(ind, na.rm = T)) %>%
    pull(h_index)

  output$output_table <- renderDT({
    datatable(output_data, rownames = FALSE)
  })

  # Calculate summary values
  output$total_publications_box <- renderValueBox({
    valueBox(

      value = nrow(output_data),
      #subtitle  = "Total Publications",
     # icon = "fa-book"
    )
  })

  output$total_cite_box <- renderValueBox({
    valueBox(
      value = sum(output_data$citations, na.rm = T),
      #subtitle  = "Total Citations",
     # icon = "fa-users"
    )
  })

  output$h_index_box <- renderValueBox({
    valueBox(
      value = h_index ,
      #subtitle  = "H Index",
     # icon = "fa-building"
    )
  })
})

}

# Run the application
shinyApp(ui = ui, server = server)
