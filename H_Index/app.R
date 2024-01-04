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
source("./getfunctions.R")
library(polite)


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
          textInput("hire_date", "Hire Date",placeholder = "Hire Date"),
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
          # box(
          #   title = "Total Unique Coauthors",
          #   status = "info",
          #   solidHeader = TRUE,
          #   width = 4,
          #   valueBoxOutput("uniquecoauthors")
          #),
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

    # global variables
    query  = query_func(

      name1 =input$full_name,
      name2 = input$full_name2,
      affiliation1 = input$affiliation_1,
      affiliation2 = input$affiliation_2,
      affiliation3 = input$affiliation_3
    )

    ids =  entrez_search("pubmed", query, retmax = 10000)$ids
    recs = entrez_fetch("pubmed", ids, rettype = "xml")
    hiredate = input$hire_date

    # main table
    withProgress(
      message = 'Loading data...',
      detail = 'This may take a while...',
      value = 0,{
     output_data = article_data(recs, hiredate)

     for (i in 1:100) {
       incProgress(1/100)
       Sys.sleep(0.1)
      }

     return(output_data)
      }
     )

    # aggregate calculations

     h_index = output_data %>%
    mutate(n = length(titles),
           ind = if_else(citations >= n, 1,0)) %>%
    dplyr::summarize(h_index = sum(ind, na.rm = T)) %>%
    pull(h_index)

   #  coauthor_count = get_coauthor_count(ids, recs)


  output$output_table <- renderDT({
    datatable(output_data, rownames = FALSE)
  })

  # Calculate summary values
  output$total_publications_box <- renderValueBox({
    valueBox(
      value = nrow(output_data),
     # subtitle  = "Total Publications"
     # icon = "fa-book"
    )
  })

  # output$uniquecoauthors <- renderValueBox({
  #   valueBox(
  #     value = coauthor_count,
  #    # icon = "fa-users"
  #   )
  # })

  output$h_index_box <- renderValueBox({
    valueBox(
      value = h_index ,
      #subtitle  = "H Index"
     # icon = "fa-building"
    )
  })
})

}

# Run the application
shinyApp(ui = ui, server = server)
