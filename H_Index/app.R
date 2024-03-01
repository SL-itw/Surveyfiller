
library(shinydashboard)
library(DT)
library(flexdashboard)
library(tidyverse)
library(rentrez)
library(lubridate)
source("./getfunctions.R")
library(polite)
library(XML)


# Define UI for application that draws a histogram
ui <- dashboardPage(

  dashboardHeader(title = "Publication Metrics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Search", tabName = "search"),
      fluidRow(
        box(
          title = "Enter Details",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          textInput("full_name", "Full Name",placeholder = "First and Last Name"),
          textInput("full_name2", "Full Name",placeholder = "Other Name (Optional)"),
          textInput("hire_date", "Hire Date",placeholder = "Hire Date"),
          textInput("affiliation_1", "Affiliation 1", placeholder = "Affiliation"),
          textInput("affiliation_2", "Affiliation 2", placeholder = "Affiliation"),
          textInput("affiliation_3", "Affiliation 3", placeholder = "Affiliation"),
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
            title = "Total Publications 2 year prior",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            valueBoxOutput("total_publications_box")
          ),
          box(
            title = "Total Unique Coauthors",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            valueBoxOutput("uniquecoauthors")
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
        ),br(),
        fluidRow(
          box(
            title = "Unique Co Authors",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("coauthor_table") # Placeholder for the output table
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


    hiredate = input$hire_date
    specific_date = lubridate::mdy(hiredate)
    two_years_prior_date <- specific_date %m-% years(2)

    # main table
    withProgress(
      message = 'Loading data...',
      detail = 'You have a lot of publications...',
      value = 0,{

        ids =  entrez_search("pubmed", query, retmax = 10000)$ids
        recs = entrez_fetch("pubmed", ids, rettype = "xml")
        # parses meta information
        parsed = XML::xmlTreeParse(recs, useInternalNodes = TRUE)
        output_data = article_data(parsed)
        coauthor_data = get_coauthor_count(ids,two_years_prior_date,specific_date)

     for (i in 1:100) {
       incProgress(1/100)
       Sys.sleep(0.1)
      }

     return(output_data)
     return(parsed)
     return(coauthor_data)
      }
     )

    # aggregate calculations

     h_index = output_data %>%
       arrange(-citations) %>%
    mutate(order = dplyr::row_number(),
           ind = if_else(citations >= order, 1,0)) %>%
    dplyr::summarize(h_index = sum(ind, na.rm = T)) %>%
    pull(h_index)

    coauthor_count = coauthor_data[2]


  output$output_table <- renderDT({
    datatable(output_data %>%
                filter(date >= two_years_prior_date & date < specific_date) %>%
                arrange(citations)
                , rownames = FALSE)
  })

  output$coauthor_table <- renderDT({
    datatable(coauthor_data[1]  %>% tibble() %>% unnest(cols = c(.)), rownames = FALSE)
  })

  # Calculate summary values
  output$total_publications_box <- renderValueBox({
    valueBox(
      value = nrow(output_data%>%
                     filter(date >= two_years_prior_date & date < specific_date)),
     # subtitle  = "Total Publications"
     # icon = "fa-book"
    )
  })

  output$uniquecoauthors <- renderValueBox({
    valueBox(
      value = coauthor_count,
     # icon = "fa-users"
    )
  })

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
