#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(httr)  
library(jsonlite)
library(shiny)
library(DT)


#Pulling the arrest data using an API

path <- "https://data.cityofnewyork.us/resource/8h9b-rp9u.json?$limit=50000"
request <- GET(url = path)
request$status_code#Checking to ensure that a non-200 status code is not returned
response <- content(request, as = "text", encoding = "UTF-8")
arrest_data <- fromJSON(response) %>%  #Putting the pulled data into a data frame
  data.frame()


ui <- fluidPage(
  
  #Title
  titlePanel("NYC Arrest Data"),
  
  #Sidebar
  sidebarLayout(
    
    sidebarPanel(
      
      #Checkbox to select boroughs
      checkboxGroupInput(inputId = "selected_borough",
                         label = "Search by borough",
                         choices = c("Q","K","M","S","B"),
                         selected = "M")
    ),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("Data Table", br(), br(), uiOutput(outputId = "n"),br(), br(),DT::dataTableOutput("DataTable"))
    )
  )
  ) 
)

server <- function(input, output, session){
  
  #Subsetting data based on the selection of boroughs
  arrest_data_subset <- reactive({
    req(input$selected_borough) # ensure availablity of value before proceeding
    filter(arrest_data, arrest_boro %in% input$selected_borough)
  })
  
  #Updating the size of the sample subset
  observe({
    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(1000, nrow(arrest_data_subset())),
                       max = nrow(arrest_data_subset())
    )
  })
  
  #Rendering the data table
  output$DataTable <- DT::renderDataTable(
    DT::datatable(data = arrest_data_subset(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  )
  
}

shinyApp(ui = ui, server = server)

