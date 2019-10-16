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
library(leaflet)
library(plotly)

#Data Source - https://data.cityofnewyork.us/Public-Safety/NYPD-Arrests-Data-Historic-/8h9b-rp9u

#Pulling the arrest data using an API
path <- "https://data.cityofnewyork.us/resource/8h9b-rp9u.json?$limit=50000"
request <- GET(url = path)
request$status_code#Checking to ensure that a non-200 status code is not returned
response <- content(request, as = "text", encoding = "UTF-8")
arrest_data <- fromJSON(response) %>%  #Putting the pulled data into a data frame
  data.frame()

#converting latitude and longitude values from characters to numeric 
arrest_data$latitude <- as.numeric(as.character(arrest_data$latitude))
arrest_data$longitude <- as.numeric(as.character(arrest_data$longitude))


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
      
      tabPanel("Data Table", br(), br(), uiOutput(outputId = "n"),br(), br(),DT::dataTableOutput("DataTable")),
      tabPanel("Donut Chart",br(), br(), plotlyOutput(outputId = "donut")),
      tabPanel("Barchart", br(), br(), plotlyOutput(outputId = "barchart"))
      # tabPanel("Map", br(), br(), br(), leafletOutput("map", height = "100%", width = "100%"))
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
  
  #Server logic for donut chart
  output$donut <- renderPlotly({
    p <- arrest_data_subset() %>%
      group_by(age_group) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~age_group, values = ~count) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Distrubution of perpetrators by age group",  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #Server logic for barchart
  #Convert to PLOTLY
  output$barchart <- renderPlotly({
    ggplotly(
      p4 <- arrest_data_subset() %>%
      group_by(age_group, perp_sex) %>%
      summarize(count = n()) %>% 
      ggplot(aes(x = age_group, y= count, fill = perp_sex)) + 
      geom_bar(stat="identity") + xlab("Age Group") + ylab("Count")
      )
    p4
  })
  
   
}

shinyApp(ui = ui, server = server)

