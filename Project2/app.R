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
library(ggplot2)
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
      radioButtons(inputId = "selected_borough",
                         label = "SEARCH BY BOROUGH",
                         choices = c("Q","K","M","S","B"),
                         selected = "M"),
      
      checkboxGroupInput(inputId = "selected_race",
                         label = "SEARCH BY RACE",
                         choices = c("BLACK","ASIAN / PACIFIC ISLANDER", "WHITE", 
                                     "WHITE HISPANIC", "BLACK HISPANIC", 
                                     "AMERICAN INDIAN/ALASKAN NATIVE"),
                         selected = "BLACK"),
      
      #Selecting the size of the sample
      numericInput(inputId = "n_samp", 
                   label = "Sample size:", 
                   min = 1, max = nrow(arrest_data), 
                   value = 1000),
      
      #Download button to download the current datatable being displayed
      downloadButton("downloadData", "Download Data")
    ),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("Data Table", br(), br(), uiOutput(outputId = "n"),br(), br(),DT::dataTableOutput("DataTable")),
      tabPanel("Donut Chart",br(), br(), plotlyOutput(outputId = "donut")),
      tabPanel("Barchart", br(), br(), plotlyOutput(outputId = "barchart")),
      tabPanel("Map", br(), br(), br(), leafletOutput("map"))
    )
   )
  ) 
)

server <- function(input, output, session){
  
  #Subsetting data based on the selection of boroughs
  arrest_data_subset <- reactive({
    req(input$selected_borough) # ensure availablity of value before proceeding
    filter(arrest_data, arrest_boro %in% input$selected_borough, perp_race %in% input$selected_race)
  })
  
  #Updating the size of the sample subset
  observe({
    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(1000, nrow(arrest_data_subset())),
                       max = nrow(arrest_data_subset())
    )
  })
  
  # A new dataframe that subsets the sample based on the size requested
  arrest_data_sample <- reactive({ 
    req(input$n_samp) 
    sample_n(arrest_data_subset(), input$n_samp)
  })
  
  #Rendering the data table
  output$DataTable <- DT::renderDataTable(
    DT::datatable(data = arrest_data_sample(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  )
  
  #Server logic for donut chart
  output$donut <- renderPlotly({
    p <- arrest_data_sample() %>%
      group_by(age_group) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~age_group, values = ~count) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Distrubution of perpetrators by age group",  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #Server logic for barchart
  output$barchart <- renderPlotly({
    ggplotly(
      p <- arrest_data_sample() %>%
      group_by(age_group, perp_sex) %>%
      summarize(count = n()) %>% 
      ggplot(aes(x = age_group, y= count, fill = perp_sex)) + 
      geom_bar(stat="identity") + xlab("Age Group") + ylab("Count")
      )
    p
  })
  
  #Download csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(arrest_data_sample(), Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(arrest_data_sample(), file, row.names = TRUE)
    }
  )
  

  #designing a custom palette
  pallete <- colorFactor(c("#3a981a","#98271a","#271a98","#98901a","#981a98"),
                         c("25-44", "18-24","45-64","65+","<18"))
  
  
  #Server logic for leaflet map, those that won't need to change dynamically
  output$map <- renderLeaflet({
    leaflet(arrest_data) %>% addTiles() %>%
       fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  
  #Incremental changes on the map are placed in this observer
  observe({
  
    leafletProxy("map", data = arrest_data_subset()) %>%
      clearShapes() %>%
      clearMarkers()%>%
      clearControls()%>%
      # Base groups
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.Watercolor, group = "Watercolor") %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1.5, color = ~pallete(age_group)) %>%
      addLegend(position = "topright" , pal = pallete, values = ~age_group, 
                title = "Age Group") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Watercolor","Terrain"),
        overlayGroups = c("Arrest Locations", "Borough Outline"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
   
}

shinyApp(ui = ui, server = server)

