library(tidyverse)  
library(httr)  
library(jsonlite)

#Pulling the arrest data using an API

path <- "https://data.cityofnewyork.us/resource/8h9b-rp9u.json?$limit=50000"
request <- GET(url = path)
request$status_code#Checking to ensure that a non-200 status code is not returned
response <- content(request, as = "text", encoding = "UTF-8")
arrests <- fromJSON(response) %>%  #Putting the pulled data into a data frame
  data.frame()


