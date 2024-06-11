# this is an attempt of extracting the geographical informations of a place starting froma  name, but for now it hasnt been succesfull due to some complications,
# it's in standby for now


####################
library(rvest)
library(purrr)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(scales)
library(tm)
library(wordcloud)
library(igraph)
library(ggraph)
library(reshape2)
library(corpustools)
library(stringr)
library(httr)
library(RSelenium)
library(robotstxt)
library(wordcloud2)
library(textplot)
library(cld2)
library(quanteda)
library(quanteda.textmodels)
library(topicmodels)
library(stringr)
library(ggplot2)
library(openxlsx)
library(tmaptools)
#################



# Geocode with Nominatim (OpenStreetMap)
location <- geocode_OSM("Villorba")

# Print the latitude and longitude
print(location)



data_complete1 = data_complete %>% mutate(place = gsub("[^a-zA-Z' ]", " ", place))
data_complete1 = data_complete1 %>% mutate(place = gsub("nell|Private seller IT |-|•|.*\\bIT\\b", " ", place))
#data_complete1 = data_complete1 %>% mutate(place = sub(".*\\bIT\\b", "", place))
#data_complete1 = data_complete1 %>% mutate(place = sub("[^a-zA-Z]", "", place))

data_complete_geo <- data_complete1 %>% mutate(x = geocode_OSM(data_complete1$place)[[2]])




data_complete_geo <- data_complete1 %>% mutate(x = if_else(length(geocode_OSM(place)) == 0, NA_real_, geocode_OSM(place)[[2]]))




