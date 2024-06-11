# this file contains a function to list all the possible motorcycles brands and models gathering the data from a website, basically it scrapes off all of that from
# a website that contais it and gives off a dataset 

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


##############

complite_brand_list = tibble()

url = paste0("https://www.moto.it/moto-usate")
html = read_html(url)
  
brand = html %>% html_elements(".smart-navigation-aside a") %>% html_text2()  

data_list = as.list(brand)
data = tibble(brand)

data = data %>% mutate(brand = str_replace_all(brand, " ", "-"))
data = data %>% mutate(brand = str_to_lower(brand))

n = seq(1, nrow(data))

for (i in n ){
  url = paste0("https://www.moto.it/moto-usate/", as.String(data[i,]), "")
  html = read_html(url)
  
  model = html %>% html_elements(".up-one-level+ ul a") %>% html_text2()  
  
  k = as.character(data_list[[i]])
  
  data1 = tibble(k, model)
  complite_brand_list = bind_rows(complite_brand_list, data1)

}

complite_brand_list = complite_brand_list %>% mutate(k = str_replace_all(k, " ", "-"))
complite_brand_list = complite_brand_list %>% rowwise() %>% mutate(model = paste0(across(everything(), as.character), collapse = " "))
complite_brand_list <- complite_brand_list %>% rename(Brand = k)


file_path <- "C:/Users/gulli/Desktop/moto/complite_brand&models_list.xlsx"

write.xlsx(complite_brand_list, file_path)

