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
library(png)

##############

cver = function(model_name) {
  
  if (model_name == "KTM 1290 Super Duke R"){
    moto.it_model = "ktm-super_duke"
  }
  
  if (startsWith(model_name, "Kawasaki Z")){
    moto.it_model = str_replace_all(model_name, "([a-zA-Z])\\s+(\\d)", "\\1\\2")
    moto.it_model <- str_replace(moto.it_model, " ", "-")
    moto.it_model <- str_replace_all(moto.it_model, " ", "_")
    moto.it_model <- str_replace_all(moto.it_model, "ò", "o")
    moto.it_model <- tolower(moto.it_model)
  }
  
  else { 
    moto.it_model <- str_replace(model_name, " ", "-")
    moto.it_model <- str_replace_all(moto.it_model, " ", "_")
    moto.it_model <- str_replace_all(moto.it_model, "ò", "o")
    moto.it_model <- tolower(moto.it_model)
  }
  
  print(moto.it_model)
  
  url = paste0("https://www.insella.it/listino_moto/", moto.it_model)
  html = read_html(url)
  
  # part
  model = model_name
  
  # stars
  variant = html %>% html_elements('.versione_auto') %>% html_text2()
  
  # stars
  cv = html %>% html_elements('td:nth-child(6)') %>% html_text2()
  
  # Return a tibble
  tibble(model, variant, cv) %>% return()
  
}


positiver = function(model_name) {
  
  if (model_name == "KTM 1290 Super Duke R"){
    moto.it_model = "ktm-super_duke"
  }
  if (startsWith(model_name, "Kawasaki Z")){
    moto.it_model = str_replace_all(model_name, "([a-zA-Z])\\s+(\\d)", "\\1\\2")
    moto.it_model <- str_replace(moto.it_model, " ", "-")
    moto.it_model <- str_replace_all(moto.it_model, " ", "_")
    moto.it_model <- str_replace_all(moto.it_model, "ò", "o")
    moto.it_model <- tolower(moto.it_model)
  }
  else { 
    moto.it_model <- str_replace(model_name, " ", "-")
    moto.it_model <- str_replace_all(moto.it_model, " ", "_")
    moto.it_model <- str_replace_all(moto.it_model, "ò", "o")
    moto.it_model <- tolower(moto.it_model)
  }
  
  
  
  url = paste0("https://www.insella.it/listino_moto/", moto.it_model)
  html = read_html(url)
  
  # part
  model = model_name
  
  # stars
  positive = html %>% html_elements('.scheda_perchesi+ dd li') %>% html_text2()
  
  # Return a tibble
  tibble(model, positive) %>% return()
  
}

negativer = function(model_name) {
  
  if (model_name == "KTM 1290 Super Duke R"){
    moto.it_model = "ktm-super_duke"
  }
  if (startsWith(model_name, "Kawasaki Z")){
    moto.it_model = str_replace_all(model_name, "([a-zA-Z])\\s+(\\d)", "\\1\\2")
    moto.it_model <- str_replace(moto.it_model, " ", "-")
    moto.it_model <- str_replace_all(moto.it_model, " ", "_")
    moto.it_model <- str_replace_all(moto.it_model, "ò", "o")
    moto.it_model <- tolower(moto.it_model)
  }
  else { 
    moto.it_model <- str_replace(model_name, " ", "-")
    moto.it_model <- str_replace_all(moto.it_model, " ", "_")
    moto.it_model <- str_replace_all(moto.it_model, "ò", "o")
    moto.it_model <- tolower(moto.it_model)
  }
  
  
  
  url = paste0("https://www.insella.it/listino_moto/", moto.it_model)
  html = read_html(url)
  
  # part
  model = model_name
  
  # stars
  negative = html %>% html_elements('.scheda_serie_percheno+ dd li') %>% html_text2()
  
  # Return a tibble
  tibble(model, negative) %>% return()
  
}


sintezier = function(model_name) {
  
  if (model_name == "KTM 1290 Super Duke R"){
    moto.it_model = "ktm-super_duke"
  }
  if (startsWith(model_name, "Kawasaki Z")){
    moto.it_model = str_replace_all(model_name, "([a-zA-Z])\\s+(\\d)", "\\1\\2")
    moto.it_model <- str_replace(moto.it_model, " ", "-")
    moto.it_model <- str_replace_all(moto.it_model, " ", "_")
    moto.it_model <- str_replace_all(moto.it_model, "ò", "o")
    moto.it_model <- tolower(moto.it_model)
  }
  else { 
    moto.it_model <- str_replace(model_name, " ", "-")
    moto.it_model <- str_replace_all(moto.it_model, " ", "_")
    moto.it_model <- str_replace_all(moto.it_model, "ò", "o")
    moto.it_model <- tolower(moto.it_model)
  }
  
  
  
  url = paste0("https://www.insella.it/listino_moto/", moto.it_model)
  html = read_html(url)
  
  # part
  model = model_name
  
  # stars
  sintesis = html %>% html_elements('.top-boxes .textarea-content-body') %>% html_text2()
  
  # Return a tibble
  tibble(model, sintesis) %>% return()
  
}




model_name = "Kawasaki Z 900"

specs = cver(model_name)



positives = as.String(positiver(model_name)[,2])
positives = positives %>% str_split(",", " ")

print(positives)
































image_pager = function(brand) {
  page_number <- 1
  total_pages <- 0
  items_per_page <- 0
  
  while (TRUE) {
    url <- paste0("https://www.moto.it/social/recensioni/", brand,"/pagina-", page_number)
    
    # Make an HTTP GET request
    response <- GET(url)
    
    # Check the HTTP status code
    if (http_type(response) == 404) {
      cat("404 Error: Page not found. Exiting the loop.\n")
      break
    }
    
    html <- read_html(content(response, "text"))
    
    # Use a CSS selector or XPath expression to locate the items on the page
    items_on_page <- html %>% html_elements(".mit-main-link")  # Example CSS selector
    
    if (length(items_on_page) == 0) {
      # No items found, exit the loop
      break
    }
    
    items_per_page <- length(items_on_page)
    total_pages <- total_pages + 1
    page_number <- page_number + 1
  }
  return(total_pages)
}

image_gather = function(brand, i) {
  
  url = paste0("https://www.moto.it/social/recensioni/", brand,"/pagina-", i)
  html = read_html(url)
  
  k = 1:15
  
  a1 = tibble()
  
  for (j in k) {
    
    model = html %>% html_elements(paste0("li.mit-list-item:nth-child(",j,") > div:nth-child(2) > a:nth-child(1)")) %>% html_text2()
    #model = model %>% c(html %>% html_elements(xpath = '//*[@id="main-content"]/div[2]/div[4]/div[1]/div/ul/li[1]/div[2]/a') %>% html_text2())
  
    image_url = html %>% html_elements(paste0("li.mit-list-item:nth-child(",j,") > div:nth-child(1) > img:nth-child(1)")) %>% map_chr(~ .x %>% html_attr("src"))
    #image_url = image_url %>% c(html %>% html_elements(xpath = '//*[@id="main-content"]/div[2]/div[4]/div[1]/div/ul/li[1]/div[1]/img') %>% map_chr(~ .x %>% html_attr("src")))
    
    a = tibble(model, image_url)
    a1 = bind_rows(a1, a)
    
  }
  
  
  
  # Return a tibble
  return(a1)
  
}

imager = function(brand) {
  
  pages = seq(1, (image_pager(brand)))
  
  images = tibble()
  
  for (i in pages) {
  
    images1 = image_gather(brand, i)
    print(images1)
    
    images = bind_rows(images, images1)
    
  }
  for (i in 1:nrow(images)) {
  
  image_url = as.String(images[i,2])
  
  download.file(image_url, destfile = paste0("C:/Users/Guglielmo/Desktop/moto/images/", toupper(brand), " ", images[i,1], ".jpg"), mode = "wb")
  }
}

complite_brand_list = read.xlsx("C:/Users/Guglielmo/Desktop/moto/complite_brand&models_list.xlsx")

complite_brand_list <- complite_brand_list %>% filter(row_number() >= which(Brand == "MBP"))

complite_brand_list = unique(complite_brand_list$Brand)

for (i in complite_brand_list) {
  
  brand = tolower(i)
  
  imager(brand)
  
}














































