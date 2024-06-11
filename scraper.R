# this is the base model of the scraping function inside of the main working file

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

##################################  


convert_model_name <- function(model_name) {
  
  moto.it_model <- str_replace(model_name, " ", "%2F")
  moto.it_model <- str_replace_all(moto.it_model, " ", "-")
  moto.it_model <- str_replace_all(moto.it_model, "ò", "o")
  moto.it_model <- gsub("\\.", "-", moto.it_model)
  moto.it_model <- tolower(moto.it_model)
  
  autoscout24_model = str_replace(model_name, " ", "/")
  autoscout24_model <- str_replace_all(autoscout24_model, " ", "-")
  autoscout24_model <- str_replace_all(autoscout24_model, "ò", "o")
  autoscout24_model <- tolower(autoscout24_model)


  #ilparking_model <- str_replace_all(model_name, " ", "-")
  
  
  subito.it_model = model_name
  
  if (model_name == "ktm 1290 super duke r"){
    vetrinamotori_model = "ktm%20superduke%201290%20r"
  }
  else {
    vetrinamotori_model <- gsub("\\.", "-", model_name)
    vetrinamotori_model <- str_replace_all(vetrinamotori_model, "ò", "o")
    vetrinamotori_model = gsub(" ", "%20", vetrinamotori_model)
  }
  
  return(list(moto.it_model, autoscout24_model, vetrinamotori_model, subito.it_model))
}

################# moto.it

moto.it_pager = function(moto.it_model) {
  page_number <- 1
  total_pages <- 0
  items_per_page <- 0
  
  while (TRUE) {
    url <- paste0("https://www.moto.it/moto-usate/ricerca/", page_number, "?offer=S&brand=&brandacc=&model=", moto.it_model, "&modelname=&version=&cat=&categoryacc=&condition_expl=&region=&province=&zipcode=&price_f=&price_t=&place=&place_rad=&longitude=&latitude=&disp_f=&disp_t=&pow_f=&pow_t=&weig_s=&weig_t=&electric=&emis_s=&strokes_s=&gear_s=&seat_s=&seat_t=&km_s=&km_t=&year_s=&year_t=&circuit=&crashed=&special=&photo=&tradein=&person=&newtype=&abs=&unpw=&sort=3_0&sortdir=&kw=&adref=&docs=&work=&rest=&pres=&asi=")
    
    # Make an HTTP GET request
    response <- GET(url)
    
    # Check the HTTP status code
    if (http_type(response) == 404) {
      cat("404 Error: Page not found. Exiting the loop.\n")
      break
    }
    
    html <- read_html(content(response, "text"))
    
    # Use a CSS selector or XPath expression to locate the items on the page
    items_on_page <- html %>% html_elements(".odd:nth-child(1) .app-infos")  # Example CSS selector
    
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

moto.it_scraper = function(moto.it_page, moto.it_model) {
  
  url = paste0("https://www.moto.it/moto-usate/ricerca/",moto.it_page,"?offer=S&brand=&brandacc=&model=",moto.it_model,"&modelname=&version=&cat=&categoryacc=&condition_expl=&region=&province=&zipcode=&price_f=&price_t=&place=&place_rad=&longitude=&latitude=&disp_f=&disp_t=&pow_f=&pow_t=&weig_s=&weig_t=&electric=&emis_s=&strokes_s=&gear_s=&seat_s=&seat_t=&km_s=&km_t=&year_s=&year_t=&circuit=&crashed=&special=&photo=&tradein=&person=&newtype=&abs=&unpw=&sort=3_0&sortdir=&kw=&adref=&docs=&work=&rest=&pres=&asi=")
  html = read_html(url) 
  brand = html %>% html_elements(".app-leaf") %>% html_text2()
  brand = brand %>%  c(html %>% html_elements(".app-leaf") %>% html_text2())
  
  # moto model
  moto_model = html %>% html_elements('.app-title') %>% html_text2()
  moto_model = moto_model %>% c(html %>% html_elements(".app-title") %>% html_text2())
  
  # moto year
  year = html %>% html_elements(".app-specs li:nth-child(2)") %>% html_text2()
  year = year %>% c(html %>% html_elements(".app-specs li:nth-child(2)") %>% html_text2())
  
  # moto km
  km = html %>% html_elements('.app-specs li:nth-child(3)') %>% html_text2()
  km = km %>% c(html %>% html_elements(".app-specs li:nth-child(3)") %>% html_text2())
  
  # moto prezzo
  price = html %>% html_elements('.app-price') %>% html_text2()
  price = price %>% c(html %>% html_elements(".app-price") %>% html_text2())
  
  # moto luogo
  place = html %>% html_elements('.app-specs li:nth-child(1)') %>% html_text2()
  place = place %>% c(html %>% html_elements(".app-specs li:nth-child(1)") %>% html_text2())
  
  # link
  link = html %>% html_elements(".app-linked-title") %>% map_chr(~ .x %>% html_attr("href"))
  link = link %>% c(html %>% html_elements(".app-linked-title") %>% map_chr(~ .x %>% html_attr("href")))
  
  # image_url
    
  image_url = html %>% html_elements("#main-content img") %>% map_chr(~ .x %>% html_attr("src"))
  image_url = image_url %>% c(html %>% html_elements("#main-content img") %>% map_chr(~ .x %>% html_attr("src")))
  
  
  # website
  website = "moto.it"
  
  
  # Return a tibble
  tibble(brand, moto_model, year, km, price, place, link, website, image_url) %>% return()
}

########## autoscout24

autoscout24_pager = function(autoscout24_model) {
  page_number <- 1
  total_pages <- 0
  items_per_page <- 0
  
  while (TRUE) {
    url <- paste0("https://www.autoscout24.com/lst-moto/",autoscout24_model,"?atype=B&cy=I&damaged_listing=exclude&desc=0&page=",page_number,"&powertype=kw&search_id=1jrd1y2yexf&sort=price&source=listpage_pagination&ustate=N%2CU")
    
    # Make an HTTP GET request
    response <- GET(url)
    
    # Check the HTTP status code
    if (http_type(response) == 404) {
      cat("404 Error: Page not found. Exiting the loop.\n")
      break
    }
    
    html <- read_html(content(response, "text"))
    
    # Use a CSS selector or XPath expression to locate the items on the page
    items_on_page <- html %>% html_elements('.NewGallery_img__bi92g')
    
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

autoscout24_scraper = function(autoscout24_page, autoscout24_model) {
  url = paste0("https://www.autoscout24.com/lst-moto/",autoscout24_model,"?atype=B&cy=I&damaged_listing=exclude&desc=0&page=",autoscout24_page,"&powertype=kw&search_id=1jrd1y2yexf&sort=price&source=listpage_pagination&ustate=N%2CU")
  
  response <- GET(url)
  
  if (http_type(response) == 404) {
    cat("404 Error: Page not found. Exiting the loop.\n")
    print("404 Error: Page not found in autoscout24")
    break
  }
  
  html = read_html(url) 
  brand = unlist(strsplit(model_name, " "))[1]
  
  #moto model
  moto_model = html %>% html_elements('#__next h2') %>% html_text2()
  moto_model = moto_model %>% c(html %>% html_elements('#__next h2') %>% html_text2())
  
  # moto year
  year = html %>% html_elements(".VehicleDetailTable_item__koEV4:nth-child(3)") %>% html_text2()
  year = year %>% c(html %>% html_elements(".VehicleDetailTable_item__koEV4:nth-child(3)") %>% html_text2())
  
  # moto km
  km = html %>% html_elements('.VehicleDetailTable_item__koEV4:nth-child(1)') %>% html_text2()
  km = km %>% c(html %>% html_elements(".VehicleDetailTable_item__koEV4:nth-child(1)") %>% html_text2())
  
  # moto prezzo
  price = html %>% html_elements('.PriceAndSeals_current_price__XscDn') %>% html_text2()
  price = price %>% c(html %>% html_elements(".PriceAndSeals_current_price__XscDn") %>% html_text2())
  
  # link
  link = html %>% html_elements(".ListItem_title__znV2I") %>% map_chr(~ .x %>% html_attr("href"))
  link = link %>% c(html %>% html_elements(".ListItem_title__znV2I") %>% map_chr(~ .x %>% html_attr("href")))
  
  # moto luogo
  place = html %>% html_elements('.SellerInfo_private__JCxcm , .SellerInfo_address__txoNV') %>% html_text2()
  place = place %>% c(html %>% html_elements(".SellerInfo_private__JCxcm , .SellerInfo_address__txoNV") %>% html_text2())
  
  image_url = html %>% html_elements('source.NewGallery_img__bi92g') %>% map_chr(~ .x %>% html_attr("srcset"))
  image_url = image_url %>% c(html %>% html_elements('source.NewGallery_img__bi92g') %>% map_chr(~ .x %>% html_attr("srcset")))
  print(image_url)
  
  # website
  website = "autoscout24"
  
  # Return a tibble
  tibble(brand, moto_model, year, km, price, link, place, website, image_url) %>%return()
}

#########vetrinamotori

vetrinamotori_pager = function(vetrinamotori_model) {
  page_number = 1
  #print(page_number)
  total_pages <- 0
  items_per_page <- 0
  
  while (TRUE) {
    #print(page_number)
    url <- paste0("https://www.vetrinamotori.it/ricerca-moto/pag-",page_number,"/?condition=&searchterm=",vetrinamotori_model,"&")
    #print(page_number)
    #print(url)
    
    # Make an HTTP GET request
    response <- GET(url)
    
    # Check the HTTP status code
    if (http_type(response) == 404) {
      #print("404 Error: Page not found in vetrinamotori paging level")
      #print(url)
      break
    }
    
    
    html1 <- read_html(content(response, "text"))
    missing1 = html1 %>% html_elements(xpath = '//*[@id="content"]/div[3]/div[2]/div/div[1]/div[1]/h2') %>% html_text2()
    missing2 = paste0("oooo", missing1)
    
    if (missing2 == "ooooOops (╥_╥)"){
      #print("moto non presente su vetrinamotori paging level")
      #print(url)
      break
    }
    
    
    
    html <- read_html(content(response, "text"))
    
    # Use a CSS selector or XPath expression to locate the items on the page
    items_on_page <- html %>% html_elements('.vehicle-photo')
    
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

vetrinamotori_scraper = function(vetrinamotori_page, vetrinamotori_model) {
  url = paste0("https://www.vetrinamotori.it/ricerca-moto/pag-",vetrinamotori_page,"/?condition=&searchterm=",vetrinamotori_model,"&")
  
  response <- GET(url)
  
  if (http_type(response) == 404) {
    print("404 Error: Page not found in vetrinamotori scraping level")
    print(url)
    break
  }
  
  html1 <- read_html(content(response, "text"))
  missing1 = html1 %>% html_elements(xpath = '//*[@id="content"]/div[3]/div[2]/div/div[1]/div[1]/h2') %>% html_text2()
  missing2 = paste0("oooo", missing1)
  
  if (missing2 == "ooooOops (╥_╥)"){
    #print("moto non presente su scraping level vetrinamotori")
    #print(url)
  }
  
  
  html = read_html(url) 
  brand = unlist(strsplit(model_name, " "))[1]
  
  # moto model
  moto_model = html %>% html_elements('.vehicle-title span') %>% html_text2()
  moto_model = moto_model %>% c(html %>% html_elements('.vehicle-title span') %>% html_text2())
  
  # moto year
  year = html %>% html_elements(".year b") %>% html_text2()
  year = year %>% c(html %>% html_elements(".year b") %>% html_text2())
  
  # moto km
  km = html %>% html_elements('.odometer b') %>% html_text2()
  km = km %>% c(html %>% html_elements(".odometer b") %>% html_text2())
  
  # moto prezzo
  price = html %>% html_elements('.price b') %>% html_text2()
  price = price %>% c(html %>% html_elements(".price b") %>% html_text2())
  
  # link
  link = html %>% html_elements(".vehicle-title a") %>% map_chr(~ .x %>% html_attr("href"))
  link = link %>% c(html %>% html_elements(".vehicle-title a") %>% map_chr(~ .x %>% html_attr("href")))
  
  # moto luogo
  place = html %>% html_elements('.location span') %>% html_text2()
  place = place %>% c(html %>% html_elements(".location span") %>% html_text2())
  
  # website
  website = "vetrina motori"
  
  
  # Return a tibble
  tibble(brand, moto_model, year, km, price, link, place, website) %>%return()
}

#########il_parking




######### subito.it

subito.it_scraper = function(subito.it_model) {
  
  # Start RSelenium and create a remote driver
  rD <- rsDriver(browser="firefox", verbose=F, chromever = NULL)
  remDr =rD[["client"]]
  #remDr$setWindowSize(width = 800, height = 300)
  remDr$maxWindowSize()
  
  # URL of the first page
  url <- "https://www.subito.it"
  
  # Create an empty data frame to store the results
  subito.it_data <- data.frame()
  
  remDr$navigate(url)
  Sys.sleep(1)
  
  
  ####################################################### coockie button click
  
  next_button1 <- tryCatch(
    remDr$findElement(using = "css", value = "#didomi-notice-agree-button"),
    error = function(e) NULL
  )
  
  if (is.null(next_button1)) {
    break  # No "Next" button found, exit the loop
  }
  
  # Click the "Next" button to go to the next page
  next_button1$clickElement()
  
  #Sys.sleep(3)
  
  ####################################################### select button 
  
  select_button1 <- tryCatch(
    remDr$findElement(using = "css", value = "#main-category-selection"),
    error = function(e) NULL
  )
  
  if (is.null(select_button1)) {
    break  # No "Next" button found, exit the loop
  }
  
  # Click the "Next" button to go to the next page
  select_button1$clickElement()
  
  ####################################################### select button click
  
  select_button2 <- tryCatch(
    remDr$findElement(using = "css", value = "#anchor-motori > ul:nth-child(2) > li:nth-child(3) > button:nth-child(1)"),
    error = function(e) NULL
  )
  
  if (is.null(select_button2)) {
    break  # No "Next" button found, exit the loop
  }
  
  # Click the "Next" button to go to the next page
  select_button2$clickElement()
  
  ####################################################  search bar click
  
  search_button1 <- tryCatch(
    remDr$findElement(using = "css", "#main-keyword-field"),
    error = function(e) NULL
  )
  
  if (is.null(search_button1)) {
    break  # No "Next" button found, exit the loop
  }
  
  # Click the "Next" button to go to the next page
  search_button1$sendKeysToElement(list(subito.it_model))
  
  #Sys.sleep(5)
  ####################################################  send search click
  
  send_button1 <- tryCatch(
    remDr$findElement(using = "css", value = ".Form_button-icon-lens__Nazjt > img:nth-child(1)"),
    error = function(e) NULL
  )
  
  if (is.null(send_button1)) {
    break  # No "Next" button found, exit the loop
  }
  
  # Click the "Next" button to go to the next page
  send_button1$clickElement()
  
  #Sys.sleep(5)
  ##################################################
  
  scroll_button1 <- tryCatch(
    remDr$findElement(using = "css", value = ".index-module_wrapper__Lj83L"),
    error = function(e) NULL
  )
  
  if (is.null(scroll_button1)) {
    break  # No "Next" button found, exit the loop
  }
  
  # Click the "Next" button to go to the next page
  scroll_button1$clickElement()
  
  #Sys.sleep(5)
  #################################################
  
  ok_button1 <- tryCatch(
    remDr$findElement(using = "css", value = "#sort__option--2"),
    error = function(e) NULL
  )
  
  if (is.null(ok_button1)) {
    break  # No "Next" button found, exit the loop
  }
  
  # Click the "Next" button to go to the next page
  ok_button1$clickElement()
  
  Sys.sleep(1)
  #################################################
  
  # Loop to navigate through pages
  while (TRUE) {
    
    # Scrape data from the current page
    html <- read_html(remDr$getPageSource()[[1]], encoding = "utf-8")
    
    brand = unlist(strsplit(model_name, " "))[1]
    
    # moto model
    moto_model = html %>% html_elements('.SmallCard-module_item-title__1y5U3') %>% html_text2()
    #moto_model = moto_model %>% c(html %>% html_elements(".SmallCard-module_item-title__1y5U3") %>% html_text2())
    #print(moto_model)
    
    # moto year
    year = html %>% html_elements('.index-module_info__GDGgZ:nth-child(2)') %>% html_text2()
    #year = year %>% c(html %>% html_elements(".index-module_info__GDGgZ:nth-child(2)") %>% html_text2())
    #print(year)
    
    
    # moto km
    km = html %>% html_elements('.index-module_info__GDGgZ:nth-child(3)') %>% html_text2()
    #km = km %>% c(html %>% html_elements(".index-module_info__GDGgZ:nth-child(3)") %>% html_text2())
    #print(km)
    
    
    # moto prezzo
    price = html %>% html_elements('.index-module_small__4SyUf') %>% html_text2()
    #price = price %>% c(html %>% html_elements('.index-module_small__4SyUf') %>% html_text2())
    #price = price[seq_along(price) %% 2 != 0]
    #print(price)
    
    
    # moto luogo
    place = html %>% html_elements('.index-module_town__2H3jy') %>% html_text2()
    #place = place %>% c(html %>% html_elements(".index-module_town__2H3jy") %>% html_text2())
    #print(place)
    
    
    # link
    link = html %>% html_elements('.SmallCard-module_link__hOkzY') %>% html_attr("href")
    #link = link[!duplicated(link)]
    #link = link %>% c(html %>% html_elements('.SmallCard-module_picture-group__asLo2') %>% map_chr(~ .x %>% html_attr("href")))
    #print(link)
    
    
    # website
    website = "subito.it"
    
    
    # Return a tibble
    data = tibble(brand, moto_model, year, km, price, place, link, website)
    
    subito.it_data = bind_rows(subito.it_data, data)
    
    # scroll down 
    
    #scroll_pixels <- 8000  # Adjust the value as needed
    
    # Execute JavaScript to scroll down the page
    #js_script <- paste("window.scrollBy(0, ", scroll_pixels, ");")
    #remDr$executeScript(script = js_script)
    
    #Sys.sleep(5)
    
    
    # Attempt to find the "Next" button
    next_button <- tryCatch(
      remDr$findElement(using = "css", value = ".index-module_icon-only__gkRU8+ .index-module_outline__reo8F"),
      error = function(e) NULL
    )
    
    if (is.null(next_button)) {
      break  # No "Next" button found, exit the loop
    }
    
    # Click the "Next" button to go to the next page
    next_button$clickElement()
    
    #######################################################
    
    
    
    
    #button_number <- 4
    
    #while (TRUE) {
    # Build the CSS selector with the current button number
    #css_selector <- paste0("button.index-module_sbt-button__hQMUx:nth-child(", button_number, ")") ".pagination__btn"
    
    # Try to find the "Next" button with the current CSS selector
    #next_button <- tryCatch(
    #remDr$findElement(using = "css", value = css_selector),
    #error = function(e) NULL
    #)
    
    #if (is.null(next_button)) {
    #break  # No "Next" button found, exit the loop
    #}
    
    # Click the "Next" button
    #next_button$clickElement()
    
    # Increment the button number for the next iteration
    #button_number <- button_number + 1
    #}
    
    # Click the "Next" button to go to the next page
    #next_button$clickElement()
    
    
    ##############################################################################
    
    # Get the URL of the next page
    url <- remDr$getCurrentUrl()
  }
  
  # Close the connection
  remDr$close()
  rD$server$stop()
  
  return(subito.it_data)
  
  
  
}

################################
#moto scraper 
scraper = function (model_name) {
  
  moto.it_model = convert_model_name(model_name)[[1]]
  moto.it_page = 1:(moto.it_pager(moto.it_model))
  data_moto.it = map_df(moto.it_page, ~moto.it_scraper(moto.it_model, moto.it_page = .))
  data_moto.it$link <- paste0("https://www.moto.it/", data_moto.it$link)
  print("moto.it cicle done")
  
  
  autoscout24_model = convert_model_name(model_name)[[2]]
  autoscout24_page = 1:(autoscout24_pager(autoscout24_model))
  data_autoscout24 = map_df(autoscout24_page, ~autoscout24_scraper(autoscout24_model, autoscout24_page = .))
  data_autoscout24$link <- paste0("https://www.autoscout24.com/", data_autoscout24$link)
  print("autoscout24 cicle done")
  
  
  
  ############### vetrinamotori

  vetrinamotori_model = convert_model_name(model_name)[[3]]
  vetrinamotori_verifier = vetrinamotori_pager(vetrinamotori_model)
  vetrinamotori_page = 1:vetrinamotori_verifier
  #print(vetrinamotori_page)
  
  if (vetrinamotori_verifier == 0) {
    data_vetrinamotori = tibble()
    print("nessun annuncio presente su vetrinamotori")
  }
  
  else {
    data_vetrinamotori = map_df(vetrinamotori_page, ~vetrinamotori_scraper(vetrinamotori_model, vetrinamotori_page = .))
    data_vetrinamotori$link <- paste0("https://www.vetrinamotori.it/", data_vetrinamotori$link)
    print("vetrinamotori cicle done")
  }
  
  ########## ilparking
  
  #in_parking_model = convert_model_name(model_name)[[5]]
  #data_in_parking = in_parking_scraper(in_parking_model)
  #print(data_in_parking)
  
  #if (nrow(data_in_parking) == 0) {
    
  #  print("nessun annuncio presente su ilparking")
  #}
  
  #else {
  #  data_in_parking$link <- paste0("https://www.ilparking-moto.it/moto-occasione-dettaglio", data_in_parking$link)
  #  print("ilparking cicle done")
  #}
  
  
  ############# subito
  
  subito.it_model = convert_model_name(model_name)[[4]]
  data_subito.it = subito.it_scraper(subito.it_model)
  print("subito.it cicle done")
  
  
  ################## data
  

  data_complete = bind_rows(data_moto.it, data_autoscout24, data_vetrinamotori, data_subito.it)
  
  
  # trim unnecessary characters
  data_complete = data_complete %>% mutate(km = str_replace_all(km, "[^0-9]+", ""))
  data_complete = data_complete %>% mutate(price = str_replace_all(price, "[^0-9]+", ""))
  data_complete = data_complete %>% mutate(year = str_replace_all(year, ".*/", ""))
  
  # turn strngs in numbers
  data_complete$km = as.numeric(data_complete$km)
  data_complete$price = as.numeric(data_complete$price)
  data_complete$year = as.numeric(data_complete$year)
  
  # eliminate multiple of the same posts
  data_complete = data_complete[!(duplicated(data_complete$km) & duplicated(data_complete$price)), ]
  
  # create a coefficient km/price
  data_complete = data_complete %>% mutate(km_on_price = (km/price))
  data_complete = data_complete %>% select(1:5, 9, 6, 7:8)

  
  return(data_complete)
  
}


##############



# works   / single tests

model_name = readline("inserire modello moto: ")
data_complete = scraper(model_name)


# try


complite_brand_list = read.xlsx("C:/Users/Guglielmo/Desktop/moto/complite_brand&models_list.xlsx")

model_name1 = as.list(complite_brand_list[,2])

model_name2 = model_name1 %>% tibble()
model_name2 = rename(model_name2,model = .)
model_name2 = model_name2 %>% mutate(model = str_replace_all(model, " ", "_"))

data_complete = tibble()

for (i in seq(1:length(model_name1))) {
  
  new_name = paste0("begin scraping: ", model_name1[[i]])
  print(new_name)
  
  model_name = model_name1[[i]]
  
  data_complete1 = scraper(model_name)
  data_complete = bind_rows(data_complete, data_complete1)
  
  new = paste0(model_name1[[i]], " dataset")
  
  file_path <- paste0("C:/Users/Guglielmo/Desktop/moto/datasets/",new ,".xlsx")
  
  write.xlsx(data_complete1, file_path)
  
  print(paste0(model_name1[[i]], " salvato con successo"))
  
  new_name1 = paste0("finished scraping: ", model_name1[[i]])
  print(new_name1)
  
  #assign(new_name, data_complete)
  
}




# Create a scatterplot
data_complete %>%
  filter(price < 9000) %>%
  #filter(km < 100000) %>%
  ggplot(aes(x = km, y = price, color = as.factor(year))) +
  geom_point(size = 3) +
  geom_smooth(col = "black") +
  labs(x = "km", y = "price", color = "year", title = model_name) +
  theme_minimal()






n = "ktm 1290 super duke r"

print(str_remove(n, "\\b\\w+\\s*"))
  




