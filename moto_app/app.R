###############

library(shiny)
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
library(DT)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinycssloaders)
library(png)
################

# global variables

complete_brand_list <- read.xlsx("D:/documenti/progetti_vari/moto/complite_brand&models_list.xlsx")

date <- Sys.Date()

##################################  


convert_model_name = function(model_name) {
  
  moto.it_model <- str_replace(model_name, " ", "%2F")
  moto.it_model <- str_replace_all(moto.it_model, " ", "-")
  moto.it_model <- str_replace_all(moto.it_model, "ò", "o")
  moto.it_model <- gsub("\\.", "-", moto.it_model)
  moto.it_model <- tolower(moto.it_model)
  #print(model.it_model)
  
  autoscout24_model = str_replace(model_name, " ", "/")
  autoscout24_model <- str_replace_all(autoscout24_model, " ", "-")
  autoscout24_model <- str_replace_all(autoscout24_model, "ò", "o")
  autoscout24_model <- tolower(autoscout24_model)
  
  
  #ilparking_model <- str_replace_all(model_name, " ", "-")
  
  
  subito.it_model = model_name
  
  if (model_name == "KTM 1290 Super Duke R"){
    vetrinamotori_model = "ktm%20superduke%201290%20r"
  }
  else {
    vetrinamotori_model <- gsub("\\.", "-", model_name)
    vetrinamotori_model <- str_replace_all(vetrinamotori_model, "ò", "o")
    vetrinamotori_model = gsub("-", " ", vetrinamotori_model)
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
    items_on_page <- html %>% html_elements('.Gallery_wrapper__iqp3u')
    
    if (length(items_on_page) == 0) {
      # No items found, exit the loop
      break
    }
    
    items_per_page <- length(items_on_page)
    total_pages <- total_pages + 1
    page_number <- page_number + 1
  }
  print(total_pages)
  return(total_pages)
}

autoscout24_scraper = function(autoscout24_page, autoscout24_model) {
  url = paste0("https://www.autoscout24.com/lst-moto/",autoscout24_model,"?atype=B&cy=I&damaged_listing=exclude&desc=0&page=",autoscout24_page,"&powertype=kw&search_id=1jrd1y2yexf&sort=price&source=listpage_pagination&ustate=N%2CU")
  print(url)
  
  response <- GET(url)
  
  if (http_type(response) == 404) {
    cat("404 Error: Page not found. Exiting the loop.\n")
    print("404 Error: Page not found in autoscout24")
    break
  }
  
  html = read_html(url)
  
  brand = unlist(strsplit(autoscout24_model, "/"))[1]
  
  #moto model
  moto_model = html %>% html_elements('#__next h2') %>% html_text2()
  moto_model = moto_model %>% c(html %>% html_elements('#__next h2') %>% html_text2())
  print(moto_model)
  
  # moto year
  year = html %>% html_elements(".VehicleDetailTable_item__4n35N:nth-child(3)") %>% html_text2()
  year = year %>% c(html %>% html_elements(".VehicleDetailTable_item__4n35N:nth-child(3)") %>% html_text2())
  print(year)
  
  # moto km
  km = html %>% html_elements('.VehicleDetailTable_item__4n35N:nth-child(1)') %>% html_text2()
  km = km %>% c(html %>% html_elements(".VehicleDetailTable_item__4n35N:nth-child(1)") %>% html_text2())
  print(km)
  
  # moto prezzo
  price = html %>% html_elements('.PriceAndSeals_current_price__ykUpx') %>% html_text2()
  price = price %>% c(html %>% html_elements(".PriceAndSeals_current_price__ykUpx") %>% html_text2())
  print(price)
  
  
  # errore in link, non trova il css indicator
  # link
  link = html %>% html_elements(".ListItem_title__ndA4s") %>% map_chr(~ .x %>% html_attr("href"))
  link = link %>% c(html %>% html_elements(".ListItem_title__ndA4s") %>% map_chr(~ .x %>% html_attr("href")))
  print(link)
  
  # moto luogo
  place = html %>% html_elements('.SellerInfo_address__leRMu , .SellerInfo_private__THzvQ') %>% html_text2()
  place = place %>% c(html %>% html_elements(".SellerInfo_address__leRMu , .SellerInfo_private__THzvQ") %>% html_text2())
  print(place)
  
  # image_url
  
  #image_url = html %>% html_elements(".NewGallery_img__bi92g") %>% map_chr(~ .x %>% html_attr("src"))
  #image_url = image_url %>% c(html %>% html_elements(".NewGallery_img__bi92g") %>% map_chr(~ .x %>% html_attr("src")))
  
  # website
  website = "autoscout24"
  
  
  # Return a tibble
  tibble(brand, moto_model, year, km, price, link, place, website) %>% return()
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
  brand = unlist(strsplit(vetrinamotori_model, "%20"))[1]
  
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
  
  # image_url
  
  image_url = html %>% html_elements("#content .img-responsive") %>% map_chr(~ .x %>% html_attr("src"))
  image_url = image_url %>% c(html %>% html_elements("#content .img-responsive") %>% map_chr(~ .x %>% html_attr("src")))
  
  # moto luogo
  place = html %>% html_elements('.location span') %>% html_text2()
  place = place %>% c(html %>% html_elements(".location span") %>% html_text2())
  
  # website
  website = "vetrina motori"
  
  
  # Return a tibble
  tibble(brand, moto_model, year, km, price, link, place, website, image_url) %>% return()
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
    
    brand = unlist(strsplit(subito.it_model, " "))[1]
    
    # moto model
    moto_model = html %>% html_elements('.SmallCard-module_item-title__1y5U3') %>% html_text2()
    #moto_model = moto_model %>% c(html %>% html_elements(".SmallCard-module_item-title__1y5U3") %>% html_text2())
    print(moto_model)
    
    # moto year
    year = html %>% html_elements('.index-module_info__GDGgZ:nth-child(2)') %>% html_text2()
    #year = year %>% c(html %>% html_elements(".index-module_info__GDGgZ:nth-child(2)") %>% html_text2())
    
    print(year)
    
    
    # moto km
    km = html %>% html_elements('.index-module_info__GDGgZ:nth-child(3)') %>% html_text2()
    #km = km %>% c(html %>% html_elements(".index-module_info__GDGgZ:nth-child(3)") %>% html_text2())
    
    if (length(km) != length(moto_model)){
      
      km = c(km, rep(0, length(moto_model) - length(km)))
      
      print ("subito è una merda")
    }
    
    else {
      km = km
    }
    
    # Print the updated km value
    print(km)
    
    
    # moto prezzo
    price = html %>% html_elements('.index-module_small__4SyUf') %>% html_text2()
    #price = price %>% c(html %>% html_elements('.index-module_small__4SyUf') %>% html_text2())
    #price = price[seq_along(price) %% 2 != 0]
    print(price)
    
    
    # moto luogo
    place = html %>% html_elements('.index-module_town__2H3jy') %>% html_text2()
    #place = place %>% c(html %>% html_elements(".index-module_town__2H3jy") %>% html_text2())
    print(place)
    
    
    # link
    link = html %>% html_elements('.SmallCard-module_link__hOkzY') %>% html_attr("href")
    #link = link[!duplicated(link)]
    #link = link %>% c(html %>% html_elements('.SmallCard-module_picture-group__asLo2') %>% map_chr(~ .x %>% html_attr("href")))
    print(link)
    
    
    # image_url
    
    #image_url = html %>% html_elements(".CardImage-module_photo-container__RYspN") %>%  map_chr(~ .x %>% html_attr("src"))
    #print(image_url)
    
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
  data_complete <- data_complete %>% mutate(km_on_price = round(((km/10)+price)/1000, 2))
  data_complete = data_complete %>% select(1:5, 10, 6, 7:9)
  
  # delete non pertinent rows
  data_complete <- data_complete %>% filter(grepl(str_remove(model_name, "\\b\\w+\\s*"), moto_model, ignore.case = TRUE))
  
  
  
  #save dataset
  new = paste0(model_name, " ", date, " dataset.xlsx")
  file_path <- paste0("D:/documenti/progetti_vari/moto/datasets/",new)
  write.xlsx(data_complete, file_path)
  
  return(data_complete)
  
}


#######
# image search
#########
cver = function(model_name) {
  
  if (model_name == "KTM 1290 Super Duke R"){
    moto.it_model = "ktm-super_duke"
  }
  
  else if (startsWith(model_name, "Kawasaki Z")){
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
  
  #print(moto.it_model)
  
  url = paste0("https://www.insella.it/listino_moto/", moto.it_model)
  html = read_html(url)
  
  # part
  model = model_name
  
  # stars
  variant = html %>% html_elements('td+ td a') %>% html_text2()
  
  # stars
  cv = html %>% html_elements('td:nth-child(6)') %>% html_text2()
  
  # Return a tibble
  tibble(model, variant, cv) %>% return()
  
}


positiver = function(model_name) {
  
  if (model_name == "KTM 1290 Super Duke R"){
    moto.it_model = "ktm-super_duke"
  }
  else if (startsWith(model_name, "Kawasaki Z")){
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
  positive = html %>% html_elements('.node__content__sub:nth-child(4) div') %>% html_text2()
  
  # Return a tibble
  tibble(model, positive) %>% return()
  
}

negativer = function(model_name) {
  
  if (model_name == "KTM 1290 Super Duke R"){
    moto.it_model = "ktm-super_duke"
  }
  else if (startsWith(model_name, "Kawasaki Z")){
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
  
  tryCatch({
    html = read_html(url)
    
    # part
    model = model_name
    
    # stars
    negative = html %>% html_elements('.node__content__sub:nth-child(5) div') %>% html_text2()
    
    # Return a tibble
    tibble(model, negative) %>% return()
    
  }, error = function(e) {
    # If an error occurs (e.g., table cannot be accessed), return an empty table
    tibble(model = character(), negative = character())
  })
  
}


sintezier = function(model_name) {
  
  if (model_name == "KTM 1290 Super Duke R"){
    moto.it_model = "ktm-super_duke"
  }
  else if (startsWith(model_name, "Kawasaki Z")){
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
  
  tryCatch({
    html = read_html(url)
    
    # part
    model = model_name
    
    # stars
    sintesis = html %>% html_elements('#dfp-ins-slot-2+ .node__content__sub div') %>% html_text2()
    
    # Return a tibble
    tibble(model, sintesis) %>% return()
  }, error = function(e) {
    # If an error occurs (e.g., table cannot be accessed), return an empty table
    tibble(model = character(), sintesis = character())
  })
  
}

#######





# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    disable = TRUE
  ),
  dashboardSidebar(
    title = tags$div(
      style = "font-size: 30px; display: flex; align-items: center; justify-content: center;",
      "Moto Finder IT"
    ),
    selectInput("brand_name", label = "Select Brand:", choices = unique(complete_brand_list$Brand)),
    selectInput("model_name", label = "Select Moto Model:", choices = NULL),
    actionButton("scrapeButton", "Search"),
    actionButton("closeButton", "Close App", style = "color: white; background-color: red;"),
    sliderInput("km_slider", "Select Kilometer Range:",
                min = 0, max = 1000000, value = c(0, 1000000), width = "100%"),
    sliderInput("price_slider", "Select Price Range:",
                min = 0, max = 1000000, value = c(0, 1000000), width = "100%"),
    sliderInput("year_slider", "Select Year Range:",
                min = 0, max = 2023, value = c(0, 2023), width = "100%")
  ),
  dashboardBody(
    tags$style(HTML(".main-sidebar {position: fixed; height: 100%;}")),
    fluidRow(
      box(
        width = 12,
        htmlOutput("ad")
      ),
      box(
        width = 8,
        title = "Announcements",
        withSpinner(DTOutput("data_complete_table")),
        withSpinner(plotlyOutput("scatter_plot")),
      ),
      box(
        width = 4,
        title = "General info",
        withSpinner(imageOutput("image")),
        withSpinner(plotlyOutput("points")),
        withSpinner(tableOutput("rating_table"))
      ),
      box(
        box(
          width = 12,
          height = 255,
          title = "Current bike",
          textOutput("brand_text"),
          textOutput("model_text"),
        ),
        box(
          width = 12,
          withSpinner(imageOutput("image1")),
          box(
            width = 12,
            box(
              width = 6,
              withSpinner(tableOutput("cv_1"))
            ),
            box(
              width = 6,
              withSpinner(tableOutput("rating_table1"))
            ),
          ),
          box(
            width = 12,
            box(
              width = 6,
              title = "positive",
              withSpinner(tableOutput("positive_1"))
            ),
            box(
              width = 6,
              title = "negative",
              withSpinner(tableOutput("negative_1"))
            ),
          ),
          box(
            width = 12,
            title = "review",
            withSpinner(tableOutput("review_1"))
          )
        ),
      ),
      box(
        box(
        width = 12,
        title = "Compare",
        selectInput("brand_name_compare", label = "Select Brand:", choices = unique(complete_brand_list$Brand)),
        selectInput("model_name_compare", label = "Select Moto Model:", choices = NULL),
        actionButton("compareButton", "Compare"),
        ),
        box(
          width = 12,
          withSpinner(imageOutput("image2")),
          box(
            width = 12,
            box(
            width = 6,
            withSpinner(tableOutput("cv_2"))
            ),
            box(
              width = 6,
              withSpinner(tableOutput("rating_table2"))
            ),
          ),
          box(
            width = 12,
            box(
            width = 6,
            title = "positive",
            withSpinner(tableOutput("positive_2"))
            ),
            box(
              width = 6,
              title = "negative",
              withSpinner(tableOutput("negative_2"))
            ),
          ),
          box(
            width = 12,
            title = "review",
            withSpinner(tableOutput("review_2"))
          )
        ),
      ),
    ),
    
    tags$script('
      Shiny.addCustomMessageHandler("closeTab", function(message) {
        window.close();
      });
    ')
  )
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  useShinyjs()
  

  
  
  # Event handler for opening the link in a browser
  observeEvent(input$data_complete_table_rows_selected, {
    selected_row <- input$data_complete_table_rows_selected
    if (length(selected_row) > 0) {
      row_data <- filteredData()[selected_row, ]
      # Assuming you have a column named "link" in your dataset
      link <- row_data$link
      
      # Open the link in a browser
      browseURL(link)
    }
  })
 
  filtered_models <- reactive({
    complete_brand_list$model[complete_brand_list$Brand == input$brand_name]
  })
  
  # Update choices in the second dropdown
  observe({
    updateSelectInput(session, "model_name", choices = filtered_models())
  })
  
  # Filter data based on slider values
  filteredData <- reactive({
    req(scrapedData())
    
    if (!is.null(scrapedData())) {
      filter(scrapedData(), 
             between(km, input$km_slider[1], input$km_slider[2]) &
               between(price, input$price_slider[1], input$price_slider[2]) &
               between(year, input$year_slider[1], input$year_slider[2])
      )
    }
  })
  
  # scraper fuction
  scrapedData <- eventReactive(input$scrapeButton, {
    
    # check if data already present
    
    folder_path <- "D:/documenti/progetti_vari/moto/datasets/"
    files <- list.files(folder_path)
    
    desired_dataset <- paste0(input$model_name, " ", date, " dataset.xlsx")
    
    if (desired_dataset %in% files) {
      # If it exists, load the dataset
      read.xlsx(paste0(folder_path, desired_dataset))
    } 
    
    else {
      # If not, call the scraper function
      scraper(input$model_name)
    }
    
  })
  
  # Output the scraped data to the DataTable
  output$data_complete_table <- renderDT({
    req(filteredData())
    
    # Find the 5% threshold for km_on_price
    threshold_value <- quantile(filteredData()$km_on_price, 0.05)
    
    # Filter the data for the 5% group
    top_5_percent_group <- filteredData() %>%
      filter(km_on_price <= threshold_value)
    
    # Find the highest value in the 5% group
    highest_value <- max(top_5_percent_group$km_on_price, na.rm = TRUE)
    
    datatable(
      filteredData(), 
      options = list(
        scrollY = '500px',
        paging = FALSE,
        escape = FALSE,
        columnDefs = list(list(visible = FALSE, targets = c(0, 6, 8, 9, 10))),
        order = list(list(6, 'asc'))
      ),
      selection = 'single'
    ) %>% formatStyle(
      columns = 1:ncol(filteredData()),
      valueColumns = "km_on_price",
      backgroundColor = styleInterval(highest_value, c('yellow','clear')))
  })
  
  
  
  
  
  
  #plot
  output$scatter_plot <- renderPlotly({
    req(filteredData())
    
    # Create a ggplot object
    gg <- ggplot(data = filteredData(), 
                 aes(x = km, y = price, color = factor(year), 
                     text = paste("Price:", price, ", Km:", km, ", Year:", year))) +
      geom_point(size = 3) +
      labs(x = "Kilometer", y = "Price") +
      theme_minimal()
    
    # Convert ggplot to plotly
    p <- ggplotly(gg, tooltip = "text", dynamicTicks = TRUE)
    
    # Register the 'plotly_selected' event
    p <- plotly::event_register(p, 'plotly_selected')
    
    # Highlight selected point in the scatter plot
    p <- plotly::highlight(p, "plotly_selected")
    
    p <- p %>% layout(legend = list(title = list(text = "Year")))
    
    return(p)
  })
  
  
  
  # Close the app when the closeButton is clicked
  observeEvent(input$closeButton, {
    # Close the browser tab
    session$sendCustomMessage(type = "closeTab", message = "Close the tab!")
    # Close the Shiny session
    session$close()
  })
  
  # Update slider ranges based on data
  observe({
    updateSliderInput(session, "km_slider", min = min(scrapedData()$km, na.rm = TRUE), max = max(scrapedData()$km, na.rm = TRUE))
    updateSliderInput(session, "price_slider", min = min(scrapedData()$price, na.rm = TRUE), max = max(scrapedData()$price, na.rm = TRUE))
    updateSliderInput(session, "year_slider", min = min(scrapedData()$year, na.rm = TRUE), max = max(scrapedData()$year, na.rm = TRUE))
  })
  
  # Reset sliders to default when the "Search" button is clicked
  observeEvent(input$scrapeButton, {
    updateSliderInput(session, "km_slider", value = c(0, 1000000))
    updateSliderInput(session, "price_slider", value = c(0, 1000000))
    updateSliderInput(session, "year_slider", value = c(0, 2023))
  })
  
  # Link selection in plot to table row selection
  observe({
    event <- event_data("plotly_selected", source = "scatter_plot")
    if (!is.null(event)) {
      selected_row <- event$points$id + 1  # +1 because DataTable rows are 1-indexed
      selectRow("data_complete_table", selected_row)
    }
  })
  
  
  
  # sidebar
  
  #compile the rating table
  ratingData <- eventReactive(input$scrapeButton, {
    
    folder_path <- "D:/documenti/progetti_vari/moto/datasets/"
    files <- list.files(folder_path)
    desired_dataset <- paste0(input$model_name, " ", date, " dataset.xlsx")
    a = read.xlsx(paste0(folder_path, desired_dataset))
    
    result <- a %>%
      group_by(year) %>%
      summarise(mean_price = mean(price, na.rm = TRUE),
                mean_km = mean(km, na.rm = TRUE))
    
    
  })
  
  # Output the plots and table
  output$points <- renderPlotly({
    rating_table <- ratingData()
    
    # Create plots
    plot_price <- plot_ly(data = rating_table, x = ~year, y = ~mean_price, type = 'scatter', mode = 'lines', name = 'Mean Price', line = list(shape = "spline", smoothing = 1.3))
    plot_km <- plot_ly(data = rating_table, x = ~year, y = ~mean_km, type = 'scatter', mode = 'lines', name = 'Mean KM', line = list(shape = "spline", smoothing = 1.3))
    
    # Create subplot
    subplot(plot_price, plot_km, nrows = 2, shareX = TRUE, titleX = FALSE) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),  # Hide x-axis grid, zeroline, line, and tick labels
        yaxis = list(showgrid = TRUE),   # Show y-axis grid
        annotations = list(
          list(
            x = 0.5,  # Adjust the x position based on your preference
            y = 1.05,  # Adjust the y position based on your preference
            xref = 'paper',
            yref = 'paper',
            text = 'Mean Price',
            showarrow = FALSE,
            font = list(size = 14)
          ),
          list(
            x = 0.5,  # Adjust the x position based on your preference
            y = 0.5,  # Adjust the y position based on your preference
            xref = 'paper',
            yref = 'paper',
            text = 'Mean KM',
            showarrow = FALSE,
            font = list(size = 14)
          )
        )
      )
  })
  
  # Define reactive expression to calculate weighted means
  means <- eventReactive(input$scrapeButton, {
    folder_path <- "D:/documenti/progetti_vari/moto/datasets/"
    files <- list.files(folder_path)
    desired_dataset <- paste0(input$model_name, " ", date, " dataset.xlsx")
    a <- read.xlsx(paste0(folder_path, desired_dataset))
    
    # Calculate weighted mean for price and km based on the year
    weighted_mean_price <- mean(a$price, na.rm = TRUE)
    weighted_mean_km <- mean(a$km, na.rm = TRUE)
    
    # Create a data frame for the table
    overall_table <- data.frame(
      Metric = c("Mean Price", "Mean KM"),
      Value = c(round(weighted_mean_price, 0), round(weighted_mean_km, 0))
    )
    
    return(overall_table)
  })
  
  # Output the overall mean table
  output$rating_table <- renderTable({
    means()
  }, rownames = FALSE, colnames = FALSE)
  


  
  
  # Event reactive for image loading
  loadImage <- eventReactive(input$scrapeButton, {
    folder_path <- "D:/documenti/progetti_vari/moto/images/"
    files <- list.files(folder_path)
    
    words <- str_split(input$model_name, "\\s+")
    first_word_uppercase <- toupper(words[[1]][1])
    result <- paste(first_word_uppercase, str_c(words[[1]][-1], collapse = " "), sep = " ")
    
    desired_dataset <- paste0(result, ".jpg")
    imagePath <- paste0(folder_path, desired_dataset)
  })
  
  # Render the image
  output$image <- renderImage({
    
    img_info <- list(src = loadImage(), width = "100%", height = "auto", alt = "Moto Image")
    
    return(img_info)
  }, deleteFile = FALSE)
  
  
  
  
  ############## left comparison part
  
  # bike info display
  observe({
    output$brand_text <- renderText({
      input$brand_name
    })
  })
  
  # Update the value of the second textInput based on input$model_name
  observe({
    output$model_text <- renderText({
      input$model_name
    })
  })
  
  
  # Output the overall mean table
  output$rating_table1 <- renderTable({
    means()
  }, rownames = FALSE, colnames = FALSE)
  
  # Event reactive for image loading
  loadImage1 <- eventReactive(input$scrapeButton, {
    folder_path <- "D:/documenti/progetti_vari/moto/images/"
    files <- list.files(folder_path)
    
    words <- str_split(input$model_name, "\\s+")
    first_word_uppercase <- toupper(words[[1]][1])
    result <- paste(first_word_uppercase, str_c(words[[1]][-1], collapse = " "), sep = " ")
    
    desired_dataset <- paste0(result, ".jpg")
    imagePath <- paste0(folder_path, desired_dataset)
  })
  
  # Render the image
  output$image1 <- renderImage({
    
    img_info <- list(src = loadImage1(), width = "100%", height = "auto", alt = "Moto Image")
    
    return(img_info)
  }, deleteFile = FALSE)
  
  # render specs
  output$cv_1 = renderTable({
    cv = cver(input$model_name)
    
    # Mutate the third column
    cv <- cv %>% mutate(cv = str_split(cv, "/") %>% sapply(`[`, 1))
    
    # Select the second and mutated third columns
    selected_columns = cv[, -1]
  }, rownames = FALSE)
  
  # render positives
  output$positive_1 = renderTable({
    positives = positiver(input$model_name)[, 2]
    
  }, rownames = FALSE, colnames = FALSE)
  
  # render negatives
  output$negative_1 = renderTable({
    negativer = negativer(input$model_name)[, 2]
    
  }, rownames = FALSE, colnames = FALSE)
  
  #render review
  output$review_1 = renderTable({
    review = sintezier(input$model_name)[, 2]
    
  }, rownames = FALSE, colnames = FALSE)
  
  
  
  
  ############# right comparison part
  
  
  # comparison button workings
  filtered_models_comparison <- reactive({
    complete_brand_list$model[complete_brand_list$Brand == input$brand_name_compare]
  })
  
  # Update choices in the second dropdown for comparison
  observe({
    updateSelectInput(session, "model_name_compare", choices = filtered_models_comparison())
  })
  
  # Define reactive expression to calculate weighted means
  means2 <- eventReactive(input$compareButton, {
    
    folder_path <- "D:/documenti/progetti_vari/moto/datasets/"
    files <- list.files(folder_path)
    
    desired_dataset <- paste0(input$model_name_compare, " ", date, " dataset.xlsx")
    
    if (desired_dataset %in% files) {
      # If it exists, load the dataset
      a2 = read.xlsx(paste0(folder_path, desired_dataset))
    } 
    
    else {
      # If not, call the scraper function
      scraper(input$model_name_compare)
      a2 = read.xlsx(paste0(folder_path, desired_dataset))
    }
    
    # Calculate weighted mean for price and km based on the year
    weighted_mean_price <- mean(a2$price, na.rm = TRUE)
    weighted_mean_km <- mean(a2$km, na.rm = TRUE)
    
    # Create a data frame for the table
    overall_table <- data.frame(
      Metric = c("Mean Price", "Mean KM"),
      Value = c(round(weighted_mean_price, 0), round(weighted_mean_km, 0))
    )
    
    return(overall_table)
  })
  
  # Output the overall mean table
  output$rating_table2 <- renderTable({
    means2()
  }, rownames = FALSE, colnames = FALSE)
  
  # Event reactive for image loading
  loadImage2 <- eventReactive(input$compareButton, {
    folder_path <- "D:/documenti/progetti_vari/moto/images/"
    files <- list.files(folder_path)
    
    words <- str_split(input$model_name_compare, "\\s+")
    first_word_uppercase <- toupper(words[[1]][1])
    result <- paste(first_word_uppercase, str_c(words[[1]][-1], collapse = " "), sep = " ")
    
    desired_dataset <- paste0(result, ".jpg")
    imagePath <- paste0(folder_path, desired_dataset)
  })
  
  # Render the image
  output$image2 <- renderImage({
    
    img_info <- list(src = loadImage2(), width = "100%", height = "auto", alt = "Moto Image")
    
    return(img_info)
  }, deleteFile = FALSE)
  
  # render specs
  output$cv_2 = renderTable({
    cv = cver(input$model_name_compare)
    
    # Mutate the third column
    cv <- cv %>% mutate(cv = str_split(cv, "/") %>% sapply(`[`, 1))
    
    # Select the second and mutated third columns
    selected_columns = cv[, -1]
  }, rownames = FALSE)
  
  # render positives
  output$positive_2 = renderTable({
    positives = positiver(input$model_name_compare)[, 2]
    
  }, rownames = FALSE, colnames = FALSE)
  
  # render negatives
  output$negative_2 = renderTable({
    negativer = negativer(input$model_name_compare)[, 2]
    
  }, rownames = FALSE, colnames = FALSE)
  
  #render review
  output$review_2 = renderTable({
    review = sintezier(input$model_name_compare)[, 2]
    
  }, rownames = FALSE, colnames = FALSE)
  
  
  
  
  
  # ads
  
  output$ad <- renderUI({
    HTML('
    <script async src="https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js?client=ca-pub-3224462193420944"
         crossorigin="anonymous"></script>
    <!-- example -->
    <ins class="adsbygoogle"
         style="display:block"
         data-ad-client="ca-pub-3224462193420944"
         data-ad-slot="2809847819"
         data-ad-format="auto"
         data-full-width-responsive="true"></ins>
    <script>
         (adsbygoogle = window.adsbygoogle || []).push({});
    </script>'
         )
  })

  

}
# Run the application 
shinyApp(ui = ui, server = server)
