in_parking_scraper = function(in_parking_model) {
  
  # Start RSelenium and create a remote driver
  rD <- rsDriver(browser="firefox", verbose=F, chromever = NULL)
  remDr =rD[["client"]]
  remDr$maxWindowSize()
  
  # URL of the first page
  url <- paste0("https://www.ilparking-moto.it/moto-occasione/",in_parking_model,".html")
  
  # Create an empty data frame to store the results
  ilparking_data <- data.frame()
  
  remDr$navigate(url)
  #Sys.sleep(5)
  
  
  ####################################################### coockie button click
  
  next_button1 <- tryCatch(
    remDr$findElement(using = "css", value = ".sd-cmp-25TOo > button:nth-child(1)"),
    error = function(e) NULL
  )
  
  if (is.null(next_button1)) {
    return(ilparking_verifier = 1)  # No "Next" button found, exit the loop
  }
  
  # Click the "Next" button to go to the next page
  next_button1$clickElement()
  
  
  ################################################## verifica presenza annunci
  html <- read_html(remDr$getPageSource()[[1]], encoding = "utf-8")
  
  if (length(html %>% html_elements('.no-partenaire-btn span')) > 0) {
    # Continue with your logic
  } else {
    remDr$close()
    rD$server$stop()
    return(tibble())
  }
  
  
  #Sys.sleep(5)
  ####################################################  tent click
  
  tent_button1 <- tryCatch(
    remDr$findElement(using = "css", value = "#pays_resume"),
    error = function(e) NULL
  )
  
  if (is.null(tent_button1)) {
    return(ilparking_verifier = 1)  # No "Next" button found, exit the loop
  }
  
  # Click the "Next" button to go to the next page
  tent_button1$clickElement()
  
  #Sys.sleep(5)
  ####################################################  country click
  
  country_button1 <- tryCatch(
    remDr$findElement(using = "css", value = "#id_pays_24"),
    error = function(e) NULL
  )
  
  if (is.null(country_button1)) {
    return(ilparking_verifier = 1)  # No "Next" button found, exit the loop
  }
  
  # Click the "Next" button to go to the next page
  country_button1$clickElement()
  
  #Sys.sleep(5)
  ####################################################  ok click
  
  ok_button1 <- tryCatch(
    remDr$findElement(using = "css", value = ".form-pays > a:nth-child(2)"),
    error = function(e) NULL
  )
  
  if (is.null(ok_button1)) {
    return(ilparking_verifier = 1)  # No "Next" button found, exit the loop
  }
  
  # Click the "Next" button to go to the next page
  ok_button1$clickElement()
  
  Sys.sleep(5)
  
  
  
  ################################################## verifica presenza annunci
  html <- read_html(remDr$getPageSource()[[1]], encoding = "utf-8")
  
  if (length(html %>% html_elements('.no-partenaire-btn span')) > 0) {
    # Continue with your logic
  } else {
    remDr$close()
    rD$server$stop()
    return(tibble())
  }
  
  # Loop to navigate through pages
  while (TRUE) {
    
    
    
    # Scrape data from the current page
    html <- read_html(remDr$getPageSource()[[1]], encoding = "utf-8")
    
    brand = unlist(strsplit(model_name, " "))[1]
    
    # moto model
    moto_model = html %>% html_elements('.nowrap.title-block') %>% html_text2()
    moto_model = moto_model %>% c(html %>% html_elements(".nowrap.title-block") %>% html_text2())
    
    
    # moto year
    year = html %>% html_elements("#resultats .clearfix li:nth-child(3) .upper") %>% html_text2()
    year = year %>% c(html %>% html_elements("#resultats .clearfix li:nth-child(3) .upper") %>% html_text2())
    
    
    # moto km
    km = html %>% html_elements('li:nth-child(2) .upper') %>% html_text2()
    km = km %>% c(html %>% html_elements("li:nth-child(2) .upper") %>% html_text2())
    
    
    # moto prezzo
    price = html %>% html_elements('#resultats .prix') %>% html_text2()
    
    price = price %>% c(html %>% html_elements('#resultats .prix') %>% html_text2())
    price = price[seq_along(price) %% 2 != 0]
    
    
    # moto luogo
    place = html %>% html_elements('.location .upper') %>% html_text2()
    place = place %>% c(html %>% html_elements(".location .upper") %>% html_text2())
    
    
    # link
    link = html %>% html_elements('.no-partenaire-btn') %>% map_chr(~ .x %>% html_attr("href"))
    link = link[!duplicated(link)]
    link = link %>% c(html %>% html_elements('.no-partenaire-btn') %>% map_chr(~ .x %>% html_attr("href")))
    
    
    # website
    website = "ilparking"
    
    
    # Return a tibble
    data = tibble(brand, moto_model, year, km, price, place, link, website)
    
    ilparking_data = bind_rows(ilparking_data, data)
    
    # Attempt to find the "Next" button
    next_button <- tryCatch(
      remDr$findElement(using = "css", value = "#paginations_top .btn-next a"),
      error = function(e) NULL
    )
    
    if (is.null(next_button)) {
      break  # No "Next" button found, exit the loop
    }
    
    # Click the "Next" button to go to the next page
    next_button$clickElement()
    
    # Get the URL of the next page
    url <- remDr$getCurrentUrl()
  }
  
  # Close the connection
  remDr$close()
  rD$server$stop()
  
  return(ilparking_data)
  
  
  
}