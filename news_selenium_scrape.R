
library(tidyverse)
library(RSelenium)
library(rvest)

# 1. Open Powershell as administrator and start Docker as administrator.
# 2. Ensure that a docker container with chrome image is running. Write "docker ps" to check.
# 3. If "selenium/standalone-chrome" is not there, write "docker start 72dec05ef912" (or another image-id, see "docker ps -a").
# (If this does not work, the container is probably gone. Write "docker run -d -p 5983:4444 selenium/standalone-chrome") to make a new one.
# (If this neither works, then the image is probably gone. Then you have to start from scratch by downloading the image. Follow this: https://rpubs.com/johndharrison/RSelenium-Docker)

# 4. Now, the virtual server is up and running. Run the script below.

# CHROME: 5983L -- using Chrome, as it requires less RAM
# FIREFOX: 6496L -- not using Firefox

remDr <- remoteDriver(port = 5983L, # Setting up server on this port (Selenium typically runs on 4444) 
                      remoteServerAddr = "localhost", browserName = "chrome") # Not necessary to specify the last two, but doing it here.

remDr$open() # Opening the server.

url <- "https://www.iso.org/newsroom/x/"

remDr$navigate(url) # Entering webpage

# Checking that we're on the right spot.
remDr$getTitle()
remDr$screenshot(display = TRUE)

# Directing the mouse to the area we want to extract using xpath. 
# Go into the webpage, click inspect on the part you want to scrape, find the area you want in the html-code ans select "copy -> XPath". 

iso_news <- function(x){
  
  elem <- remDr$findElement(using="xpath", # Choose the area you want
                            value='//*[@id="list-e8d29424-fd67-49ae-a7c7-3f456dceb571"]/div[4]/div')
  elemlink <- elem$getElementAttribute("outerHTML")[[1]] # Get the HTML
  
  link <- read_html(elemlink) %>% # Extract the links from the HTML
    html_nodes("div.media-body > h4 > a") %>% 
    html_attr("href") %>% 
    str_c("https://www.iso.org", .)
  
  return(link)
  
}

iso_click <- function(x){
  
  # Make an object of the spot for the "next" button on the webpage
  click <- remDr$findElement(using= "xpath", 
                             '//*[@id="content"]/section[2]/div/div[2]/div/div[3]/div/nav/ul/li[12]/a') 
  
  remDr$mouseMoveToLocation(webElement = click) # Put the mouse on the button
  #remDr$screenshot(display = TRUE) # Check that the mouse is at the right place.
  
  click$clickElement() # Click the button!
  
  #remDr$screenshot(display = TRUE) # Check that the button was clicked.
  
}

links <- list()

# Run a loop with the two functions that scrape all news content then clicks the next button and repeats.
for (i in 1:1317) { # These numbers are tested in batches, looks like there's 1317 news
  
  links[[i]] <- iso_news()
  iso_click()
  
  print(paste(i))
  
  Sys.sleep(2)
  
}

links <- unlist(links) # Make the list of lists into a character vector

save(links, file = "links.rda")

date <- list()
topics <- list()
iso <- list()
tags <- list()
headline <- list()
text <- list()

for (i in 1:length(links)) {
  
  link <- read_html(links[[i]])
  
  date[[i]] <- link %>%
    html_nodes("#metadata > div > div.row.row-bottom-sm > div:nth-child(1) > div > time") %>%
    html_text() %>%
    str_trim() 
  
  topics[[i]] <- link %>% 
    html_nodes("#header-img > div.flex-left-50pct > div > div > div > div.section-head") %>% 
    html_text()
  
  iso[[i]] <- link %>%
    html_nodes("aside") %>%
    html_text() %>%
    str_trim()
  
  tags[[i]] <- link %>%
    html_nodes("#metadata > div > div:nth-child(5) > div.col-md-6.text-right-md") %>%
    html_text() %>%
    str_replace_all("Tagged as", "") %>%
    str_trim()
  
  headline[[i]] <- link %>% 
    html_nodes("h1") %>% 
    html_text()
  
  text[[i]] <- link %>% 
    html_nodes("p") %>% 
    html_text()
  
  print(paste0(i))
  
}

iso_news <- tibble(date = date,
                   topics = topics,
                   tags = tags,
                   headline = headline,
                   text = text)

saveRDS(iso_news, file = "iso_news.rds")

