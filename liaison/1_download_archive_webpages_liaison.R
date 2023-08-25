
################################################################################################################################
#################################                    DOWNLOAD LIAISON WEBPAGES                     #############################
################################################################################################################################

##### FIRST VERSION #####
#### 2001 - 2007 ####

query <- cdx_basic_query("http://www.iso.org:80/iso/en/stdsdevelopment/liaisonorglist/LiaisonOrgDetailPage.LiaisonOrgDetail", # The URL for the first version
                         match_type = "prefix", # Find all urls that start with the above
                         collapse = NULL) %>% # List all timestamps, not just the last one
  filter(str_detect(original, "ACRONYM=")) # Extract the organization from the link
urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% 
  pull(url) # Add to the URL an extension of the timestamp, this might as well be 000000 as it redirects automatically

acronym <- urls %>% str_extract("ACRONYM=[A-Za-z]+") %>% str_remove("ACRONYM=") # Make a string with just member number
dates <- query %>% pull(timestamp) # Find the dates
ids <- str_c(acronym, "_", dates) # Add together to an id

walk2(urls, ids, function(link, id) { # Download the files to a local folder
  
  destfile <- paste0("../raw_data/archive_liaison/version1/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
    download.file(link, destfile = destfile, quiet = TRUE)
    Sys.sleep(5)
    
    }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
  
})

## Because the second and third version of the site requires digging in deep to find the TCs of liaison organizations, I use a two-step method.
## The first step downloads pages for the TCs. They contain better accessible information on which TCs the organization were in liaison with.
## The second step downloads pages for the organizations. They contain more information on the organizations, e.g. address.

##### SECOND VERSION #####
#### 2008 - 2012 ####

## TC site ## 

query <- cdx_basic_query("http://www.iso.org/iso/standards_development/technical_committees/list_of_iso_technical_committees/iso_technical_committee.htm?commid=", match_type = "prefix", collapse = NULL)
query <- query %>%
  mutate(year = str_extract(timestamp, "[0-9]{4}")) %>%
  filter(year >= 2008,
         year <= 2012)

urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

acronym <- urls %>% str_extract("id=[0-9]+") %>% str_remove("id=")
dates <- query %>% pull(timestamp)
ids <- str_c(acronym, "_", dates)

walk2(urls, ids, function(link, id) {

  destfile <- paste0("../raw_data/archive_liaison/version2/tc_site/", id, ".htm")

  if(!file.exists(destfile)){

    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)

      }, error=function(e){message("Problematic page, skipped ", id)})

  }

})

## Organization site ##

query <- cdx_basic_query("http://www.iso.org/iso/about/organizations_in_liaison/organizations_in_liaison_details.htm?id=", match_type = "prefix", collapse = NULL) %>%
  mutate(year = str_extract(timestamp, "[0-9]{4}")) %>%
  filter(year >= 2008,
         year <= 2012)

query_1 <- query %>%
  mutate(liaisonlist = ifelse(str_detect(original, "&LiaisonList=True"), 1, 0)) %>%
  filter(liaisonlist == 0)

urls <- query_1 %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

acronym <- urls %>% str_extract("id=[0-9]+") %>% str_remove("id=")
dates <- query_1 %>% pull(timestamp)
ids <- str_c(acronym, "_", dates)

walk2(urls, ids, function(link, id) {

  destfile <- paste0("../raw_data/archive_liaison/version2/organization_site/", id, ".htm")

  if(!file.exists(destfile)){

    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)

      }, error=function(e){message("Problematic page, skipped ", id)})

  }

})

query_2 <- query %>%
  mutate(liaisonlist = ifelse(str_detect(original, "&LiaisonList=True"), 1, 0)) %>%
  filter(liaisonlist == 1)

urls <- query_2 %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

acronym <- urls %>% str_extract("id=[0-9]+") %>% str_remove("id=")
dates <- query_2 %>% pull(timestamp)
ids <- str_c(acronym, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_liaison/version2/organization_site/tc_folder/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
      
    }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
  
})


##### THIRD VERSION #####
#### 2013 - 2016 ####

## TC site ## 

query <- cdx_basic_query("http://www.iso.org/iso/home/standards_development/list_of_iso_technical_committees/iso_technical_committee.htm?commid=", match_type = "prefix", collapse = NULL)
query <- query %>%
  mutate(year = str_extract(timestamp, "[0-9]{4}")) %>%
  filter(year >= 2013,
         year <= 2016)

urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

acronym <- urls %>% str_extract("id=[0-9]+") %>% str_remove("id=")
dates <- query %>% pull(timestamp)
ids <- str_c(acronym, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_liaison/version3/tc_site/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
      
    }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
  
})

## Organization site ##

query <- cdx_basic_query("http://www.iso.org/iso/home/about/organizations_in_liaison/organizations_in_liaison_details.htm?id=", match_type = "prefix", collapse = NULL)
query <- query %>%
  mutate(year = str_extract(timestamp, "[0-9]{4}")) %>%
  filter(year >= 2013,
         year <= 2016)

query_1 <- query %>%
  mutate(liaisonlist = ifelse(str_detect(original, "&LiaisonList=True"), 1, 0)) %>%
  filter(liaisonlist == 0)

urls <- query_1 %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

acronym <- urls %>% str_extract("id=[0-9]+") %>% str_remove("id=")
dates <- query_1 %>% pull(timestamp)
ids <- str_c(acronym, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_liaison/version3/organization_site/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
      
    }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
  
})

query_2 <- query %>%
  mutate(liaisonlist = ifelse(str_detect(original, "&LiaisonList=True"), 1, 0)) %>%
  filter(liaisonlist == 1)

urls <- query_2 %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

acronym <- urls %>% str_extract("id=[0-9]+") %>% str_remove("id=")
dates <- query_2 %>% pull(timestamp)
ids <- str_c(acronym, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_liaison/version3/organization_site/tc_folder/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
      
    }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
  
})


##### FOURTH VERSION #####
#### 2017 - 2021 ####

query <- cdx_basic_query("https://www.iso.org/organization/", match_type = "prefix", collapse = NULL)
urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

acronym <- urls %>% str_extract("organization\\/[0-9]+") %>% str_remove("organization\\/")
dates <- query %>% pull(timestamp)
ids <- str_c(acronym, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_liaison/version4/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
      
    }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
})


##### CURRENT VERSION #####
#### 2023 ####

urls <- read_html("https://www.iso.org/organizations-in-cooperation-with-iso.html") %>%
  html_elements("tbody > tr > td > a") %>%
  html_attr("href") %>%
  str_c("https://www.iso.org", .)

acronym <- urls %>% str_extract("[0-9]+")
dates <- "2023-07-31"
ids <- str_c(acronym, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_liaison/current-2023-07-31/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
      
    }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
})
