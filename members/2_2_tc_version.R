
pacman::p_load(tidyverse, rvest, wayback)

##### VERSION 1 #####
#### 2001 - 2007 ####

query <- cdx_basic_query("http://www.iso.org/iso/en/stdsdevelopment/tc/tclist/TechnicalCommitteeParticipationListPage.TechnicalCommitteeParticipationList?COMMID=", match_type = "prefix", collapse = NULL)
urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(substr(as.character(timestamp), 1, 10), "-", ""), "000000/", original)) %>% pull(url)

commid <- urls %>% str_extract("COMMID=[0-9]+") %>% str_remove("COMMID=")
dates <- query %>% pull(timestamp) %>% substr(1, 10)
ids <- str_c(commid, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("./raw_data/archive_members/version1/tc_site/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(urls[1], destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
    }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
})

files_v1 <- list.files("./raw_data/archive_members/version1/tc_site/", full.names = TRUE) %>%
  stringi::stri_remove_empty_na()

fetch_tc1 <- function(page){
  
  tryCatch({
    
    committee <- read_html(page) %>%
      html_nodes("tr > td > span > a") %>% 
      html_text2() %>%
      str_squish() %>%
      .[1]
    
    secretariat <- read_html(page) %>%
      html_nodes("tr > td > span > a") %>%
      html_text2() %>%
      .[2]
    
    participating <- read_html(page) %>%
      html_nodes("tr > td") %>% 
      html_text() %>%
      .[106] %>%
      str_split("\n") %>%
      .[[1]] %>%
      str_squish() %>%
      stringi::stri_remove_empty() %>%
      tibble(participating = .)
    
    observing <- read_html(page) %>%
      html_nodes("tr > td") %>%
      html_text() %>%
      .[108] %>%
      str_split("\n") %>%
      .[[1]] %>%
      str_squish() %>%
      stringi::stri_remove_empty() %>%
      tibble(observing = .)
    
    date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") 
    
    timeback <- tibble(committee = committee,
                       secretariat = secretariat,
                       date = date,
                       participating = nest(participating),
                       observing = nest(observing),
                       webid = str_remove_all(page, "./raw_data/archive_members/version1/tc_site/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
    
    return(timeback)
    
  }, error=function(e){message("Parsing error occurred at webpage ", page)})
  
}

tcs_v1 <- lapply(files_v1, fetch_tc1)

secretariat <- bind_rows(tcs_v1) %>%
  select(committee, date, secretariat) %>%
  rename(country = secretariat) %>%
  mutate(membership = "secretariat")

participating <- bind_rows(tcs_v1) %>%
  unnest() %>%
  select(committee, date, data) %>%
  unnest(cols = c(data)) %>%
  mutate(membership = "P-member") %>%
  rename(country = participating) 

observing <- bind_rows(tcs_v1) %>%
  unnest() %>%
  select(committee, date, data1) %>%
  unnest(cols = c(data1)) %>%
  mutate(membership = "O-member") %>%
  rename(country = observing) 

v1_tcs <- bind_rows(participating, observing) %>%
  bind_rows(secretariat) %>%
  mutate(committee = str_remove(committee, " \\- .*")) %>%
  mutate(year = substr(date, 1, 4)) %>%
  mutate(country = str_remove(country, "\\(.*\\)"),
         country = str_squish(country)) %>%
  select(-date) 

saveRDS(v1_tcs, file = "./validation/data/v1_tcs.rds")


##### VERSION 2 #####
#### 2008 - 2012 ####

query <- cdx_basic_query("http://www.iso.org/iso/standards_development/technical_committees/list_of_iso_technical_committees/iso_technical_committee_participation.htm?commid=", match_type = "prefix", collapse = NULL)
urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

commid <- urls %>% str_extract("commid=[0-9]+") %>% str_remove("commid=")
dates <- query %>% pull(timestamp)
ids <- str_c(commid, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_members/version2/tc_site/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile,  quiet = TRUE)
      Sys.sleep(5)
    }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
})


files_v2 <- list.files("./raw_data/archive_members/version2/tc_site/", full.names = TRUE) %>%
  stringi::stri_remove_empty_na()

fetch_tc2 <- function(page){
  
  tryCatch({
    
    committee <- read_html(page) %>%
      html_nodes("#content > h1:nth-child(3)") %>%
      html_text2() %>%
      str_squish()
    
    secretariat <- read_html(page) %>%
      html_node("#content > ul:nth-child(6) > li") %>%
      html_text()
    
    participating <- read_html(page) %>%
      html_node("#content > ul:nth-child(8)") %>%
      html_text() %>%
      str_split("\n") %>%
      .[[1]] %>%
      str_squish() %>%
      stringi::stri_remove_empty() %>%
      tibble(participating = .)
    
    observing <- read_html(page) %>%
      html_node("#content > ul:nth-child(10)") %>%
      html_text() %>%
      str_split("\n") %>%
      .[[1]] %>%
      str_squish() %>%
      stringi::stri_remove_empty() %>%
      tibble(observing = .)
    
    date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") 
    
    timeback <- tibble(committee = committee,
                       secretariat = secretariat,
                       date = date,
                       participating = nest(participating),
                       observing = nest(observing),
                       webid = str_remove_all(page, "./raw_data/archive_members/version2/tc_site/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
    
    return(timeback)
    
  }, error=function(e){message("Parsing error occurred at webpage ", page)})
  
}

tcs_v2 <- lapply(files_v2, fetch_tc2)

secretariat <- bind_rows(tcs_v2) %>%
  select(committee, date, secretariat) %>%
  rename(country = secretariat) %>%
  mutate(membership = "secretariat")

participating <- bind_rows(tcs_v2) %>%
  unnest() %>%
  select(committee, date, data) %>%
  unnest(cols = c(data)) %>%
  mutate(membership = "P-member") %>%
  rename(country = participating) 

observing <- bind_rows(tcs_v2) %>%
  unnest() %>%
  select(committee, date, data1) %>%
  unnest(cols = c(data1)) %>%
  mutate(membership = "O-member") %>%
  rename(country = observing) 

v2_tcs <- bind_rows(participating, observing) %>%
  bind_rows(secretariat) %>%
  mutate(committee = str_remove(committee, " \\- .*")) %>%
  mutate(year = substr(date, 1, 4)) %>%
  mutate(country = str_remove(country, "\\(.*\\)"),
         country = str_squish(country)) %>%
  select(-date) 

saveRDS(v2_tcs, file = "./validation/data/v2_tcs.rds")

##### VERSION 3 #####
#### 2013 - 2016 ####

query <- cdx_basic_query("http://www.iso.org/iso/home/standards_development/list_of_iso_technical_committees/iso_technical_committee_participation.htm?commid=", match_type = "prefix", collapse = NULL)
urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

commid <- urls %>% str_extract("commid=[0-9]+") %>% str_remove("commid=")
dates <- query %>% pull(timestamp)
ids <- str_c(commid, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_members/version3/tc_site/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
    }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
})

files_v3 <- list.files("./raw_data/archive_members/version3/tc_site/", full.names = TRUE) %>%
  stringi::stri_remove_empty_na()

fetch_tc3 <- function(page){
  
  tryCatch({
    
    committee <- read_html(page) %>%
      html_nodes("body > div:nth-child(6)") %>%
      html_text2() %>%
      str_squish()
    
    secretariat <- read_html(page) %>%
      html_node("div > div > ul > li > a") %>%
      html_text()
    
    participating <- read_html(page) %>%
      html_nodes("div > div > ul") %>% 
      html_text() %>%
      .[2] %>%
      str_split("\n") %>%
      .[[1]] %>%
      str_squish() %>%
      stringi::stri_remove_empty() %>%
      tibble(participating = .)
    
    observing <- read_html(page) %>%
      html_nodes("div > div > ul") %>%
      html_text() %>%
      .[3] %>%
      str_split("\n") %>%
      .[[1]] %>%
      str_squish() %>%
      stringi::stri_remove_empty() %>%
      tibble(observing = .)
    
    date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") 
    
    timeback <- tibble(committee = committee,
                       secretariat = secretariat,
                       date = date,
                       participating = nest(participating),
                       observing = nest(observing),
                       webid = str_remove_all(page, "./raw_data/archive_members/version3/tc_site/|_[0-9]{4}-[0-9]{2}-[0-9]{2}_[A-Z]+.htm"))
    
    return(timeback)
    
  }, error=function(e){message("Parsing error occurred at webpage ", page)})
  
}

tcs_v3 <- lapply(files_v3, fetch_tc3)

secretariat <- bind_rows(tcs_v3) %>%
  select(committee, date, secretariat) %>%
  rename(country = secretariat) %>%
  mutate(membership = "secretariat")

participating <- bind_rows(tcs_v3) %>%
  unnest() %>%
  select(committee, date, data) %>%
  unnest(cols = c(data)) %>%
  mutate(membership = "P-member") %>%
  rename(country = participating) 

observing <- bind_rows(tcs_v3) %>%
  unnest() %>%
  select(committee, date, data1) %>%
  unnest(cols = c(data1)) %>%
  mutate(membership = "O-member") %>%
  rename(country = observing) 

v3_tcs <- bind_rows(participating, observing) %>%
  bind_rows(secretariat) %>%
  mutate(committee = str_remove(committee, " \\- .*")) %>%
  mutate(year = substr(date, 1, 4)) %>%
  mutate(country = str_remove(country, "\\(.*\\)"),
         country = str_squish(country)) %>%
  select(-date) 

saveRDS(v3_tcs, file = "./validation/data/v3_tcs.rds")

##### VERSION 4 #####
#### 2017 - 2023 ####

query <- cdx_basic_query("https://www.iso.org/committee/", match_type = "prefix", collapse = NULL) %>%
  filter(str_detect(original, "view=participation"))
urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

tc <- urls %>% str_extract("committee\\/[0-9]+") %>% str_remove("committee\\/")
dates <- query %>% pull(timestamp)
ids <- str_c(tc, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_members/version4/tc_site/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
    }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
})

files_v4 <- list.files("./raw_data/archive_members/version4/tc_site/", full.names = TRUE) %>%
  stringi::stri_remove_empty_na()

fetch_tc_v4 <- function(page){
  
  tryCatch({
  
    committee <- read_html(page) %>%
      html_node("#content > section.section-navigation > div > div > div > div > nav > div") %>%
      html_text()
    
    secretariat <- read_html(page) %>%
      html_node("#content > section.bg-lightgray.section-sm > div") %>%
      html_text() %>%
      str_split("\n") %>%
      .[[1]] %>%
      str_remove_all("\\r") %>%
      str_remove_all("Secretariat") %>%
      str_replace_all(" ", "") %>%
      stringi::stri_remove_empty() %>%
      str_split("-") %>%
      .[[1]] %>%
      .[1]
    
    participating <- read_html(page) %>%
      html_node("#datatable-PART_P_OC > tbody") %>%
      html_table() 
    
    observing <- read_html(page) %>%
      html_node("#datatable-PART_O_OC") %>%
      html_table() 
    
    date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") 
    
    timeback <- tibble(committee = committee,
                       secretariat = secretariat,
                       date = date,
                       participating = nest(participating),
                       observing = nest(observing),
                       webid = str_remove_all(page, "../raw_data/archive_members/version4/tc_site/|_[0-9]{4}-[0-9]{2}-[0-9]{2}_[A-Z]+.htm"))
    
    return(timeback)
    
  }, error=function(e){message("Parsing error occurred at webpage ", page)})
  
}

tcs_v4 <- lapply(files_v4, fetch_tc_v4)

secretariat <- bind_rows(tcs_v4) %>%
  select(committee, date, secretariat) %>%
  rename(country = secretariat) %>%
  mutate(membership = "secretariat")

participating <- bind_rows(tcs_v4) %>%
  unnest() %>%
  select(committee, date, data) %>%
  unnest(cols = c(data)) %>%
  mutate(membership = "P-member") %>%
  rename(country = `Country/Territory`) %>%
  select(-X2)

observing <- bind_rows(tcs_v4) %>%
  unnest() %>%
  select(committee, date, data1) %>%
  unnest(cols = c(data1)) %>%
  mutate(membership = "O-member") %>%
  rename(country = `Country/Territory`) %>%
  select(-Acronym, -Country)

v4_tcs <- bind_rows(participating, observing) %>%
  bind_rows(secretariat) %>%
  mutate(committee = str_remove(committee, " \\- .*")) %>%
  mutate(year = substr(date, 1, 4)) %>%
  mutate(country = str_remove(country, "\\(.*\\)"),
         country = str_squish(country)) %>%
  select(-date) 
  
saveRDS(v4_tcs, file = "./validation/data/v4_tcs.rds")
