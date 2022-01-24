
library(tidyverse)
library(rvest)
library(httr)

scrape_subcommittee <- function(webpage){
  
  try(html <- read_html(str_c(webpage)))
  
  try(tc_html <- html %>%
    html_nodes("#datatable-committee-children > tbody") %>% 
    html_elements("a") %>%
    html_attr("href") %>%
    as_tibble() %>%
    mutate(n = nchar(value)) %>%
    filter(n <= 55) %>%
    pull(value) %>%
    str_c("https://www.iso.org", .))
  
  subtc_html <- list()
  dates_table <- list()
  life_cycle <- list()
  dates <- list()
  life <- list()
  dates_life <- list()
  
  for (i in 1:length(tc_html)) { 
    
    if((tc_html[i] != "https://www.iso.org")==TRUE){
    
        try(link <- read_html(str_c(tc_html[[i]], "/p/1/u/1/w/1/d/1")))
        
        try(subtc_html <- link %>%
          html_nodes("#datatable-tc-projects > tbody") %>% 
          html_elements("a") %>%
          html_attr("href") %>%
          str_extract("/standard/.*") %>%
          na.omit() %>%
          str_c("https://www.iso.org", .))
        
        print(paste(c("main:", i)))
        
        for (j in 1:length(subtc_html)) { 
          
          k <<- k+1
          
          try(date_link <- read_html(str_c(subtc_html[[j]])))
          
          try(dates_table[[k]] <- date_link %>%
            html_nodes("#product-details > div > div > div.col-md-7") %>%
            html_text2() %>%
            str_replace_all("General information ???", "") %>%
            str_replace_all("Technical Committee\n:\n", "Technical Committee:") %>%
            str_replace_all("ICS :\n", "ICS:") %>%
            str_split(., pattern = "\n") %>%
            purrr::flatten_chr() %>%
            stringi::stri_remove_empty() %>%
            str_squish() %>%
            as_tibble() %>%
            separate(value, sep = ":", into = c("variable", "value")))
          
          try(life_cycle[[k]] <- date_link %>%
            html_nodes("#stages") %>%
            html_text2() %>%
            str_split("\n") %>%
            purrr::flatten_chr() %>%
            as_tibble() %>%
            mutate(stage = str_extract(value, "[0-9]{2}.[0-9]{2}")) %>%
            mutate(date = str_extract(value, "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}")) %>%
            mutate(action = str_extract(value, "(?<=\\-[0-9]{2}\\-[0-9]{2} ).*")))
          
          print(paste(c("sub:", j)))
          print(paste(c("total:", k)))
          
          Sys.sleep(2)
          
        }
        
        } else {
          
          print("No webpage.")
          
        }
    
    Sys.sleep(2)
    
  }
  
  dates_life <- try(list(dates_table, life_cycle))
  
  return(dates_life)
  
}

htmls <- read_html("https://www.iso.org/standards-catalogue/browse-by-tc.html") %>%
  html_node("#datatable-committees > tbody") %>%
  html_elements("a") %>%
  html_attr("href") %>%
  str_c("https://www.iso.org", .)

subcommittee_time <- list()
k <- 0
for (p in 1:length(htmls)) { 
  
  try(subcommittee_time[[p]] <- scrape_subcommittee(htmls[p]))
  
  print(str_c("Finishing no.", p))
  
}

saveRDS(subcommittee_time, file = "./data/subcommittee_time.rds")

