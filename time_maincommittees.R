
library(tidyverse)
library(rvest)

html <- read_html("https://www.iso.org/standards-catalogue/browse-by-tc.html")

tc_html <- html %>%
  html_nodes("#datatable-committees > tbody") %>%
  html_elements("a") %>%
  html_attr("href") %>%
  as_tibble() %>%
  mutate(n = nchar(value)) %>%
  filter(n <= 50) %>%
  pull(value) %>%
  str_c("https://www.iso.org", .)

k <- 0
subtc_html <- list()
dates_table <- list()
life_cycle <- list()

for (i in 1:length(tc_html)) {
  
  link <- read_html(str_c(tc_html[[i]], "/p/1/u/1/w/1/d/1"))
  
  subtc_html <- link %>%
    html_nodes("#datatable-tc-projects > tbody") %>%
    html_elements("a") %>%
    html_attr("href") %>%
    str_extract("/standard/.*") %>%
    na.omit() %>%
    str_c("https://www.iso.org", .)
  
  print(paste(c("main:", i)))
  
  for (j in 1:length(subtc_html)) {
    
    k <- k+1
    
    date_link <- read_html(subtc_html[[j]])
    
    dates_table[[k]] <- date_link %>%
      html_nodes("#product-details > div > div > div.col-md-7") %>%
      html_text2() %>%
      str_replace_all("General information ??? ", "") %>%
      str_replace_all("Technical Committee\n:\n", "Technical Committee:") %>%
      str_replace_all("ICS :\n", "ICS:") %>%
      str_split(., pattern = "\n") %>%
      purrr::flatten_chr() %>%
      stringi::stri_remove_empty() %>%
      str_squish() %>%
      as_tibble() %>%
      separate(value, sep = ":", into = c("variable", "value"))
    
    life_cycle[[k]] <- date_link %>%
      html_nodes("#stages") %>%
      html_text2() %>%
      str_split("\n") %>%
      purrr::flatten_chr() %>%
      as_tibble() %>%
      mutate(stage = str_extract(value, "[0-9]{2}.[0-9]{2}")) %>%
      mutate(date = str_extract(value, "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}")) %>%
      mutate(action = str_extract(value, "(?<=\\-[0-9]{2}\\-[0-9]{2} ).*"))
    
    print(paste(c("sub:", j)))
    print(paste(c("total:", k)))
    
    Sys.sleep(2)
    
  }
  
  Sys.sleep(2)
  
}

iso_dates_other2 <- tibble(dates_table = dates_table,
                           life_cycle = life_cycle)

main_iso_code <- tibble(names_table = dates_table,
                        dates_table = dates_table,
                        life_cycle = life_cycle) %>%
  unnest(names_table) %>%
  mutate(main_iso_code = str_extract(value, "ISO/.*")) %>%
  filter(variable == "Technical Committee") %>%
  select(main_iso_code, dates_table, life_cycle)

main_iso_name <- main_iso_code %>%
  unnest(dates_table) %>%
  .[which(.$variable == "Technical Committee") + c(1),] %>%
  pull(variable)

time_main <- main_iso_code %>%
  rename(Committee = main_iso_code,
         Title = main_iso_name) %>%
  select(main_iso_code, main_iso_name, dates_table, life_cycle)

saveRDS(time_main, file = "./data/time_main.rds")

