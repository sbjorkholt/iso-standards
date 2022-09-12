
#### STEP 2 ####

options(warn=0) # Set to -1 to suppress warnings

all_files <- list.files("C:/Users/solvebjo/OneDrive - Universitetet i Oslo/PhD/Paper 2 - Standards/after_feedback/data/webpages", full.names = TRUE)

standards_df <- tibble(committee = as.character(), 
                        title = as.character(), 
                        ics_id = as.character(), 
                        ics_name = as.character(),
                        status = tibble(), 
                        life_cycle = tibble(), 
                        link = as.character(), 
                        standard_no = as.character())

for (i in 1:length(all_files)) { 
  
  page <- all_files[[i]]
  standard_no <- str_remove_all(all_files, "_|.html|C:/Users/solvebjo/OneDrive - Universitetet i Oslo/PhD/Paper 2 - Standards/after_feedback/data/webpages/")[i]
  
  date_link <- read_html(page)
  
  dates_table_i <- date_link %>%  # Exctract from the html code the status of the standard
    html_nodes("#product-details > div > div > div.col-md-7") |> 
    html_text2() %>%
    str_replace_all("", "") |> 
    str_replace_all("General information", "") %>%
    str_replace_all("Technical Committee\n:\r\n\r", "Technical Committee:") %>%
    str_replace_all("ICS :\n", "ICS:") %>%
    str_remove_all("\r") %>%
    str_replace("(?<=ICS:) \n\n", "") %>%
    str_split(., pattern = "\n") %>%
    purrr::flatten_chr() %>%
    str_squish() %>%
    stringi::stri_remove_empty() %>%
    as_tibble() %>%
    separate(value, sep = ":", into = c("variable", "value")) %>%
    mutate(variable = str_squish(variable),
           value = str_squish(value))
  
  life_cycle_i <- date_link %>% # Extract from the html-code the life cycle of the standard
    html_nodes("#stages") %>%
    html_text2() %>%
    str_split("\n") %>%
    purrr::flatten_chr() %>%
    as_tibble() %>%
    mutate(stage = str_extract(value, "[0-9]{2}.[0-9]{2}")) %>%
    mutate(date = str_extract(value, "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}")) %>%
    mutate(action = str_extract(value, "(?<=\\-[0-9]{2}\\-[0-9]{2} ).*")) %>%
    mutate(value = str_squish(value)) 
  
  write.csv(dates_table_i, file = paste0("C:/Users/solvebjo/OneDrive - Universitetet i Oslo/PhD/Paper 2 - Standards/after_feedback/data/dates_table/dates_table_", standard_no, "_.csv"))
  
  write.csv(life_cycle_i, file = paste0("C:/Users/solvebjo/OneDrive - Universitetet i Oslo/PhD/Paper 2 - Standards/after_feedback/data/life_cycle/life_cycle_", standard_no, "_.csv"))
  
  message(paste("Done parsing no.", i, "of", length(all_files)))
  
}


