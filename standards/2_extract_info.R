
#### STEP 2 ####

options(warn=0) # Set to -1 to suppress warnings

# List up all scraped webpages in folder
all_files <- list.files("C:/Users/solvebjo/OneDrive - Universitetet i Oslo/PhD/Paper 2 - Standards/after_feedback/data/webpages", full.names = TRUE)

# Extract table with status, dates, etc. and life cycle table

## page <- "C:/Users/solvebjo/OneDrive - Universitetet i Oslo/PhD/Paper 2 - Standards/after_feedback/data/webpages/_26058_.html"

for (i in 1:length(all_files)){
  
  #### Id ####
  
  page <- all_files[[i]]
  
  standard_no <- str_remove_all(all_files[[i]], "_|.html|C:/Users/solvebjo/OneDrive - Universitetet i Oslo/PhD/Paper 2 - Standards/after_feedback/data/webpages/")
  
  #### Data table ####
  
  dates_table_path <- paste0("C:/Users/solvebjo/OneDrive - Universitetet i Oslo/PhD/Paper 2 - Standards/after_feedback/data/dates_table/dates_table_", standard_no, "_.rds")

  if(!file.exists(dates_table_path)){ # Do not download if file exists in folder already

    tryCatch({ 

      dates_table_i <- read_html(page) %>%  # Exctract from the html code the status of the standard
        html_nodes("#product-details > div > div > div.col-md-7") %>%
        html_text2() %>%
        str_remove_all("\r") %>% # Remove any \r lineshifts as we will only be operating with \n to divide relevant parts
        str_replace_all("", "") %>% # Remove noise
        str_replace_all("General information", "") %>% # Remove noise
        str_replace_all("Preview", "") %>% # Remove noise
        str_replace_all("Abstract(\\s+)?\n\n", "Abstract:") %>% # Attempt to divide Abstract with \n. Some abstracts are long with multiple \n so I do it again below
        str_replace_all(" \n", "") %>% # Remove \n with space in front
        str_replace_all("Status\n :", "Status:") %>% # Separating by :, so remove \n in front of separator variable
        str_replace_all("Edition\n :", "Edition: ") %>% 
        str_replace_all("Publication date\n :", "Publication date:") %>%
        str_replace_all("Number of pages\n :", "Number of pages:") %>%
        str_replace_all("Technical Committee\n(\\s+)?:(\n)?(\n)?", "Technical Committee:") %>% # Technical committee can be followed by a combination of whitespace and lineshift - remove it
        str_replace_all("(?<=(ISO/TC [0-9]?[0-9]?[0-9]?[0-9]?[0-9]?))\n*", " ") %>% # To extract both TC [numbers] and TC [numbers]/SC [numbers] I use a lookbehind for both TC
        str_replace_all("(?<=(/SC [0-9]?[0-9]?[0-9]?[0-9]?[0-9]?))\n*", " ") %>% # and for SC
        str_replace_all("\n ISO/TMBG\n\nTechnical Management Board - groups", "ISO/TMBG Technical Management Board - groups") %>% # If the TC is TMBG, just indicate that
        str_replace_all("ICS(\n)?(\\s+)?:(\n)?(\n)?", "ICS:") %>% # Trying to fetch ICS, but difficult since there are lineshifts and irregular number of ICS codes, so I do it again below
        str_replace_all("\n+(?=([0-9]+?\\.[0-9]+?))", "") %>% # Attempt at fetching ICS codes
        str_replace_all("This standard contributes to the following Sustainable Development Goal(s)?:\n*", "\nSustainable Development Goals:") # If there are SDGs, fetch these
      
      ### ABSTRACT ###
      abstractvalues <- str_squish(str_remove(str_remove(str_extract(dates_table_i, "(?s)(Abstract:)(.*?)(Status)"), "Status"), "Abstract:"))
      
      ### ICS ###
      icsvalues <- str_squish(str_remove(str_extract(dates_table_i, "(?s)(ICS:.*$)"), "ICS:"))
      
      sdgs <- str_extract(icsvalues, "Sustainable Development Goal(s)?:.*")
      sdgs <- str_squish(str_remove(sdgs, "Sustainable Development Goal(s)?:"))
      
      icsvalues <- str_remove_all(icsvalues, "Sustainable Development Goal(s)?:.*")
      
      ics_number <- str_extract_all(icsvalues, "\\d+(\\.\\d+)*")[[1]]
      ics_text <- str_split(str_remove_all(icsvalues, "\\d+(\\.\\d+)*")[[1]], "\\s{2}")[[1]]
      
      ics <- tibble(ics_number = ics_number,
                    ics_text = str_squish(ics_text))
      
      ### SUSTAINABILITY GOALS ###
      sdg_matches <- str_match_all(sdgs, "(\\d+)\\s([A-Za-z\\s-]+)")[[1]]
      
      sdg_number <- as.numeric(sdg_matches[, 2])
      sdg_text <- str_squish(sdg_matches[, 3])
      
      sgds <- tibble(sdg_number = sdg_number,
                     sdg_text = sdg_text)
      
      ### TABLE ###
      dates_table_i <- dates_table_i %>%
        str_split(., pattern = "\n") %>%
        purrr::flatten_chr() %>%
        str_squish() %>%
        stringi::stri_remove_empty() %>%
        as_tibble() %>%
        separate(value, sep = ":", into = c("variable", "value")) %>%
        mutate(variable = str_squish(variable),
               value = str_squish(value),
               value = str_replace(value, "(\\d)\\s+(\\d)", "\\1\\2")) %>%
        add_row(variable = c("abstract"),
                value = c(abstractvalues)) %>%
        filter(variable %in% c("abstract", "Status", "Publication date", "Edition", "Number of pages", "Technical Committee")) %>%
        nest(data = everything()) %>%
        mutate("SDGs" = list(sgds),
               "ICS" = list(ics))

    saveRDS(dates_table_i, file = dates_table_path)
    
    }, error = function(cond){message(paste("Could not parse ", page))})

  } else {

    message(paste("Data table no. ", i, "exists already."))

  }
  
  life_cycle_path <- paste0("C:/Users/solvebjo/OneDrive - Universitetet i Oslo/PhD/Paper 2 - Standards/after_feedback/data/life_cycle/life_cycle_", standard_no, "_.csv")

  if(!file.exists(life_cycle_path)){ # Do not download if file exists in folder already
    
    tryCatch({
      
      life_cycle_i <- read_html(page) %>% # Extract from the html-code the life cycle of the standard
        html_nodes("#stages") %>%
        html_text2() %>%
        str_split("\n") %>%
        purrr::flatten_chr() %>%
        as_tibble() %>%
        mutate(stage = str_extract(value, "[0-9]{2}.[0-9]{2}")) %>%
        mutate(date = str_extract(value, "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}")) %>%
        mutate(action = str_extract(value, "(?<=\\-[0-9]{2}\\-[0-9]{2} ).*")) %>%
        mutate(value = str_squish(value))
    
     write.csv(life_cycle_i, file = life_cycle_path)
     
     }, error = function(cond){message(paste("Could not parse ", page))})
    
  } else {
    
    message(paste("Life cycle table no. ", i, "exists already."))
    
  }
  
  message(paste("Done parsed no. ", i, "of", length(all_files)))
    
}


