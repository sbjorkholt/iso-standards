

################################################################################################################################
#################################                       EXTRACT TABLES                            ##############################
################################################################################################################################

##### VERSION 1 #####

files <- list.files("../raw_data/archive_members/version1/", full.names = TRUE) # List up the downloaded webpages
names <- read_csv("../raw_data/archive_members/acronyms_v1.csv") %>% janitor::clean_names() %>% select(acronym, country) # Read in csv with list of standard body names and country they belong to 

members_v1 <- function(page) {
  
  tryCatch({
    
  name <- str_remove_all(page, "../raw_data/archive_members/version1/|_[0-9]+-[0-9]+-[0-9]+.htm") %>% # Remove date from linkname, stripping down to acronym
    tibble(acronym = .) %>% # Make a tibble
    left_join(names, by = "acronym") # Left join with csv-file with country names
  
  date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract the date from the linkname
  
  table <- read_html(page) %>% 
    html_node("body > table") %>% # The table node contains info on TC participation, among other things
    html_table() %>% # Parse into a table
    select(X2, X3, X4) %>% # Select the variables containing committee, name of committee and membership type
    filter(str_detect(X2, "[A-Z]{2,3} [0-9]+")) %>% # Extract from the column the rows containing committee ids, with two or three characters followed by numbers (e.g., TC 32)
    filter(nchar(X2) < 50) %>%  # Also catches some rows containing noise with this pattern, they are usually very long, remove those rows with more than 50 characters
    mutate(X4 = str_remove_all(X4, "\\[|\\]")) # Remove brackets from the membership variable
  
  timeback <- tibble(acronym = name$acronym, # Put into a dataframe
                     country = name$country,
                     date = date,
                     committee = table$X2,
                     title = table$X3,
                     membership = table$X4,
                     webid = str_remove_all(page, "../raw_data/archive_members/version1/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm")) 
  
  return(timeback)
  
}, error=function(e){message("Parsing error occurred at webpage ", page)})
  
}

membership_v1 <- lapply(files, members_v1) # Run the function on all webpages from the first version

membership_v1_table <- do.call(rbind, membership_v1) # Bind the lists into a dataframe

# saveRDS(membership_v1_table, file = "../raw_data/archive_members/participants_v1.rds")

##### VERSION 2 #####

files <- list.files("../raw_data/archive_members/version2/", full.names = TRUE) %>%
  str_remove("../raw_data/archive_members/version2/pdc") %>%
  stringi::stri_remove_empty_na()

members_v2 <- function(page) {
  
  tryCatch({
    
  acronym <- read_html(page) %>%
    html_node("h2") %>% # Here, we find the acronym in the h2 node of the webpage
    html_text() %>% # Parse to text
    str_extract("\\([A-Z]+( [A-Z]+)?( [A-Z]+)?\\)") %>% # The acronym is wrapped in parantheses. Most often, this acronym has one set of capital letters, but sometimes there are more sets (e.g., GOST R)
    str_remove_all("\\(|\\)") # Remove the parantheses
  
  country <- read_html(page) %>%
    html_node("h2") %>% # The name of the country also exists in the h2 node
    html_text() %>% # Parse to text
    str_remove(" \\([A-Z]+\\)") # Remove the acronym from this string
  
  date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract the date from the linkname
  
  table <- read_html(page) %>%
    html_nodes("#content > ul > li") %>% # Committee participation is in the li node
    html_text() %>% # Since it is saved in a list, need to parse it to text
    str_squish() %>% # Remove whitespace and other unnecessary clutter
    str_replace(., " - ", "SEPME") %>% # title and committee is in one, separated by -. - also occurs other places, so replace - once with SEPME, and use this as separator
    tibble(committee = .) %>% # Make a tibble
    separate(committee, into = c("committee", "title"), sep = "SEPME") %>% # Separate the column into committee and title using the separator
    mutate(membership = str_extract(title, "P-Member|O-Member|Secretariat"), # Fetch from the string what type of membership there is
           title = str_squish(str_remove_all(title, "P-Member|O-Member|Secretariat|\\(|\\)")), # Clean the remaining title column
           membership = str_replace(membership, "M", "m")) %>% # Replace large M in "member" with small m to keep consistent with other versions
    drop_na(title) # Some final NAs that are noise, drop them
  
  timeback <- tibble(acronym = acronym, # Make into a tibble
                     country = country,
                     date = date,
                     committee = str_squish(table$committee),
                     title = str_squish(table$title),
                     membership = table$membership,
                     webid = str_remove_all(page, "../raw_data/archive_members/version2/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
  
  return(timeback)
  
  }, error=function(e){message("Parsing error occurred at webpage ", page)})
  
}

membership_v2 <- lapply(files, members_v2)

membership_v2_table <- do.call(rbind, membership_v2) %>%
  mutate(acronym = ifelse(country == "Moldova, Republic of (insm)", # One acronym with non-capital letters
                          "INSM", acronym), # Fixing this manually
         country = str_remove(country, "\\(insm\\)")) # Clean

saveRDS(membership_v2_table, file = "../raw_data/archive_members/participants_v2.rds")


##### VERSION 3 #####

files <- list.files("../raw_data/archive_members/version3/", full.names = TRUE) %>%
  str_remove("../raw_data/archive_members/version3/pdc") %>%
  stringi::stri_remove_empty_na()

members_v3 <- function(page) {
  
  tryCatch({
    
    acronym <- read_html(page) %>% 
      html_nodes("body > div.content.clearfix > h2") %>% # The country name and acronym exists in this node
      html_text() %>% # Parse to text
      str_extract("\\([A-Z]+( [A-Z]+)?( [A-Z]+)?\\)") %>% # Fetch the acronym
      str_remove_all("\\(|\\)")
  
    country <- read_html(page) %>% # Same procedure as with version 2
      html_node("body > div.content.clearfix > h2") %>% # Just change the node
      html_text() %>%
      str_remove(" \\([A-Z]+\\)") 
  
    date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}")
  
    table <- read_html(page) %>%  # Same procedure as with version 2
      html_nodes("body > div.content.clearfix > ul > li") %>% # Just change the node
      html_text() %>%
      str_squish() %>%
      str_replace(., " - ", "SEPME") %>%
      tibble(committee = .) %>%
      separate(committee, into = c("committee", "title"), sep = "SEPME") %>%
      mutate(membership = str_extract(title, "P-Member|O-Member|Secretariat"),
             title = str_squish(str_remove_all(title, "P-Member|O-Member|Secretariat|\\(|\\)")),
             membership = str_replace(membership, "M", "m")) %>%
      drop_na(title)
  
    timeback <- tibble(acronym = acronym,
                       country = country,
                       date = date,
                       committee = str_squish(table$committee),
                       title = str_squish(table$title),
                       membership = table$membership,
                       webid = str_remove_all(page, "../raw_data/archive_members/version3/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
  
    return(timeback)
    
  }, error=function(e){message("Parsing error occurred at webpage ", page)})
  
}

membership_v3 <- lapply(files, members_v3)

membership_v3_table <- do.call(rbind, membership_v3) %>% 
  # Adding one webpage that did not work with the code due to format issues
  add_row(acronym = c("BHN", "BHN"),
          country = c("Haiti", "Haiti"),
          date = c("2016-09-21", "2016-09-21"),
          committee = c("TC 23", "TC 292"),
          title = c("Food products", "Security and resilience"),
          membership = c("P-member", "P-member"),
          webid = c("5304435", "5304435"))

saveRDS(membership_v3_table, file = "../raw_data/archive_members/participants_v3.rds")


##### VERSION 4 #####

## Running the TC part would add extra data but also cause issues in imputation since the imputations are per missing country and not per missing TC

# ### TC ###
# files <- list.files("../raw_data/archive_members/version4/tc_site/", full.names = TRUE) 
# names <- read_csv("../raw_data/archive_members/acronyms_v4.csv") %>% select(acronym, country) 
# 
# members_tc_v4 <- function(page) {
#   
#   tryCatch({
#     
#     webpage <- read_html(page)
#     
#     participating <- webpage %>%
#       html_node("#datatable-PART_P_OC") %>% # Acronym exists in this node
#       html_table() %>%
#       magrittr::set_colnames(c("country", "acronym")) %>%
#       mutate(membership = "P-member")
#     
#     observing <- webpage %>% 
#       html_node("#datatable-PART_O_OC") %>% # Acronym exists in this node
#       html_table() %>%
#       magrittr::set_colnames(c("country", "acronym")) %>%
#       mutate(membership = "O-member")
#     
#     secretariat <- webpage %>%
#       html_node("#content > section.bg-lightgray.section-sm > div > div > div > p") %>%
#       html_text() 
#     
#     secretariat <- tibble(country = str_squish(str_remove(secretariat, "\\-.*")),
#                           acronym = str_squish(str_remove(str_remove(str_extract(secretariat, "\\(.*\\)"),"\\("), "\\)"))) %>%
#       mutate(membership = "Secretariat")
#     
#     memberships <- bind_rows(participating, observing, secretariat)
# 
#     tc <- webpage %>%
#       html_node("#content > section.section-navigation > div > div > div > div > nav > div > a") %>% # Fetching the table with TC membership
#       html_text() # Parse to table
#     
#     date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract date from linkname
#     
#     timeback <- tibble(acronym = memberships$acronym, # Make a tibble with the final data
#                        country = memberships$country,
#                        date = date,
#                        committee = tc,
#                        title = NA,
#                        membership = memberships$membership,
#                        webid = str_remove_all(page, "../raw_data/archive_members/version4/tc_folder/|_[0-9]{4}-[0-9]{2}-[0-9]{2}_[A-Z]+.htm"))
#     
#     return(timeback)
#     
#   }, error=function(e){message("Parsing error occurred at webpage ", page)})
#   
#   
# }
# 
# membership_tc_v4 <- lapply(files, members_tc_v4) # Run function on all webpages


### MEMBER ###
files <- list.files("../raw_data/archive_members/version4/member_site/", full.names = TRUE) 
names <- read_csv("../raw_data/archive_members/acronyms_v4.csv") %>% select(acronym, country) 
# Version 4 does not have country names in webpage, thus need external csv with convertions
# As acronyms can change, use another one for 2017-2021

members_v4 <- function(page) {
  
  tryCatch({
    
    name <- read_html(page) %>%
      html_node("div > div > div > nav > div > a") %>% # Acronym exists in this node
      html_text() %>%
      tibble(acronym = .) %>% # Make a tibble
      left_join(names, by = "acronym") # Find country names according to name on standards body
  
    table <- read_html(page) %>%
      html_node("#datatable-participation > tbody") %>% # Fetching the table with TC membership
      html_table() # Parse to table

    membership <- str_remove(str_extract(page, "[A-Z]+.htm"), ".htm") # Membership is in this case collected from the linkname

    date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract date from linkname
    
    timeback <- tibble(acronym = name$acronym, # Make a tibble with the final data
                     country = name$country,
                     date = date,
                     committee = str_squish(table$X1),
                     title = str_squish(table$X2),
                     membership = membership,
                     webid = str_remove_all(page, "../raw_data/archive_members/version4/|_[0-9]{4}-[0-9]{2}-[0-9]{2}_[A-Z]+.htm"))
    
    return(timeback)
    
  }, error=function(e){message("Parsing error occurred at webpage ", page)})
  
  
}

membership_v4_tc_table <- lapply(files, members_v4) # Run function on all webpages

membership_v4_tc_table <- do.call(rbind, membership_v4_tc_table)

# membership_v4_tc_table <- membership_v4_tc_table %>%  
#   bind_rows(do.call(rbind, membership_tc_v4))

membership_v4_table <- membership_v4_tc_table %>%  # Make into a tibble
  filter(!membership %in% c("OP", "PP")) %>% # Not including PDC memberships for now
  mutate(membership = ifelse(membership == "OT", "O-member", # Change names of membership types to make compatible with earlier versions
                             ifelse(membership == "PT", "P-member",
                                    ifelse(membership == "S", "Secretariat",
                                           ifelse(membership == "TS", "Twinned secretariat", # New category added
                                                  #ifelse(membership == "OP", "O-member PDC",
                                                         #ifelse(membership == "PP", "P-member PDC",
                                                                membership))))) %>%
  # Some pages were weirdly downloaded and missed name and acronym, adding them manually
  mutate(country = ifelse(date == "2019-05-14" & webid == "1619", "Canada",
                          ifelse(date == "2019-06-18" & webid == "1835", "Japan",
                                 ifelse(date == "2017-07-20" & webid == "1815", "Ireland",
                                        ifelse(date == "2019-05-13" & webid == "2036", "Peru",
                                               country)))),
         acronym = ifelse(date == "2019-05-14" & webid == "1619", "SCC", 
                          ifelse(date == "2019-06-18" & webid == "1835", "JISC", 
                                 ifelse(date == "2017-07-20" & webid == "1815", "NSAI",
                                        ifelse(date == "2019-05-13" & webid == "2036", "INACAL",
                                               acronym)))))

saveRDS(membership_v4_table, file = "../raw_data/archive_members/participants_v4.rds")


##### CURRENT VERSION #####

### CURRENT VERSION ROUND 1 ###

files <- list.files("../raw_data/archive_members/current-2022-08-31/", full.names = TRUE) 
names <- read_csv("../raw_data/archive_members/acronyms_v4.csv") %>% select(acronym, country) 

current <- function(page) { # Webpage hasn't changed since 2017, so procedure follows that of version 4
  
  name <- read_html(page) %>%
    html_node("div > div > div > nav > div > a") %>% 
    html_text() %>%
    tibble(acronym = .) %>% 
    left_join(names, by = "acronym") 
  
  table <- read_html(page) %>%
    html_node("#datatable-participation > tbody") %>% 
    html_table() 
  
  membership <- str_remove(str_extract(page, "[A-Z]+.htm"), ".htm") 
  
  date <- "2022-08-31" 
  
  timeback <- tibble(acronym = name$acronym, 
                     country = name$country,
                     date = date,
                     committee = str_squish(table$X1),
                     title = str_squish(table$X2),
                     membership = membership,
                     webid = str_remove_all(page, "../raw_data/archive_members/current/|_[0-9]{4}-[0-9]{2}-[0-9]{2}_[A-Z]+.htm"))
  
  return(timeback)
  
}

membership_current <- lapply(files, current) 

membership_current_table1 <- do.call(rbind, membership_current) %>%  
  filter(!membership %in% c("OP", "PP")) %>% # Not including PDC memberships for now
  mutate(membership = ifelse(membership == "OT", "O-member", 
                             ifelse(membership == "PT", "P-member",
                                    ifelse(membership == "S", "Secretariat",
                                           ifelse(membership == "TS", "Twinned secretariat", 
                                                  #ifelse(membership == "OP", "O-member PDC",
                                                         #ifelse(membership == "PP", "P-member PDC",
                                                                membership)))))

saveRDS(membership_current_table1, file = "../raw_data/archive_members/participants_current_2022.rds")


### CURRENT VERSION ROUND 2 ###

files <- list.files("../raw_data/archive_members/current-2023-07-26/", full.names = TRUE) 
names <- read_csv("../raw_data/archive_members/acronyms_v4.csv") %>% select(acronym, country) 

current <- function(page) { # Webpage hasn't changed since 2017, so procedure follows that of version 4
  
  name <- read_html(page) %>%
    html_node("div > div > div > nav > div > a") %>% 
    html_text() %>%
    tibble(acronym = .) %>% 
    left_join(names, by = "acronym") 
  
  table <- read_html(page) %>%
    html_node("#datatable-participation > tbody") %>% 
    html_table() 
  
  membership <- str_remove(str_extract(page, "[A-Z]+.htm"), ".htm") 
  
  date <- "2023-07-26" 
  
  timeback <- tibble(acronym = name$acronym, 
                     country = name$country,
                     date = date,
                     committee = str_squish(table$X1),
                     title = str_squish(table$X2),
                     membership = membership,
                     webid = str_remove_all(page, "../raw_data/archive_members/current/|_[0-9]{4}-[0-9]{2}-[0-9]{2}_[A-Z]+.htm"))
  
  return(timeback)
  
}

membership_current <- lapply(files, current) 

membership_current_table2 <- do.call(rbind, membership_current) %>% 
  filter(!membership %in% c("OP", "PP")) %>% # Not including PDC memberships for now
  mutate(membership = ifelse(membership == "OT", "O-member", 
                             ifelse(membership == "PT", "P-member",
                                    ifelse(membership == "S", "Secretariat",
                                           ifelse(membership == "TS", "Twinned secretariat", 
                                                  #ifelse(membership == "OP", "O-member PDC",
                                                         #ifelse(membership == "PP", "P-member PDC",
                                                                membership)))))

saveRDS(membership_current_table2, file = "../raw_data/archive_members/participants_current_2023.rds")

