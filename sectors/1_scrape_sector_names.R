
################################################################################################################################
#################################                            SECTORS                              ##############################
################################################################################################################################

##### MAIN COMMITTEES ####

library(rvest)

html <- read_html("https://www.iso.org/technical-committees.html")

html %>%
  html_nodes("#datatable-committees > tbody > tr > td > a") %>%
  html_attr("href") %>%
  str_extract("/committee/[0-9]+.html") -> links

links <- str_c("https://www.iso.org", links[complete.cases(links)])

TC_name <- list()
creation_date <- list()

for (i in 1:length(links)) {
  
  TC_name[[i]] <- read_html(links[[i]]) %>%
    html_node("#content > section.section-navigation > div > div > div > nav") %>%
    html_text()
  
  creation_date[[i]] <- read_html(links[[i]]) %>%
    html_node("#section-details > div > div > div.col-md-7 > ul > li:nth-child(4)") %>%
    html_text()
  
  Sys.sleep(5)
  
  message(paste(i))
  
}

TC_creation <- tibble(TC = unlist(TC_name)) %>%
  separate(TC, into = c("TC1", "committee", "title"), sep = "\n") %>%
  mutate(committee = str_squish(committee),
         committee = str_squish(str_remove(committee, "\\[STANDBY\\]")),
         title = str_squish(title)) %>%
  mutate(creation_date = str_remove_all(str_squish(unlist(creation_date)), "Creation date: ")) %>%
  select(title, committee, creation_date)

TC_creation <- TC_creation %>%
  mutate(creation_date = ifelse(committee == "ISO/TC 92", 1958,
                                ifelse(committee == "ISO/PC 305", 2016,
                                       ifelse(committee == "ISO/TC 321", 2018, creation_date))))


##### SUB COMMITTEES ####

sub_links <- list()

for (i in 1:length(links)) { 
  
  sub_links1 <- read_html(links[[i]]) %>%
    html_nodes("tbody > tr > td > a") %>%
    html_attr("href") %>%
    str_extract("/committee/[0-9]+.html") %>%
    unique()
  
  sub_links[[i]] <- str_c("https://www.iso.org", sub_links1[complete.cases(sub_links1)])
  
  Sys.sleep(5)
  
  message(paste(i))
  
}

sub_links_unlist <- unlist(compact(sub_links)) %>% unique()

TC_sub_name <- list()
creation_sub_date <- list()

for (i in 1:length(sub_links_unlist)) {  
  
  TC_sub_name[[i]] <- read_html(sub_links_unlist[[i]]) %>%
    html_node("#content > section.section-navigation > div > div > div > nav") %>%
    html_text()
  
  creation_sub_date[[i]] <- read_html(sub_links_unlist[[i]]) %>%
    html_node("#section-details > div > div > div.col-md-7 > ul > li:nth-child(4)") %>%
    html_text()
  
  Sys.sleep(2)
  
  message(paste(i))
  
}

TC_sub_creation <- tibble(TC = unlist(TC_sub_name)) %>%
  separate(TC, into = c("TC1", "committee", "title"), sep = "\n") %>%
  mutate(committee = str_squish(committee),
         committee = str_squish(str_remove(committee, "\\[STANDBY\\]")),
         title = str_squish(title)) %>%
  mutate(creation_date = str_remove_all(str_squish(unlist(creation_sub_date)), "Creation date: ")) %>%
  select(title, committee, creation_date)

saveRDS(TC_sub_creation, file = "./data/TC_sub_creation.rds")

# TC_sub_creation %>%
#   add_row(title = "Gypsum, gypsum plasters and gypsum products", committee = "ISO/TC 152", creation_date = "1960",
#           title = "Limits and fits", committee = "ISO/TC 3", creation_date = "1947",
#           title = "Rivets", committee = "ISO/TC 7", creation_date = "1947",
#           title = "Shipbuilding details for sea navigation", committee = "ISO/TC 9", creation_date = NA,
#           title = "Shaft heights of machinery", committee = "ISO/TC 13", creation_date = "1951",
#           title = "")

##### ALL TCS #####

all_tcs <- read_html("https://en.wikipedia.org/wiki/List_of_ISO_technical_committees") %>%
  html_node("#mw-content-text > div.mw-parser-output > table > tbody") %>%
  html_table() %>%
  tibble() %>%
  rename(committee = Committee,
         title = Title) %>%
  mutate(committee = str_c("ISO/", committee))

# https://en.wikipedia.org/wiki/List_of_ISO_technical_committees

old_tcs <- anti_join(all_tcs, tc_creation, by = join_by(committee)) %>%
  filter(!committee == "ISO/ISO/IEC JTC 1")


#### CODING MISSINGS #####

library(httr)

create_prompt <- function(old_tcs){
  prompts <- purrr::map2(old_tcs$committee, old_tcs$title,
                         
                         ~list(
                           
                           list(
                             "role" = "system",
                             "content" = stringr::str_c(
                               
                               "You are an expert on the history of the International Standardization Organization (ISO)")
                           ),
                           
                           list(
                             "role" = "user",
                             "content" = stringr::str_c(
                               
                               "I have two questions. ",
                               
                               "First, in which year was the ISO technical committee '", .x, " ", .y, "' established?  ",
                               "Provide only the year. ",
                               
                               "Second, ISO has a few categories called sectors that they categorize each technical committee into. ",
                               "Which sector do you think the technical committee", .x, "would belong to? ",
                               "Answer with only the sector name. ",
                               "1. Transport ", 
                               "2. Building and construction ", 
                               "3. Service ",
                               "3. Information technology, graphics and photography ", 
                               "4. Sustainability and environment ",
                               "5. Mechanical engineering ", 
                               "6. Energy ", 
                               "7. Chemicals ",
                               "8. Ores and metals ",
                               "9. Special technologies ", 
                               "10. Food and agriculture ", 
                               "11. Non-metallic materials ", 
                               "12. Horizontal subjects ",
                               "13. Health, medicine and laboratory equipment ", 
                               "14. Freight, packaging and distribution ",
                               "15. Business management and innovation ", 
                               "16. Security, safety and risk ")
                             
                           )
                         )
  )
  prompts
}

prompts <- create_prompt(old_tcs)

api_key <- read_lines("../credentials/api_key_chatgpt")

submit_prompt <- function(prompt, temperature = 0.0, n = 1) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      temperature = temperature,
      messages = prompt,
      n = n))
  Sys.sleep(1)
  message(paste0("Finished."))
  str_trim(content(response)$choices[[1]]$message$content)
}

openai_completions <- prompts %>%
  purrr::map(submit_prompt)

openai_completions

sectors <- sectormerge %>% group_by(sector) %>% count() %>% na.omit () %>% pull(sector) %>% c("Service")

old_tc_df <- tibble(committee = unlist(openai_completions)) %>%
  separate(committee, into = c("creation_date", "sector2"), sep = "\n\n") %>%
  mutate(creation_date = str_extract(creation_date, "[0-9]{4}")) %>% 
  mutate(sector = str_extract(sector2, paste(sectors, collapse="|"))) %>%
  rowid_to_column() %>%
  mutate(sector = ifelse(rowid == 60, "Services",
                         ifelse(rowid == 62, "Security, safety and risk",
                                ifelse(rowid == 88, "Sustainability and environment",
                                       ifelse(sector == "Service", "Services", sector))))) %>%
  cbind(old_tcs %>% select(-Notes)) %>%
  select(-c(sector2, rowid)) 

### Make sector merge file ###

sectormerge <- TC_creation %>%
  select(title, committee, sector) %>%
  unique()

sectormerge <- standards %>%
  mutate(main_tc = str_extract(committee, "ISO/TC [0-9]+|ISO/IEC JTC [0-9]+")) %>%
  left_join(sectors %>% select(-title), by = c("main_tc" = "committee")) %>%
  select(-main_tc) %>%
  select(committee, sector) %>% 
  unique()

sectormerge <- sectormerge %>%
  mutate(sector = case_match(committee,
                             "ISO/TMBG" ~ "Other", # "Technical Management Board Groups",
                             "ISO/TC 175" ~ "Non-metallic materials",
                             "ISO/TC 65" ~ "Ores and metals",
                             "ISO/TC 128" ~ "Mechanical engineering",
                             "ISO/TC 56" ~ "Non-metallic materials",
                             "ISO/TC 144/SC 1" ~ "Mechanical engineering",
                             "ISO/TC 144/SC 2" ~ "Mechanical engineering",
                             "ISO/CS" ~ "Other", # "ISO Central Secretariat",
                             "ISO/TC 144/SC 3" ~ "Mechanical engineering",
                             "IULTCS" ~ "Other", # "International Union of Leather Technologists",
                             "ISO/TC 152" ~ "Chemicals",
                             "ISO/TC 177" ~ "Transport",
                             "ISO/TC 144" ~ "Mechanical engineering",
                             "IIW" ~ "Other", # "International Institute of Welding",
                             "CIE" ~ "Other", # "International Commission on Illumination",
                             "ISO/TC 116/SC 3" ~ "Special technologies",
                             "ISO/COPOLCO" ~ "Other", # "Committee on consumer policy",
                             "ISO/CASCO" ~ "Other", # "Committee for conformity assessment",
                             "ISO/TC 203" ~ "Energy",
                             "ISO/TC 50" ~ "Non-metallic materials",
                             "ISO/TC 208" ~ "Mechanical engineering",
                             "ISO/TC 179/SC 1" ~ "Building and construction",
                             "ISO/TC 179/SC 3" ~ "Building and construction",
                             "ISO/TC 75" ~ "Health, medicine and laboratory equipment",
                             "ISO/TC 73" ~ "Services",
                             "ISO/TC 62" ~ "Building and construction",
                             "ISO/TC 80" ~ "Security, safety and risk",
                             "VAMAS" ~ "Other", # "Versailles Project on Advanced Materials and Standards",
                             "ISO/TC 223" ~ "Security, safety and risk",
                             "ISO/TC 64" ~ "Mechanical engineering",
                             "ISO/TC 95" ~ "Mechanical engineering",
                             "ISO/PC 245" ~ "Services",
                             "ISO/TC 78" ~ "Chemicals",
                             "ISO/PC 311" ~ "Services",
                             "ISO/PC 316" ~ "Services",
                             "ISO/PC 317" ~ "Services",
                             "ISO/PC 329" ~ "Security, safety and risk",
                             "ISO/TC 116" ~ "Mechanical engineering",
                             "ISO/PC 337" ~ "Business management and innovation",
                             .default = sector
  ))

write_csv2(sectormerge_standards, file = "../datasets/sectormerge.csv")

sectormerge <- read_csv("../datasets/sectormerge.csv")

sectormerge <- sectormerge %>%
  distinct(committee, .keep_all = TRUE)

saveRDS(sectormerge, file = "../datasets/sectors.rds")


                       