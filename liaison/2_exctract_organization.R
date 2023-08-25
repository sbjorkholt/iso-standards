

################################################################################################################################
#################################                       EXTRACT TABLES                            ##############################
################################################################################################################################

##### VERSION 1 #####

files <- list.files("../raw_data/archive_liaison/version1/", full.names = TRUE) # List up the downloaded webpages
names <- read_csv("../raw_data/archive_liaison/acronyms_v1.csv") %>% janitor::clean_names() %>% select(acronym, name) %>% # Read in csv with list of acronym and name of organization
  drop_na(name) %>%
  unique()

liaison_v1 <- function(page) {
  
  tryCatch({
    
    webpage <- read_html(page)
    
  name <- str_remove_all(page, "../raw_data/archive_liaison/version1/|_[0-9]+-[0-9]+-[0-9]+.htm") %>% # Remove date from linkname, stripping down to acronym
    tibble(acronym = .) %>% # Make a tibble
    left_join(names, by = "acronym") # Left join with csv-file with country names
  
  date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract the date from the linkname
  
    table <- webpage %>% 
      html_node("body > table > tr") %>% # The table node contains info on TC participation, among other things
      html_table() %>% # Parse into a table
      select(X1, X2, X3, X4) %>% # Select the variables containing committee, name of committee and membership type
      mutate(address = str_c(X2, " ", lead(X2))) %>% # Pasting together first and second part of address
      mutate(address = ifelse(X1 == "Address", address, NA)) %>% # Remove all cells that are not address in new variable
      mutate(address = str_squish(str_replace_all(address, "(?<!^)(?=[A-Z])", " "))) %>% # Add spaces into the address and format out extra whitespace
      mutate(liaison = ifelse(X1 == "In liaison with:", X2, NA)) %>% # Gather liaison TCs to separate variable
      separate_rows(liaison, sep = ",") %>%
      select(address, liaison) %>%
      mutate(liaison = str_squish(liaison))
  
    table <- as_tibble(list(address = table %>% drop_na(address) %>% pull(address), 
                            liaison = table %>% drop_na(liaison) %>% pull(liaison)))
  
    table <- tibble(acronym = name$acronym, # Put into a dataframe
                     organization = name$name,
                     date = date,
                     country = word(table$address, -1),
                     address = table$address,
                     liaison = table$liaison)
  
  message("Finished up ", str_remove(page, "../raw_data/archive_liaison/version1/"))
  
    return(table)
  
  }, error=function(e){message("No liaison organizations for TC in page ", page)})
  
}

liaison_v1 <- lapply(files, liaison_v1) # Run the function on all webpages from the first version

liaison_v1_table <- do.call(rbind, liaison_v1) %>% # Bind the lists into a dataframe
  mutate(webid = "") %>% # Add variable to make the dataframes consistent over time (but there are no webids for the first version of the webpage)
  mutate(country = ifelse(country == "A", "United States",
                          ifelse(country == "Africa", "South Africa",
                                 ifelse(country == "Federation", "Russian Federation",
                                        ifelse(country == "Kingdom", "United Kingdom",
                                               ifelse(country == "Zealand", "New Zealand",
                                                      ifelse(acronym == "MasterCard", "United States",
                                                             ifelse(country == "Ivoire", "Côte-d' Ivoire",
                                                                    country)))))))) %>%
  filter(acronym != "Error in try(return(timeback)) : object 'timeback' not found") 
  
liaison_v1_table <- liaison_v1_table %>%
  mutate(organization = case_when( # Manually adding names for the organizations that were not in the acronyms csv file
    acronym == "AIM" ~ "Association for Automatic Identification and Mobility",
    acronym == "AIPC" ~ "International Association for Bridge and Structural Engineering",
    acronym == "AISM"   ~ "International Association of Lighthouse Authority",
    acronym == "ASD" ~ "Aero Space and Defence Industries Association of Europe",
    acronym == "BIS" ~ "Bank for International Settlements",
    acronym == "CCETT"  ~ "Common Study Center of Telediffusion and Telecommunication",
    acronym == "CE" & country == "France"     ~ "Council of Europe Secrétariat Général",
    acronym == "CE" & country == "Belgium" ~ "European Commission",
    acronym == "CE" & country == "Portugal" ~ "Confédération européenne du liège Apartado",
    acronym == "CEA" & country == "France"    ~ "European Insurance Committee Secrétariat de la Commission Incendie",
    acronym == "CEA" & country == "Belgium" ~ "European Confederation of Agriculture",
    acronym == "CEC" & str_detect(address, "Coordinating European Council for the Development of Performance Tests for Transportation Fuels") ~ "Coordinating European Council for the Development of Performance Tests for Transportation Fuels",
    acronym == "CEC" & str_detect(address, "European Confederation of the Footwear Industry") ~ "European Confederation of the Footwear Industry",
    acronym == "CEFACT" ~ "UN/ECECEFACT International Trade and Business Processes Group",
    acronym == "CEI" ~ "European Confederation of Wood- Working Industries",
    acronym == "CET" & country == "Belgium" ~ "European Ceramic Tile Manufacturers' Federation",
    acronym == "CET" & country == "Germany" ~ "European Tea Committee Gotenstrasse",
    acronym == "CICILS" ~ "International Pulse Trade and Industry Confederation Bureau",
    acronym == "CICR" ~ "International Committee of the Red Cross",
    acronym == "CMDC" ~ "World Circle of the Consensus/ World Sustainable Energy Coalition",
    acronym == "CODEX" ~ "CODEX Alimentarius Secretariat",
    acronym == "COPA" ~ "Committee of Professional Agircultural Organisations in the European Union (COPA)",
    acronym == "EBA" ~ "The European Boating Association",
    acronym == "ECF" & str_detect(address, "European Caravan") ~ "European Caravan Federation",
    acronym == "ECF" & str_detect(address, "European Cyclist") ~ "European Cyclists' Federation",
    acronym == "eCl"    ~ "European Consortium for the Certificate Attainment in Modern Languages",
    acronym == "ECLAC" ~ "United Nations Economic Commission for Latin America and the Caribbean",
    acronym == "Ecma" ~ "Ecma International",
    acronym == "ESA" & country == "France"  ~ "European Space Agency",
    acronym == "ESA" & country == "Germany"  ~ "European Spice Association",
    acronym == "FAECF" ~ "Federation of European Window and Curtain Wall Manufacturers' Associations",
    acronym == "FEC"    ~ "Federation of the European Cutlery and Flatware Industries",
    acronym == "FEMIB" ~ "European Federation of Building Joinery Manufacturers",
    acronym == "FEPF" ~ "European Federation of the Industries of Earthenware and China Tableware and Ornamental Ware",
    acronym == "FISD" ~ "Financial Information Services Division Software & Information Industry Association",
    acronym == "FMAC" ~ "World Veterans Federation",
    acronym == "FunStep" ~ "Standard for the Exchange of Forniture Product Data",
    acronym == "GAS" ~ "European Committee of Manufacturers of Gas- Welding Equipment",
    acronym == "GS" ~ "G S1 (formerly EAN International)",
    acronym == "I" ~ "International Institute of Noise Control Engineering",
    acronym == "IAA" ~ "International Academy of Astronautics",
    acronym == "IACS" ~ "International Association of Classification Societies",
    acronym == "IAIDQ" ~ "The International Association for Information and Data Quality",
    acronym == "ICA" & country == "Canada" ~ "International Cartographic Association",
    acronym == "ICA" & country == "United Kingdom" ~ "International Council on Archives",
    acronym == "ICA" & country == "Switzerland" ~ "International Co-operative Alliance",
    acronym == "ICC" & country == "France" ~ "International Chamber of Commerce",
    acronym == "ICC" & country == "Austria" ~ "International Association for Cereal Science and Technology",
    acronym == "ICC" & country == "United States" ~ "International Color Consortiumc",
    acronym == "ICO" & country == "France" ~ "International Commission for Opticsc", # This is the same for both France
    acronym == "ICO" & country == "Spain" ~ "International Commission for Optics", # and Spain
    acronym == "ICO" & country == "United Kingdom" ~ "International Coffee Organization",
    acronym == "ICPR" ~ "International Commission for the Protection of the Rhine",
    acronym == "ICS" & str_detect(address, "International Chamber") ~ "International Chamber of Shipping",
    acronym == "ICS" & str_detect(address, "International Continence") ~ "International Continence Society",
    acronym == "ICSTI" ~ "International Council for Scientific and Technical Information", # This is the same for both France and Russia
    acronym == "IEA" & country == "Netherlands" ~ "International Ergonomics Association",
    acronym == "IEA" & country == "France" ~ "International Energy Agency",
    acronym == "IFOMA" ~ "International Fishmeal and Oil Manufacturers Association",
    acronym == "IFRA" & country == "Belgium" ~ "International Fragrance Association",
    acronym == "IFRA" & country == "Germany" ~ "INCA-FIEJ Research Association",
    acronym == "IH" ~ "International Hotel & Restaurant Association",
    acronym == "IMA" ~ "Industrial Minerals Association",
    acronym == "IMS" ~ "IMS Global Learning Consortium",
    acronym == "Int" ~ "International Pepper Community",
    acronym == "INT" ~ "Institut National des Télécommunications",
    acronym == "ISEO" ~ "International Sustainable Energy Organization for renewable energy and enery efficiency",
    acronym == "ISITC" ~ "International Securities Association for Institutional Trade Communication International Operation Association",
    acronym == "ISMA" ~ "International Superphosphate and Compound Manufacturers' Association",
    acronym == "ISOC" ~ "Internet Society",
    acronym == "ISSN" ~ "ISSN International Centre",
    acronym == "ITSO" ~ "International Telecommunications Satellite Organization",
    acronym == "JCGM" ~ "Joint Committee for Guides in Metrology",
    acronym == "JRC" ~ "Joint Research Centre",
    acronym == "OMD" ~ "World Customs Organization",
    acronym == "PC" ~ "Permanent Committee on Spatial Data Infrastructure for the Americas",
    acronym == "UEA" & country == "Netherlands" ~ "Universala Esperanto",
    acronym == "UEA" & country == "Belgium" ~ "European Furniture Manufacturers Federation",
    acronym == "UER" ~ "European Broadcasting Union Centre",
    acronym == "UIPPA" ~ "International Union of Pure and Applied Physics",
    acronym == "UNECA" ~ "UN Economic Commission for Africa",
    acronym == "UNECE" ~ "United Nations Economic Commission for Europe",
    acronym == "UNESCAP" ~ "United Nations Economic and Social Commission for Asia and the Pacific",
    acronym == "UNI" ~ "Union Network International",
    acronym == "W"      ~ "World Wide Web Consortium",
    acronym == "World" ~ "World Shipping Council",
    TRUE ~ organization  # For cases where the acronym doesn't match any known values
  ))

saveRDS(liaison_v1_table, file = "../raw_data/archive_liaison/liaison_v1_table.rds")


##### VERSION 2 #####

### TC ###

files <- list.files("../raw_data/archive_liaison/version2/tc_site/", full.names = TRUE)

liaison_tc_v2 <- function(page) {
  
  webpage <- read_html(page)
  
  tryCatch({
    
    liaison_check <- webpage %>%
        html_elements("h3") %>%
        html_text2() %>%
        str_extract_all("Organizations in liaison") %>%
        unlist()
  
  if(liaison_check == "Organizations in liaison"){
    
    acronym <- webpage %>%
      html_elements("p") %>% 
      html_text() %>% # Parse to text
      str_squish() %>%
      str_remove_all("\\*.*") %>%
      stringi::stri_remove_empty() %>%
      last()
    
    acronym <- paste0(extract_acronyms(acronym), collapse = ", ")
  
    tc_number <- webpage %>%
      html_elements("#content > h1") %>% 
      html_text() %>% # Parse to text
      str_squish()
    
    tc_name <- webpage %>%
      html_elements("#content > h2") %>% 
      html_text() %>% 
      str_squish()
    
    date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract the date from the linkname
    
    table <- tibble(committee = tc_number,
                    title = tc_name, 
                    acronym = acronym,
                    date = date,
                    webid = str_remove_all(page, "../raw_data/archive_liaison/version2/tc_site/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
    
  } else { 
    
    message("No organizations in liaison for ", page)
    
  }
    
    return(table)
    
  
  }, error=function(e){message("No liaison organizations for TC in page ", page)})

}

liaison_tc_v2 <- lapply(files, liaison_tc_v2)

### Organization ###

##### 1

files <- list.files("../raw_data/archive_liaison/version2/organization_site/", full.names = TRUE)  %>%
  str_remove("../raw_data/archive_liaison/version2/organization_site/tc_folder") %>%
  stringi::stri_remove_empty_na()

liaison_org_v2 <- function(page) {
  
  tryCatch({
  
  acronym <- read_html(page) %>%
    html_elements("div > h1") %>% 
    html_text() %>% 
    str_squish() %>%
    first()
  
  name <- read_html(page) %>%
    html_elements("div > h2") %>% 
    html_text() %>% 
    str_squish() %>%
    str_remove_all("International Organization for Standardization") %>%
    stringi::stri_remove_empty() %>%
    first()
  
  address <- read_html(page) %>%
    html_elements("p") %>% 
    html_text() %>% 
    str_squish() %>%
    first()
    
  address <- str_squish(str_replace_all(address, "(?<!^)(?=[A-Z])", " ")) # Add spaces into the address and format out extra whitespace
  
  date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract the date from the linkname
  
  table <- tibble(acronym = acronym,
                  name = str_squish(name), 
                  address = str_squish(address),
                  country = word(address, -1),
                  date = date,
                  webid = str_remove_all(page, "../raw_data/archive_liaison/version2/organization_site/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
  
  return(table)
  
  }, error=function(e){message("Failed to parse page ", page)})
  
}

liaison_org_v2 <- lapply(files, liaison_org_v2)

###### 2

files <- list.files("../raw_data/archive_liaison/version2/organization_site/tc_folder/", full.names = TRUE)

liaison_orgtc_v2 <- function(page) {
  
  tryCatch({
    
    acronym <- read_html(page) %>%
      html_elements("div > h1") %>% 
      html_text() %>% 
      str_squish() %>%
      first()
    
    name <- read_html(page) %>%
      html_elements("div > h2") %>% 
      html_text() %>% 
      str_squish() %>%
      str_remove_all("International Organization for Standardization") %>%
      stringi::stri_remove_empty() %>%
      first()
    
    liaisons <- read_html(page) %>%
      html_elements("table") %>%
      html_table() %>%
      .[[1]] %>%
      mutate(Reference = str_remove(Reference, "^[0-9]+ ")) %>%
      nest()
    
    date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract the date from the linkname
    
    table <- tibble(acronym = acronym,
                    name = str_squish(name), 
                    liaisons = liaisons,
                    date_orgtc = date,
                    webid_orgtc = str_remove_all(page, "../raw_data/archive_liaison/version2/organization_site/tc_folder/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
    
    return(table)
    
  }, error=function(e){message("Failed to parse page ", page)})
  
}

liaison_orgtc_v2 <- lapply(files, liaison_orgtc_v2)


### Putting them together ###

liaison_tc_v2_table <- bind_rows(liaison_tc_v2) %>%
  separate_rows(acronym, sep = ",\\s*") %>%
  mutate(year = str_extract(date, "[0-9]{4}")) %>%
  rename(webid_tcsite = webid, 
         date_tcsite = date)
    
liaison_org_v2_table <- bind_rows(liaison_org_v2) %>%
  mutate(country = ifelse(country == "A", "United States",
                          ifelse(country == "Africa", "South Africa",
                                 ifelse(country == "Federation", "Russian Federation",
                                        ifelse(country == "Kingdom", "United Kingdom",
                                               ifelse(country == "Zealand", "New Zealand",
                                                      ifelse(acronym == "MasterCard", "United States",
                                                             ifelse(country == "Ivoire", "Côte-d' Ivoire",
                                                                    country)))))))) %>%
  mutate(year = str_extract(date, "[0-9]{4}")) %>%
  rename(webid_orgsite = webid, 
         date_orgsite = date) %>%
  filter(country != "liaison")

acronym_countries <- liaison_org_v2_table %>% 
  select(acronym, country, year) %>% 
  unique()

liaison_orgtc_v2_table <- bind_rows(liaison_orgtc_v2) %>%
  unnest(cols = c(liaisons)) %>%
  unnest(cols = c(data)) %>%
  mutate(year = str_extract(date_orgtc, "[0-9]{4}")) %>%
  rename(committee = Reference,
         title = Title) %>%
  select(-name) %>%
  filter(acronym != "More in this section") %>%
  left_join(acronym_countries, by = join_by(acronym, year), relationship = "many-to-many")
  
liaison_v2_table <- liaison_org_v2_table %>%
  select(-c(date_orgsite, webid_orgsite)) %>%
  full_join(liaison_tc_v2_table %>% select(-c(date_tcsite, webid_tcsite)), by = join_by(acronym, year), relationship = "many-to-many") %>%
  full_join(liaison_orgtc_v2_table %>% select(-c(date_orgtc, webid_orgtc)), by = join_by(acronym, year, country, committee, title), relationship = "many-to-many") %>%
  unique()

## Still some NA for some organizations on some countries
    
saveRDS(liaison_v2_table, file = "../raw_data/archive_liaison/liaison_v2_table.rds")


##### VERSION 3 #####

### TC ###

files <- list.files("../raw_data/archive_liaison/version3/tc_site/", full.names = TRUE)

liaison_tc_v3 <- function(page) {
  
  webpage <- read_html(page)
  
  tryCatch({
    
    liaison_check_A_B <- webpage %>% 
      html_elements("#liaisons > h3") %>% 
      html_text2() %>%
      str_extract_all("Organizations in liaison \\(Category A") %>%
      unlist() %>%
      first()  %>%
      replace_na("Non")
    
    liaison_check_C_D <- webpage %>% 
      html_elements("#liaisons > h3") %>% 
      html_text2() %>%
      str_extract_all("Organizations in liaison \\(Category C") %>%
      unlist() %>%
      first() %>%
      replace_na("Non")
    
    liaison_check_A_B_C_D <- webpage %>% 
      html_elements("#liaisons > h3") %>% 
      html_text2() %>%
      str_extract_all("Organizations in liaison \\(Category A|Organizations in liaison \\(Category C") %>%
      unlist() %>%
      paste0(collapse = " ") %>%
      replace_na("Non")
  
    if(liaison_check_A_B == "Organizations in liaison (Category A" & liaison_check_A_B_C_D != "Organizations in liaison (Category A Organizations in liaison (Category C"){
    
      acronym <- webpage %>% 
        html_elements("#liaisons > p") %>%
        html_text2() %>%
        str_squish() %>%
        stringi::stri_remove_empty() %>%
        last()
      
      acronym <- paste0(extract_acronyms(acronym), collapse = ", ")

      tc_number <- webpage %>%
        html_elements("body > div.content.clearfix > h1") %>% 
        html_text() %>%
        str_squish() %>%
        str_extract("ISO/(IEC JTC )?(TC )?[0-9]+(\\/SC )?([0-9]+)?")
      
      tc_name <- webpage %>%
        html_elements("body > div.content.clearfix > h1") %>% 
        html_text() %>% 
        str_squish() %>%
        str_remove("ISO/(IEC JTC )?(TC )?[0-9]+(\\/SC )?([0-9]+)?") %>%
        str_squish()
      
      date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract the date from the linkname
      
      table <- tibble(committee = tc_number,
                      title = tc_name, 
                      acronym = acronym,
                      date = date,
                      webid = str_remove_all(page, "../raw_data/archive_liaison/version3/tc_site/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
      
    } 
    
    if(liaison_check_C_D == "Organizations in liaison (Category C" & liaison_check_A_B_C_D != "Organizations in liaison (Category A Organizations in liaison (Category C"){
        
        acronym <- webpage %>% 
          html_elements("#liaisons > p") %>%
          html_text2() %>%
          str_squish() %>%
          stringi::stri_remove_empty() %>%
          last()
        
        acronym <- paste0(extract_acronyms(acronym), collapse = ", ")
        
        tc_number <- webpage %>%
          html_elements("body > div.content.clearfix > h1") %>% 
          html_text() %>%
          str_squish() %>%
          str_extract("ISO/(IEC JTC )?(TC )?[0-9]+(\\/SC )?([0-9]+)?")
        
        tc_name <- webpage %>%
          html_elements("body > div.content.clearfix > h1") %>% 
          html_text() %>% 
          str_squish() %>%
          str_remove("ISO/(IEC JTC )?(TC )?[0-9]+(\\/SC )?([0-9]+)?") %>%
          str_squish()
        
        date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract the date from the linkname
        
        table <- tibble(committee = tc_number,
                        title = tc_name, 
                        acronym = acronym,
                        date = date,
                        webid = str_remove_all(page, "../raw_data/archive_liaison/version3/tc_site/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
        
      } 
    
    if(liaison_check_A_B_C_D == "Organizations in liaison (Category A Organizations in liaison (Category C"){
          
          acronym <- webpage %>% 
            html_elements("#liaisons > p") %>%
            html_text2() %>%
            str_squish() %>%
            stringi::stri_remove_empty() %>%
            tail(2) %>%
            paste0(collapse = ", ")
          
          acronym <- paste0(extract_acronyms(acronym), collapse = ", ")
          
          tc_number <- webpage %>%
            html_elements("body > div.content.clearfix > h1") %>% 
            html_text() %>%
            str_squish() %>%
            str_extract("ISO/(IEC JTC )?(TC )?[0-9]+(\\/SC )?([0-9]+)?")
          
          tc_name <- webpage %>%
            html_elements("body > div.content.clearfix > h1") %>% 
            html_text() %>% 
            str_squish() %>%
            str_remove("ISO/(IEC JTC )?(TC )?[0-9]+(\\/SC )?([0-9]+)?") %>%
            str_squish()
          
          date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract the date from the linkname
          
          table <- tibble(committee = tc_number,
                          title = tc_name, 
                          acronym = acronym,
                          date = date,
                          webid = str_remove_all(page, "../raw_data/archive_liaison/version3/tc_site/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
        
    } else { 
      
      message("No organizations in liaison for ", page)
      
    }
    
    return(table)
    
  }, error=function(e){message("Failed to parse page ", page)})
  
}

liaison_tc_v3 <- lapply(files, liaison_tc_v3)

### Organization ###

##### 1

files <- list.files("../raw_data/archive_liaison/version3/organization_site/", full.names = TRUE)   %>%
  str_remove("../raw_data/archive_liaison/version3/organization_site/tc_folder") %>%
  stringi::stri_remove_empty_na()

liaison_org_v3 <- function(page) {
  
  webpage <- read_html(page)
  
  tryCatch({
    
    acronym <- webpage %>%
      html_elements("body > div.content.clearfix > h1") %>% 
      html_text() %>% 
      str_squish() %>%
      first()
    
    name <- webpage %>%
      html_elements("body > div.content.clearfix > h2") %>% 
      html_text() %>% 
      str_squish() %>%
      stringi::stri_remove_empty() %>%
      first()
    
    address <- webpage %>%
      html_elements("body > div.content.clearfix > p") %>% 
      html_text() %>% 
      str_squish() %>%
      first()
    
    address <- str_squish(str_replace_all(address, "(?<!^)(?=[A-Z])", " ")) # Add spaces into the address and format out extra whitespace
    
    date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract the date from the linkname
    
    table <- tibble(acronym = acronym,
                    name = str_squish(name), 
                    address = str_squish(address),
                    country = word(address, -1),
                    date = date,
                    webid = str_remove_all(page, "../raw_data/archive_liaison/version3/organization_site/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
    
    return(table)
    
  }, error=function(e){message("Failed to parse page ", page)})
  
}

liaison_org_v3 <- lapply(files, liaison_org_v3)

###### 2

files <- list.files("../raw_data/archive_liaison/version3/organization_site/tc_folder/", full.names = TRUE)

liaison_orgtc_v3 <- function(page) {
  
  tryCatch({
    
    acronym <- read_html(page) %>%
      html_elements("div > h1") %>% 
      html_text() %>% 
      str_squish() %>%
      first()
    
    name <- read_html(page) %>%
      html_elements("div > h2") %>% 
      html_text() %>% 
      str_squish() %>%
      str_remove_all("International Organization for Standardization") %>%
      stringi::stri_remove_empty() %>%
      first()
    
    liaisons <- read_html(page) %>%
      html_elements("table") %>%
      html_table() %>%
      .[[1]] %>%
      mutate(Reference = str_remove(Reference, "^[0-9]+ ")) %>%
      nest()
    
    date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract the date from the linkname
    
    table <- tibble(acronym = acronym,
                    name = str_squish(name), 
                    liaisons = liaisons,
                    date_orgtc = date,
                    webid_orgtc = str_remove_all(page, "../raw_data/archive_liaison/version2/organization_site/tc_folder/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
    
    return(table)
    
  }, error=function(e){message("Failed to parse page ", page)})
  
}

liaison_orgtc_v3 <- lapply(files, liaison_orgtc_v3)


### Putting them together ###

liaison_tc_v3_table <- bind_rows(liaison_tc_v3) %>%
  separate_rows(acronym, sep = ",\\s*") %>%
  mutate(year = str_extract(date, "[0-9]{4}")) %>%
  rename(webid_tcsite = webid,
         date_tcsite = date)

liaison_org_v3_table <- bind_rows(liaison_org_v3) %>%
  mutate(country = ifelse(country == "A", "United States",
                          ifelse(country == "Africa", "South Africa",
                                 ifelse(country == "Federation", "Russian Federation",
                                        ifelse(country == "Kingdom", "United Kingdom",
                                               ifelse(country == "Zealand", "New Zealand",
                                                      ifelse(acronym == "MasterCard", "United States",
                                                             ifelse(country == "Ivoire", "Côte-d' Ivoire",
                                                                    country)))))))) %>%
  mutate(year = str_extract(date, "[0-9]{4}")) %>%
  rename(webid_orgsite = webid,
         date_orgsite = date) %>%
  filter(country != "liaison")
 
acronym_countries <- liaison_org_v3_table %>%
  select(acronym, country, year) %>%
  unique()

liaison_orgtc_v3_table <- bind_rows(liaison_orgtc_v3) %>%
  unnest(cols = c(liaisons)) %>%
  unnest(cols = c(data)) %>%
  mutate(year = str_extract(date_orgtc, "[0-9]{4}")) %>%
  rename(committee = Reference,
         title = Title) %>%
  select(-name) %>%
  filter(acronym != "More in this section") %>%
  left_join(acronym_countries, by = join_by(acronym, year), relationship = "many-to-many")

liaison_v3_table <- liaison_org_v3_table %>%
  select(-c(date_orgsite, webid_orgsite)) %>%
  full_join(liaison_tc_v3_table %>% select(-c(date_tcsite, webid_tcsite)), by = join_by(acronym, year), relationship = "many-to-many") %>%
  full_join(liaison_orgtc_v3_table %>% select(-c(date_orgtc, webid_orgtc)), by = join_by(acronym, year, country, committee, title), relationship = "many-to-many") %>%
  unique()

## Still some NA for some organizations on some countries

saveRDS(liaison_v3_table, file = "../raw_data/archive_liaison/liaison_v3_table.rds")



##### VERSION 4 #####

files <- list.files("../raw_data/archive_liaison/version4/", full.names = TRUE) 

liaison_v4 <- function(page) {

  tryCatch({
  
    webpage <- read_html(page)
    
    acronym <- webpage %>%
      html_elements("nav > h1") %>%
      html_text2()
    
    name <- webpage %>%
      html_elements("nav > h2") %>% 
      html_text2() %>%
      str_squish()
    
    address <- webpage %>%
      html_elements("div > div > div > div > p") %>%
      html_text2() %>%
      first()
    
    address <- str_squish(str_replace_all(address, "(?<!^)(?=[A-Z])", " ")) # Add spaces into the address and format out extra whitespace
  
    liaisons <- webpage %>%
      html_node("#datatable-participations > tbody") %>% 
      html_table()
    
    date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract date from linkname
    
    table <- tibble(acronym = acronym,
                    name = str_squish(name), 
                    address = str_squish(address),
                    country = word(address, -1),
                    date = date,
                    liaison = liaisons %>% nest(),
                    webid = str_remove_all(page, "../raw_data/archive_liaison/version4/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
    
    return(table)
    
  }, error=function(e){message("Failed to parse page ", page)})
  
}

liaison_v4 <- lapply(files, liaison_v4) 

liaison_v4_table <- bind_rows(liaison_v4) %>%
  unnest() %>%
    mutate(country = ifelse(country == "States", "United States",
                            ifelse(country == "Africa", "South Africa",
                                   ifelse(country == "Federation", "Russian Federation",
                                          ifelse(country == "Kingdom", "United Kingdom",
                                                 ifelse(country == "Zealand", "New Zealand",
                                                        ifelse(acronym == "MasterCard", "United States",
                                                               ifelse(country == "Ivoire", "Côte-d' Ivoire",
                                                                      country)))))))) %>%
  filter(country != "www.icsh.org") %>%
  unnest() %>%
  rename(committee = X1,
         title = X2,
         Type = X3)

saveRDS(liaison_v4_table, file = "../raw_data/archive_liaison/liaison_v4_table.rds")


##### CURRENT VERSION #####

files <- list.files("../raw_data/archive_liaison/current-2023-07-31/", full.names = TRUE) 

liaison_current <- function(page) { # Webpage hasn't changed since 2017, so procedure follows that of version 4
  
  webpage <- read_html(page)
  
  acronym <- webpage %>%
    html_elements("div > div > div > nav > h1") %>% 
    html_text2()
  
  name <- webpage %>%
    html_elements("div > div > div > nav > h2") %>% 
    html_text2()
  
  address <- webpage %>%
    html_elements("div > div > div > div > p") %>%
    html_text2() %>%
    first()
  
  address <- str_squish(str_replace_all(address, "(?<!^)(?=[A-Z])", " ")) # Add spaces into the address and format out extra whitespace
  
  liaisons <- webpage %>%
    html_node("#datatable-participations > tbody") %>% 
    html_table()
  
  date <- str_extract(page, "[0-9]{4}-[0-9]{2}-[0-9]{2}") # Regex to extract date from linkname
  
  table <- tibble(acronym = acronym,
                  name = str_squish(name), 
                  address = str_squish(address),
                  country = word(address, -1),
                  date = date,
                  liaison = liaisons %>% nest(),
                  webid = str_remove_all(page, "../raw_data/archive_liaison/current-2023-07-31/|_[0-9]{4}-[0-9]{2}-[0-9]{2}.htm"))
  
  return(table)
  
}

liaison_current <- lapply(files, liaison_current) 

liaison_current_table <- bind_rows(liaison_current) %>%
  unnest() %>%
  mutate(country = ifelse(country == "States", "United States",
                          ifelse(country == "Africa", "South Africa",
                                 ifelse(country == "Federation", "Russian Federation",
                                        ifelse(country == "Kingdom", "United Kingdom",
                                               ifelse(country == "Zealand", "New Zealand",
                                                      ifelse(acronym == "MasterCard", "United States",
                                                             ifelse(country == "Ivoire", "Côte-d' Ivoire",
                                                                    country)))))))) %>%
  unnest() %>%
  rename(committee = X1,
         title = X2,
         Type = X3)

saveRDS(liaison_current_table, file = "../raw_data/archive_liaison/liaison_current_table.rds")


