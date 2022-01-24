
library(tidyverse)

## DISCLAIMER: 
# The ISO Survey is not a database. The providers of the data are the certification bodies
# accredited by IAF members and they participate on a voluntary basis. The level of
# participation fluctuates from one edition of the survey to another and can impact the survey
# results especially at the country level. Interpretations of the results and any conclusions
# on the trends should be made with these considerations in mind


########################################
######### COUNTRY AND SECTOR ###########
########################################

list.files("M:/Documents/PhD/Drafts/Standards/data/ISO survey")

list_2009 <- list()

for (i in 2:length(readxl::excel_sheets("./Data_per_sector_and_per_country_2009.xls"))) {
  
  ll <- suppressMessages(readxl::read_xls("./Data_per_sector_and_per_country_2009.xls", sheet = i)) 
  
  names(ll)[2] <- pull(ll[2,][2]) # Rename columns after the value in table
  names(ll)[3] <- pull(ll[2,][3])
  names(ll)[4] <- pull(ll[2,][4])
  
  ll <- ll[-1,] # Remove first row which is only NA
  
  ll <- ll %>%
    mutate(country = pull(ll[1][1,])) %>% # Make a country variable
    mutate(year = "2009") %>%
    rename(industry = `ISO Survey of Certifications 2009 - Industrial sectors`) %>%
    select(country, year, industry, `ISO 9001`, `ISO 14001`, `ISO/IEC 27001`)
  
  list_2009[[i]] <- ll[-1,] # Remove first row again, now that we have all information stored in variables
  
}

list_2009 <- do.call(rbind, list_2009)


list_2010 <- list()

for (i in 2:length(readxl::excel_sheets("./Data_per_sector_and_per_country_2010.xls"))) {
  
  ll <- suppressMessages(readxl::read_xls("./Data_per_sector_and_per_country_2010.xls", sheet = i)) 
  
  names(ll)[2] <- pull(ll[2,][2]) # Rename columns after the value in table
  names(ll)[3] <- pull(ll[2,][3])
  names(ll)[4] <- pull(ll[2,][4])
  
  ll <- ll[-1,] # Remove first row which is only NA
  
  ll <- ll %>%
    mutate(country = pull(ll[1][1,])) %>% # Make a country variable
    mutate(year = "2010") %>%
    rename(industry = `ISO Survey of Certifications 2010 - Industrial sectors`) %>%
    select(country, year, industry, `ISO 9001`, `ISO 14001`, `ISO/IEC 27001`)
  
  ll <- ll[-1,] # Remove first row again, now that we have all information stored in variables
  
  list_2010[[i]] <- ll
  
}

list_2010 <- do.call(rbind, list_2010)

list_2011 <- list()

for (i in 2:length(readxl::excel_sheets("./Data_per_sector_and_per_country_2011.xls"))) {
  
  ll <- suppressMessages(readxl::read_xls("./Data_per_sector_and_per_country_2011.xls", sheet = i)) 
  
  names(ll)[2] <- pull(ll[2,][2]) # Rename columns after the value in table
  names(ll)[3] <- pull(ll[2,][3])
  names(ll)[4] <- pull(ll[2,][4])
  
  ll <- ll[-1,] # Remove first row which is only NA
  
  ll <- ll %>%
    mutate(country = pull(ll[1][1,])) %>% # Make a country variable
    mutate(year = "2011") %>%
    rename(industry = `ISO Survey of Certifications 2011 - Industrial sectors`) %>%
    select(country, year, industry, `ISO 9001`, `ISO 14001`, `ISO/IEC 27001`)
  
  ll <- ll[-1,] # Remove first row again, now that we have all information stored in variables
  
  list_2011[[i]] <- ll
  
}

list_2011 <- do.call(rbind, list_2011)

list_2012 <- list()

for (i in 2:length(readxl::excel_sheets("./Data_per_sector_and_per_country_2012.xlsx"))) {
  
  ll <- suppressMessages(readxl::read_xlsx("./Data_per_sector_and_per_country_2012.xlsx", sheet = i)) 
  
  names(ll)[2] <- pull(ll[2,][2]) # Rename columns after the value in table
  names(ll)[3] <- pull(ll[2,][3])
  names(ll)[4] <- pull(ll[2,][4])
  
  ll <- ll[-1,] # Remove first row which is only NA
  
  ll <- ll %>%
    mutate(country = pull(ll[1][1,])) %>% # Make a country variable
    mutate(year = "2012") %>%
    rename(industry = `ISO Survey of Certifications 2012 - Industrial sectors`) %>%
    select(country, year, industry, `ISO 9001`, `ISO 14001`, `ISO/IEC 27001`)
  
  ll <- ll[-1,] # Remove first row again, now that we have all information stored in variables
  
  list_2012[[i]] <- ll
  
}

list_2012 <- do.call(rbind, list_2012)

list_2013 <- list()

for (i in 3:length(readxl::excel_sheets("./Data_per_sector_and_per_country_2013.xlsx"))) {
  
  ll <- suppressMessages(readxl::read_xlsx("./Data_per_sector_and_per_country_2013.xlsx", sheet = i)) 
  
  names(ll)[2] <- pull(ll[2,][2]) # Rename columns after the value in table
  names(ll)[3] <- pull(ll[2,][3])
  names(ll)[4] <- pull(ll[2,][4])
  
  ll <- ll[-1,] # Remove first row which is only NA
  
  ll <- ll %>%
    mutate(country = pull(ll[1][1,])) %>% # Make a country variable
    mutate(year = "2013") %>%
    rename(industry = `ISO Survey of Certifications 2013 - Industrial sectors`) %>%
    select(country, year, industry, `ISO 9001`, `ISO 14001`, `ISO/IEC 27001`)
  
  ll <- ll[-1,] # Remove first row again, now that we have all information stored in variables
  
  list_2013[[i]] <- ll
  
}

list_2013 <- do.call(rbind, list_2013)

list_2014 <- list()

for (i in 3:length(readxl::excel_sheets("./Data_per_sector_and_per_country_2014.xlsx"))) {
  
  ll <- suppressMessages(readxl::read_xlsx("./Data_per_sector_and_per_country_2014.xlsx", sheet = i)) 
  
  names(ll)[2] <- pull(ll[2,][2]) # Rename columns after the value in table
  names(ll)[3] <- pull(ll[2,][3])
  names(ll)[4] <- pull(ll[2,][4])
  
  ll <- ll[-1,] # Remove first row which is only NA
  
  ll <- ll %>%
    mutate(country = pull(ll[1][1,])) %>% # Make a country variable
    mutate(year = "2014") %>%
    rename(industry = `ISO Survey of Certifications 2014 - Industrial sectors`) %>%
    select(country, year, industry, `ISO 9001`, `ISO 14001`, `ISO/IEC 27001`)
  
  ll <- ll[-1,] # Remove first row again, now that we have all information stored in variables
  
  list_2014[[i]] <- ll
  
}

list_2014 <- do.call(rbind, list_2014)

list_2015_ISO14001 <- readxl::read_xlsx("./Data_per_sector_and_per_country_2015.xlsx", sheet = 5) %>%
  gather(2:40,
         key = "industry", value = "ISO 14001") %>%
  rename(country = `Industrial sectors / Countries`) %>%
  mutate(year = "2015") %>%
  select(country, year, industry, `ISO 14001`) %>%
  mutate(country = str_to_title(country))

list_2015_ISOIEC27001_2013 <- readxl::read_xlsx("./Data_per_sector_and_per_country_2015.xlsx", sheet = 8) %>%
  gather(2:40,
         key = "industry", value = "ISO/IEC 27001") %>%
  rename(country = `Industrial sectors / Countries`) %>%
  mutate(year = "2015") %>%
  select(country, year, industry, `ISO/IEC 27001`) %>%
  mutate(country = str_to_title(country)) %>%
  mutate(`ISO 9001` = NA)

list_2015 <- left_join(list_2015_ISO14001, list_2015_ISOIEC27001_2013) %>%
  mutate(`ISO 14001` = as.character(`ISO 14001`),
         `ISO/IEC 27001` = as.character(`ISO/IEC 27001`))

### 2016 sector and country does not exist

readxl::excel_sheets("./Data_per_sector_and_per_country_2017.xlsx")

list_2017_ISO14001 <- readxl::read_xlsx("./Data_per_sector_and_per_country_2017.xlsx", sheet = 3, skip = 1) %>%
  gather(2:40,
         key = "industry", value = "ISO 14001") %>%
  rename(country = `Land/Sector`) %>%
  mutate(`ISO 14001` = ifelse(is.na(`ISO 14001`)==TRUE, 0, `ISO 14001`))

list_2017_ISOIEC27001 <- readxl::read_xlsx("./Data_per_sector_and_per_country_2017.xlsx", sheet = 4, skip = 1) %>%
  gather(2:40,
         key = "industry", value = "ISO/IEC 27001") %>%
  rename(country = `Land/Sector`) %>%
  mutate(`ISO/IEC 27001` = ifelse(is.na(`ISO/IEC 27001`)==TRUE, 0, `ISO/IEC 27001`))

list_2017 <- left_join(list_2017_ISO14001, list_2017_ISOIEC27001) %>%
  mutate(`ISO 14001` = as.character(`ISO 14001`),
         `ISO/IEC 27001` = as.character(`ISO/IEC 27001`)) %>%
  mutate(year = "2017") %>%
  mutate(`ISO 9001` = NA)

readxl::excel_sheets("./Data_per_sector_and_per_country_2018.xlsx")

list_2018_ISO14001 <- readxl::read_xlsx("./Data_per_sector_and_per_country_2018.xlsx", sheet = 3, skip = 2) %>%
  gather(2:41,
         key = "industry", value = "ISO 14001") %>%
  rename(country = `Land/Sector`) %>%
  mutate(`ISO 14001` = ifelse(is.na(`ISO 14001`)==TRUE, 0, `ISO 14001`))

list_2018_ISOIEC27001 <- readxl::read_xlsx("./Data_per_sector_and_per_country_2018.xlsx", sheet = 4, skip = 2) %>%
  gather(2:41,
         key = "industry", value = "ISO/IEC 27001") %>%
  rename(country = `Land/Sector`) %>%
  mutate(`ISO/IEC 27001` = ifelse(is.na(`ISO/IEC 27001`)==TRUE, 0, `ISO/IEC 27001`))

list_2018 <- left_join(list_2018_ISO14001, list_2018_ISOIEC27001) %>%
  mutate(`ISO 14001` = as.character(`ISO 14001`),
         `ISO/IEC 27001` = as.character(`ISO/IEC 27001`)) %>%
  mutate(year = "2018") %>%
  mutate(`ISO 9001` = NA)

readxl::excel_sheets("./Data_per_sector_and_per_country_2019.xlsx")

list_2019_ISO14001 <- readxl::read_xlsx("./Data_per_sector_and_per_country_2019.xlsx", sheet = 3, skip = 2) %>%
  gather(2:41,
         key = "industry", value = "ISO 14001") %>%
  rename(country = `Land/Sector`) %>%
  mutate(`ISO 14001` = ifelse(is.na(`ISO 14001`)==TRUE, 0, `ISO 14001`))

list_2019_ISOIEC27001 <- readxl::read_xlsx("./Data_per_sector_and_per_country_2019.xlsx", sheet = 4, skip = 2) %>%
  gather(2:41,
         key = "industry", value = "ISO/IEC 27001") %>%
  rename(country = `Land/Sector`) %>%
  mutate(`ISO/IEC 27001` = ifelse(is.na(`ISO/IEC 27001`)==TRUE, 0, `ISO/IEC 27001`))

list_2019 <- left_join(list_2019_ISO14001, list_2019_ISOIEC27001) %>%
  mutate(`ISO 14001` = as.character(`ISO 14001`),
         `ISO/IEC 27001` = as.character(`ISO/IEC 27001`)) %>%
  mutate(year = "2019") %>%
  mutate(`ISO 9001` = NA)

readxl::excel_sheets("./Data_per_sector_and_per_country_2019.xlsx")

list_2020_ISO14001 <- readxl::read_xlsx("./Data_per_sector_and_per_country_2020.xlsx", sheet = 3, skip = 2) %>%
  gather(2:41,
         key = "industry", value = "ISO 14001") %>%
  rename(country = `Land/Sector`) %>%
  mutate(`ISO 14001` = ifelse(is.na(`ISO 14001`)==TRUE, 0, `ISO 14001`))

list_2020_ISOIEC27001 <- readxl::read_xlsx("./Data_per_sector_and_per_country_2020.xlsx", sheet = 4, skip = 2) %>%
  gather(2:41,
         key = "industry", value = "ISO/IEC 27001") %>%
  rename(country = `Land/Sector`) %>%
  mutate(`ISO/IEC 27001` = ifelse(is.na(`ISO/IEC 27001`)==TRUE, 0, `ISO/IEC 27001`))

list_2020 <- left_join(list_2020_ISO14001, list_2020_ISOIEC27001) %>%
  mutate(`ISO 14001` = as.character(`ISO 14001`),
         `ISO/IEC 27001` = as.character(`ISO/IEC 27001`)) %>%
  mutate(year = "2020") %>%
  mutate(`ISO 9001` = NA)

iso_2009_2020 <- bind_rows(list_2009, list_2010, list_2011, list_2012, list_2013, list_2014, list_2015, list_2017, list_2018, list_2019, list_2020) %>%
  mutate(country = ifelse(is.na(country)==TRUE & year=="2012", "Afghanistan", country)) %>% # Afghanistan had fell out of the 2012-sheets
  mutate(obs = ifelse(year == "2015", "ISO/IEC 27001 seems like it's from 2013", NA))

save(iso_2009_2020, file = "./data/industry_certification_survey_2009_2020.rds")


