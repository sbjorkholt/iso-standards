

########################################################################## GATHERING FROM HISTORICAL ARCHIVE #############################################################################################

##############################
######### ISO 9001 ###########
##############################

###### COUNTRY ######

country_iso9001 <- list()

for (i in 4:8) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data//ISO survey/ISO_9001_data_per_country_and_sector_1993_to_2017.xlsm", sheet = i, skip = 1)) 
  # Read in the sheets containing info on country certifications (divided by continent)
  
  ll <- ll |> 
    rename(country = Year) |> # Change name of variable to country
    filter(country != "Country") |> # Remove the row beneath containing total number of certificates for given table
    pivot_longer(cols = c(`1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, # Convert into long format to have country, year and certificates side by side
                          `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`,
                          `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`,
                          `2014`, `2015`, `2016`, `2017`), 
                 names_to = "year", values_to = "certificates") |> 
    mutate(iso = "iso_9001")
  
  country_iso9001[[i]] <- ll # Place into common list
  
}

country_iso9001 <- compact(country_iso9001) # Remove empty list elements
country_iso9001[[3]] <- country_iso9001[[3]] |>  select(-`...27`) # Table 3 had some formatting issues, remove column

country_iso9001 <- do.call(rbind, compact(country_iso9001)) # Bind together to one dataframe to get all countries (continents) and years in one place

country_iso9001 <- country_iso9001 |> filter(!country %in% c("ISO 9001 - North America", "ISO 9001 - Central and South Asia", "Year")) # Removing other formatting issues

###### INDUSTRY ######

industry_iso9001 <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_9001_data_per_country_and_sector_1993_to_2017.xlsm", sheet = 9, skip = 4)) |> # Read in sheet number nine
  rename(industry = `ISO 9001 BY INDUSTRIAL SECTOR`) |>  # Rename the column on industry
  select(-`EA*                   Code Nos.`) # Remove the first messy column

industry_iso9001 <- industry_iso9001[3:nrow(industry_iso9001),] # Remove the first two rows containing missin values

industry_iso9001 <- industry_iso9001 |> 
  pivot_longer(cols = c(`1998`, `1999`, # Convert into long format to have industry, year and certificates side by side
                        `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`,
                        `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`,
                        `2014`, `2015`, `2016`, `2017`),  
               names_to = "year", values_to = "certificates") |> 
  mutate(iso = "iso_9001")

industry_iso9001 <- industry_iso9001 |> drop_na(industry) # Cells with "total" category received NA


################################
########## ISO 14001 ###########
################################

###### COUNTRY ######

country_iso14001 <- list()

for (i in 4:8) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_14001_data_per_country_and_sector_1999_to_2017.xlsm", sheet = i, skip = 1)) 

  ll <- ll |> 
    rename(country = Year) |> 
    filter(country != "Country") |> 
    pivot_longer(cols = c(`1999`, 
                          `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`,
                          `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`,
                          `2014`, `2015`, `2016`, `2017`), 
                 names_to = "year", values_to = "certificates") |> 
    mutate(iso = "iso_14001")
  
  country_iso14001[[i]] <- ll 
  
}

country_iso14001 <- compact(country_iso14001) 
country_iso14001[[3]] <- country_iso14001[[3]] |>  select(-`...21`) 

country_iso14001 <- do.call(rbind, compact(country_iso14001)) 

country_iso14001 <- country_iso14001 |> filter(!country %in% c("ISO 14001 - North America", "ISO 14001 - Central and South Asia", "Year"))

###### INDUSTRY ######

industry_iso14001 <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_14001_data_per_country_and_sector_1999_to_2017.xlsm", sheet = 9, skip = 2)) |> 
  rename(industry = `ISO 14001 BY INDUSTRIAL SECTOR`) |> 
  select(-`EA*                   Code Nos.`)

industry_iso14001 <- industry_iso14001[3:nrow(industry_iso14001),] 

industry_iso14001 <- industry_iso14001 |> 
  pivot_longer(cols = c(`1998`, `1999`, 
                        `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`,
                        `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`,
                        `2014`, `2015`, `2016`, `2017`), 
               names_to = "year", values_to = "certificates") |> 
  mutate(iso = "iso_14001")

industry_iso14001 <- industry_iso14001 |> drop_na(industry)

################################
########## ISO 27001 ###########
################################

###### COUNTRY ######

country_iso27001 <- list()

for (i in 5:11) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_IEC_27001_data_per_country_and_sector_2006_to_2017.xlsm", sheet = i, skip = 1)) 

  ll <- ll |> 
    rename(country = Year) |> 
    filter(country != "Country") |> 
    pivot_longer(cols = c(`2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`,
                          `2014`, `2015`, `2016`, `2017`), 
                 names_to = "year", values_to = "certificates") |> 
    mutate(iso = "iso_27001")
  
  country_iso27001[[i]] <- ll 
  
}

country_iso27001 <- compact(country_iso27001) 
country_iso27001 <- do.call(rbind, compact(country_iso27001)) 

###### INDUSTRY ######

industry_iso27001 <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_IEC_27001_data_per_country_and_sector_2006_to_2017.xlsm", sheet = 12, skip = 2)) |>
  rename(industry = `ISO/IEC 27001 BY INDUSTRIAL SECTOR`) |>  
  select(-`EA*                   Code Nos.`)

industry_iso27001 <- industry_iso27001[2:nrow(industry_iso27001),] |> 
  drop_na(industry) # Extra empty columns in excel yield NA, remove them

industry_iso27001 <- industry_iso27001 |> 
  pivot_longer(cols = c(`2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`,
                        `2014`, `2015`, `2016`, `2017`), 
               names_to = "year", values_to = "certificates") |> 
  mutate(iso = "iso_27001")


################################
########## ISO 50001 ###########
################################

###### COUNTRY ######

country_iso50001 <- list()

for (i in 5:11) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_50001_data_per_country_and_sector_2011_to_2017.xlsx", sheet = i, skip = 1)) 

  ll <- ll |> 
    rename(country = Year) |> 
    filter(country != "Country") |> 
    pivot_longer(cols = c(`2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`), 
                 names_to = "year", values_to = "certificates") |> 
    mutate(iso = "iso_50001")
  
  country_iso50001[[i]] <- ll 
  
}

country_iso50001 <- compact(country_iso50001) 
country_iso50001 <- do.call(rbind, compact(country_iso50001)) 


###### INDUSTRY ######

industry_iso50001 <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_50001_data_per_country_and_sector_2011_to_2017.xlsx", sheet = 12, skip = 2)) |> 
  rename(industry = `ISO 50001 BY INDUSTRIAL SECTOR`) |>  
  select(-`EA*                   Code Nos.`) 

industry_iso50001 <- industry_iso50001[3:nrow(industry_iso50001),] |> 
  drop_na(industry)

industry_iso50001 <- industry_iso50001 |> 
  pivot_longer(cols = c(`2015`, `2016`, `2017`), 
               names_to = "year", values_to = "certificates") |> 
  mutate(iso = "iso_50001")


################################
########## ISO 22000 ###########
################################

###### COUNTRY ######

country_iso22000 <- list()

for (i in 5:11) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_22000_data_per_country_2007_to_2017.xlsx", sheet = i, skip = 1)) 

  ll <- ll |> 
    rename(country = Year) |>
    filter(country != "Country") |> 
    pivot_longer(cols = c(`2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`), 
                 names_to = "year", values_to = "certificates") |> 
    mutate(iso = "iso_20000")
  
  country_iso22000[[i]] <- ll 
  
}

country_iso22000 <- compact(country_iso22000) 
country_iso22000 <- do.call(rbind, compact(country_iso22000)) 


################################
########## ISO 13485 ###########
################################

###### COUNTRY ######

country_iso13485 <- list()

for (i in 5:11) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_13485_data_per_country_2004_to_2017.xlsm", sheet = i, skip = 1)) 

  ll <- ll |> 
    rename(country = Year) |> 
    filter(country != "Country") |>
    pivot_longer(cols = c(`2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`), 
                 names_to = "year", values_to = "certificates") |> 
    mutate(iso = "iso_13485")
  
  country_iso13485[[i]] <- ll 
  
}

country_iso13485 <- compact(country_iso13485) 
country_iso13485 <- do.call(rbind, compact(country_iso13485)) 


################################
########## ISO 22301 ###########
################################

###### COUNTRY ######

country_iso22301 <- list()

for (i in 5:11) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_22301_data_per_country_and_sector_2014_to_2017.xlsx", sheet = i, skip = 1)) 

  ll <- ll |> 
    rename(country = Year) |> 
    filter(country != "Country") |> 
    pivot_longer(cols = c(`2014`, `2015`, `2016`, `2017`), 
                 names_to = "year", values_to = "certificates") |> 
    mutate(iso = "iso_22301")
  
  country_iso22301[[i]] <- ll
  
}

country_iso22301 <- compact(country_iso22301)
country_iso22301 <- do.call(rbind, compact(country_iso22301))

###### INDUSTRY ######

industry_iso22301 <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_22301_data_per_country_and_sector_2014_to_2017.xlsx", sheet = 12, skip = 2)) |> 
  rename(industry = `ISO 22301 BY INDUSTRIAL SECTOR`) |>  
  select(-`EA*                   Code Nos.`) 

industry_iso22301 <- industry_iso22301[3:nrow(industry_iso22301),] |> 
  drop_na(industry)

industry_iso22301 <- industry_iso22301 |> 
  pivot_longer(cols = c(`2014`, `2015`, `2016`, `2017`), 
               names_to = "year", values_to = "certificates") |> 
  mutate(iso = "iso_22301")


###################################
########### ISO 20000-1 ###########
###################################

###### COUNTRY ######

country_iso20000_1 <- list()

for (i in 5:11) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_20000-1_data_per_country_and_sector_2015_to_2017.xlsx", sheet = i, skip = 1)) 
  
  ll <- ll |> 
    rename(country = Year) |> 
    filter(country != "Country") |> 
    pivot_longer(cols = c(`2015`, `2016`, `2017`), 
                 names_to = "year", values_to = "certificates") |> 
    mutate(iso = "iso_20000_1")
  
  country_iso20000_1[[i]] <- ll
  
}

country_iso20000_1 <- compact(country_iso20000_1)
country_iso20000_1 <- do.call(rbind, compact(country_iso20000_1))

###### INDUSTRY ######

industry_iso20000_1 <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_20000-1_data_per_country_and_sector_2015_to_2017.xlsx", sheet = 12, skip = 2)) |> 
  rename(industry = `ISO 20000-1 BY INDUSTRIAL SECTOR`) |>  
  select(-`EA*                   Code Nos.`) 

industry_iso20000_1 <- industry_iso20000_1[3:nrow(industry_iso20000_1),] |> 
  drop_na(industry)

industry_iso20000_1 <- industry_iso20000_1 |> 
  pivot_longer(cols = c(`2015`, `2016`, `2017`), 
               names_to = "year", values_to = "certificates") |> 
  mutate(iso = "iso__20000_1")


#################################
########### ISO 28000 ###########
#################################

###### COUNTRY ######

country_iso28000 <- list()

for (i in 5:11) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_28000_data_per_country_and_sector_2016_to_2017.xlsx", sheet = i, skip = 1)) 
  
  ll <- ll |> 
    rename(country = Year) |> 
    filter(country != "Country") |> 
    pivot_longer(cols = c(`2016`, `2017`), 
                 names_to = "year", values_to = "certificates") |> 
    mutate(iso = "iso__28000")
  
  country_iso28000[[i]] <- ll
  
}

country_iso28000 <- compact(country_iso28000)
country_iso28000 <- do.call(rbind, compact(country_iso28000))

###### INDUSTRY ######

industry_iso28000 <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_28000_data_per_country_and_sector_2016_to_2017.xlsx", sheet = 12, skip = 2)) |> 
  rename(industry = `ISO 28000 BY INDUSTRIAL SECTOR`) |>  
  select(-`EA*                   Code Nos.`) 

industry_iso28000 <- industry_iso28000[3:nrow(industry_iso28000),] |> 
  drop_na(industry)

industry_iso28000 <- industry_iso28000 |> 
  pivot_longer(cols = c(`2016`, `2017`), 
               names_to = "year", values_to = "certificates") |> 
  mutate(iso = "iso__28000")


#################################
########### ISO 39001 ###########
#################################

###### COUNTRY ######

country_iso39001 <- list()

for (i in 5:11) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_39001_data_per_country_and_sector_2016_to_2017.xlsx", sheet = i, skip = 1)) 
  
  ll <- ll |> 
    rename(country = Year) |> 
    filter(country != "Country") |> 
    pivot_longer(cols = c(`2016`, `2017`), 
                 names_to = "year", values_to = "certificates") |> 
    mutate(iso = "iso__39001")
  
  country_iso39001[[i]] <- ll
  
}

country_iso39001 <- compact(country_iso39001)
country_iso39001 <- do.call(rbind, compact(country_iso39001))

###### INDUSTRY ######

industry_iso39001 <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_39001_data_per_country_and_sector_2016_to_2017.xlsx", sheet = 12, skip = 2)) |> 
  rename(industry = `ISO 39001 BY INDUSTRIAL SECTOR`) |>  
  select(-`EA*                   Code Nos.`) 

industry_iso39001 <- industry_iso39001[3:nrow(industry_iso39001),] |> 
  drop_na(industry)

industry_iso39001 <- industry_iso39001 |>
  select(-c(`...5`,  `...6`,  `...7`,  `...8`,  `...9`,  `...10`, `...11`, `...12`)) |> 
  pivot_longer(cols = c(`2016`, `2017`), 
               names_to = "year", values_to = "certificates") |> 
  mutate(iso = "iso_39001")


################################################################################### GATHERING FROM NEWER ARCHIVE ##########################################################################################

## 2018

country_2018 <- list()
industry_2018 <- list()

for (i in 2:13) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_Survey_2018_results_Number_of_certificates_and_sites_per_country_and_the_number_of_sector_overall.xlsx", sheet = i, skip = 1))
  iso <- readxl::excel_sheets("../raw_data/ISO survey/ISO_Survey_2018_results_Number_of_certificates_and_sites_per_country_and_the_number_of_sector_overall.xlsx")[i] %>%
    str_to_lower() %>%
    str_replace_all(., " ", "_")
  
  country <- ll |> 
    select(Country, certificates) |> 
    rename(country = Country)
  
  if(iso == "iso_22000" | iso == "iso_13485"){ # Industry datasets did not exist for these committees for 2020
    
    next
    
  } else {
    
    industry <- ll |> 
      select(Sector, Number) |> 
      rename(industry = Sector,
             certificates = Number)
    
  }
  
  country_2018[[i]] <- country |> mutate(iso = iso, year = "2018")
  industry_2018[[i]] <- industry |> mutate(iso = iso, year = "2018")
  
}

country_2018 <- do.call(rbind, compact(country_2018))
industry_2018 <- do.call(rbind, compact(industry_2018)) |> 
  drop_na(industry)

## 2019

country_2019 <- list()
industry_2019 <- list()

for (i in 2:13) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_Survey_2019_results_Number_of_certificates_and_sites_per_country_and_the_number_of_sector_overall.xlsx", sheet = i, skip = 1))
  iso <- readxl::excel_sheets("../raw_data/ISO survey/ISO_Survey_2019_results_Number_of_certificates_and_sites_per_country_and_the_number_of_sector_overall.xlsx")[i] %>%
    str_to_lower() %>%
    str_replace_all(., " ", "_")
  
  country <- ll |> 
    select(Country, certificates) |> 
    rename(country = Country)
  
  if(iso == "iso_22000" | iso == "iso_13485"){ # Industry datasets did not exist for these committees for 2020
    
    next
    
  } else {
    
    industry <- ll |> 
      select(Sector, Number) |> 
      rename(industry = Sector,
             certificates = Number)
    
  }
  
  country_2019[[i]] <- country |> mutate(iso = iso, year = "2019")
  industry_2019[[i]] <- industry |> mutate(iso = iso, year = "2019")
  
}

country_2019 <- do.call(rbind, compact(country_2019))
industry_2019 <- do.call(rbind, compact(industry_2019)) |> 
  drop_na(industry)


## 2020

country_2020 <- list()
industry_2020 <- list()

for (i in 2:13) {
  
  ll <- suppressMessages(readxl::read_excel("../raw_data/ISO survey/ISO_Survey_2020_results_Number_of_certificates_and_sites_per_country_and_the_number_of_sector_overall.xlsx", sheet = i, skip = 1))
  iso <- readxl::excel_sheets("../raw_data/ISO survey/ISO_Survey_2020_results_Number_of_certificates_and_sites_per_country_and_the_number_of_sector_overall.xlsx")[i] %>%
    str_to_lower() %>%
    str_replace_all(., " ", "_")
  
  country <- ll |> 
    select(Country, certificates) |> 
    rename(country = Country)
  
  if(iso == "iso_22000" | iso == "iso_13485"){ # Industry datasets did not exist for these committees for 2020
    
    next
    
    } else {
    
    industry <- ll |> 
      select(Sector, Number) |> 
      rename(industry = Sector,
             certificates = Number)
    
    }
  
  country_2020[[i]] <- country |> mutate(iso = iso, year = "2020")
  industry_2020[[i]] <- industry |> mutate(iso = iso, year = "2020")
  
}

country_2020 <- do.call(rbind, compact(country_2020))
industry_2020 <- do.call(rbind, compact(industry_2020)) |> 
  drop_na(industry)

################################################################################# MERGING #################################################################################################

country_certifications <- bind_rows(country_iso9001, 
                                    country_iso14001, 
                                    country_iso27001, 
                                    country_iso50001, 
                                    country_iso22000, 
                                    country_iso13485, 
                                    country_iso22301, 
                                    country_iso20000_1, 
                                    country_iso28000, 
                                    country_iso39001,
                                    country_2018,
                                    country_2019, 
                                    country_2020)


country_certifications <- country_certifications %>% 
  mutate(iso = ifelse(iso == "iso_20000_1", "iso_20000-1",
                      ifelse(iso == "iso_iec_20000-1", "iso_20000-1",
                             ifelse(iso == "iso_iec_27001", "iso_27001",
                                    ifelse(iso == "iso__20000_1", "iso_20000-1",
                                           ifelse(iso == "iso__39001", "iso_39001",
                                                  ifelse(iso == "iso__28000", "iso_28000", iso)))))))

table(country_certifications$iso, country_certifications$year)

industry_certifications <- bind_rows(industry_iso9001, 
                                     industry_iso14001, 
                                     industry_iso27001, 
                                     industry_iso50001, 
                                     industry_iso22301, 
                                     industry_iso20000_1, 
                                     industry_iso28000, 
                                     industry_iso39001,
                                     industry_2018,
                                     industry_2019, 
                                     industry_2020)

industry_certifications <- industry_certifications %>% 
  mutate(iso = ifelse(iso == "iso_20000_1", "iso_20000-1",
                      ifelse(iso == "iso__20000_1", "iso_20000-1",
                      ifelse(iso == "iso_iec_20000-1", "iso_20000-1",
                             ifelse(iso == "iso_iec_27001", "iso_27001",
                                    ifelse(iso == "iso__28000", "iso_28000", iso))))))

table(industry_certifications$iso, industry_certifications$year)

saveRDS(country_certifications, file = "../datasets/country_certifications.rds")
saveRDS(industry_certifications, file = "../datasets//industry_certifications.rds")
