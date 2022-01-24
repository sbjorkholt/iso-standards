
library(tidyverse)

## DISCLAIMER: 
# The ISO Survey is not a database. The providers of the data are the certification bodies
# accredited by IAF members and they participate on a voluntary basis. The level of
# participation fluctuates from one edition of the survey to another and can impact the survey
# results especially at the country level. Interpretations of the results and any conclusions
# on the trends should be made with these considerations in mind


#############################
######### COUNTRY ###########
#############################

# https://isotc.iso.org/livelink/livelink?func=ll&objId=18808772&objAction=browse&viewType=1 

## 2020 ##

readxl::excel_sheets("./data/ISO survey/Data_per_country_2020.xlsx")

country_2020_ISO14001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2020.xlsx", sheet = 3, skip = 1) %>%
  rename(country = Country) %>%
  mutate(`ISO 14001 certificate` = ifelse(is.na(certificates)==TRUE, 0, certificates),
         `ISO 14001 site` = ifelse(is.na(sites)==TRUE, 0, sites),
         year = "2020") %>%
  select(country, year, `ISO 14001 certificate`, `ISO 14001 site`)

# sector_2020_ISO14001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2020.xlsx", sheet = 3, skip = 1) %>%
#   rename(sector = Sector) %>%
#   mutate(`ISO 14001` = ifelse(is.na(Number)==TRUE, 0, Number),
#          year = "2020") %>%
#   select(sector, year, `ISO 14001`) %>%
#   na.omit()

country_2020_ISOIEC27001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2020.xlsx", sheet = 4, skip = 1) %>%
  rename(country = Country) %>%
  mutate(`ISO/IEC 27001 certificate` = ifelse(is.na(certificates)==TRUE, 0, certificates),
         `ISO/IEC 27001 site` = ifelse(is.na(sites)==TRUE, 0, sites),
         year = "2020") %>%
  select(country, year, `ISO/IEC 27001 certificate`, `ISO/IEC 27001 site`) 

# sector_2020_ISOIEC27001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2020.xlsx", sheet = 4, skip = 1) %>%
#   rename(sector = Sector) %>%
#   mutate(`ISO/IEC 27001` = ifelse(is.na(Number)==TRUE, 0, Number),
#          year = "2020") %>%
#   select(sector, year, `ISO/IEC 27001`) %>%
#   na.omit()

country_2020_ISO9001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2020.xlsx", sheet = 2, skip = 1) %>%
  rename(country = Country) %>%
  mutate(`ISO 9001 certificate` = ifelse(is.na(certificates)==TRUE, 0, certificates),
         `ISO 9001 site` = ifelse(is.na(sites)==TRUE, 0, sites),
         year = "2020") %>%
  select(country, year, `ISO 9001 certificate`, `ISO 9001 site`) 

# sector_2020_ISO9001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2020.xlsx", sheet = 2, skip = 1) %>%
#   rename(sector = Sector) %>%
#   mutate(`ISO 9001` = ifelse(is.na(Number)==TRUE, 0, Number),
#          year = "2020") %>%
#   select(sector, year, `ISO 9001`) %>%
#   na.omit()

# sector_2020 <- left_join(sector_2020_ISO14001, sector_2020_ISOIEC27001) %>%
#   left_join(sector_2020_ISO9001) %>%
#   mutate(`ISO 14001` = as.character(`ISO 14001`),
#          `ISO/IEC 27001` = as.character(`ISO/IEC 27001`),
#          `ISO 9001` = as.character(`ISO 9001`)) %>%
#   mutate(year = "2020")

country_2020 <- left_join(country_2020_ISO14001, country_2020_ISOIEC27001) %>%
  left_join(country_2020_ISO9001) %>%
  mutate(`ISO 14001 certificate` = as.numeric(`ISO 14001 certificate`),
         `ISO 14001 site` = as.numeric(`ISO 14001 site`),
         `ISO/IEC 27001 site` = as.numeric(`ISO/IEC 27001 site`),
         `ISO/IEC 27001 certificate` = as.numeric(`ISO/IEC 27001 certificate`),
         `ISO 9001 certificate` = as.numeric(`ISO 9001 certificate`),
         `ISO 9001 site` = as.numeric(`ISO 9001 site`)) %>%
  mutate(year = "2020")

## 2019 ##

readxl::excel_sheets("./data/ISO survey/Data_per_country_2019.xlsx")

country_2019_ISO14001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2019.xlsx", sheet = 3, skip = 1) %>%
  rename(country = Country) %>%
  mutate(`ISO 14001 certificate` = ifelse(is.na(certificates)==TRUE, 0, certificates),
         `ISO 14001 site` = ifelse(is.na(sites)==TRUE, 0, sites),
         year = "2019") %>%
  select(country, year, `ISO 14001 certificate`, `ISO 14001 site`)

# sector_2019_ISO14001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2019.xlsx", sheet = 3, skip = 1) %>%
#   rename(sector = Sector) %>%
#   mutate(`ISO 14001` = ifelse(is.na(Number)==TRUE, 0, Number),
#          year = "2019") %>%
#   select(sector, year, `ISO 14001`) %>%
#   na.omit()

country_2019_ISOIEC27001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2019.xlsx", sheet = 4, skip = 1) %>%
  rename(country = Country) %>%
  mutate(`ISO/IEC 27001 certificate` = ifelse(is.na(certificates)==TRUE, 0, certificates),
         `ISO/IEC 27001 site` = ifelse(is.na(sites)==TRUE, 0, sites),
         year = "2019") %>%
  select(country, year, `ISO/IEC 27001 certificate`, `ISO/IEC 27001 site`) 

# sector_2019_ISOIEC27001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2019.xlsx", sheet = 4, skip = 1) %>%
#   rename(sector = Sector) %>%
#   mutate(`ISO/IEC 27001` = ifelse(is.na(Number)==TRUE, 0, Number),
#          year = "2019") %>%
#   select(sector, year, `ISO/IEC 27001`) %>%
#   na.omit()

country_2019_ISO9001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2019.xlsx", sheet = 2, skip = 1) %>%
  rename(country = Country) %>%
  mutate(`ISO 9001 certificate` = ifelse(is.na(certificates)==TRUE, 0, certificates),
         `ISO 9001 site` = ifelse(is.na(sites)==TRUE, 0, sites),
         year = "2019") %>%
  select(country, year, `ISO 9001 certificate`, `ISO 9001 site`) 

# sector_2019_ISO9001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2019.xlsx", sheet = 2, skip = 1) %>%
#   rename(sector = Sector) %>%
#   mutate(`ISO 9001` = ifelse(is.na(Number)==TRUE, 0, Number),
#          year = "2019") %>%
#   select(sector, year, `ISO 9001`) %>%
#   na.omit()

# sector_2019 <- left_join(sector_2020_ISO14001, sector_2019_ISOIEC27001) %>%
#      left_join(sector_2019_ISO9001) %>%
#      mutate(`ISO 14001` = as.character(`ISO 14001`),
#             `ISO/IEC 27001` = as.character(`ISO/IEC 27001`),
#             `ISO 9001` = as.character(`ISO 9001`)) %>%
#      mutate(year = "2019")

country_2019 <- left_join(country_2019_ISO14001, country_2019_ISOIEC27001) %>%
  left_join(country_2019_ISO9001) %>%
  mutate(`ISO 14001 certificate` = as.numeric(`ISO 14001 certificate`),
         `ISO 14001 site` = as.numeric(`ISO 14001 site`),
         `ISO/IEC 27001 site` = as.numeric(`ISO/IEC 27001 site`),
         `ISO/IEC 27001 certificate` = as.numeric(`ISO/IEC 27001 certificate`),
         `ISO 9001 certificate` = as.numeric(`ISO 9001 certificate`),
         `ISO 9001 site` = as.numeric(`ISO 9001 site`)) %>%
  mutate(year = "2019")


## 2018 ##

readxl::excel_sheets("./data/ISO survey/Data_per_country_2018.xlsx")

country_2018_ISO14001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2018.xlsx", sheet = 3, skip = 1) %>%
  rename(country = Country) %>%
  mutate(`ISO 14001 certificate` = ifelse(is.na(certificates)==TRUE, 0, certificates),
         `ISO 14001 site` = ifelse(is.na(sites)==TRUE, 0, sites),
         year = "2018") %>%
  select(country, year, `ISO 14001 certificate`, `ISO 14001 site`)

# sector_2018_ISO14001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2018.xlsx", sheet = 3, skip = 1) %>%
#   rename(sector = Sector) %>%
#   mutate(`ISO 14001` = ifelse(is.na(Number)==TRUE, 0, Number),
#          year = "2018") %>%
#   select(sector, year, `ISO 14001`) %>%
#   na.omit()

country_2018_ISOIEC27001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2018.xlsx", sheet = 4, skip = 1) %>%
  rename(country = Country) %>%
  mutate(`ISO/IEC 27001 certificate` = ifelse(is.na(certificates)==TRUE, 0, certificates),
         `ISO/IEC 27001 site` = ifelse(is.na(sites)==TRUE, 0, sites),
         year = "2018") %>%
  select(country, year, `ISO/IEC 27001 certificate`, `ISO/IEC 27001 site`) 

# sector_2018_ISOIEC27001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2018.xlsx", sheet = 4, skip = 1) %>%
#   rename(sector = Sector) %>%
#   mutate(`ISO/IEC 27001` = ifelse(is.na(Number)==TRUE, 0, Number),
#          year = "2018") %>%
#   select(sector, year, `ISO/IEC 27001`) %>%
#   na.omit()

country_2018_ISO9001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2018.xlsx", sheet = 2, skip = 1) %>%
  rename(country = Country) %>%
  mutate(`ISO 9001 certificate` = ifelse(is.na(certificates)==TRUE, 0, certificates),
         `ISO 9001 site` = ifelse(is.na(sites)==TRUE, 0, sites),
         year = "2018") %>%
  select(country, year, `ISO 9001 certificate`, `ISO 9001 site`) 

# sector_2018_ISO9001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_2018.xlsx", sheet = 2, skip = 1) %>%
#   rename(sector = Sector) %>%
#   mutate(`ISO 9001` = ifelse(is.na(Number)==TRUE, 0, Number),
#          year = "2018") %>%
#   select(sector, year, `ISO 9001`) %>%
#   na.omit()

# sector_2019 <- left_join(sector_2020_ISO14001, sector_2018_ISOIEC27001) %>%
#      left_join(sector_2019_ISO9001) %>%
#      mutate(`ISO 14001` = as.character(`ISO 14001`),
#             `ISO/IEC 27001` = as.character(`ISO/IEC 27001`),
#             `ISO 9001` = as.character(`ISO 9001`)) %>%
#      mutate(year = "2018")

country_2018 <- left_join(country_2018_ISO14001, country_2018_ISOIEC27001) %>%
  left_join(country_2018_ISO9001) %>%
  mutate(`ISO 14001 certificate` = as.numeric(`ISO 14001 certificate`),
         `ISO 14001 site` = as.numeric(`ISO 14001 site`),
         `ISO/IEC 27001 site` = as.numeric(`ISO/IEC 27001 site`),
         `ISO/IEC 27001 certificate` = as.numeric(`ISO/IEC 27001 certificate`),
         `ISO 9001 certificate` = as.numeric(`ISO 9001 certificate`),
         `ISO 9001 site` = as.numeric(`ISO 9001 site`)) %>%
  mutate(year = "2018")


## X-2017 ##

readxl::excel_sheets("./data/ISO survey/Data_per_country_timeseries.xlsx")

country_2006_2017_ISO_IEC_27001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_timeseries.xlsx", sheet = 1) %>%
  rename(country = Country) %>%
  gather(`2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`,
         key = "year", value = "ISO/IEC 27001 certificate") %>%
  mutate(`ISO/IEC 27001 certificate` = ifelse(is.na(`ISO/IEC 27001 certificate`)==TRUE, 0, `ISO/IEC 27001 certificate`))

country_2006_2017_ISO_14001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_timeseries.xlsx", sheet = 2) %>%
  rename(country = Country) %>%
  gather(`1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`,
         key = "year", value = `ISO 14001 certificate`) %>%
  mutate(`ISO 14001 certificate` = ifelse(is.na(`ISO 14001 certificate`)==TRUE, 0, `ISO 14001 certificate`))

country_2006_2017_ISO_9001 <- readxl::read_xlsx("./data/ISO survey/Data_per_country_timeseries.xlsx", sheet = 3) %>%
  rename(country = Year) %>%
  gather(`1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`,
         key = "year", value = `ISO 9001 certificate`) %>%
  mutate(`ISO 9001 certificate` = ifelse(is.na(`ISO 9001 certificate`)==TRUE, 0, `ISO 9001 certificate`))



## MERGE ##

africa <- country_certifications %>% filter(World == "Africa") %>% select(country, World) %>% unique() %>% 
  pull(country) %>% c("Cabo Verde", "Congo", "Congo (Democratic Republic of the)", "Gibraltar", "Guinea-Bissau", "Mauritania", "Sao Tome and Principe", "Côte d'Ivoire")
north_america <- country_certifications %>% filter(World == "North America") %>% select(country, World) %>% unique() %>% pull(country)
middle_east <- country_certifications %>% filter(World == "Middle East") %>% select(country, World) %>% unique() %>% 
  pull(country) %>% c("Iran (Islamic Republic of)", "Libya", "Palestine, State of")
east_asia_pacific <- country_certifications %>% filter(World == "East Asia and Pacific") %>% select(country, World) %>% unique() %>% 
  pull(country) %>% c("Hong Kong", "Korea (Democratic People's Republic of)", "Korea (Republic of)", "Macao", "Taiwan, Province of China")
central_south_asia <- country_certifications %>% filter(World == "Central and South Asia") %>% select(country, World) %>% unique() %>% 
  pull(country) %>% c("Micronesia (Federated States of)", "Palau", "Marshall Islands", "Samoa")
europe <- country_certifications %>% filter(World == "Europe") %>% select(country, World) %>% unique() %>% 
  pull(country) %>% c("Cayman Islands", "Macedonia (the former Yugoslav Republic of)", "Moldova (Republic of)", "San Marino", "United Kingdom of Great Britain and Northern Ireland")
central_south_america <- country_certifications %>% filter(World == "Central and South America") %>% select(country, World) %>% unique() %>% 
  pull(country) %>% c("Bolivia (Plurinational State of)", "Saint Vincent and the Grenadines", "Venezuela (Bolivarian Republic of)")

country_time <- left_join(country_2006_2017_ISO_9001, country_2006_2017_ISO_14001) %>%
  left_join(country_2006_2017_ISO_IEC_27001)

country_certifications <- bind_rows(country_2020, country_2019, country_2018) %>%
  select(country, year, `ISO 14001 certificate`, `ISO/IEC 27001 certificate`, `ISO 9001 certificate`) %>%
  bind_rows(country_time) %>%
  mutate(World = ifelse(year %in% c(2018, 2019, 2020) & country %in% c(africa), "Africa", 
                        ifelse(year %in% c(2018, 2019, 2020) & country %in% c(north_america), "North America",
                               ifelse(year %in% c(2018, 2019, 2020) & country %in% c(middle_east), "Middle East",
                                      ifelse(year %in% c(2018, 2019, 2020) & country %in% c(east_asia_pacific), "East Asia and Pacific",
                                             ifelse(year %in% c(2018, 2019, 2020) & country %in% c(central_south_asia), "Central and South Asia",
                                                    ifelse(year %in% c(2018, 2019, 2020) & country %in% c(europe), "Europe",
                                                           ifelse(year %in% c(2018, 2019, 2020) & country %in% c(central_south_america), "Central and South America",
                                                                  World))))))))

save(country_certifications, file = "country_certifications.rda")

## COUNTRY ANALYSIS

options(scipen=999)

ISO14001_plot <- country_certifications %>%
  group_by(year, World) %>%
  summarise(ISO14001 = sum(`ISO 14001 certificate`, na.rm = TRUE),
            ISO27001 = sum(`ISO/IEC 27001 certificate`, na.rm = TRUE),
            ISO9001 = sum(`ISO 9001 certificate`, na.rm = TRUE)) %>%
  gather(ISO14001, ISO27001, ISO9001,
         key = "ISOtype", value = "Certificates") %>%
  filter(ISOtype == "ISO14001") %>%
  filter(year %in% (1999:2020)) %>%
  ggplot(aes(year, Certificates, group = World, color = World)) +
  geom_line(size = 0.8) +
  ggtitle("ISO 14001") +
  theme(legend.position="none",
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "lightgrey"),
        panel.background = element_rect(fill = "white", color= "lightgrey")) +
  scale_color_brewer(palette="Dark2")

ISO27001_plot <- country_certifications %>%
  group_by(year, World) %>%
  summarise(ISO14001 = sum(`ISO 14001 certificate`, na.rm = TRUE),
            ISO27001 = sum(`ISO/IEC 27001 certificate`, na.rm = TRUE),
            ISO9001 = sum(`ISO 9001 certificate`, na.rm = TRUE)) %>%
  gather(ISO14001, ISO27001, ISO9001,
         key = "ISOtype", value = "Certificates") %>%
  filter(ISOtype == "ISO27001") %>%
  filter(year %in% (2006:2020)) %>%
  ggplot(aes(year, Certificates, group = World, color = World)) +
  geom_line(size = 0.8) +
  ggtitle("ISO/IEC 27001") +
  theme(legend.position="none",
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "lightgrey"),
        panel.background = element_rect(fill = "white", color= "lightgrey")) +
  scale_color_brewer(palette="Dark2")

ISO9001_plot <- country_certifications %>%
  group_by(year, World) %>%
  summarise(ISO14001 = sum(`ISO 14001 certificate`, na.rm = TRUE),
            ISO27001 = sum(`ISO/IEC 27001 certificate`, na.rm = TRUE),
            ISO9001 = sum(`ISO 9001 certificate`, na.rm = TRUE)) %>%
  gather(ISO14001, ISO27001, ISO9001,
         key = "ISOtype", value = "Certificates") %>%
  filter(ISOtype == "ISO9001") %>%
  ggplot(aes(year, Certificates, group = World, color = World)) +
  geom_line(size = 0.8) +
  ggtitle("ISO 9001") +
  theme(legend.position="bottom", legend.text = element_text(size=20),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "lightgrey"),
        panel.background = element_rect(fill = "white", color= "lightgrey")) +
  scale_color_brewer(palette="Dark2")

library(gridExtra)

grid.arrange(ISO14001_plot, ISO27001_plot, ISO9001_plot, 
             ncol=1)

