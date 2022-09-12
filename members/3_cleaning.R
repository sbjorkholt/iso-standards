
###############################################################################################################################
#################################                           CLEANING                             ##############################
###############################################################################################################################

#### 1. Order data and fix names so that they correspond over time ####

membership_wayback <- rbind(membership_v1, # Bind together data from all webpage versions
                            membership_v2,
                            membership_v3,
                            membership_v4,
                            membership_current)

membership_wayback <- membership_wayback %>%
  mutate(country = str_squish(country)) %>% # Remove any whitespace from the country variable
  # Change names of countries that have had encoding trouble, changed names or otherwise inconsistent coding
  mutate(country = case_when(country == "Bolivia, Plurinational State of" ~ "Bolivia",
                             country == "C\xf4te-d'Ivoire" ~ "Cote d'Ivoire",
                             country == "Côte-d'Ivoire" ~ "Cote d'Ivoire",
                             country == "Congo, The Democratic Republic of the" ~ "Congo",
                             country == "Congo, The Democratic Republic of" ~ "Congo",
                             country == "Congo, the Republic of the" ~ "Congo",
                             country == "Korea, Democratic People's Republic" ~ "North Korea",
                             country == "Korea, Democratic People's Republic of" ~ "North Korea",
                             country == "Korea, Republic of" ~ "South Korea",
                             country == "Lao People's Democratic Republic" ~ "Lao",
                             country == "Libyan Arab Jamahiriya" ~ "Libya",
                             country == "Macao Special Administrative Region of China" ~ "Macao",
                             country == "Moldova, Republic of" ~ "Moldova",
                             country == "Russian Federation (GOST R)" ~ "Russain Federation",
                             country == "Singapore (SPRING SG)" ~ "Singapore",
                             country == "Greece (NQIS ELOT)" ~ "Greece",
                             country == "Syrian Arab Republic" ~ "Syria",
                             country == "Tanzania, United Republic of" ~ "Tanzania",
                             country == "The former Yugoslav Republic of Macedonia" ~ "North Macedonia",
                             country == "The Former Yugoslav Republic of Macedonia" ~ "North Macedonia",
                             country == "Palestine, State of" ~ "Palestine",
                             country == "Hong Kong, China" ~ "Hong Kong",
                             country == "USA" ~ "United States",
                             TRUE ~ country)) %>%
  mutate(country = ifelse(str_detect(country, "Ivoire"), "Cote d'Ivoire", country)) %>%
  # Adding names for standards bodies that had fallen out due to unstandard webpages
  mutate(country = ifelse(acronym == "ISIRI", "Iran",
                          ifelse(acronym == "DGNTI", "Panama",
                                 ifelse(acronym == "SSB", "Suriname",
                                        ifelse(acronym == "BBS", "Belize", # This had been coded wrongly for 2015
                                               country))))) %>%
  # This is done to avoid duplicates per year
  # Got way too extensive as I dug deeper into the duplicates ... do not try ifelse for this at home
  mutate(acronym = ifelse(acronym == "ACONOR", "OCC", # Congo had two different names for their standards body, choosing the most used name
                          ifelse(acronym == "CONACYT", "OSN", # El Salvador, choosing the name listed on ISO's webpage
                                 ifelse(acronym == "GSB", "GSA", # Ghana, choosing the name listed on ISO's webpage
                                        ifelse(acronym == "FASONORM", "ABNORM", # Burkina Faso, had two names for their standard body, just choosing one
                                               ifelse(acronym == "DZNM", "HZN", # Croatia, choosing the name listed on ISO's webpage
                                                      ifelse(acronym == "DIGENOR", "INDOCAL", # Dominican Republic, one version of the webpage had a different language
                                                             ifelse(acronym == "DNTMS", "FTSQCO", # Fiji, choosing the official name of their standards body
                                                                    ifelse(acronym == "INS", "INSM", # Moldova, seems like a character was missing for some webpages
                                                                           ifelse(acronym == "DRI", "MSTRD", # Myanmar, choosing the official name
                                                                                  ifelse(acronym == "INACAL", "INDECOPI", # Peru, one Spanish and one English name, choosing the English one
                                                                                         ifelse(acronym %in% c("DTR", "DSTU"), "DSSU", # Ukraine, seems like the standards body changed name and it has lingered
                                                                                                ifelse(acronym == "DISM", "DOSM", # Lao, spelling mistake
                                                                                                       ifelse(acronym == "INORPI", "INNORPI", # Tunisia, spelling mistake
                                                                                                              ifelse(acronym == "GOST", "GOST R", # Russia
                                                                                                                     ifelse(acronym %in% c("CNI", "CSNI"), "UNMZ", # Czech Republic
                                                                                                                            ifelse(acronym == "MNCSM", "MASM", # Mongolia
                                                                                                                                   ifelse(acronym == "SA" & country == "Australia", "SAI", # Australia
                                                                                                                                          ifelse(acronym %in% c("ZSM", "ISRM"), "ISRSM", # North Macedonia
                                                                                                                                                 ifelse(acronym == "ON" & country == "Austria", "ASI", # Austria
                                                                                                                                                        ifelse(acronym == "IBN", "NBN", # Belgium
                                                                                                                                                               ifelse(acronym %in% c("CPRU", "ABCI"), "NSC", # Brunei Darussalam
                                                                                                                                                                      ifelse(acronym == "CDNQ", "ANOR", # Cameroon
                                                                                                                                                                             ifelse(acronym %in% c("QSAE", "ESA"), "IES", # Ethiopia
                                                                                                                                                                                    ifelse(acronym == "SEE", "ILNAS", # Luxembourg,
                                                                                                                                                                                           ifelse(acronym == "JBS", "BSJ", # Jamaica
                                                                                                                                                                                                  ifelse(acronym == "JISM", "JSMO", # Jordan
                                                                                                                                                                                                         ifelse(acronym == "MSA", "MCCAA", # Malta
                                                                                                                                                                                                                ifelse(acronym == "SNIMA", "IMANOR", # Morocco
                                                                                                                                                                                                                       ifelse(acronym %in% c("MOLDST", "INSM"), "ISM", # Moldova
                                                                                                                                                                                                                              ifelse(acronym %in% c("SPRING SG", "SPRING"), "SSC", # Singapore
                                                                                                                                                                                                                                     ifelse(acronym == "TCVN", "STAMEQ", # Viet Nam
                                                                                                                                                                                                                                            ifelse(acronym %in% c("CEBENOR", "ABENOR"), "ANM", # Benin
                                                                                                                                                                                                                                                   ifelse(acronym == "SQCA", "BSB", # Bhutan
                                                                                                                                                                                                                                                          ifelse(acronym == "GAMSFP", "TGSB", # Gambia
                                                                                                                                                                                                                                                                 ifelse(acronym == "AENOR", "UNE", # Spain
                                                                                                                                                                                                                                                                        ifelse(acronym == "ELOT", "NQIS ELOT", # Greece
                                                                                                                                                                                                                                                                               ifelse(acronym == "MLIDNI", "AMANORM", # Mali
                                                                                                                                                                                                                                                                                      ifelse(acronym == "ANTT", "AGANOR", # Gabon
                                                                                                                                                                                                                                                                                             ifelse(acronym == "ISIRI", "INSO", # Iran
                                                                                                                                                                                                                                                                                                    ifelse(acronym == "SNZ", "NZSO", # New Zealand
                                                                                                                                                                                                                                                                                                           ifelse(acronym %in% c("SOSMT", "SUTN"), "UNMS SR", # Slovakia
                                                                                                                                                                                                                                                                                                                  ifelse(acronym == "RBS", "RSB", # Rwanda
                                                                                                                                                                                                                                                                                                                         ifelse(acronym == "COPANIT", "DGNTI", # Panama
                                                                                                                                                                                                                                                                                                                                ifelse(acronym == "CSN", "ATN", # Togo
                                                                                                                                                                                                                                                                                                                                       ifelse(acronym %in% c("DNPQM", "SON"), "ANMC", # Niger
                                                                                                                                                                                                                                                                                                                                              ifelse(acronym == "TJKSTN", "TAJIKSTANDARD", # Tajikistan
                                                                                                                                                                                                                                                                                                                                                     ifelse(acronym %in% c("BASMP", "BAS"), "ISBIH", # Bosnia and Hezegovina
                                                                                                                                                                                                                                                                                                                                                            acronym)))))))))))))))))))))))))))))))))))))))))))))))) %>%
  mutate(committee = str_remove(committee, "ISO/"), # Some cleaning in the committee variable
         committee = str_remove(committee, "IEC"),
         committee = str_remove(committee, "Stand by"),
         committee = str_squish(committee)) %>%
  mutate(committee = ifelse(committee == "/JTC 2", "JTC 2",
                            ifelse(committee == "/TC 113", "TC 113",
                                   ifelse(committee == "/TC 47", "TC 47",
                                          ifelse(committee == "/TC 86", "TC 86",
                                                 ifelse(committee == "/TC 93", "TC 93",
                                                        committee)))))) %>%
  mutate(standby = ifelse(str_detect(title, "STAND BY"), 1, 0)) %>% # Making a separate variable on whether the committee is stand-by
  mutate(title = str_remove(title, " - STAND BY")) %>%
  mutate(title = ifelse(committee == "TC 93", "Starch (including derivatives and by-products)", 
                        ifelse(committee == "TC 47", "Chemistry",
                               ifelse(committee == "TC 86", "Refrigeration and air-conditioning",
                                      title)))) %>%
  mutate(country = ifelse(webid == "1619", "Canada",
                          ifelse(webid == "1815", "Ireland",
                                 ifelse(webid == "1835", "Japan",
                                        ifelse(webid == "2036", "Peru",
                                               ifelse(webid == "1533", "Bahrain",
                                                      ifelse(webid == "1725", "Ethiopia", country))))))) %>%
  mutate(acronym = ifelse(webid == "1619", "SCC",
                          ifelse(webid == "1815", "NSAI",
                                 ifelse(webid == "1835", "JISC",
                                        ifelse(webid == "2036", "INACAL",
                                               ifelse(webid == "1533", "BTMD",
                                                      ifelse(webid == "1725", "IES", acronym)))))))

membership_per_year <- membership_wayback %>% # Make a dataset with country-year units
  mutate(year = str_extract(date, "[0-9]{4}")) %>% # Extract the years
  select(-c(date, webid)) %>% # Remove variables used in scraping
  unique() # Find the unique rows


#### 2. Add missing for the webpages that were not available ####

# Make a dataframe with all the potential years, memberships and country combinations there are
years <- as.character(c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))
memberships <- membership_wayback %>% select(membership) %>% unique() %>% pull(membership)
countries <- membership_wayback %>% select(country, acronym) %>% unique() %>% na.omit() %>% pull(country)
acronyms <- membership_wayback %>% select(country, acronym) %>% unique() %>% na.omit() %>% pull(acronym)

countries_full <- tibble(country = rep(countries, each = length(years)),
                         acronym = rep(acronyms, each = length(years)),
                         year = rep(years, length(countries))) %>% # First make a dataset and repeat the years and countries/acronyms according to each others' length
  mutate("P-member" = "", # Making a few membership variables
         "O-member" = "", # This is done to not get a 3 by 3 rep() situation above
         "Secretariat" = "",
         "Twinned secretariat" = "") %>%
  gather(4:7, key = "membership", value = "value") %>% # And gathering them to get everything into a long dataframe
  select(-value) # Remove this extra variable

membership_per_year_na <- membership_per_year %>%
  right_join(countries_full, by = c("country", "year", "acronym", "membership")) # Right join so that all rows without data originally are now given NA
# OBS: NAs might both be because the given country was not in a technical committee at that time, and because Wayback did not save that webpage


#### 3. Adding imputations ####
# Data contains lots of missing for certain years due to the changed structure of ISO's webpage
# Assuming that the countries do not change committees much from year to year, I reproduce their closest past committee information to missing years
# Also adding a variable "imputed"
# This is only done forwards (for example reproducing from 2016 to 2017), and not backwards 
# (assuming that a country being in a committee in 2016 means it is also in that committee in 2002 is simply unrealistic)
# the rule is thus: If data for committees is missing for 2004-2021, repeat this committee information from previous closest year containing data

membership_per_year_na_new <- membership_per_year_na %>%
  mutate(year = as.numeric(year)) %>%
  # Creating a variable to sort out rows for 2001, 2002 and 2003. There is too little data on these years to warrant imputation.
  # Also filtering out year 2022 in this variable. Data from 2022 is certainly correct, so if there is missing here, it's because the country was not in a committee at that time
  mutate(var = ifelse(is.na(committee) & year < 2004 | is.na(committee) & year == 2022, "ok1",
                      # Also making a variable for the available years, just in case
                      ifelse(!is.na(committee) & year < 2004 | !is.na(committee) & year != 2022, "ok2", 
                             "missing"))) %>% # Adding a missing variable
  filter(var != "ok1") %>%
  select(-var)

countries <- membership_per_year_na_new %>% select(country) %>% pull() %>% unique() # To not make a mess, loop over each year separately

# And do it one time for each membership type (this could have been a loop within a loop, but to avoid unnecessary confusion, I do it a bit less elegantly)
pmembers <- list()

for (i in 1:length(countries)) {
  
  tmp <- membership_per_year_na_new %>%
    mutate(year = as.numeric(year)) %>% # Make the year variable numeric to that it can be ordered
    filter(country == countries[i]) %>% # Filter out each country
    filter(membership == "P-member") %>% # Filter out P-membership
    mutate(impute = ifelse(is.na(title) & is.na(committee), 1, 0)) %>% # Make a variable that takes "impute" if there are missing values
    group_by(year, impute) %>% # Group by variables and....
    nest() %>% # ... make nested columns with info on committee membership in given years
    arrange(-desc(year)) %>% # Order the years
    ungroup() %>% # Ungroup to not create clutter later 
    mutate(change = ifelse(lead(impute) == 1 & impute == 0, 1, 0)) # A variable telling us which is the closest year with non-missing values
  
  pmembers[[i]] <- tmp %>% 
    mutate(data2 = ifelse(impute == 1, # If impute is 1
                          tmp %>% filter(impute == 0 & change == 1) %>% select(data) %>% pull(), # Then fill the row with info from the closest observation with non-missing values
                          data)) %>% # Otherwise, just reproduce the nested cell
    select(-change) # Remove the change variable
  
}

omembers <- list() # Repeat procedure for O-members

for (i in 1:length(countries)) {
  
  tmp <- membership_per_year_na_new %>%
    mutate(year = as.numeric(year)) %>%
    filter(country == countries[i]) %>%
    filter(membership == "O-member") %>%
    mutate(impute = ifelse(is.na(title) & is.na(committee), 1, 0)) %>%
    group_by(year, impute) %>%
    nest() %>%
    arrange(-desc(year)) %>%
    ungroup() %>%
    mutate(change = ifelse(lead(impute) == 1 & impute == 0, 1, 0))
  
  omembers[[i]] <- tmp %>%
    mutate(data2 = ifelse(impute == 1, 
                          tmp %>% filter(impute == 0 & change == 1) %>% select(data) %>% pull(),
                          data)) %>%
    select(-change)
  
}

secretariat <- list() # Repeat procedure for secretariat

for (i in 1:length(countries)) {
  
  tmp <- membership_per_year_na_new %>%
    mutate(year = as.numeric(year)) %>%
    filter(country == countries[i]) %>%
    filter(membership == "Secretariat") %>%
    mutate(impute = ifelse(is.na(title) & is.na(committee), 1, 0)) %>%
    group_by(year, impute) %>%
    nest() %>%
    arrange(-desc(year)) %>%
    ungroup() %>%
    mutate(change = ifelse(lead(impute) == 1 & impute == 0, 1, 0))
  
  secretariat[[i]] <- tmp %>%
    mutate(data2 = ifelse(impute == 1, 
                          tmp %>% filter(impute == 0 & change == 1) %>% select(data) %>% pull(),
                          data)) %>%
    select(-change)
  
}

# Make dataframes of the lists and bind them together to one dataframe
membership_per_year_na_impute <- bind_rows(do.call(rbind, pmembers), 
                                           do.call(rbind, omembers), 
                                           do.call(rbind, secretariat)) %>%
  select(-data) %>% # Remove old nested variable
  unnest(cols = c(data2)) # Unnest the new imputed variable

twinned_secretariat <- membership_per_year_na_new %>%
  filter(membership == "Twinned secretariat") %>%
  na.omit() 

membership_per_year_na_impute <- bind_rows(membership_per_year_na_impute, twinned_secretariat)
  
memberships <- membership_per_year_na_impute %>%  
  rename(sdo = acronym) %>%
  select(country, sdo, year, title, committee, membership, standby, impute)

saveRDS(memberships, file = "../../data/final_data/memberships.rds")


### 4. Checking data on plots ###

countries <- membership_per_year_na %>% select(country) %>% pull() %>% unique()

for (i in 1:length(countries)) {
  
  plot <- membership_per_year_na %>%
    filter(country == countries[i]) %>%
    na.omit() %>%
    group_by(year, membership) %>%
    count() %>%
    ggplot(aes(year, n, color = membership)) + 
    geom_point() 

    ggsave(plot, filename = str_c("../../data/archive/eyeballs/", countries[i], ".png"))
  
}

