
###############################################################################################################################
#################################                           CLEANING                             ##############################
###############################################################################################################################

participant_v1 <- read_rds("../raw_data/archive_members/participants_v1.rds")
participant_v2 <- read_rds("../raw_data/archive_members/participants_v2.rds")
participant_v3 <- read_rds("../raw_data/archive_members/participants_v3.rds")
participant_v4 <- read_rds("../raw_data/archive_members/participants_v4.rds")
participant_current_2022 <- read_rds("../raw_data/archive_members/participants_current_2022.rds")
participant_current_2023 <- read_rds("../raw_data/archive_members/participants_current_2023.rds")

#### 1. Order data and fix names so that they correspond over time ####

participant_wayback <- rbind(participant_v1, # Bind together data from all webpage versions
                            participant_v2,
                            participant_v3,
                            participant_v4,
                            participant_current_2022,
                            participant_current_2023)

participant_wayback <- participant_wayback %>%
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
                             country == "Russian Federation (GOST R)" ~ "Russian Federation",
                             country == "Russain Federation" ~ "Russian Federation",
                             country == "Singapore (SPRING SG)" ~ "Singapore",
                             country == "Greece (NQIS ELOT)" ~ "Greece",
                             country == "Syrian Arab Republic" ~ "Syria",
                             country == "Tanzania, United Republic of" ~ "Tanzania",
                             country == "The former Yugoslav Republic of Macedonia" ~ "North Macedonia",
                             country == "The Former Yugoslav Republic of Macedonia" ~ "North Macedonia",
                             country == "Palestine, State of" ~ "Palestine",
                             country == "Hong Kong, China" ~ "Hong Kong",
                             country == "USA" ~ "United States",
                             country == "North Macedonia" ~ "Macedonia",
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
  mutate(country = ifelse(country == "Azerbaijan�",  "Azerbaijan", 
                          ifelse(country == "Russian Federation", "Russia", country))) %>%
  mutate(acronym = ifelse(webid == "1619", "SCC",
                          ifelse(webid == "1815", "NSAI",
                                 ifelse(webid == "1835", "JISC",
                                        ifelse(webid == "2036", "INACAL",
                                               ifelse(webid == "1533", "BTMD",
                                                      ifelse(webid == "1725", "IES", acronym)))))))

participant_per_year <- participant_wayback %>% # Make a dataset with country-year units
  mutate(year = str_extract(date, "[0-9]{4}")) %>% # Extract the years
  select(-c(date, webid)) %>% # Remove variables used in scraping
  unique() %>% # Find the unique rows
  select(-standby) %>%
  mutate(year = as.numeric(year)) %>%
  drop_na(country)

#### 2. Merge with TC data ####

tc_v1 <- read_rds("../raw_data/archive_members/v1_tcs.rds")
tc_v2 <- read_rds("../raw_data/archive_members/v2_tcs.rds")
tc_v3 <- read_rds("../raw_data/archive_members/v3_tcs.rds")
tc_v4 <- read_rds("../raw_data/archive_members/v4_tcs.rds")

tc_per_year <- bind_rows(tc_v1, tc_v2, tc_v3, tc_v4) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(cyc = str_c(country, "_", year, "_", committee)) %>%
  mutate(committee = str_remove(committee, "ISO/"),
         committee = str_remove(committee, "IEC"),
         committee = str_squish(committee)) %>%
  filter(!committee %in% c("CASCO", "COPOLCO", "COUNCIL", "DEVCO")) %>%
  mutate(country = case_when(
    country == "Bolivia, Plurinational State of" ~ "Bolivia",
    country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
    country == "Côte-d'Ivoire" ~ "Cote d'Ivoire",
    country == "Congo, The Democratic Republic of the" ~ "Congo",
    country == "Congo, the Republic of the" ~ "Congo",
    country == "Hong Kong Special Administrative Region of China" ~ "Hong Kong",
    country == "Hong Kong, China" ~ "Hong Kong",
    country == "Iran, Islamic Republic of" ~ "Iran",
    country == "Korea, Democratic People's Republic" ~ "North Korea",
    country == "Korea, Democratic People's Republic of" ~ "North Korea",
    country == "Korea, Republic of" ~ "South Korea",
    country == "Korea,Republicof" ~ "South Korea",
    country == "Lao People's Democratic Republic" ~ "Lao",
    country == "Libyan Arab Jamahiriya" ~ "Libya",
    country == "Macao Special Administrative Region of China" ~ "Macao",
    country == "Moldova, Republic of" ~ "Moldova",
    country == "Palestine, State of" ~ "Palestine",
    country == "Russian Federation" ~ "Russia",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "Tanzania, United Republic of" ~ "Tanzania",
    country == "The Former Yugoslav Republic of Macedonia" ~ "North Macedonia",
    country == "The former Yugoslav Republic of Macedonia" ~ "North Macedonia",
    country == "Türkiye" ~ "Turkey",
    country == "UnitedKingdom" ~ "United Kingdom",
    country == "UnitedStates" ~ "United States",
    country == "USA" ~ "United States",
    TRUE ~ country
  )) %>%
  filter(!country %in% c("Standards",
                         "BenefitsCertificationManagement system standardsISO 26000 - Social responsibilityISO 31000 - Risk managementISO 4217 - Currency codesISO 8601 - Time and date formatISO 639 - Language codesISO 3166 - Country codesISO 37001 - Anti-bribery management systems"))

participant_per_year <- participant_per_year %>%
  mutate(cyc = str_c(country, "_", year, "_", committee))

tc_per_year <- tc_per_year %>%
  filter(!cyc %in% participant_per_year$cyc)

participant_per_year <- participant_per_year %>%
  bind_rows(tc_per_year %>%
              mutate(title = NA)) %>% 
  distinct(country, membership, year, committee, .keep_all = TRUE) %>%
  drop_na(committee) %>%
  group_by(committee) %>%
  fill(committee, title) %>%
  ungroup() %>%
  select(-cyc) %>%
  drop_na(country)

#### 3. Imputation for the webpages that were not available ####

### First: When a country is both P-member and has the Secretariat, then they should be coded Secretariat
### This includes years when countries were coded P-members even though they held the Secretariat

membertest <- participant_per_year %>% # Make a dataset with country-year units %>%
  mutate(cyc = str_c(country, "_", year, "_", committee)) %>% # Make unit to check match
  filter(membership %in% c("P-member", "O-member"))

sectest <- participant_per_year %>% # Make a dataset with country-year units
  mutate(cyc = str_c(country, "_", year, "_", committee)) %>% # Make unit to check match
  filter(membership %in% c("Secretariat", "Twinned secretariat"))

membertest <- membertest %>%
  mutate(double = ifelse(cyc %in% sectest$cyc, 1, 0)) %>% # If the unit occurs in Secretariat as well
  filter(double != 1) %>% # Remove it
  select(-double)

participant_per_year <- bind_rows(membertest, sectest)

# An error has happened causing multiple countries that are Secretariat member to be coded P-members
# Here, if a country has the Secretariat in previous and coming years, then the mid-years should also be coded with Secretariat

participant_per_year <- participant_per_year %>%
  group_by(country, committee) %>%
  arrange(desc(year)) %>%
  mutate(lead_year_membership = lead(membership, 1)) %>%
  mutate(lag_year_membership = lag(membership, 1)) %>%
  mutate(membership = ifelse(year == 2020 & membership == "P-member" & lead_year_membership == "Secretariat", "Secretariat", membership)) %>%
  mutate(membership = ifelse(year == 2021 & membership == "P-member" & lag_year_membership == "Secretariat", "Secretariat", membership)) %>%
  select(-lead_year_membership, -lag_year_membership, -cyc) %>%
  ungroup()

### Second: Run through each membership type and fill in missing values

participant_per_year_df <- participant_per_year %>%
  mutate(impute = 0) %>%
  mutate(year = as.numeric(year)) %>%
  ungroup()

generate_fill <- function(country, start, end, committee) {
  tibble(country = country, 
         year = seq(start, end, by = 1), 
         committee = committee)
}

countries <- participant_per_year_df %>% select(country) %>% filter(country != "Serbia and Montenegro") %>% # Serbia and Montenegro separated, and should thus not be imputed
  pull() %>% unique() # To not make a mess, loop over each country separately

tcs <- participant_per_year_df %>% select(committee) %>% na.omit() %>% pull() %>% unique() # And each TC separately

rm(participant_v1, participant_v2, participant_v3, participant_v4,
   participant_current_2022, participant_current_2023,
   tc_v1, tc_v2, tc_v3, tc_v4)

#### Participants ####

df1 <- list()

for (i in 1:length(countries)){

  df2 <- list()

  for (j in 1:length(tcs)) {

    tmp <- participant_per_year_df %>%
      filter(country == countries[i],
             committee == tcs[j])

    if(nrow(tmp) > 0){

      tmp <- tmp %>%
        arrange(desc(year))
      
      st_year <- tmp %>% mutate(st_year = min(year)) %>% select(st_year) %>% unique() %>% pull()
      end_year <- tmp %>% mutate(end_year = max(year)) %>% select(end_year) %>% unique() %>% pull()

      df2[[j]] <- tmp %>%
        complete(year = st_year:end_year) %>%
        mutate(impute = ifelse(is.na(impute), 1, impute)) %>%
        fill(committee, title, acronym, country, .direction = "down") %>%
        fill(membership, .direction = "downup") %>% # Meet in the middle
        unique()
      }

  }

  df1[[i]] <- df2

  message(str_c("Finished country ", countries[i],"."))

}

participation <- bind_rows(df1) %>%
  distinct(country, year, committee, title, membership, impute, .keep_all = TRUE)

#### 4. Removing duplicates ####

### P-members and O-members

P_members <- participation %>%
  filter(membership == "P-member")

O_members <- participation %>%
  filter(membership == "O-member")

Ptest <- P_members %>%
  mutate(cyc = str_c(country, "_", year, "_", committee)) # Make unit to check match

Otest <- O_members %>%
  mutate(cyc = str_c(country, "_", year, "_", committee)) # Make unit to check match

Ptest2 <- Ptest %>%
  mutate(double = ifelse(cyc %in% Otest$cyc, 1, 0)) %>% # If the unit occurs in the O-membership as well
  arrange(desc(double)) %>%
  mutate(skip = ifelse(double == 1 & impute == 1, 1, 0)) %>% # Then make a variable that takes 1 if the unit was imputed
  filter(skip != 1) %>% # Remove these units that were duplicated and imputed
  select(-skip, -double)

Otest2 <- Otest %>%
  mutate(double = ifelse(cyc %in% Ptest$cyc, 1, 0)) %>% # If the unit occurs in the P-membership as well
  arrange(desc(double)) %>%
  mutate(skip = ifelse(double == 1 & impute == 1, 1, 0)) %>% # Then make a variable that takes 1 if the unit was imputed
  filter(skip != 1) %>% # Remove these units that were duplicated and imputed
  select(-skip, -double)

# Some duplicates still remain, possibly because they change membership type during the year, and two webpages from that same year are used.
# I take the future membership and impute it where there are duplicates.
# Since this is not possible for the latest year, 2023, I impute these manually by checking the current webpage.

Ptest2 <- Ptest2  %>% 
  mutate(membership = case_when(
    country == "Argentina" & committee == "TC 34/SC 19" ~ "O-member",
    country == "Argentina" & committee == "TC 34/SC 9" ~ "O-member",
    country == "Cameroon" & committee == "TC 34" ~ "O-member",
    country == "China" & committee == "TC 38/SC 20" ~ "P-member",
    country == "China" & committee == "TC 182" ~ "P-member",
    country == "Costa Rica" & committee == "TC 10" ~ "P-member",
    country == "Costa Rica" & committee == "TC 10/SC 6" ~ "P-member",
    country == "Costa Rica" & committee == "TC 229" ~ "P-member",
    country == "Croatia" & committee == "TC 147/SC 4" ~ "P-member",
    country == "Croatia" & committee == "TC 43/SC 2" ~ "O-member",
    country == "Mexico" & committee == "TC 61/SC 4" ~ "P-member",
    country == "Morocco" & committee == "TC 34/SC 17" ~ "P-member",
    country == "Spain" & committee == "TC 147/SC 6" ~ "P-member",
    country == "Spain" & committee == "TC 298" ~ "P-member",
    country == "Spain" & committee == "TC 34/SC 12" ~ "O-member",
    TRUE ~ membership
  )) %>%
  filter(membership == "P-member")

Otest2 <- Otest2 %>%
  mutate(membership = case_when(
    country == "Argentina" & committee == "TC 34/SC 19" ~ "O-member",
    country == "Argentina" & committee == "TC 34/SC 9" ~ "O-member",
    country == "Cameroon" & committee == "TC 34" ~ "O-member",
    country == "China" & committee == "TC 38/SC 20" ~ "P-member",
    country == "China" & committee == "TC 182" ~ "P-member",
    country == "Costa Rica" & committee == "TC 10" ~ "P-member",
    country == "Costa Rica" & committee == "TC 10/SC 6" ~ "P-member",
    country == "Costa Rica" & committee == "TC 229" ~ "P-member",
    country == "Croatia" & committee == "TC 147/SC 4" ~ "P-member",
    country == "Croatia" & committee == "TC 43/SC 2" ~ "O-member",
    country == "Mexico" & committee == "TC 61/SC 4" ~ "P-member",
    country == "Morocco" & committee == "TC 34/SC 17" ~ "P-member",
    country == "Spain" & committee == "TC 147/SC 6" ~ "P-member",
    country == "Spain" & committee == "TC 298" ~ "P-member",
    country == "Spain" & committee == "TC 34/SC 12" ~ "O-member",
    TRUE ~ membership
  )) %>%
  filter(membership == "O-member")

Ptest3 <- Ptest2 %>%
  mutate(double = ifelse(cyc %in% Otest2$cyc, 1, 0)) %>% # Then checking duplicates
  bind_rows(Otest2 %>% # Binding with the O-membership data 
              mutate(double = ifelse(cyc %in% Ptest2$cyc, 1, 0)) %>% # That is not duplicated
                       filter(double != 1)) %>%
  group_by(committee, country) %>%
  arrange(desc(year)) %>%
  mutate(future_membership = lag(membership)) %>% # Find the future membership of given country in given committee
  ungroup() %>%
  mutate(membership = ifelse(double == 1 & !is.na(future_membership), future_membership, membership)) %>% # If there is a duplicate and the future membership is not NA, then impute
  filter(membership == "P-member") # We're still segregating by membership type, so filter Ps out.

Otest3 <- Otest2 %>% # Same process for O-memberships
  mutate(double = ifelse(cyc %in% Ptest2$cyc, 1, 0)) %>%
  bind_rows(Ptest2 %>% 
              mutate(double = ifelse(cyc %in% Otest2$cyc, 1, 0)) %>% 
              filter(double != 1)) %>%
  group_by(committee, country) %>%
  arrange(desc(year)) %>%
  mutate(future_membership = lag(membership)) %>%
  ungroup() %>%
  mutate(membership = ifelse(double == 1 & !is.na(future_membership), future_membership, membership)) %>%
  filter(membership == "O-member")

# Do it twice to sort of two-time errors
Ptest4 <- Ptest3 %>%
  mutate(double = ifelse(cyc %in% Otest3$cyc, 1, 0)) %>% # Then checking duplicates
  bind_rows(Otest3 %>% # Binding with the O-membership data 
              mutate(double = ifelse(cyc %in% Ptest3$cyc, 1, 0)) %>% # That is not duplicated
              filter(double != 1)) %>%
  group_by(committee, country) %>%
  arrange(desc(year)) %>%
  mutate(future_membership = lag(membership)) %>% # Find the future membership of given country in given committee
  ungroup() %>%
  mutate(membership = ifelse(double == 1 & !is.na(future_membership), future_membership, membership)) %>% # If there is a duplicate and the future membership is not NA, then impute
  filter(membership == "P-member") # We're still segregating by membership type, so filter Ps out.

Otest4 <- Otest3 %>% # Same process for O-memberships
  mutate(double = ifelse(cyc %in% Ptest3$cyc, 1, 0)) %>%
  bind_rows(Ptest3 %>% 
              mutate(double = ifelse(cyc %in% Otest3$cyc, 1, 0)) %>% 
              filter(double != 1)) %>%
  group_by(committee, country) %>%
  arrange(desc(year)) %>%
  mutate(future_membership = lag(membership)) %>%
  ungroup() %>%
  mutate(membership = ifelse(double == 1 & !is.na(future_membership), future_membership, membership)) %>%
  filter(membership == "O-member")

# For those whose future membership cannot be imputed, choose last years membership
Ptest5 <- Ptest4 %>%
  mutate(double = ifelse(cyc %in% Otest4$cyc, 1, 0)) %>% 
  bind_rows(Otest4 %>% 
              mutate(double = ifelse(cyc %in% Ptest4$cyc, 1, 0)) %>%
              filter(double != 1)) %>%
  group_by(committee, country) %>%
  arrange(desc(year)) %>%
  mutate(past_membership = lead(membership)) %>% 
  ungroup() %>%
  mutate(membership = ifelse(double == 1 & !is.na(past_membership), past_membership, membership)) %>% 
  filter(membership == "P-member") 

Otest5 <- Otest4 %>% 
  mutate(double = ifelse(cyc %in% Ptest4$cyc, 1, 0)) %>%
  bind_rows(Ptest4 %>% 
              mutate(double = ifelse(cyc %in% Otest4$cyc, 1, 0)) %>% 
              filter(double != 1)) %>%
  group_by(committee, country) %>%
  arrange(desc(year)) %>%
  mutate(past_membership = lag(membership)) %>%
  ungroup() %>%
  mutate(membership = ifelse(double == 1 & !is.na(past_membership), past_membership, membership)) %>%
  filter(membership == "O-member")

# For the remaining 14 observations, assume O-membership as this is what it seems most common
Ptest5 <- Ptest5 %>%
  mutate(double = ifelse(cyc %in% Otest5$cyc, 1, 0)) %>%
  filter(double != 1)

membership <- bind_rows(Ptest5, Otest5) %>%
  select(-cyc, -double, -future_membership, -past_membership)

## Check
# Ptest5 %>%
#   filter(cyc %in% Otest5$cyc) # No duplicates
# 
# Otest5 %>%
#   filter(cyc %in% Ptest5$cyc) # No duplicates
# 
# Ptest5 %>%
#   group_by(country, year, committee) %>%
#   count(sort = TRUE) # No duplicates
# 
# Otest5 %>%
#   group_by(country, year, committee) %>%
#   count(sort = TRUE) # No duplicates

### Secretariat

Secretariat <- participation %>%
  filter(membership == "Secretariat")

Secretariat %>%
  group_by(country, year, committee) %>%
  count(sort = TRUE) # No duplicated secretariats

memtest <- participation %>%
  filter(membership %in% c("O-member", "P-member")) %>%
  mutate(cyc = str_c(country, "_", year, "_", committee)) # Make unit to check match

sectest <- Secretariat %>%
  mutate(cyc = str_c(country, "_", year, "_", committee)) # Make unit to check match

sectest2 <- sectest %>%
  mutate(double = ifelse(cyc %in% memtest$cyc, 1, 0)) %>% # If the unit occurs in membership as well
  arrange(desc(double)) %>%
  mutate(skip = ifelse(double == 1 & impute == 1, 1, 0)) %>% # Then make a variable that takes 1 if the unit was imputed
  filter(skip != 1) %>% # Remove these units that were duplicated and imputed
  select(-skip, -double)

memtest2 <- memtest %>%
  mutate(double = ifelse(cyc %in% sectest$cyc, 1, 0)) %>% # If the unit occurs in the Secretariat as well
  arrange(desc(double)) %>%
  mutate(skip = ifelse(double == 1 & impute == 1, 1, 0)) %>% # Then make a variable that takes 1 if the unit was imputed
  filter(skip != 1) %>% # Remove these units that were duplicated and imputed
  select(-skip, -double)

### Twinned secretariat

Twinned_secretariat <- participant_per_year %>%
  filter(membership == "Twinned secretariat") %>%
  mutate(impute = 0)

Twinned_secretariat %>%
  group_by(country, year, committee) %>%
  count(sort = TRUE) # No duplicated secretariats

twinnedsectest <- Twinned_secretariat %>%
  mutate(cyc = str_c(country, "_", year, "_", committee)) # Make unit to check match

twinnedsectest2 <- twinnedsectest %>%
  mutate(double = ifelse(cyc %in% memtest$cyc, 1, 0)) %>% # If the unit occurs in membership as well
  arrange(desc(double)) %>%
  mutate(skip = ifelse(double == 1 & impute == 1, 1, 0)) %>% # Then make a variable that takes 1 if the unit was imputed
  filter(skip != 1) %>% # Remove these units that were duplicated and imputed
  select(-skip, -double)

twinnedsectest3 <- twinnedsectest2 %>%
  mutate(double = ifelse(cyc %in% sectest$cyc, 1, 0)) %>% # If the unit occurs in secretariat as well
  arrange(desc(double)) %>%
  mutate(skip = ifelse(double == 1 & impute == 1, 1, 0)) %>% # Then make a variable that takes 1 if the unit was imputed
  filter(skip != 1) %>% # Remove these units that were duplicated and imputed
  select(-skip, -double)

memtest3 <- memtest %>%
  mutate(double = ifelse(cyc %in% twinnedsectest3$cyc, 1, 0)) %>% # If the unit occurs in the Twinned Secretariat as well
  arrange(desc(double)) %>%
  mutate(skip = ifelse(double == 1 & impute == 1, 1, 0)) %>% # Then make a variable that takes 1 if the unit was imputed
  filter(skip != 1) %>% # Remove these units that were duplicated and imputed
  select(-skip, -double)

sectest3 <- sectest2 %>%
  mutate(double = ifelse(cyc %in% twinnedsectest3$cyc, 1, 0)) %>% # If the unit occurs in the Twinned Secretariat as well
  arrange(desc(double)) %>%
  mutate(skip = ifelse(double == 1 & impute == 1, 1, 0)) %>% # Then make a variable that takes 1 if the unit was imputed
  filter(skip != 1) %>% # Remove these units that were duplicated and imputed
  select(-skip, -double)


#### 5. Include all in one dataset 

participation <- bind_rows(memtest3, sectest3, twinnedsectest3) %>%
  select(-cyc)

## Sectors 
sectors <- readRDS("../datasets/sectors.rds") %>%
  unnest() %>%
  ungroup() %>%
  mutate(committee = str_remove_all(committee, "ISO/"),
         committee = str_remove_all(committee, "IEC "))

participation <- participation %>%
  left_join(sectors, by = join_by("committee"), relationship = "many-to-many")

na_sectors <- participation %>%
  distinct() %>% 
  filter(is.na(sector))

na_sectors_filled <- na_sectors %>% 
  select(-sector) %>%
  mutate(committee2 = str_remove(committee, "\\/SC.*")) %>%
  left_join(sectors, by = c("committee2" = "committee")) %>%
  select(-committee2)

participation <- participation %>%
  anti_join(na_sectors, by = join_by(committee, title)) %>%
  bind_rows(na_sectors_filled)

saveRDS(participation, file = "../datasets/participation.rds")

