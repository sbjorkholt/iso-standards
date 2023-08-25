
###############################################################################################################################
#################################                           CLEANING                             ##############################
###############################################################################################################################

tc_names <- readRDS("../raw_data/archive_members/participants_v1.rds") %>%
  select(committee, title)

liaison_v1 <- read_rds("../raw_data/archive_liaison/liaison_v1_table.rds") %>% 
  rename(name = organization) %>%
  mutate(year = str_extract(date, "[0-9]{4}")) %>%
  unnest(cols = c(liaison)) %>%
  mutate(type = NA) %>%
  select(-webid) %>%
  rename(committee = liaison) %>%
  left_join(tc_names, by = join_by(committee)) %>%
  select(acronym, name, address, country, year, committee, title, type)

liaison_v2 <- read_rds("../raw_data/archive_liaison/liaison_v2_table.rds") %>%
  rename(type = Type) %>%
  select(acronym, name, address, country, year, committee, title, type)

liaison_v3 <- read_rds("../raw_data/archive_liaison/liaison_v3_table.rds") %>%
  rename(type = Type) %>%
  select(acronym, name, address, country, year, committee, title, type)

liaison_v4 <- read_rds("../raw_data/archive_liaison/liaison_v4_table.rds") %>%
  rename(type = Type) %>%
  mutate(year = str_extract(date, "[0-9]{4}")) %>%
  select(acronym, name, address, country, year, committee, title, type)

liaison_current_2023 <- read_rds("../raw_data/archive_liaison/liaison_current_table.rds") %>%
  rename(type = Type) %>%
  mutate(year = 2023) %>%
  select(acronym, name, address, country, year, committee, title, type)

#### 1. Order data and fix names so that they correspond over time ####

liaison_wayback <- rbind(liaison_v1, # Bind together data from all webpage versions
                         liaison_v2,
                         liaison_v3,
                         liaison_v4,
                         liaison_current_2023) %>%
  mutate(country = ifelse(country == "", NA, country),
         name = ifelse(name == "", NA, name)) %>%
  mutate(country = ifelse(country == "Rica", "Costa Rica",
                          ifelse(country == "of", "Iran",
                                 ifelse(country == "Kong", "Hong Kong",
                                        ifelse(country == "States", "United States",
                                               country))))) %>%
  mutate(acronym = case_when(
    acronym == "ECOS (Europe" ~ "ECOS (Europe)",
    acronym == "INT, France" ~ "INT",
    acronym == "CET - thé" & country == "Germany" ~ "European Tea Committee",
    acronym == "CET - ceramic" & country == "Belgium" ~ "European Ceramic Tile Manufacturers' Federation",
    acronym == "Canada" ~ "ISSPA, Canada",
    acronym == "IFRA - fragrance" ~ "IFRA",
    acronym == "IFA-International Fireworks Association" ~ "IFA - International Fireworks Association",
    acronym == "isda" ~ "ISDA",
    acronym == "WHO" & country == "Denmark" ~ "WHO-ECEH",
    acronym == "iiSBE - iiSBE" ~ "iiSBE",
    acronym == "eCl@ss e.V" ~ "eCl@ss e.V.",
    acronym == "ISC)2" ~ "(ISC)2",
    acronym == "NATO AC 35, United Kingdom" ~ "NATO AC 35",
    acronym == "Opengroup, United Kingdom" ~ "Opengroup",
    acronym == "World Shipping Council" ~ "WSC",
    acronym == "DMSC Inc" ~ "DMSC",
    acronym == "DMSC Inc." ~ "DMSC",
    acronym == "AIB_Association of Issuing Bodies" ~ "AIB",
    acronym == "Global Platform - Global Platform Inc." ~ "Global Platform",
    acronym == "IMS Global ." ~ "IMS Global",
    acronym == "MiA_Marketplace Industry Association, Inc." ~ "MiA",
    acronym == "QuEST Forum" ~ "QuEST",
    TRUE ~ acronym)
    ) %>%
  mutate(name = case_when(
    acronym == "WIPO" ~ "World Intellectual Property Organization",
    acronym == "WHO" & country == "Switzerland" ~ "World Health Organization",
    acronym == "WHO-ECEH" ~ "WHO European Centre for Environment and Health",
    acronym == "WPO" ~ "World Packaging Organization",
    acronym == "WASPaLM" ~ "World Association of Societies of Pathology and Laboratory Medicine",
    acronym == "WMO" ~ "World Meteorological Organization",
    acronym == "WFO" ~ "World Foundrymen Organization",
    acronym == "WAPOR" ~ "World Association for Public Opinion Research (WAPOR)",
    acronym == "UNSCETDG" ~ "UN/ECOSOC Sub-Committee of Experts on the transport of Dangerous Goods (TDG)",
    acronym == "UNIDO" ~ "United Nations Industrial Development Organization",
    acronym == "AccountAbility" ~ "AccountAbility",
    acronym == "BIAC" ~ "Business at OECD",
    acronym == "CCIB" ~ "Barcelona Convention Bureau",
    acronym == "CIAA" ~ "Confederation of the. Food and Drink Industries of the European Union",
    acronym == "DMSC" ~ "Digitial Metrology Standards Consortium",
    acronym == "EBEN" ~ "Eben Consultants (F.E.) Pte Ltd",
    acronym == "EFAEP" ~ NA,
    acronym == "EIRIS" ~ "Ethical Investment Research and Information Service",
    acronym == "FIDIS" ~ "Future of Identity in the Information Society",
    acronym == "FLA" ~ "Fair Labor Association",
    acronym == "Forum Empresa" ~ "Forum Empresa/Ethos Institute",
    acronym == "IABC" ~ "International Association of Business Communicators",
    acronym == "IDOIF" ~ NA,
    acronym == "IIC" ~ "International Chamber of Commerce",
    acronym == "IOOC/COI" ~ "International Olive Council",
    acronym == "(ISC)2" ~ "International Information Systems Security Certification Consortium, Inc.",
    acronym == "NATO AC 35" ~ "North Atlantic Treaty Organisation (AC/35)",
    acronym == "Opengroup" ~ "Opengroup",
    acronym == "Red Puentes" ~ "Red Puentes",
    acronym == "SAI" ~ "SAI Global Limited",
    acronym == "UN Global Compact" ~ "UN Global Compact",
    acronym == "UNECLAC" ~ "Economic Commission for Latin America and the Caribbean",
    acronym == "WSA" ~ "World Steel Association",
    acronym == "WSC" ~ "World Shipping Council",
    acronym == "EFCO&HPA;" ~ "European Federation of Campingsite Organisations and Holiday Park Associations",
    acronym == "IH&RA;" ~ "International Hotel & Restaurant Association",
    acronym == "QuEST" ~ "Quality Excellence for Suppliers of Telecommunications",
    acronym == "LTAC/TerminOrgs" ~ "Language Terminology/Translation and Authoring Consortium/Terminology for Large Organizations",
    acronym == "The SPICE User Group" ~ "Software Process Improvement and Capability Determination User Group",
    TRUE ~ name)
  ) %>%
  mutate(country = case_when(
    acronym == "WHO-ECEH" ~ "Denmark",
    TRUE ~ country
  )) %>%
  mutate(name = ifelse(acronym == "INT", "Institut National des Télécommunications",
                       ifelse(name == "Confederation of the. Food and Drink Industries of the European Union", "Confederation of the Food and Drink Industries of the European Union",
                              ifelse(name == "Interdisciplinary Centre for Law and ICT (ICRI),", "Interdisciplinary Centre for Law and ICT (ICRI)",
                                     name))),
         address = ifelse(acronym == "INT", "Institut National des Télécommunications9, Rue Charles Fourier 91011 Evry France", address),
         country = ifelse(acronym == "INT", "France", country)) %>%
  filter(!committee %in% c("ISO/CASCO", "CASCO", "REMCO", "ISO/REMCO")) %>%
  filter(!acronym %in% c("France", "United Kingdom", "USA")) %>%
  mutate(acronym = str_replace_all(acronym, "ō", "o"),
         name = str_replace_all(name, "ō", "o")) %>%
  mutate(committee = str_remove_all(committee, "ISO/")) %>%
  mutate(year = as.numeric(year)) %>%
  unique()

rm(liaison_v1, liaison_v2, liaison_v3, liaison_v4, liaison_current_2023)

##### 2. Adding imputations ######

## Filling in potentially missing years (between two years) - marking these impute_year_1 = 1 ##

liaison_wayback <- liaison_wayback %>%
  select(-address) %>%
  mutate(committee = str_replace(committee, "^[IEC ]*", ""),
         committee = str_replace(committee, "\\/WG [0-9]+", ""),
         committee = str_replace(committee, "\\/JWG [0-9]+", "")) %>%
  group_by(acronym, name, country) %>%
  mutate(impute_year_1 = 0) %>%
  complete(year = seq(min(year), max(year), by = 1)) %>%
  mutate(impute_year_1 = ifelse(is.na(impute_year_1), 1, impute_year_1)) %>%
  ungroup() 

liaison_wayback <- impute_knn(
  liaison_wayback,
  committee ~ title + name | acronym,
  pool = "complete",
  k = 2)

liaison_wayback <- impute_knn(
  liaison_wayback,
  title ~ committee + name | acronym,
  pool = "complete",
  k = 2)

liaison_wayback <- liaison_wayback %>% 
  distinct(acronym, name, country, committee, year, .keep_all = TRUE)

## Filling in years otherwise in the sample possibly lost - marking these impute_year_2 = 1 ##

# Make a dataframe with all the potential years, memberships and country combinations there are
# years <- as.numeric(c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))
# acronyms <- liaison_wayback %>% select(acronym) %>% unique() %>% pull(acronym)
# names <- liaison_wayback %>% select(name) %>% unique() %>% na.omit() %>% pull(name)
# 
# acronyms_full <- tibble(acronym = rep(acronyms, each = length(years)),
#                          year = rep(years, length(acronyms))) # First make a dataset and repeat the years and countries/acronyms according to each others' length
# 
# liaison_wayback <- liaison_wayback %>%
#   mutate(impute_year_2 = 0) %>%
#   full_join(acronyms_full, by = c("year", "acronym")) %>% # Right join so that all rows without data originally are now given NA
#   mutate(impute_year_2 = ifelse(is.na(impute_year_2), 1, impute_year_2))
# OBS: NAs might both be because the given organization was not in a technical committee at that time, and because Wayback did not save that webpage

## Adding country of organization based on available data from other webpages
liaison_wayback <- liaison_wayback %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  left_join(liaison_wayback, by = c("country")) %>%
  group_by(acronym) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  mutate(country = first(na.omit(country))) %>%
  select(-n)  %>%
  ungroup()

## Adding name of organization based on similiar methods as above
liaison_wayback <- liaison_wayback %>%
  group_by(name) %>%
  summarise(n = n()) %>%
  left_join(liaison_wayback, by = c("name")) %>%
  group_by(acronym, country) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  mutate(name = first(na.omit(name))) %>%
  select(-n) %>%
  ungroup()

# # Same imputation method of missing TC as for members
# liaison_wayback_na <- liaison_wayback %>%
#   mutate(year = as.numeric(year)) %>%
#   mutate(var = ifelse(is.na(committee) & year < 2004 | is.na(committee) & year == 2023 & year == 2022, "ok1",
#                       ifelse(!is.na(committee) & year < 2004 | !is.na(committee) & year != 2023 & year != 2022, "ok2", 
#                              "missing")))
# 
# liaison_wayback_na <- liaison_wayback_na %>% # Adding a missing variable
#   filter(var != "ok1") %>%
#   select(-var)
# 
# organizations <- liaison_wayback_na %>% select(acronym) %>% 
#   pull() %>% unique()
# 
# acronyms_list <- list()
# 
# for (i in 1:length(organizations)) {
#   
#   tmp <- liaison_wayback_na %>%
#     mutate(year = as.numeric(year)) %>% # Make the year variable numeric to that it can be ordered
#     filter(acronym == organizations[i]) %>% # Filter out each organization
#     mutate(impute = ifelse(is.na(title) & is.na(committee), 1, 0)) %>% # Make a variable that takes "impute" if there are missing values
#     group_by(year, impute) %>% # Group by variables and....
#     nest() %>% # ... make nested columns with info on committee membership in given years
#     arrange(-desc(year)) %>% # Order the years
#     ungroup() %>% # Ungroup to not create clutter later 
#     mutate(change = ifelse(lead(impute, 1) == 1 & impute == 0, 1, 0)) # A variable telling us which is the closest year with non-missing values
#   
#   acronyms_list[[i]] <- tmp %>% 
#     mutate(data2 = ifelse(impute == 1, # If impute is 1
#                           tmp %>% filter(impute == 0 & change == 1) %>% select(data) %>% pull(), # Then fill the row with info from the closest observation with non-missing values
#                           data)) %>% # Otherwise, just reproduce the nested cell
#     select(-change) # Remove the change variable
#   
# }
# 
# liaison_wayback_imputed <- bind_rows(acronyms_list) %>%
#   select(-data) %>% # Remove old nested variable
#   unnest(cols = c(data2)) # Unnest the new imputed variable

liaison_wayback %>%
  mutate(` ` = ifelse(impute_year_1 == 0, "Original", "Imputed")) %>%
  ggplot(aes(year, acronym, fill = ` `)) +
  scale_fill_manual(values = c("gray", "black")) +
  geom_tile() +
  labs(x = "", y = "") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 6))

ggsave("../figures/imputations_liaison.png", width = 10, height = 12)


###### 3. Classification of organizations ########

organizations <- liaison_wayback %>%
  select(acronym, name) %>%
  unique() %>%
  na.omit()

categories <- tibble(category_number = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                     category = c("Industry", "Consultant and Registrar", "Standards organization", "Government", "Research", "NGO", "Financial institutions",
                                  "Media", "Intergovernmental organizations", "Other"),
                     description = c("Individual firms and industry associations representing a specific industry or group of professionals.",
                                     "Firms that provide engineering/technical services, or support or training related to standards.",
                                     "Representatives from national member bodies, standard development organizations, and accreditation bodies.",
                                     "Representatives from governmental agencies/ministries.",
                                     "Research and/or academic institutions.",
                                     "Non-governmental organizations, such as consumer organizations, advocacy groups, or other civil society representatives.",
                                     "Institutions dealing with finance such as banks, insurance companies and real estate firms.",
                                     "Organizations dealing with public media such as TV stations, news outlets and journalistic organizations.",
                                     "Organizations that work on an international level.",
                                     "Other organizations.")) %>%
  mutate(category_number = as.character(category_number))

# Type	Description
# Industry - Individual firms and industry associations representing a specific industry or group of professionals.
# Consultant and Registrar - Firms that provide engineering/technical services, or support or training related to standards.
# Standards organization - Representatives from national member bodies, standard development organizations, and accreditation bodies.
# Government - Representatives from governmental agencies/ministries.
# Research - Research and/or academic institutions.
# NGO - Non-governmental organizations, such as consumer organizations, advocacy groups, or other civil society representatives.
# Financial institutions - Institutions dealing with finance such as banks, insurance companies and real estate firms.
# Media	- Organizations dealing with public media such as TV stations, news outlets and journalistic organizations.
# Intergovernmental organizations	- Organizations that work on an international level.
# Other	- Other organizations.

create_prompt <- function(organizations){
  prompts <- purrr::map2(organizations$acronym, organizations$name,
                         
                         ~list(
                           
                           list(
                             "role" = "system",
                             "content" = stringr::str_c(
                               
                               "You are an expert various organizations, associations, and standards bodies from different industries and sectors.",
                               
                               "You are to classify organizations into their appropriate category based on the definitions below. ",
                               
                               "Industry: Individual firms and industry associations representing a specific industry or group of professionals.",
                               "Consultant and Registrar: Firms that provide engineering/technical services, or support or training related to standards.",
                               "Standards organization: Representatives from national member bodies, standard development organizations, and accreditation bodies.",
                               "Government: Representatives from governmental agencies/ministries",
                               "Research: Research and/or academic institutions.",
                               "NGO: Non-governmental organizations, such as consumer organizations, advocacy groups, or other civil society representatives.",
                               "Financial institution: Institutions dealing with finance such as banks, insurance companies and real estate firms.",
                               "Media: Organizations dealing with public media such as TV stations, news outlets and journalistic organizations.",
                               "Intergovernmental organization: Organizations that work on an international level.",
                               "Other: Other organizations.")
                           ),
                           
                           list(
                             "role" = "user",
                             "content" = stringr::str_c(
                               
                               "Which category of organization is organization with acronym '", .x, " and name ", .y, "' ?  ",
                               
                               "Answer with one of the following types: ",
                               "1. Industry ",
                               "2. Consultant and Registrar ",
                               "3. Standards organization ",
                               "4. Government ",
                               "5. Research ",
                               "6. NGO ",
                               "7. Financial institution ",
                               "8. Media ",
                               "9. Intergovernmental organization ",
                               "10. Other",
                               
                               "Use this template to answer. Please follow the template exactly.",

                               "Acronym: ", .x, " | ",
                               "Name: ", .y, " | ",
                               
                               "Category: Number from 1 to 10 | ",
                               "Justification \n\n")
                             
                           )
                         )
  )
  prompts
}

prompts <- create_prompt(organizations)

api_key <- read_lines("../../credentials/api_key_chatgpt")

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

organizations_tagged <- tibble(category = unlist(openai_completions)) %>%
  separate_wider_delim(category, delim = "|", names = c("Acronym", "Name", "Category", "Justification"), too_few = "debug", too_many = "debug") %>%
  mutate(Acronym = str_remove(Acronym, "Acronym: "),
         Name = str_remove(Name, "Name: "),
         Category = str_remove(Category, "Category: "),
         Justification = str_remove(Justification, "Justification: ")) %>%
  mutate_all(str_squish) %>%
  left_join(categories, by = c("Category" = "category_number")) %>%
  filter(category_ok == TRUE) %>%
  mutate(Category = str_extract(Category, "[0-9]"))

organizations_fixed <- read.csv("../raw_data/archive_liaison/ai_tagging/errors.csv") %>%
  mutate(Category = as.character(Category)) %>%
  mutate_all(str_squish)

organizations_tagged_finished <- bind_rows(organizations_tagged, organizations_fixed) %>%
  select(Acronym, Name, Category, Justification) %>%
  rename_all(str_to_lower) 

# saveRDS(category_justification, file = "../_raw_data/archive_liaison/ai_tagging/category_justification.rds")

liaison_wayback <- liaison_wayback %>%
  left_join(category_justification, by = join_by(acronym, country), relationship = "many-to-many") %>%
  rename(impute = impute_year_1) %>%
  left_join(categories %>% rename(category_text = category, category = category_number), by = join_by(category)) %>%
  select(acronym, name, year, country, category, category_text, description, justification, committee, title, type, impute) %>%
  distinct()

## Sectors 
sectors <- readRDS("../datasets/sectors.rds") %>%
  unnest() %>%
  ungroup() %>%
  mutate(committee = str_remove_all(committee, "ISO/"),
         committee = str_remove_all(committee, "IEC "))

liaison_wayback <- liaison_wayback %>%
  left_join(sectors, by = join_by("committee"), relationship = "many-to-many")

na_sectors <- liaison_wayback %>%
  distinct() %>% 
  filter(is.na(sector))

na_sectors_filled <- na_sectors %>% 
  select(-sector) %>%
  mutate(committee2 = str_remove(committee, "\\/SC.*")) %>%
  left_join(sectors, by = c("committee2" = "committee")) %>%
  select(-committee2)

liaison_wayback <- liaison_wayback %>%
  anti_join(na_sectors, by = join_by(committee, title)) %>%
  bind_rows(na_sectors_filled)

# test <- liaison_wayback %>% 
#   select(committee, title, sector) %>%
#   drop_na(committee) %>%
#   filter(is.na(sector)) %>%
#   filter(str_detect(committee, "TC|PC")) %>% 
#   unique()

saveRDS(liaison_wayback, file = "../datasets/liaison.rds")

