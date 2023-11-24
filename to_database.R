
#########################################################
################## TO DATABASE ##########################
#########################################################

library(dplyr)
library(tidyr)
library(stringr)
library(DBI)
library(RSQLite)

#### Load data ####

## Participants
participants <- readRDS("./datasets/participation.rds")

## Liaison
liaison <- readRDS("./datasets/liaison.rds")

## Standards
standards <- readRDS("./datasets/standards_df.rds")

sectors <- readRDS("./datasets/sectors.rds") %>%
  unnest() %>%
  ungroup()

# standards <- left_join(standards, sectors, by = join_by(committee))
# 
# na_sectors <- standards %>%
#   distinct() %>% 
#   filter(is.na(sector))
# 
# na_sectors_filled <- na_sectors %>% 
#   select(-sector) %>%
#   mutate(committee2 = str_remove(committee, "\\/SC.*")) %>%
#   left_join(sectors, by = c("committee2" = "committee")) %>%
#   select(-committee2)
# 
# standards <- standards %>%
#   anti_join(na_sectors, by = join_by(committee, title)) %>%
#   bind_rows(na_sectors_filled)

ICS_df <- standards %>%
  select(stdno, rowid, ICS) %>%
  unnest_longer(col = ICS) %>%
  unnest(cols = c(ICS))

SDG_df <- standards %>%
  select(stdno, rowid, SDGs) %>%
  unnest_longer(col = SDGs) %>%
  unnest(cols = SDGs)

life_cycle_df <- standards %>%
  select(stdno, rowid, life_cycle) %>%
  unnest_longer(col = life_cycle) %>%
  unnest() %>%
  filter(str_detect(value, "[0-9]+")) %>%
  rename(life_stage = value,
         life_stage_code = stage)
  
standards <- standards %>%
  select(-c(ICS, SDGs, life_cycle)) %>%
  mutate(year = str_extract(publication_date, "[0-9]{4}"))

standards_status <- standards %>%
  full_join(ICS_df, by = join_by(stdno, rowid), relationship = "many-to-many") %>%
  select(stdno, year, title, committee, status, publication_date, edition, pages, abstract, ics_number, ics_text, link) %>%
  drop_na(stdno)

standards_sdgs <- standards %>%
  full_join(SDG_df, by = join_by(stdno, rowid), relationship = "many-to-many") %>%
  select(stdno, year, title, committee, sdg_number, sdg_text, link) %>%
  drop_na(stdno)

standards_life_cycle <- standards %>%
  full_join(life_cycle_df, by = join_by(stdno, rowid), relationship = "many-to-many") %>%
  select(stdno, year, title, committee, life_stage, life_stage_code, date, link) %>%
  drop_na(stdno)


## Certifications
country_certifications <- readRDS("./datasets/country_certifications.rds") %>%
  unnest() %>%
  ungroup() %>%
  mutate(iso = ifelse(iso == "iso_27001", "iso_iec_27001", 
                      ifelse(iso == "iso_20000_1", "iso_iec_20000-1",
                             ifelse(iso == "iso__39001", "iso_39001", iso)))) %>%
  mutate(iso_name = case_when(
    iso == "iso_9001" ~ "Quality management systems",
    iso == "iso_iec_27001" ~ "Information security management",
    iso == "iso_iec_20000-1" ~ "Information technology",
    iso == "iso_50001" ~ "Energy management",
    iso == "iso_45001" ~ "Environmental management",
    iso == "iso_39001" ~ "Road traffic safety (RTS) management systems",
    iso == "iso_37001" ~ "Anti-bribery management systems",
    iso == "iso_28000" ~ "Specification for security management systems for supply chains",
    iso == "iso_22301" ~ "Security and resilience",
    iso == "iso_14001" ~ "Environmental management",
    iso == "iso_13485" ~ "Medical devices - Quality management systems",
    iso == "iso__28000" ~ "Specification for security management systems for supply chains", 
    TRUE ~ iso)
  )
country_per_industry_certifications <- readRDS("./datasets/country_per_industry_certifications_2009_2020.rds") %>%
  unnest() %>%
  ungroup() 
industry_certifications <- readRDS("./datasets/industry_certifications.rds") %>%
  unnest() %>%
  ungroup() %>%
  mutate(iso = ifelse(iso == "iso_27001", "iso_iec_27001", 
                      ifelse(iso == "iso_20000_1", "iso_iec_20000-1",
                             ifelse(iso == "iso__39001", "iso_39001", iso)))) %>%
  mutate(iso_name = case_when(
    iso == "iso_9001" ~ "Quality management systems",
    iso == "iso_iec_27001" ~ "Information security management",
    iso == "iso_iec_20000-1" ~ "Information technology",
    iso == "iso_50001" ~ "Energy management",
    iso == "iso_45001" ~ "Environmental management",
    iso == "iso_39001" ~ "Road traffic safety (RTS) management systems",
    iso == "iso_37001" ~ "Anti-bribery management systems",
    iso == "iso_28000" ~ "Specification for security management systems for supply chains",
    iso == "iso_22301" ~ "Security and resilience",
    iso == "iso_14001" ~ "Environmental management",
    iso == "iso_13485" ~ "Medical devices - Quality management systems",
    iso == "iso__28000" ~ "Specification for security management systems for supply chains", 
    TRUE ~ iso)
  )

## Historical membership

sectors <- readRDS("./datasets/sectors.rds")

historical_memberships <- readRDS("./datasets/memberships.rds") %>%
  unnest() %>%
  ungroup() %>%
  rename(membership_role = func,
         membership_status = status) %>%
  select(year, country, continent, membership_status, membership_role)

## Historical TC creation
historical_tc_creation <- readRDS("./datasets/tc_creation.rds") %>%
  unnest() %>%
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  drop_na(year) %>%
  left_join(sectors) %>%
  filter(!str_detect(title, "To organize")) %>%
  select(year, title, committee, sector)

#### Database ####

con <- dbConnect(RSQLite::SQLite(), "./iso_standards.sqlite")

dbWriteTable(con, "participants", participants, overwrite = TRUE)
dbWriteTable(con, "liaison", liaison, overwrite = TRUE)
dbWriteTable(con, "standards_status", standards_status, overwrite = TRUE)
dbWriteTable(con, "standards_sdgs", standards_sdgs, overwrite = TRUE)
dbWriteTable(con, "standards_life_cycle", standards_life_cycle, overwrite = TRUE)
dbWriteTable(con, "country_certifications", country_certifications, overwrite = TRUE)
dbWriteTable(con, "country_per_industry_certifications", country_per_industry_certifications, overwrite = TRUE)
dbWriteTable(con, "industry_certifications", industry_certifications, overwrite = TRUE)
dbWriteTable(con, "historical_memberships", historical_memberships, overwrite = TRUE)
dbWriteTable(con, "historical_tc_creation", historical_tc_creation, overwrite = TRUE)

dbListTables(con)

dbDisconnect(con)
