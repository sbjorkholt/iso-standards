
#########################################################
################## TO DATABASE ##########################
#########################################################

library(dplyr)
library(tidyr)
library(DBI)
library(RSQLite)

#### Load data ####

## Participants
participants <- readRDS("./data/final_data/participants.rds") %>%
  unnest() %>%
  ungroup()

## Liaison
liaison <- readRDS("./data/final_data/liaison.rds") %>%
  unnest() %>%
  ungroup()

## Standards
standards <- readRDS("./data/final_data/standards_df.rds")

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
  unnest()

standards <- standards %>%
  select(-c(ICS, SDGs, life_cycle)) %>%
  full_join(ICS_df, by = join_by(stdno, rowid), relationship = "many-to-many") %>%
  full_join(SDG_df, by = join_by(stdno, rowid), relationship = "many-to-many") %>%
  full_join(life_cycle_df, by = join_by(stdno, rowid), relationship = "many-to-many")

## Sectors 
sectors <- readRDS("./data/final_data/sectormerge_standards.rds") %>%
  unnest() %>%
  ungroup()

## Certifications
country_certifications <- readRDS("./data/final_data/country_certifications.rds") %>%
  unnest() %>%
  ungroup()
country_per_industry_certifications <- readRDS("./data/final_data/country_per_industry_certifications_2009_2020.rds") %>%
  unnest() %>%
  ungroup()
industry_certifications <- readRDS("./data/final_data/industry_certifications.rds") %>%
  unnest() %>%
  ungroup()

## Historical membership
historical_memberships <- readRDS("./data/final_data/memberships.rds") %>%
  unnest() %>%
  ungroup()

## Historical TC creation
historical_tc_creation <- readRDS("./data/final_data/tc_creation.rds") %>%
  unnest() %>%
  ungroup()

#### Database ####

con <- dbConnect(RSQLite::SQLite(), "./data/final_data/iso_standards.sqlite")

dbWriteTable(con, "participants", participants)
dbWriteTable(con, "liaison", liaison)
dbWriteTable(con, "standards", standards)
dbWriteTable(con, "sectors", sectors)
dbWriteTable(con, "country_certifications", country_certifications)
dbWriteTable(con, "country_per_industry_certifications", country_per_industry_certifications)
dbWriteTable(con, "industry_certifications", industry_certifications)
dbWriteTable(con, "historical_memberships", historical_memberships)
dbWriteTable(con, "historical_tc_creation", historical_tc_creation)

dbListTables(con)

