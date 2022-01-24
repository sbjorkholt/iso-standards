
library(tidyverse)
library(readtext)
library(stringr)

#####################################################
################ BUSINESS REPORTS ###################
#####################################################

# Business reports gathered from https://isotc.iso.org/livelink/livelink?func=ll&objId=687806&objAction=browse&sort=name&viewType=1

business_papers <- readtext("../data/Business_reports")

business_reports <- business_papers %>% 
  mutate(text = str_squish(text)) %>%
  mutate(Committee = str_extract(doc_id, "ISOTC(\\s+)?([0-9]+)?|ISOIEC_JTC_1"),
         Title = str_replace_all(doc_id, "ISOTC(\\s+)?([0-9]+)?|ISOIEC_JTC_1|.pdf|_", ""),
         Title = str_squish(Title)) %>%
  select(Committee, Title, text) %>%
  as_tibble()

business_reports$text[1]

saveRDS(business_reports, file = "./data/business_reports.rds")

#########################################################
################## TO DATABASE ##########################
#########################################################

library(DBI)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "./data/iso_standards.sqlite")

dbWriteTable(con, "business_reports", business_reports)

dbListTables(con)
