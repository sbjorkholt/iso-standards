
library(tidyverse)

# Manually collected from ISO's own internal wikipedia and data from the webpage of SN (Standard Norway). 

proposals <- read_csv("proposals.csv")

proposals <- proposals %>%
  rename(sdo = acronym) %>%
  select(country, sdo, year, title, pass, committee, secretariat)

saveRDS(proposals, file = "../../data/final_data/proposals.rds")
