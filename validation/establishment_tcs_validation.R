

##### VALIDATING ESTABLISHMENT YEAR OF TECHNICAL COMMITTEES ######

pacman::p_load(tidyverse, pdftools, DBI, RSQLite)

con <- dbConnect(RSQLite::SQLite(), "./iso_standards.sqlite")

tc_creation <- dbReadTable(con, "historical_tc_creation")
tc_creation <- tc_creation %>%
  rename(year_original = year)

dbDisconnect(con)

ref_table <- pdf_text("./validation/documents/report_2000.pdf")[42:52] %>%
  read_lines() %>%
  grep("^TC", ., value = TRUE) %>%
  paste(collapse = '\n') %>%  
  read_fwf(fwf_empty(.)) %>%  
  mutate(committee = str_c("ISO/", X1, " ", str_extract(X2, "[0-9]+"))) %>%
  mutate(year = str_extract(X2, "[0-9]{4}")) %>%
  mutate(X2 = str_squish(str_remove_all(X2, "[0-9]+"))) %>%
  rename("US_participation" = "X4",
         "secretariat" = "X3",
         "title" = "X2") %>%
  select("committee", "title", "year", "secretariat", "US_participation") %>%
  mutate(year = as.numeric(year)) %>%
  rename(year_validation = year) %>%
  select(committee, year_validation)

joined <- left_join(tc_creation, ref_table, by = join_by(committee)) 

joined %>%
  mutate(diff = year_original - year_validation) %>%
  summarise(year_diff =mean(diff, na.rm = T)) 

joined %>%
  select(year_original, year_validation) %>%
  mutate(diff = year_original - year_validation) %>%
  na.omit() %>%
  ggplot(aes(year_original, year_validation)) + 
  geom_point() +
  labs(x = "Original year",
       y = "Validation year") +
  theme_bw()

ggsave("./figures/validation_tc_establishment1.pdf", width = 10, height = 10)

joined %>%
  mutate(ok = ifelse(year_original - year_validation, 0, 1)) %>%
  count(ok) 

joined %>%
  mutate(ok = ifelse(year_original - year_validation, 0, 1)) %>%
  filter(ok == 0) %>%
  mutate(diff = year_original - year_validation) %>%
  summarise(year_diff =mean(diff, na.rm = T))

joined %>%
  mutate(ok = ifelse(year_original - year_validation, 0, 1)) %>%
  filter(ok == 0) %>%
  mutate(diff = year_original - year_validation) %>%
  mutate(title = str_c(title, " (", committee, ")")) %>%
  gather(year_original, year_validation, key = "type", value = "year") %>%
  mutate(Type = ifelse(type == "year_original", "Original", 
                       ifelse(type == "year_validation", "Validation", type))) %>%
  ggplot(aes(title, year, color = Type)) + 
  geom_point(aes(shape = Type)) +
  scale_color_manual(values = c("black", "darkgrey")) +
  labs(x = "", y = "") +
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.text=element_text(size=12))

ggsave("./figures/validation_tc_establishment2.pdf", width = 10, height = 10)

