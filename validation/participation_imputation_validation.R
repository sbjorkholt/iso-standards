
##### VALIDATING TC PARTICIPATION ######

pacman::p_load(tidyverse, wayback, DBI, RSQLite, officer, pdftools, rvest)

con <- dbConnect(RSQLite::SQLite(), "./iso_standards.sqlite")

participation <- dbReadTable(con, "participants")
liaison <- dbReadTable(con, "liaison")

dbDisconnect(con)

#### IMPUTE PLOT ####

participation %>%
  group_by(country, membership, year, impute) %>%
  count() %>%
  mutate(year = as.numeric(year)) %>%
  filter(membership != "Twinned secretariat") %>%
  rename("Number of imputed TCs" = n) %>%
  #mutate(` ` = ifelse(impute == 0, "Original", "Imputed")) %>%
  ggplot(aes(year, country, fill = `Number of imputed TCs`)) +
  scale_fill_distiller(type = "seq",
                        direction = 1,
                        palette = "Greys") +
  geom_tile() +
  labs(x = "", y = "") +
  facet_wrap(~ membership) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 6))

# ggsave("./figures/imputations_participation.png", width = 12, height = 15)

liaison %>%
  group_by(name, type, year, impute) %>%
  count() %>%
  mutate(type = ifelse(is.na(type), "Unknown", type)) %>%
  mutate(year = as.numeric(year)) %>%
  rename("Number of imputed TCs" = n) %>%
  ggplot(aes(year, name, fill = `Number of imputed TCs`)) +
  scale_fill_distiller(type = "seq",
                       direction = 1,
                       palette = "Greys") +
  geom_tile() +
  labs(x = "", y = "") +
  facet_wrap(~ type) +
  theme_classic() +
  theme(legend.position = "bottom",
        #axis.text.y = element_text(size = 6),
        axis.text.y = element_blank())

# ggsave("./figures/imputations_liaison.png", width = 12, height = 15)

## Countries 

countries <- participation %>% select(country) %>% filter(country != "Serbia and Montenegro") %>% # Serbia and Montenegro separated, and should thus not be imputed
  pull() %>% unique() # To not make a mess, loop over each country separately

countries <- sort(countries)

for (i in 1:length(countries)) {
  
  plot <- participation %>%
    mutate(year = factor(year)) %>%
    filter(country == countries[i]) %>%
    mutate(impute_year_1 = ifelse(impute == 1, "impute", "original")) %>%
    mutate(membership = str_c(membership, "_", impute_year_1),
           membership = factor(membership, levels = c("P-member_original", "P-member_impute",
                                                      "O-member_original", "O-member_impute",
                                                      "Secretariat_original", "Secretariat_impute"))) %>%
    ggplot(aes(year, committee, fill = membership)) + 
    geom_tile() + 
    ggtitle(countries[i]) +
    scale_fill_manual(values = c("P-member_original" = "darkblue",
                                 "P-member_impute" = "darkred",
                                 "O-member_original" = "steelblue",
                                 "O-member_impute" = "salmon",
                                 "Secretariat_original" = "black",
                                 "Secretariat_impute" = "darkgrey"))
  
  ggsave(plot, heigh = 12, width = 10, filename = str_c("./validation/plots/eyeballs/countries/", countries[i], ".png"))
  
  message("Country ", countries[i], " done!")
  
}

## Liaison

names <- liaison %>% select(name) %>% pull() %>% unique() 

names <- sort(names)

for (i in 1:length(names)) {
  
  plot <- liaison %>%
    mutate(year = factor(year)) %>%
    filter(name == names[i]) %>%
    mutate(type = ifelse(is.na(type), "Unknown", type)) %>%
    mutate(impute_year_1 = ifelse(impute == 1, "impute", "original")) %>%
    mutate(type = str_c(type, "_", impute_year_1),
           type = factor(type, levels = c("A_original", "A_impute",
                                          "B_original", "B_impute",
                                          "C_original", "C_impute",
                                          "Unknown_original", "Unknown_impute"))) %>%
    ggplot(aes(year, committee, fill = type)) + 
    geom_tile() + 
    ggtitle(names[i]) +
    scale_fill_manual(values = c("A_original" = "darkblue",
                                 "A_impute" = "darkred",
                                 "B_original" = "steelblue",
                                 "B_impute" = "salmon",
                                 "C_original" = "purple",
                                 "C_impute" = "pink",
                                 "Unknown_original" = "darkgrey",
                                 "Unknown_impute" = "lightgrey"))
  
  ggsave(plot, heigh = 12, width = 10, filename = str_c("./validation/plots/eyeballs/organizations/", names[i], ".png"))
  
  message("Organization ", names[i], " done!")
  
}


##### FROM REPORTS #####

#### TC 34/SC 3 - 2016 ####

## Participating

val_participating_tc34sc3 <- pptx_summary(read_pptx(path = "./validation/documents/report_2016_TC_34_SC_3.pptx"))[42,][1]  %>%
  str_split(",") %>%
  tibble() %>% 
  unnest() %>%
  rename("country" = ".") %>%
  separate(country, into = c("country", "acronym"), sep = "\\(") %>%
  mutate(country = str_squish(country),
         acronym = str_squish(str_remove(acronym, "\\)"))) %>%
  mutate(country = ifelse(country == "Korea", "South Korea", # Korea got split up, original file says it's South Korea 
                          ifelse(country == "Russian Federation", "Russia", country))) %>% 
  filter(!country %in% c("Islamic Republic of", # Belongs to Iran, was split because of comma (Iran, Islamic Republic of)
                         "Republic of", # Belongs to Korea, was split because of comma (Korea, Republic of)
                         "United Rep. of")) %>% # Belongs to Tanzania, was split because of comma (Tanzania, United Rep. of)
  select(-acronym)

orig_participating_tc34sc3 <- participation %>%
  filter(committee == "TC 34/SC 3",
         year == 2016,
         membership %in% c("P-member"))

tc34sc3_2016_p_false_negative <- val_participating_tc34sc3 %>%
  mutate(correct = ifelse(country %in% orig_participating_tc34sc3$country, 1, 0))

tc34sc3_2016_p_false_positive <- orig_participating_tc34sc3 %>%
  anti_join(val_participating_tc34sc3)


## Observing

val_observing_tc34sc3 <- pptx_summary(read_pptx(path = "./validation/documents/report_2016_TC_34_SC_3.pptx"))[54,][1] %>%
  str_split(",") %>%
  tibble() %>% 
  unnest() %>%
  rename("country" = ".") %>%
  separate(country, into = c("country", "acronym"), sep = "\\(") %>%
  mutate(country = str_squish(country),
         acronym = str_squish(str_remove(acronym, "\\)"))) %>%
  mutate(country = ifelse(country == "Côte d'Ivoire", "Cote d'Ivoire",
                          ifelse(country == "Korea", "North Korea", country))) %>% # Korea split up, original file says it's North Korea 
  filter(!country %in% c("Democratic People's Republic of", # Belongs to North Korea, was split because of comma (Korea, Democratic People's Republic of)
                         "United Republic of")) %>% # Belongs to Tanzania, was split because of comma (Tanzania, United Republic of)
  select(-acronym)

orig_observing_tc34sc3 <- participation %>%
  filter(committee == "TC 34/SC 3",
         year == 2016,
         membership %in% c("O-member"))

tc34sc3_2016_o_false_negative <- val_observing_tc34sc3 %>%
  mutate(correct = ifelse(country %in% orig_observing_tc34sc3$country, 1, 0))

tc34sc3_2016_o_false_positive <- orig_observing_tc34sc3 %>%
  anti_join(val_observing_tc34sc3)


#### TC 146/SC 2 - 2016 ####

## Participating

val_file_tc146_sc2 <- pdf_text("./validation/documents/report_2016_TC_146_SC_2.pdf")[7] %>%
  str_split("\n")

val_participating_tc146sc2 <- val_file_tc146_sc2[[1]][23:32] %>%
  str_replace_all(",", "") %>%
  str_replace_all("\\b[a-z]+\\s+", "") %>%
  str_replace_all("non-", "") %>%
  str_replace_all("European Commission", "") %>%
  str_replace_all("Author Manuscript", "") %>%
  str_split(" ") %>%
  do.call(c, .) %>%
  stringi::stri_remove_empty() %>%
  tibble() %>%
  rename("country" = ".") %>%
  mutate(country = ifelse(country == "Russian", "Russia",
                          ifelse(country == "Korea", "South Korea",
                                 ifelse(country == "Kingdom", "United Kingdom", 
                                        ifelse(country == "Africa", "South Africa", country))))) %>%
  filter(!country %in% c("and", "Federation", "United", "South", "of", "Republic"))

orig_participating_tc146sc2 <- participation %>%
  filter(committee == "TC 146/SC 2",
         year == 2016,
         membership %in% c("P-member"))

tc146sc2_2016_p_false_negative <- val_participating_tc146sc2 %>%
  mutate(correct = ifelse(country %in% orig_participating_tc146sc2$country, 1, 0))

tc146sc2_2016_p_false_positive <- orig_participating_tc146sc2 %>%
  anti_join(val_participating_tc146sc2)


## Observing

val_observing_tc146sc2 <- val_file_tc146_sc2[[1]][39:46] %>%
  str_replace_all(",", "") %>%
  str_replace_all("\\b[a-z]+\\s+", "") %>%
  str_replace_all("non-", "") %>%
  str_replace_all("European Commission", "") %>%
  str_replace_all("Author Manuscript", "") %>%
  str_split(" ") %>%
  do.call(c, .) %>%
  stringi::stri_remove_empty() %>%
  tibble() %>%
  rename("country" = ".") %>%
  mutate(country = ifelse(country == "Zealand", "New Zealand",
                          ifelse(country == "Sri", "Sri Lanka",
                                 ifelse(country == "Saudi", "Saudi Arabia",
                                        ifelse(country == "Czech", "Czech Republic",
                                               ifelse(country == "Russian Federation", "Russia", country)))))) %>%
  filter(!country %in% c("New", "Arabia", "Lanka", "United", "Republic", "and"))

orig_observing_tc146sc2 <- participation %>%
  filter(committee == "TC 146/SC 2",
         year == 2016,
         membership %in% c("O-member"))

tc146sc2_2016_o_false_negative <- val_observing_tc146sc2 %>%
  mutate(correct = ifelse(country %in% orig_observing_tc146sc2$country, 1, 0))

tc146sc2_2016_o_false_positive <- orig_observing_tc146sc2 %>%
  anti_join(val_observing_tc146sc2)


#### TC 258 - 2019 ####

## Participating

val_file_tc258 <- pdf_text("./validation/documents/report_2019_TC_258.pdf")[2] %>%
  str_split("\n")

val_participating_tc258 <- val_file_tc258[[1]][7:11] %>%
  str_replace_all(",", "") %>%
  str_replace_all("\\b[a-z]+\\s+", "") %>%
  str_replace_all("ISO/TC 258 P – members.", "") %>%
  str_split(" ") %>%
  do.call(c, .) %>%
  stringi::stri_remove_empty() %>%
  tibble() %>%
  rename("country" = ".") %>%
  mutate(country = ifelse(country == "Costa", "Costa Rica",
                          ifelse(country == "Kingdom", "United Kingdom",
                                 ifelse(country == "States", "United States", 
                                        ifelse(country == "Africa", "South Africa", 
                                               ifelse(country == "Russia", "Russia",
                                                      ifelse(country == "Korea", "South Korea", country))))))) %>%
  filter(!country %in% c("Rica", "United", "South",
                         "United States")) # United States has the Secretariat

orig_participating_tc258 <- participation %>%
  filter(committee == "TC 258",
         year == 2019,
         membership %in% c("P-member"))

tc258_2019_p_false_negative <- val_participating_tc258 %>%
  mutate(correct = ifelse(country %in% orig_participating_tc258$country, 1, 0))

tc258_2019_p_false_positive <- orig_participating_tc258 %>%
  anti_join(val_participating_tc258)

## Observing

val_observing_tc258 <- val_file_tc258[[1]][13:16] %>%
  str_replace_all(",", "") %>%
  str_replace_all("\\b[a-z]+\\s+", "") %>%
  str_replace_all("ISO/TC 258 P – members.", "") %>%
  str_split(" ") %>%
  do.call(c, .) %>%
  stringi::stri_remove_empty() %>%
  tibble() %>%
  rename("country" = ".") %>%
  mutate(country = ifelse(country == "Hong", "Hong Kong",
                          ifelse(country == "Bosnia", "Bosnia and Herzegovina",
                                 ifelse(country == "Zealand", "New Zealand", country)))) %>%
  filter(!country %in% c("Rep.", "and", "Herzegovina", "New")) 

orig_observing_tc258 <- participation %>%
  filter(committee == "TC 258",
         year == 2019,
         membership %in% c("O-member"))

tc258_2019_o_false_negative <- orig_observing_tc258 %>%
  mutate(correct = ifelse(country %in% orig_observing_tc258$country, 1, 0))

tc258_2019_o_false_positive <- orig_observing_tc258 %>%
  anti_join(val_observing_tc258)


#### TC 211 - 2002 ####

## Participating

val_file_tc211 <- pdf_text("./validation/documents/UN_report_2002_TC_211.pdf")[3] %>%
  str_split("\n")

val_participating_tc211 <- val_file_tc211[[1]][17:26] %>%
  str_split("    ") %>%
  do.call(c, .) %>%
  stringi::stri_remove_empty() %>%
  str_squish() %>%
  tibble() %>%
  rename("country" = ".") %>%
  mutate(country = ifelse(country == "South Afkica", "South Africa", 
                          ifelse(country == "Czech Rep.", "Czech Republic",
                                 ifelse(country == "United States of America", "Untied States",
                                        ifelse(country == "Republic of Korea", "South Korea", country)))))

orig_participating_tc211 <- participation %>%
  filter(committee == "TC 211",
         year == 2003,
         membership %in% c("P-member"))

tc211_2002_p_false_negative <- val_participating_tc211 %>%
  mutate(correct = ifelse(country %in% orig_participating_tc211$country, 1, 0))

tc211_2002_p_false_positive <- orig_participating_tc211 %>%
  anti_join(val_participating_tc211)

## Observing

val_observing_tc211 <- val_file_tc211[[1]][31:39] %>%
  str_split("    ") %>%
  do.call(c, .) %>%
  stringi::stri_remove_empty() %>%
  str_squish() %>%
  tibble() %>%
  rename("country" = ".") %>%
  mutate(country = ifelse(country == "Isl. Rep. of Iran", "Iran", 
                          ifelse(country == "Bahrain (corr.)", "Bahrain",
                                 ifelse(country == "Brunei Damssalam (corr.)", "Brunei Damssalam",
                                        ifelse(country == "Estonia (corr.)", "Estonia",
                                               ifelse(country == "Hong Kong (corr.)", "Hong Kong", country))))))

orig_observing_tc211 <- participation %>%
  filter(committee == "TC 211",
         year == 2002,
         membership %in% c("O-member"))

tc211_2002_o_false_negative <- val_observing_tc211 %>%
  mutate(correct = ifelse(country %in% orig_observing_tc211$country, 1, 0))

tc211_2002_o_false_positive <- orig_observing_tc211 %>%
  anti_join(val_observing_tc211)


#### TC 207 - 2010 ####

val_file_207 <- pdf_text("./validation/documents/report_2010_TC_207.pdf")[7] %>%
  str_split("\n") %>%
  do.call(c, .) %>%
  str_remove_all("\\( [A-Z]+ ([A-Z]+ )?\\)") %>%
  tibble() %>%
  unnest(cols = c()) %>%
  rename("country" = ".") %>%
  mutate(participating1 = str_squish(str_extract(country, "[A-Z][a-z]+")),
         participating2 = str_squish(str_replace(country, "[A-Z][a-z]+(')?( \\(.*\\))?\\s+", "")),
         participating2 = str_squish(str_replace(participating2, "\\s+[A-Z][a-z]+", "")),
         observing = str_squish(str_replace(country, "[A-Z][a-z]+( \\(.*\\))?(\\s+)?([A-Z][a-z]+)?", ""))) %>%
  .[4:49,] %>%
  mutate(participating1 = ifelse(participating1 == "Hong", NA,
                                 ifelse(participating1 == "Czech", "Czech Republic",
                                        ifelse(participating1 == "Ivoire", "Cote d'Ivoire",
                                               ifelse(participating1 == "Costa", "Costa Rica",
                                                      ifelse(participating1 == "Montenegro", NA,
                                                             ifelse(participating1 == "Swaziland", NA,
                                                                    ifelse(participating1 == "Bosnia", NA,
                                                                           ifelse(participating1 == "Botswana", NA,
                                                                                  ifelse(participating1 == "Pakistan", NA,
                                                                                         ifelse(participating1 == "Iceland", NA,
                                                                                                ifelse(participating1 == "Mongolia", NA,
                                                                                                       ifelse(participating1 == "Montenegro", NA,
                                                                                                              ifelse(participating1 == "Sudan", NA,
                                                                                                                     ifelse(participating1 == "Swaziland", NA,
                                                                                                                            ifelse(participating1 == "Trinidad", NA,
                                                                                                                                   participating1)))))))))))))))) %>%
  mutate(participating2 = ifelse(participating2 == "Korea, People's Republic Belarus", "South Korea",
                                 ifelse(participating2 == "Korea, of", "North Korea", 
                                        ifelse(participating2 == "and", NA,
                                               ifelse(participating2 == "Libyan Jamahiriya", "Libya",
                                                      ifelse(participating2 == "Mauritius, The Democratic Republic of", "Mauritius",
                                                             ifelse(participating2 == "Mexico the", "Mexico",
                                                                    ifelse(participating2 == "New Ethiopia", "New Zealand",
                                                                           ifelse(participating2 == "Rica", "Norway",
                                                                                  ifelse(participating2 == "Republic", "Pakistan",
                                                                                         ifelse(participating2 == "Côte-d'Peru", "Peru",
                                                                                                ifelse(participating2 == "Romania, Republic of", "Romania",
                                                                                                       ifelse(participating2 == "Russian", "Russia",
                                                                                                              ifelse(participating2 == "Saudi", "Saudi Arabia",
                                                                                                                     ifelse(participating2 == "South Saint Lucia", "South Africa",
                                                                                                                            ifelse(participating2 == "Sri Slovakia", "Sri Lanka",
                                                                                                                                   ifelse(participating2 == "Iran, of Sweden Slovenia", "Sweden",
                                                                                                                                          ifelse(participating2 == "Tanzania, Republic of", "Tanzania",
                                                                                                                                                 ifelse(participating2 == "Italy55 BP )(2010/09/21)", "Trinidad and Tobago",
                                                                                                                                                        ifelse(participating2 == "Tunisia 7", "Tunisia",
                                                                                                                                                               ifelse(participating2 == "( UNI and Yemen", NA,
                                                                                                                                                                      ifelse(participating2 == "Kong,", NA,
                                                                                                                                                                             ifelse(participating2 == "Bolivia", NA,
                                                                                                                                                                                    participating2))))))))))))))))))))))) %>%
  mutate(observing = ifelse(observing == ", Democratic People's Republic Belarus", "Belarus",
                            ifelse(observing == ", Republic of", "Bolivia",
                                   ifelse(observing == "and Herzegovina", "Bosnia and Herzegovina",
                                          ifelse(observing == "Arab Jamahiriya", "Botswana",
                                                 ifelse(observing == "Congo, The Democratic Republic of", "Congo",
                                                        ifelse(observing == "the", NA,
                                                               ifelse(observing == "Zealand Ethiopia", "Ethiopia",
                                                                      ifelse(observing == "Norway", "Hong Kong",
                                                                             ifelse(observing == ", China", "Iceland",
                                                                                    ifelse(observing == "Côte-d' Jordan", "Jordan",
                                                                                           ifelse(observing == "Moldova, Republic of", "Moldova",
                                                                                                  ifelse(observing == "Federation", "Mongolia",
                                                                                                         ifelse(observing == "Arabia", "Montenegro",
                                                                                                                ifelse(observing == "Africa Saint Lucia", "Saint Lucia",
                                                                                                                       ifelse(observing == "Lanka Slovakia", "Slovakia",
                                                                                                                              ifelse(observing == ", Islamic Republic of Sweden Slovenia", "Slovenia",
                                                                                                                                     ifelse(observing == ", United Republic of", "Sudan",
                                                                                                                                            ifelse(observing == "55 BP )(2010/09/21)", "Swaziland",
                                                                                                                                                   ifelse(observing == "( UNI and Tobago Yemen", "Yemen",
                                                                                                                                                          ifelse(observing == "Zimbabwe 7", "Zimbabwe", observing))))))))))))))))))))) %>%
  mutate(participating1 = ifelse(participating1 == "", NA, participating1),
         participating2 = ifelse(participating2 == "", NA, participating2),
         observing = ifelse(observing == "", NA, observing)) %>%
  select(participating1, participating2, observing) 

## Participating

val_participating_tc207 = c(val_file_207$participating1, val_file_207$participating2) %>%
  na.omit() %>%
  as.character() %>%
  tibble() %>%
  rename("country" = ".")

orig_participating_tc207 <- participation %>%
  filter(committee == "TC 207",
         year == 2009,
         membership %in% c("P-member"))

tc207_2010_p_false_negative <- val_participating_tc207 %>%
  mutate(correct = ifelse(country %in% orig_participating_tc207$country, 1, 0))

tc207_2010_p_false_positive <- orig_participating_tc207 %>%
  anti_join(val_participating_tc207)

## Observing

val_observing_tc207 = c(val_file_207$observing) %>%
  na.omit() %>%
  as.character() %>%
  tibble() %>%
  rename("country" = ".")

orig_observing_tc207 <- participation %>%
  filter(committee == "TC 207",
         year == 2009,
         membership %in% c("O-member"))

tc207_2010_o_false_negative <- val_observing_tc207 %>%
  mutate(correct = ifelse(country %in% orig_observing_tc207$country, 1, 0))

tc207_2010_o_false_positive <- orig_observing_tc207 %>%
  anti_join(val_observing_tc207)


#### CHECK VALIDATION ####

make_accuracy <- function(false_negative, false_positive){
  
  correct_count <- false_negative %>% count(correct) # Find correctly and misclassified countries (false negative, aka missing)
  false_positive_count <- false_positive %>% count() %>% pull(n) # Count the false negatives, aka wrongly imputed
  
  correct_count <- correct_count %>%
    add_row(correct = 0, n = 0) # In case all instanced are correct, still need this for function to run
  
  correct_count <- correct_count %>%
    distinct(correct, .keep_all = TRUE) %>%
    mutate(n = ifelse(correct == 0, n+false_positive_count, n)) # Add the false positive to wrongly classified
  
  correct_count <- correct_count %>%
    spread(correct, n) %>% 
    summarise(percentage = `1`/(`1`+`0`)*100) %>% # Find percentage of correctly classified, aka accuracy
    pull()

  return(correct_count)
  
}

tc34sc3_2016_p <- tibble(committee = "TC 34/SC 3",
                       year = 2016, 
                       membership = "P-member",
                       false_negative = tc34sc3_2016_p_false_negative %>% group_by(correct) %>% count() %>% filter(correct == 0) %>% pull(n),
                       false_positive = tc34sc3_2016_p_false_positive %>% count() %>% pull(n),
                       accuracy = make_accuracy(tc34sc3_2016_p_false_negative, tc34sc3_2016_p_false_positive),
                       total = participation %>%
                         filter(committee == "TC 34/SC 3",
                                year == 2016,
                                membership == "P-member") %>%
                         count() %>% pull(n))

tc34sc3_2016_o <- tibble(committee = "TC 34/SC 3",
                         year = 2016, 
                         membership = "O-member",
                         false_negative = tc34sc3_2016_o_false_negative %>% group_by(correct) %>% count() %>% filter(correct == 0) %>% pull(n), 
                         false_positive = tc34sc3_2016_o_false_positive %>% count() %>% pull(n),
                         accuracy = make_accuracy(tc34sc3_2016_o_false_negative, tc34sc3_2016_o_false_positive),
                         total = participation %>%
                           filter(committee == "TC 34/SC 3",
                                  year == 2016,
                                  membership == "O-member") %>%
                           count() %>% pull(n)) 

#

tc146sc2_2016_p <- tibble(committee = "TC 146/SC 2",
                         year = 2016, 
                         membership = "P-member",
                         false_negative = tc146sc2_2016_p_false_negative %>% group_by(correct) %>% count() %>% filter(correct == 0) %>% pull(n), 
                         false_positive = tc146sc2_2016_p_false_positive %>% count() %>% pull(n),
                         accuracy = make_accuracy(tc146sc2_2016_p_false_negative, tc146sc2_2016_p_false_positive),
                         total = participation %>%
                           filter(committee == "TC 146/SC 2",
                                  year == 2016,
                                  membership == "P-member") %>%
                           count() %>% pull(n)) 

tc146sc2_2016_o <- tibble(committee = "TC 146/SC 2",
                          year = 2016, 
                          membership = "O-member",
                          false_negative = 0, # All are correct: tc146sc2_2016_o_false_negative %>% group_by(correct) %>% count() %>% filter(correct == 0) %>% pull(n) 
                          false_positive = tc146sc2_2016_o_false_positive %>% count() %>% pull(n),
                          accuracy = make_accuracy(tc146sc2_2016_o_false_negative, tc146sc2_2016_o_false_positive),
                          total = participation %>%
                            filter(committee == "TC 146/SC 2",
                                   year == 2016,
                                   membership == "O-member") %>%
                            count() %>% pull(n)) 

#

tc258_2019_p <- tibble(committee = "TC 258",
                          year = 2019, 
                          membership = "P-member",
                          false_negative = tc258_2019_p_false_negative %>% group_by(correct) %>% count() %>% filter(correct == 0) %>% pull(n), 
                          false_positive = tc258_2019_p_false_positive %>% count() %>% pull(n),
                       accuracy = make_accuracy(tc258_2019_p_false_negative, tc258_2019_p_false_positive),
                       total = participation %>%
                         filter(committee == "TC 258",
                                year == 2019,
                                membership == "P-member") %>%
                         count() %>% pull(n))

tc258_2019_o <- tibble(committee = "TC 258",
                       year = 2019, 
                       membership = "O-member",
                       false_negative = 0, # All are correct: tc258_2019_o_false_negative %>% group_by(correct) %>% count() %>% filter(correct == 0) %>% pull(n), 
                       false_positive = tc258_2019_o_false_positive %>% count() %>% pull(n),
                       accuracy = make_accuracy(tc258_2019_o_false_negative, tc258_2019_o_false_positive),
                       total = participation %>%
                         filter(committee == "TC 258",
                                year == 2019,
                                membership == "O-member") %>%
                         count() %>% pull(n))

# 

tc211_2002_p <- tibble(committee = "TC 211",
                       year = 2002, 
                       membership = "P-member",
                       false_negative = tc211_2002_p_false_negative %>% group_by(correct) %>% count() %>% filter(correct == 0) %>% pull(n), 
                       false_positive = tc211_2002_p_false_positive %>% count() %>% pull(n),
                       accuracy = make_accuracy(tc211_2002_p_false_negative, tc211_2002_p_false_positive),
                       total = participation %>%
                         filter(committee == "TC 211",
                                year == 2003,
                                membership == "P-member") %>%
                         count() %>% pull(n))

tc211_2002_o <- tibble(committee = "TC 211",
                       year = 2002, 
                       membership = "O-member",
                       false_negative = tc211_2002_o_false_negative %>% group_by(correct) %>% count() %>% filter(correct == 0) %>% pull(n), 
                       false_positive = tc211_2002_o_false_positive %>% count() %>% pull(n),
                       accuracy = make_accuracy(tc211_2002_o_false_negative, tc211_2002_o_false_positive),
                       total = participation %>%
                         filter(committee == "TC 211",
                                year == 2003,
                                membership == "O-member") %>%
                         count() %>% pull(n))

#

tc201_2010_p <- tibble(committee = "TC 207",
                       year = 2010, 
                       membership = "P-member",
                       false_negative = tc207_2010_p_false_negative %>% group_by(correct) %>% count() %>% filter(correct == 0) %>% pull(n), 
                       false_positive = tc207_2010_p_false_positive %>% count() %>% pull(n),
                       accuracy = make_accuracy(tc207_2010_p_false_negative, tc207_2010_p_false_positive),
                       total = participation %>%
                         filter(committee == "TC 207",
                                year == 2010,
                                membership == "P-member") %>%
                         count() %>% pull(n))

tc201_2010_o <- tibble(committee = "TC 207",
                       year = 2010, 
                       membership = "O-member",
                       false_negative = tc207_2010_o_false_negative %>% group_by(correct) %>% count() %>% filter(correct == 0) %>% pull(n), 
                       false_positive = tc207_2010_o_false_positive %>% count() %>% pull(n),
                       accuracy = make_accuracy(tc207_2010_o_false_negative, tc207_2010_o_false_positive),
                       total = participation %>%
                         filter(committee == "TC 207",
                                year == 2010,
                                membership == "O-member") %>%
                         count() %>% pull(n))

bind_rows(tc211_2002_p, tc211_2002_o,
          tc201_2010_p, tc201_2010_o,
          tc146sc2_2016_p, tc146sc2_2016_o,
          tc34sc3_2016_p, tc34sc3_2016_o,
          tc258_2019_p, tc258_2019_o) %>%
  mutate(accuracy = round(accuracy, 2)) %>%
  rename("Technical committee" = "committee", 
         "Year" = "year",
         "Membership" = "membership",
         "False negative" = "false_negative",
         "False positive" = "false_positive",
         "Accuracy" = "accuracy",
         "Countries in committee" = "total") %>%
  kableExtra::kable("latex") %>%
  kableExtra::kable_styling(font_size = 9)

##### US SECRETARIAT ######

# query <- cdx_basic_query("http://www.ansi.org/standards_activities/iso_programs/held_secreteriats", # The URL for the first version
#                          match_type = "prefix", # Find all urls that start with the above
#                          collapse = NULL) # List all timestamps, not just the last one
# urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% 
#   pull(url) # Add to the URL an extension of the timestamp, this might as well be 000000 as it redirects automatically
# 
# dates <- query %>% pull(timestamp) # Find the dates
# ids <- substr(dates, 1, 10) # Add together to an id
# 
# walk2(urls, ids, function(link, id) { # Download the files to a local folder
#   
#   destfile <- paste0("../validation/data/", id, ".htm")
#   
#   if(!file.exists(destfile)){
#     
#     tryCatch({
#       download.file(link, destfile = destfile, quiet = TRUE)
#       Sys.sleep(5)
#       
#     }, error=function(e){message("Problematic page, skipped ", id)})
#     
#   }
# })

files <- list.files("./validation/data/", full.names = TRUE) %>%
  stringi::stri_remove_empty_na()

parse_secretariat <- function(page) {
  
  year <- str_extract(page, "[0-9]{4}")
  
  read_html(page) %>%
    html_elements("p") %>%
    html_text() %>%
    .[5:58] %>%
    str_extract("ISO.*") %>%
    stringi::stri_remove_na() %>%
    tibble() %>%
    mutate(committee = str_extract(., "ISO(/IEC)?( )?(JTC)?(/TC)?(/PC)?( )?[0-9]+(/SC [0-9]+)?")) %>%
    select(-.) %>%
    mutate(year = as.numeric(year))

}

us_secretariat <- lapply(files, parse_secretariat)
us_secretariat_validation <- bind_rows(us_secretariat) %>%
  mutate(committee = str_remove(committee, "ISO/"),
         committee = str_remove(committee, "IEC ")) %>%
  filter(!str_detect(committee, "PC")) %>%
  mutate(committee = ifelse(committee == "JTC1/SC 34", "JTC 1/SC 34",
                            ifelse(committee == "JTC1/SC 22", "JTC 1/SC 22",
                                   ifelse(committee == "JTC1/SC 11", "JTC 1/SC 11", committee)))) %>%
  mutate(committee_year = str_c(committee, "_", year)) %>%
  na.omit()

us_secretariat_original <- participation %>%
  filter(country == "United States",
         membership == "Secretariat") %>%
  mutate(committee_year = str_c(committee, "_", year))

us_validation1 <- us_secretariat_validation %>%
  mutate(Captured = factor(ifelse(committee_year %in% us_secretariat_original$committee_year, 1, 0))) 

us_validation1 %>%
  #filter(year >= 2004) %>%
  mutate(Captured = ifelse(Captured == 0, "No", 
                           ifelse(Captured == 1, "Yes", Captured))) %>%
  ggplot(aes(year, committee, fill = Captured)) +
  geom_tile() +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("lightgrey", "darkgrey")) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(filename = "./figures/us_validation.pdf", height = 10, width = 12)

us_validation1 %>%
  #filter(year >= 2004) %>%
  count(Captured) %>%
  spread(Captured, n) %>%
  mutate(percent = `1`/(`0`+`1`)*100)

# us_validation2 <- us_secretariat_original %>%
#   mutate(ok = factor(ifelse(committee_year %in% us_secretariat_validation$committee_year, 1, 0))) 
# 
# us_validation2 %>%
#   ggplot(aes(year, committee, fill = ok)) +
#   geom_tile()

