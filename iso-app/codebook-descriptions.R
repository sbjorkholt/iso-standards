
overall_cb <- tibble("Category" = c("Standards", "Participation", "Historical", "Certifications"),
                     "Time series" =c("1947 - 2023", "2002/4 - 2023", "1947 - 2015", "1993 – 2020, but varies depending on ISO series."),
                     "Description" = c("Data on specific standards, including which technical committee that developed them, the life cycle of their production, year they were published, edition, number of pages, whether they have been withdrawn, abstract, sustainability goals and ICS code.",
                                       "Data on the actors that participate in producing standards (i.e. in technical committees). There is one dataset on the countries (i.e. national member bodies) and one on the organizations in liaison.",
                                       "Data on the historical development of ISO. One dataset includes membership in ISO over time, including type of membership and function of membership. One dataset shows when different technical committees were established.",
                                       "Data on certifications of ISO standards. This includes data on the year of the survey, number of certificated provided by accredited certification bodies per country, industry and ISO standard series. The ISO Survey covers a selection of the ISO standard series. An overview of coverage and time series per coverage is given below."),
                     "Source and method" = c("www.iso.org. With sublinks to every standard. Collected through webscraping using rvest.", 
                                             "Wayback Machine. Collected through webscraping using wayback, rvest and httr.", 
                                             "Membership parsed from pdf. TC establishment scraped from iso.org and missing categories were categorized using ChatGPT.", 
                                             "The ISO Survey. The survey data is parsed from excel files."),
                     "Comments" = c("The data has been subject to significant amounts of data cleaning.",
                                    "Because there is only a selection of snapshots of webpages in the archive, the data is incomplete. Imputation methods based on the collected data replaces for some of the missing values. The cells that have been imputed are indicated. The categorization of liaison organizations was made using ChatGPT. The data has been subject to significant amounts of data cleaning.",
                                    "The data has been subject to moderate amounts of data cleaning.",
                                    "From ISO: 'Every year we perform a survey of certifications to our management system standards. The survey shows the number of valid certificates to ISO management standards (such as ISO 9001 and ISO 14001) reported for each country, each year. [...]
                                    The ISO Survey is not a database. The providers of the data are the certification bodies accredited by IAF members and they participate on a voluntary basis. The level of  participation fluctuates from one edition of the survey to another and can impact the survey results especially at the country level. Interpretations of the results and any conclusions on the trends should be made with these considerations in mind. The data has been subject to moderate amounts of data cleaning."))

# data.frame(overall_cb) %>% kable("latex", booktabs = TRUE, escape = FALSE, caption = "Overview of the datasets in the StanDat database.") %>%
#   kable_styling(full_width = FALSE, font_size = 8) %>%
#   column_spec(1, width = "5em") %>%
#   column_spec(2, width = "5em") %>%
#   column_spec(3, width = "22em") %>%
#   column_spec(4, width = "10em") %>%
#   column_spec(5, width = "26em")


standards_cb <- tibble("Variable" = c("stdno", "year", "title", "committee", "status", "publication_date", "edition", "pages",
                                   "abstract", "ics_number", "ics_text", "link",
                                   "One row represents: "),
                    "Description" = c("Standard number", "Year the standard was published (standards under development are NA)",
                                      "Name of the TC the standard was developed within", 
                                      "ID of the TC the standard was developed within",
                                      "If the standard is withdrawn, deleted, developing or published",
                                      "When the standard was published (if published)",
                                      "The edition of the standard",
                                      "Number of pages of the standard",
                                      "Abstract of the contents of the standard",
                                      "Name of the ICS code the standard is categorized into (can be more than one)", 
                                      "ID of the ICS code the standard is categorized into (can be more than one)",
                                      "Link to the webpage where the information was scraped",
                                      "One standard"))

sdgs_cb <- tibble("Variable" = c("stdno", "year", "title", "committee",
                              "sgd_number", "sgd_text", "link",
                              "One row represents: "),
               "Description" = c("Standard number", "Year the standard was published (standards under development are NA)",
                                 "Name of the TC the standard was developed within", 
                                 "ID of the TC the standard was developed within",
                                 "Number of the sustainability goal that ISO reports the standard to contribute to (if any).",
                                 "Name of the sustainability goal that ISO reports the standard to contribute to (if any).",
                                 "Link to the webpage where the information was scraped",
                                 "One standard's SDG goals"))

life_cycle_cb <- tibble("Variable" = c("stdno", "year", "title", "committee",
                              "life_stage", "life_stage_code", "date", "link",
                              "One row represents: "),
               "Description" = c("Standard number", "Year the standard was published (standards under development are NA)",
                                 "Name of the TC the standard was developed within", 
                                 "ID of the TC the standard was developed within",
                                 "Stage reported in the life cycle of a given standard",
                                 "The code of the stage reported in the life cycle of a given standard",
                                 "Date that the standard was at this life cycle stage",
                                 "Link to the webpage where the information was scraped",
                                 "One standard's life cycle stage"))

participants_cb <- tibble("Variable" = c("year", "acronym", "country", "committee", "title", "membership", 
                                         "impute", "sector",
                                         "One row represents: "),
                       "Description" = c("Year of membership", 
                                         "Name of main standardization developing organization in the country",
                                         "Country name", 
                                         "Number of the TC the country participates in",
                                         "Name of the TC the country participates in", 
                                         "Type of membership, either participating (P-member), observing (O-member), secretariat or twinned secretariat",
                                         "Whether memberships were imputed",
                                         "The sector that ISO categorizes the TC into",
                                         "One country (national member body) in a given year"))

liaison_cb <- tibble("Variable" = c("year", "name", "country", "acronym", "address", "committee", "title", 
                                    "type", "impute", "sector",
                                 "One row represents: "),
                  "Description" = c("Year of liaison", "Name of organization", "Country where the organization is located",
                                    "Organization's acronym", "Address of the organization",
                                    "Number of the committee that the organization was in liaison with", 
                                    "Name of the committee that the organization was in liaison with",
                                    "Type of liaison for the given organization", 
                                    "Whether memberships were imputed",
                                    "The sector that ISO categorizes the TC into",
                                    "One organization in a given year"))

member_historical_cb <- tibble("Variable" = c("year", "country", "continent", "membership_status", "membership_role",
                                              "One row represents: "),
                            "Description" = c("Year", "Country", "Continent of country",
                                              "Which membership status the country had in the given year. U = No membership, M = membership, C = Correspondent member, S = Subscriber member.",
                                              "If there were any particular changes to the membership in the given year. with = Withdrawn, sus = Suspended, council = Council.",
                                              "One country in a given year"))

tc_historical_cb <- tibble("Variable" = c("year", "title", "committee", "sector",
                                          "One row represents: "),
                        "Description" = c("Year of establishment", "Name of committee", "ID of committee",
                                          "Sector that ISO categorizes the TC into",
                                          "One technical committee (TC) in a given year"))

country_certifications_cb <- tibble("Variable" = c("country", "year", "certificates", "iso", "iso_name",
                                                   "One row represents: "),
                                 "Description" = c("Country name", "Year of survey", 
                                                   "Number of certificates as provided by accredited certification bodies in the ISO Survey",
                                                   "Code of ISO management standards series", "Name of ISO management standards series",
                                                   "One country in a given year"))

industry_certifications_cb <- tibble("Variable" = c("industry", "year", "certificates", "iso", "iso_name",
                                                    "One row represents: "),
                                  "Description" = c("Aggregate industry level", "Year of survey", 
                                                    "Number of certificates as provided by accredited certification bodies in the ISO Survey",
                                                    "Code of ISO management standards series", "Name of ISO management standards series",
                                                    "One industry group in a given year"))

country_per_industry_certifications_cb <- tibble("Variable" = c("country", "year", "industry", "ISO 9001", "ISO 14001", "ISO/IEC 27001",
                                                                "One row represents: "),
                                              "Description" = c("Country name", "Year of survey", "Aggregate industry level", 
                                                                "Number of certificates within the ISO 9001 series", 
                                                                "Number of certificates within the ISO 14001 series", 
                                                                "Number of certificates within the ISO/IEC 27001 series",
                                                                "One country's industry level in a given year"))

