
membertype <- openxlsx::read.xlsx("../raw_data/members-historical/membership_color.xlsx") %>%
  mutate(
    across(everything(), ~replace_na(.x, "U"))
  ) %>%
  rename(country = year) %>%
  gather(2:70,
         key = year, value = status) %>%
  mutate(country = ifelse(country == "lceland", "Iceland", country)) %>%
  filter(!country %in% c("Czechoslovakia", "ICAITI", "Serbia and Montenegro")) %>%
  mutate(continent = countrycode::countrycode(sourcevar = country,
                                              origin = "country.name",
                                              destination = "continent"))

memberfunction <- openxlsx::read.xlsx("../raw_data/members-historical/membership_sign.xlsx") %>%
  mutate(
    across(everything(), ~replace_na(.x, "U"))
  ) %>%
  rename(country = year) %>%
  gather(2:70,
         key = year, value = func) %>%
  mutate(country = ifelse(country == "lceland", "Iceland", country)) %>%
  filter(!country %in% c("Czechoslovakia", "ICAITI", "Serbia and Montenegro")) %>%
  mutate(continent = countrycode::countrycode(sourcevar = country,
                                              origin = "country.name",
                                              destination = "continent"))

memberships <- left_join(membertype, memberfunction, join_by(country, year, continent))

saveRDS(memberships, file = "../datasets/memberships.rds")
