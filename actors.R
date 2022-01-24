
pacman::p_load(tidyverse, rvest)

########################################################
##################### COUNTRIES ########################
########################################################

html <- read_html("https://www.iso.org/members.html") 

country_html <- html %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_extract("member/[0-9]+") %>%
  stringi::stri_omit_na() %>%
  str_c("https://www.iso.org/", .)

member <- list()
address <- list()
observing <- list()
participating <- list()
secretariat <- list()

for (i in 1:length(country_html)) {
  
  tryCatch({
    
    member[[i]] <- read_html(str_c(country_html[i], ".html")) %>%
      html_node("#content > section.section-navigation > div > div > div > nav > h2") %>%
      html_text2()
    
    address[[i]] <- read_html(str_c(country_html[i], ".html")) %>%
      html_node("#section-details > div > div.col-md-4.col-md-offset-1.top-md-push-4 > div") %>%
      html_text2()
    
    observing[[i]] <- read_html(str_c(country_html[i], ".html?view=participation&t=OT")) %>%
      html_node("#datatable-participation") %>%
      html_table()
    
    participating[[i]] <- read_html(str_c(country_html[i], ".html?view=participation&t=PT")) %>%
      html_node("#datatable-participation") %>%
      html_table()

    secretariat[[i]] <- read_html(str_c(country_html[i], ".html?view=participation&t=S")) %>%
      html_node("#datatable-participation") %>%
      html_table()
    
  }, error = function(e){""})
  
  print(paste(i))
  
  Sys.sleep(5)
  
}

secretariat3 <- list()

for (i in 1:length(secretariat)) {
  if(is.null(secretariat[[i]])) {
      print("null")
    } else {
      secretariat3[[i]] <- secretariat[[i]] %>% mutate(Committee = as.character(Committee), Title = as.character(Title)) 
    }
}

participating3 <- list()

for (i in 1:length(participating)) {
  
  if(is.null(secretariat[[i]])) {
    print("null")
  } else {
    participating3[[i]] <- participating[[i]] %>% mutate(Committee = as.character(Committee), Title = as.character(Title))
  }
}

observing3 <- list()

for (i in 1:length(observing)) {
  if(is.null(secretariat[[i]])) {
    print("null")
  } else {
    observing3[[i]] <- observing[[i]] %>% mutate(Committee = as.character(Committee), Title = as.character(Title))
  }
}

members_s <- tibble(member = member,
                    address = address,
                    secretariat = secretariat3,
                    participating = participating3,
                    observing = observing3)

secretariat_members <- members_s %>%
  select(member, address, secretariat) %>% 
  unnest(cols = c(member, address, secretariat)) %>%
  mutate(role = "secretariat")

participating_members <- members_s %>%
  select(member, address, participating) %>% 
  unnest(cols = c(member, address, participating)) %>%
  mutate(role = "participating")

observing_members <- members_s %>%
  select(member, address, observing) %>% 
  unnest(cols = c(member, address, observing)) %>%
  mutate(role = "observing")

members <- bind_rows(secretariat_members, participating_members, observing_members) %>%
  group_by(Committee, Title) %>%
  nest(data = c(member, address, role))


############################################################
##################### ORGANIZATIONS ########################
############################################################

html <- read_html("https://www.iso.org/organizations-in-cooperation-with-iso.html?f=FULL")

committee_html <- html %>%
  html_nodes("#datatable-orgs > tbody") %>%
  html_elements("a") %>%
  as.character() %>%
  str_extract_all("/organization/[0-9]+.html") %>%
  str_c("https://www.iso.org", .)

iso_org <- list()
country <- list()
acronym <- list()
org <- list()

for (i in 1:length(committee_html)) {
  
  tryCatch({
    
    link <- read_html(committee_html[[i]])
    
    iso_org[[i]] <- link %>%
      html_node("#datatable-participations > tbody") %>%
      html_table()
    
    country[[i]] <- link %>%
      html_node("#content > section.section-navigation > div > div > div.col-md-4.col-md-offset-1 > div > p:nth-child(1)") %>%
      html_text2()
    
    acronym[[i]] <- link %>%
      html_node("nav > h1") %>%
      html_text()
    
    org[[i]] <- link %>%
      html_node("nav > h2") %>%
      html_text()
    
  }, error = function(e){"nope"})
  
  print(paste(i))
  
  Sys.sleep(5)
  
}

country2 <- str_split(country, "\\n")

country3 <- list()

for (i in 1:length(country2)) {
  country3[[i]] <- tail(country2[[i]], 1)
}

liaison_s <- tibble(acronym = acronym, 
                    org = org,
                    country = country3,
                    iso_org = iso_org)

liaison <- liaison_s %>%
  unnest(cols = c(acronym, org, country, iso_org)) %>%
  rename(Committee = X1,
         Title = X2,
         liaison_group = X3) %>%
  group_by(Committee, Title) %>%
  nest(data = c(acronym, org, country, liaison_group))
