
################################################################################################################################
#################################                    DOWNLOAD MEMBER WEBPAGES                     ##############################
################################################################################################################################

##### FIRST VERSION #####
#### 2001 - 2007 ####

query <- cdx_basic_query("http://www.iso.org:80/iso/en/stdsdevelopment/tcpartip/MemberParticipationDetailPage.MemberParticipationDetail?", # The URL for the first version
                         match_type = "prefix", # Find all urls that start with the above
                         collapse = NULL) %>% # List all timestamps, not just the last one
  filter(str_detect(original, "MEMBER=")) # Extract the member from the link
urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% 
  pull(url) # Add to the URL an extension of the timestamp, this might as well be 000000 as it redirects automatically

member <- urls %>% str_extract("MEMBER=[A-Za-z]+") %>% str_remove("MEMBER=") # Make a string with just member number
dates <- query %>% pull(timestamp) # Find the dates
ids <- str_c(member, "_", dates) # Add together to an id

walk2(urls, ids, function(link, id) { # Download the files to a local folder
  
  destfile <- paste0("../raw_data/archive_members/version1/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
      
    }, error=function(e){message("Problematic page, skipped ", id)})
    
    }
})


##### SECOND VERSION #####
#### 2008 - 2012 ####

query <- cdx_basic_query("http://www.iso.org/iso/about/iso_members/iso_member_participation_tc.htm", match_type = "prefix", collapse = NULL) %>%
  filter(str_detect(original, "member_id"))
urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

member <- urls %>% str_extract("member_id=[0-9]+") %>% str_remove("member_id=")
dates <- query %>% pull(timestamp)
ids <- str_c(member, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_members/version2/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
      }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
})

# ## PDCs ##
# 
# query <- cdx_basic_query("http://www.iso.org/iso/home/about/iso_members/iso_member_participation_pdc.htm", match_type = "prefix", collapse = NULL) %>%
#   filter(str_detect(original, "member_id"))
# urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)
# 
# member <- urls %>% str_extract("member_id=[0-9]+") %>% str_remove("member_id=")
# dates <- query %>% pull(timestamp)
# ids <- str_c(member, "_", dates)
# 
# walk2(urls, ids, function(link, id) {
#   
#   destfile <- paste0("../raw_data/archive_members/version2/", id, "_pdc", ".htm")
#   
#   if(!file.exists(destfile)){
#     
#     tryCatch({
#       download.file(link, destfile = destfile, quiet = TRUE)
#       Sys.sleep(5)
#     }, error=function(e){message("Problematic page, skipped ", id)})
#     
#   }
# })


##### THIRD VERSION #####
#### 2013 - 2016 ####

## TCs ## 

query <- cdx_basic_query("http://www.iso.org/iso/home/about/iso_members/iso_member_participation_tc.htm", match_type = "prefix", collapse = NULL) %>%
  filter(str_detect(original, "member_id"))
urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

member <- urls %>% str_extract("member_id=[0-9]+") %>% str_remove("member_id=")
dates <- query %>% pull(timestamp)
ids <- str_c(member, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_members/version3/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
      }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
})

# ## PDCs ##
# 
# query <- cdx_basic_query("http://www.iso.org/iso/home/about/iso_members/iso_member_participation_pdc.htm", match_type = "prefix", collapse = NULL) %>%
#   filter(str_detect(original, "member_id"))
# urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)
# 
# member <- urls %>% str_extract("member_id=[0-9]+") %>% str_remove("member_id=")
# dates <- query %>% pull(timestamp)
# ids <- str_c(member, "_", dates)
# 
# walk2(urls, ids, function(link, id) {
#   
#   destfile <- paste0("../raw_data/archive_members/version3/", id, "_pdc", ".htm")
#   
#   if(!file.exists(destfile)){
#     
#     tryCatch({
#       download.file(link, destfile = destfile, quiet = TRUE)
#       Sys.sleep(5)
#     }, error=function(e){message("Problematic page, skipped ", id)})
#     
#   }
# })


##### FOURTH VERSION #####
#### 2017 - 2023 ####

### TC ###
query <- cdx_basic_query("https://www.iso.org/committee/", match_type = "prefix", collapse = NULL) %>%
  filter(str_detect(original, "view=participation"))
urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

tc <- urls %>% str_extract("committee\\/[0-9]+") %>% str_remove("committee\\/")
dates <- query %>% pull(timestamp)
ids <- str_c(tc, "_", dates)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_members/version4/tc_site/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
    }, error=function(e){message("Problematic page, skipped ", id)})
    
  }
})

### MEMBER ###

query <- cdx_basic_query("https://www.iso.org/member/", match_type = "prefix", collapse = NULL) %>%
  filter(str_detect(original, "view=participation"))
urls <- query %>% mutate(url = str_c("http://web.archive.org/", str_replace_all(as.character(timestamp), "-", ""), "000000/", original)) %>% pull(url)

member <- urls %>% str_extract("member\\/[0-9]+") %>% str_remove("member\\/")
dates <- query %>% pull(timestamp)
membership <- urls %>% str_extract("=[A-Z]+") %>% str_remove("=")
ids <- str_c(member, "_", dates, "_", membership)

walk2(urls, ids, function(link, id) {
  
  destfile <- paste0("../raw_data/archive_members/version4/member_site/", id, ".htm")
  
  if(!file.exists(destfile)){
    
    tryCatch({
      download.file(link, destfile = destfile, quiet = TRUE)
      Sys.sleep(5)
      }, error=function(e){message("Problematic page, skipped ", id)})
  
    }
})

#### Extras ####

# Version 4 of the webpage has a structure that gives a separate link to participating, observing and secretariat webpages
# Since wayback doesn't store all webpages, some information might be missing, but we don't know which

# member <- urls %>% str_extract("member\\/[0-9]+") %>% str_remove("member\\/") %>% unique()
# timestamps <- c("20170101000000", "20180101000000", "20190101000000", "20200101000000", "20210101000000")
# 
# member_list <-rep(member, each = length(timestamps))
# stamps_list <- rep(timestamps, length(member))
# 
# version4_tables <- tibble(member = member_list,
#                           timestamps = stamps_list) %>%
#   rowid_to_column()
# 
# extras <- list()
# 
# for (i in 1:length(version4_tables$rowid)) {
# 
#   extras[[i]] <- read_html(str_c("http://web.archive.org/web/", version4_tables$timestamps[i], "/https://www.iso.org/member/", version4_tables$member[i], ".html")) %>%
#     html_elements("body > div > div > div > ul > li > a") %>%
#     as.character() %>%
#     str_extract("t=.*") %>%
#     str_remove_all('t=|>|</a>') %>%
#     str_replace_all('\\"', "_") %>%
#     str_c(., "+", version4_tables$member[i], "-", str_extract(version4_tables$timestamps[i], "[0-9]{4}"))
# 
#   Sys.sleep(5)
# 
#   message("Finished no. ", i)
# 
# }
# 
# extra_links <- extras %>% unlist()
# 
# extra_links_tibble <- tibble(membership = str_extract(extra_links, "[A-Za-z]+ [A-Za-z]+"),
#                              webid = str_remove(str_extract(extra_links, "\\+[0-9]{4}"), "\\+"),
#                              year = str_remove(str_extract(extra_links, "-[0-9]{4}"), "-")) %>%
#   na.omit()
# 
# saveRDS(extra_links, file = "../raw_data/archive_members/extralinks_version4.rds")


##### CURRENT VERSION #####
#### 2023 ####

urls <- read_html("https://www.iso.org/members.html") %>%
  html_elements("tbody > tr > td > a") %>%
  html_attr("href") %>%
  str_c("https://www.iso.org", .)

links <- list()

for (i in 1:length(urls)) {
  
  links[[i]] <- read_html(urls[i]) %>%
    html_elements("div > span > a") %>%
    html_attr("href") %>%
    str_c("https://www.iso.org", .)
  
  Sys.sleep(5)
  
}

urls <- links %>% unlist() 
membership <- urls %>% str_extract("=[A-Z]+") %>% str_remove("=")
member <- urls %>% str_extract("member\\/[0-9]+") %>% str_remove("member\\/")
ids <- str_c(member, "_", "2023-07-26", "_", membership)

pages <- tibble(urls, ids) %>% na.omit() 

walk2(pages$urls, pages$ids, function(link, id) {
  
  tryCatch({
    
    download.file(link, destfile = paste0("../raw_data/archive_members/current-2023-07-26/", id, ".htm"), quiet = TRUE)
    Sys.sleep(5)
    
  }, error=function(e){message("Problematic page, skipped ", id)})

  })

