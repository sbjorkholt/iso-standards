
##############################
#####  SCRAPE MEMBERS  #######
##############################

library(tidyverse)
library(wayback)
library(rvest)
library(httr)

extract_acronyms <- function(input_string) {
  pattern <- "\\b(\\-)?.*(?:\\w+(?:, )?)+\\b(\\-)?"
  result <- str_extract(input_string, pattern)
  return(result)
}

#### ISO's webpage has changed four times since they bought the www.iso.org domain
### The scraping and parsing is informed by these changes

# VERSION 1: 2001 - 2007
# VERSION 2: 2008 - 2012
# VERSION 3: 2013 - 2016
# VERSION 4: 2017 - 2022

## STEP 1:
# Download html webpages using current www.iso.org and Wayback's snapshots of www.iso.org back in time into your own local folder
# Using Wayback's API 

source("1_download_archive_webpages_liaison.R")


## STEP 2:
# Parse from the webpages info on each member and add to dataframe

source("2_exctract_organization.R.R")


## STEP 3:
# Clean data, including (1) fixing name misalignments, (2) handling missing, (3) imputation and (4) final checks

source("3_cleaning.R")

