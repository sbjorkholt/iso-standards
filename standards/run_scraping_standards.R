
##############################
##### SCRAPE STANDARDS #######
##############################

library(tidyverse)
library(rvest)
library(vroom)

## STEP 1:
# Download html webpages from www.iso.org into your own local folder

source("1_download_webpages.R")

## STEP 2:
# Extract the information on status and life cycle from the html-pages

source("2_extract_info.R")

## STEP 3:
# Parse and collect the data into one dataframe

source("3_gather_data.R")





