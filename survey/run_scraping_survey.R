
##########################################
######## SCRAPE CERTIFICATES #############
##########################################

## Every year we perform a survey of certifications to our management system standards. 
## The survey shows the number of valid certificates to ISO management standards (such as ISO 9001 and ISO 14001) reported for each country, each year.
## ISO does not perform certification. Organizations looking to get certified to an ISO standard must contact an independent certification body. 
## The ISO Survey counts the number of certificates issued by certification bodies that have been accredited by members of the International Accreditation Forum (IAF).

## DISCLAIMER: 
# The ISO Survey is not a database. The providers of the data are the certification bodies
# accredited by IAF members and they participate on a voluntary basis. The level of
# participation fluctuates from one edition of the survey to another and can impact the survey
# results especially at the country level. Interpretations of the results and any conclusions
# on the trends should be made with these considerations in mind

## Survey can be found at ISO's archive: https://isotc.iso.org/livelink/livelink?func=ll&objId=18808772&objAction=browse&sort=name&viewType=1

library(tidyverse)
setwd("C:/Users/solvebjo/OneDrive - Universitetet i Oslo/PhD/Paper 2 - Standards/after_feedback/data/ISO survey")

## SCRIPT 1:

## Scrape from excel sheets the number of certificates per country and per industry. To various degrees, the following committees are covered in the ISO Survey:
# ISO 9001 — Quality management systems (country: 1993-2020, industry: 1998-2020)
# ISO 14000 — Environmental management (country: 1999-2020, industry: 1998-2020)
# ISO/IEC 27001 — Information security management (country: 2006-2020, industry: 2006-2020)
# ISO 50001 — Energy management (country: 2011-2020, industry: 2015-2020)
# ISO 22000 — Food safety management (country: 2007-2020, industry: --)
# ISO 13485 — Medical devices — Quality management systems (country: 2004-2020, industry: --)
# ISO 22301 — Security and resilience (country: 2014-2020, industry: 2014-2020)
# ISO/IEC 20000-1 — Information technology (country: 2015-2020, industry: 2015-2020)
# ISO 28000 — Specification for security management systems for the supply chain (country: 2016-2020, industry: 2016-2020)
# ISO 39001 — Road traffic safety (RTS) management systems (country: 2016-2020, industry: 2016-2020)

source("country_and_industry.R")

## SCRIPT 2:

## Scrape from excel sheets the number of certificates per industry in each country. Limited to ISO 9001, ISO 14001 and ISO/IEC 27001, except for years 2018, 2019 and 2020.
# This survey is of quite dubious quality:
# Afghanistan is not available for 2012
# The 2015 survey has too problematic quality
# There is no data for 2016

source("country_per_sector.R")
