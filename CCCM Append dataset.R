### Script that takes V1 and V2 files and appends them to Masters
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V2
# 21/05/2020

rm(list=ls())
today <- Sys.Date()

require(tidyverse)
require(openxlsx)
require(stringr)

source("./R/moveme.R")

### Load kobo choices file to vlook up old site codes
choices <- read.csv("./data/kobo/choices.csv", check.names = F)
external_choices <- read.csv("./data/kobo/external_choices.csv", check.names = F)

external_choices <- filter(external_choices, external_choices$list_name == "sitename")
names(external_choices)[names(external_choices) == "name"] <- "a4_site_code"
names(external_choices)[names(external_choices) == "label::english"] <- "a4_site_name"


### Load latest DB files to be merged
last_db_V1 <- read.xlsx("./output/dashboard/CCCM_Site Reporting_V1_2020-05-14.xlsx")
last_db_V2 <- read.xlsx("./output/dashboard/CCCM_Site Reporting_V2_2020-05-13.xlsx")

## Rename columns so that they match and delete extra ones
names(last_db_V1)[names(last_db_V1) == "b4_site_smc_agency_name"] <- "b2_site_smc_agency_name"
names(last_db_V1)[names(last_db_V1) == "b5_smc_agency_fp_name"] <- "b3_smc_agency_fp_name"
names(last_db_V1)[names(last_db_V1) == "b6_smc_agency_fp_mobile_number"] <- "b4_smc_agency_fp_mobile_number"
names(last_db_V1)[names(last_db_V1) == "b7_community_committee_in_place"] <- "b5_community_committee_in_place"
names(last_db_V1)[names(last_db_V1) == "b8_community_committee_fp_name"] <- "b6_community_committee_fp_name"
names(last_db_V1)[names(last_db_V1) == "b9_community_committee_fp_cell"] <- "b7_community_committee_fp_cell"

drops <- c("b4_site_smc_agency_name", "b5_smc_agency_fp_name", "b6_smc_agency_fp_mobile_number", "b7_community_committee_in_place", 
           "b8_community_committee_fp_name", "b9_community_committee_fp_cell")

last_db_V2 <- last_db_V2[ , !(names(last_db_V2) %in% drops)]
new_db <- rbind(last_db_V1, last_db_V2)

## Delete unnecessary column and add new one
new_db$X__version__ <- NULL
new_db$X_validation_status <- NULL
new_db$comments <- ""

## Trim leading and trailing spaces
new_db$a4_site_name <- str_trim(new_db$a4_site_name)

## SiteID codes
## Load site ID master list
id_list <- read.xlsx("./output/id master list/SiteIDCodes MasterList V2.xlsx")
id_list$a4_site_name <- str_trim(id_list$a4_site_name)

## Replace the site ID with those in the master list 
## How to VlookUp in R: https://www.rforexcelusers.com/vlookup-in-r/
new_db$a4_site_code<- id_list$a4_site_code[match(new_db$a4_site_name, id_list$a4_site_name)]


