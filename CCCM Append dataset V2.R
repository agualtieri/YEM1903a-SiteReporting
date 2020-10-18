### Script that takes V1 and V2 files and appends them to Masters
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V2
# 29/07/2020

## Useful resource
# https://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in

rm(list=ls())
today <- Sys.Date()

require(tidyverse)
require(openxlsx)

source("./R/moveme.R")

### Load kobo choices file to vlook up old site codes
choices <- read.csv("./data/kobo/choices.csv", check.names = FALSE)
external_choices <- read.csv("./data/kobo/choices_external.csv", check.names = FALSE)

external_choices <- filter(external_choices, external_choices$list_name == "sitename")
names(external_choices)[names(external_choices) == "name"] <- "a4_site_code"
names(external_choices)[names(external_choices) == "label::english"] <- "a4_site_name"

## Load site ID master list
id_list <- read.xlsx("./output/id master list/SiteIDCodes MasterList V2 - July 2020.xlsx")
names(id_list)[names(id_list) == "site.name"] <- "a4_site_name"
names(id_list)[names(id_list) == "new.code"] <- "a4_site_code"
id_list$a4_site_name <- str_trim(id_list$a4_site_name)

## Load empty dataframes for rbind
empty_internal <- read.xlsx("./output/empty dfs/CCCM_SiteReporting_All Internal (WithID)_blank.xlsx")
empty_external <- read.xlsx("./output/empty dfs/CCCM_SiteReporting_All External (WithID)_blank.xlsx")
empty_dashboard <- read.xlsx("./output/empty dfs/CCCM_SiteReporting_All DB (WithID)_blank.xlsx")

########################################################### INTERNAL ##############################################################################

### Load Last Cleaned files ###
## Internal
last_internal_v1 <- read.xlsx("./output/internal/CCCM_SiteReporting_V1 Internal_2020-10-11.xlsx")
last_internal_v2 <- read.xlsx("./output/internal/CCCM_SiteReporting_V2 Internal_2020-10-11.xlsx")

## Rename vars to match
names(last_internal_v1)[names(last_internal_v1) == "b4_site_smc_agency_name"] <- "b2_site_smc_agency_name"
names(last_internal_v1)[names(last_internal_v1) == "b7_community_committee_in_place"] <- "b3_smc_agency_fp_name"

## Delete useless cols
last_internal_v2$b4_site_smc_agency_name <- NULL
last_internal_v2$b7_community_committee_in_place <- NULL

## Merge files
final_internal <- plyr::rbind.fill(empty_internal, last_internal_v1, last_internal_v2)
final_internal$country_name <- "Yemen"
final_internal$country_id <- "YE"

### Internal Site ID code
final_internal$a4_site_name <- str_trim(final_internal$a4_site_name, "both")
final_internal$a4_site_code <- external_choices$a4_site_code[match(final_internal$a4_site_name, external_choices$a4_site_name)]
final_internal <- final_internal[order(final_internal$a4_site_code, final_internal$a4_site_name, na.last = FALSE),]

final_internal <- final_internal %>% mutate(a4_site_code = ifelse(is.na(a4_site_code) == TRUE, paste0(a2_district_code,"_",seq(1185, 5000)), paste0(a4_site_code)))
final_internal$a4_site_code

## Fix codes based on master list
## Trim leading and trailing spaces
final_internal$a4_site_name <- str_trim(final_internal$a4_site_name)

## Replace the site ID with those in the master list 
## How to VlookUp in R: https://www.rforexcelusers.com/vlookup-in-r/
final_internal$master_list <- id_list$a4_site_code[match(final_internal$a4_site_name, id_list$a4_site_name)]
final_internal <- final_internal %>% mutate(id_check = ifelse(a4_site_code == master_list, a4_site_code, master_list)) %>%
  mutate(a4_site_code_final = ifelse(is.na(id_check), a4_site_code, id_check))

final_internal$a4_site_code <- final_internal$a4_site_code_final

final_internal$master_list <- NULL
final_internal$id_check <- NULL
final_internal$a4_site_code_final <- NULL

## Save file
write.xlsx(final_internal, paste0("./output/internal/CCCM_SiteReporting_All Internal (WithID)_",today,".xlsx"))


########################################################### EXTERNAL ##############################################################################

### Load Last Cleaned files ###
## External
last_external_v1 <- read.xlsx("./output/external/CCCM_SiteReporting_V1 External_2020-10-11.xlsx")
last_external_v2 <- read.xlsx("./output/external/CCCM_SiteReporting_V2 External_2020-10-11.xlsx")

## Create V1 and V2 variable
last_external_v1$kobo_version <- "V1"
last_external_v2$kobo_version <- "V2"

last_external_v2$b7_community_committee_fp_cell <- NULL

## Merge files
final_external <- plyr::rbind.fill(empty_external, last_external_v1, last_external_v2)
final_external$country_name <- "Yemen"
final_external$country_id <- "YE"

### External Site ID code
final_external$a4_site_name <- str_trim(final_external$a4_site_name, "both")
final_external$a4_site_code <- external_choices$a4_site_code[match(final_external$a4_site_name, external_choices$a4_site_name)]
final_external <- final_external[order(final_external$a4_site_code, final_external$a4_site_name, na.last = FALSE),]

final_external <- final_external %>% mutate(a4_site_code = ifelse(is.na(a4_site_code) == TRUE, paste0(a2_district_code,"_",seq(1185,5000)), paste0(a4_site_code)))
final_external$a4_site_code

## Fix codes based on master list
## Trim leading and trailing spaces
final_external$a4_site_name <- str_trim(final_external$a4_site_name)

## Replace the site ID with those in the master list 
## How to VlookUp in R: https://www.rforexcelusers.com/vlookup-in-r/
final_external$master_list <- id_list$a4_site_code[match(final_external$a4_site_name, id_list$a4_site_name)]
final_external <- final_external %>% mutate(id_check = ifelse(a4_site_code == master_list, a4_site_code, master_list)) %>%
  mutate(a4_site_code_final = ifelse(is.na(id_check), a4_site_code, id_check))

final_external$a4_site_code <- final_external$a4_site_code_final

final_external$master_list <- NULL
final_external$id_check <- NULL
final_external$a4_site_code_final <- NULL


write.xlsx(final_external, paste0("./output/external/CCCM_SiteReporting_All External (WithID)_",today,".xlsx"))


########################################################### DASHBOARD ##############################################################################

### Load Last Cleaned files ###
## Dashboard
last_db_v1 <- read.xlsx("./output/dashboard/CCCM_Site Reporting_V1_2020-10-11.xlsx")
last_db_v2 <- read.xlsx("./output/dashboard/CCCM_Site Reporting_V2_2020-10-11.xlsx")


## Rename vars to match
names(last_db_v1)[names(last_db_v1) == "b4_site_smc_agency_name"] <- "b2_site_smc_agency_name"
names(last_db_v1)[names(last_db_v1) == "b5_smc_agency_fp_name"] <- "b3_smc_agency_fp_name"
names(last_db_v1)[names(last_db_v1) == "b6_smc_agency_fp_mobile_number"] <- "b4_smc_agency_fp_mobile_number"
names(last_db_v1)[names(last_db_v1) == "b7_community_committee_in_place"] <- "b5_community_committee_in_place"
names(last_db_v1)[names(last_db_v1) == "b8_community_committee_fp_name"] <- "b6_community_committee_fp_name"
names(last_db_v1)[names(last_db_v1) == "b9_community_committee_fp_cell"] <- "b7_community_committee_fp_cell"


## Merge files
final_db <- plyr::rbind.fill(empty_dashboard, last_db_v1, last_db_v2)
final_db <- final_db %>% select(-X__version__ , -X_validation_status, -b4_site_smc_agency_name,
                                 -b5_smc_agency_fp_name, -b6_smc_agency_fp_mobile_number, -b7_community_committee_in_place,
                                 -b8_community_committee_fp_name, -b9_community_committee_fp_cell)

## Create the DB site code
final_db$a4_site_name <- str_trim(final_db$a4_site_name, "both")
final_db$a4_site_code <- external_choices$a4_site_code[match(final_db$a4_site_name, external_choices$a4_site_name)]
final_db <- final_db[order(final_db$a4_site_code, final_db$a4_site_name, na.last = FALSE),]

final_db <- final_db %>% mutate(a4_site_code = ifelse(is.na(a4_site_code) == TRUE, paste0(a2_district_code,"_",seq(1185,5000)), paste0(a4_site_code)))
final_db$a4_site_code <- str_replace(final_db$a4_site_code, "_", "-")

final_db$a4_site_code

## Fix codes based on master list
## Trim leading and trailing spaces
final_db$a4_site_name <- str_trim(final_db$a4_site_name)

## Replace the site ID with those in the master list 
## How to VlookUp in R: https://www.rforexcelusers.com/vlookup-in-r/
final_db$master_list <- id_list$a4_site_code[match(final_db$a4_site_name, id_list$a4_site_name)]
final_db <- final_db %>% mutate(id_check = ifelse(a4_site_code == master_list, a4_site_code, master_list)) %>%
  mutate(a4_site_code_final = ifelse(is.na(id_check), a4_site_code, id_check))

final_db$a4_site_code <- final_db$a4_site_code_final

final_db$master_list <- NULL
final_db$id_check <- NULL
final_db$a4_site_code_final <- NULL

write.xlsx(final_db, paste0("./output/dashboard/CCCM_SiteReporting_All DB (WithID)_",today,".xlsx"))

