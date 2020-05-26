### Script that takes V1 and V2 files and appends them to Masters
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V1
# 08/01/2020

## Useful resource
# https://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in

rm(list=ls())
today <- Sys.Date()

require(tidyverse)
require(openxlsx)

source("./R/moveme.R")

### Load kobo choices file to vlook up old site codes
choices <- read.csv("./data/kobo/choices.csv", check.names = F)
external_choices <- read.csv("./data/kobo/external_choices.csv", check.names = F)

external_choices <- filter(external_choices, external_choices$list_name == "sitename")
names(external_choices)[names(external_choices) == "name"] <- "a4_site_code"
names(external_choices)[names(external_choices) == "label::english"] <- "a4_site_name"

## Load site ID master list
id_list <- read.xlsx("./output/id master list/SiteIDCodes MasterList V2.xlsx")
id_list$a4_site_name <- str_trim(id_list$a4_site_name)

#################################### INTERNAL ##############################################################################

### Produce Internal Updated dataset ###
#### Internal Master file
master_all_int <- read.xlsx("./output/internal/CCCM_SiteReporting_All Internal (WithID)_2020-05-14_real.xlsx")

### Load Last Cleaned files ###
## Internal
last_internal_v1 <- read.xlsx("./output/internal/CCCM_SiteReporting_V1 Internal_2020-05-14.xlsx")
last_internal_v2 <- read.xlsx("./output/external/CCCM_SiteReporting_V2 External_2020-05-13.xlsx")

## If only TRUE it means that no new values have been cleaned
unique(last_internal_v1$uuid %in% master_all_int$uuid)
unique(last_internal_v2$uuid %in% master_all_int$uuid)

## Take latest dataset and remove the duplicated entries using the master
new_v1 <- anti_join(last_internal_v1, master_all_int, "uuid")
new_v2 <- anti_join(last_internal_v2, master_all_int, "uuid")

new_int <- plyr::rbind.fill(new_v1, new_v2)

## Append the unique new entries to the final Master ALL Internal
new_master_all_int <- plyr::rbind.fill(master_all_int, new_int)
write.xlsx(new_master_all_int, paste0("./output/internal/CCCM_SiteReporting_All Internal_",today,".xlsx"))

### Internal Site ID code
new_master_all_int$a4_site_name <- str_trim(new_master_all_int$a4_site_name, "both")
new_master_all_int$a4_site_code <- external_choices$a4_site_code[match(new_master_all_int$a4_site_name, external_choices$a4_site_name)]
new_master_all_int <- new_master_all_int[order(new_master_all_int$a4_site_code, new_master_all_int$a4_site_name, na.last = FALSE),]

new_master_all_int <- new_master_all_int %>% mutate(a4_site_code = ifelse(is.na(a4_site_code) == TRUE, paste0(a2_district_code,"_",seq(1185, 5000)), paste0(a4_site_code)))
new_master_all_int$a4_site_code

## Fix codes based on master list
## Trim leading and trailing spaces
new_db$a4_site_name <- str_trim(new_db$a4_site_name)

## Replace the site ID with those in the master list 
## How to VlookUp in R: https://www.rforexcelusers.com/vlookup-in-r/
new_master_all_int$master_list <- id_list$a4_site_code[match(new_master_all_int$a4_site_name, id_list$a4_site_name)]
new_master_all_int <- new_master_all_int %>% mutate(id_check = ifelse(a4_site_code == master_list, a4_site_code, master_list)) %>%
  mutate(a4_site_code_final = ifelse(is.na(id_check), a4_site_code, id_check))

new_master_all_int$a4_site_code <- new_master_all_ext$a4_site_code_final

new_master_all_int$master_list <- NULL
new_master_all_int$id_check <- NULL
new_master_all_int$a4_site_code_final <- NULL

write.xlsx(new_master_all_int, paste0("./output/internal/CCCM_SiteReporting_All Internal (WithID)_",today,".xlsx"))

################################## EXTERNAL #############################################################

### Produce External Updated dataset ###
master_all_ext <- read.xlsx("./output/external/CCCM_SiteReporting_All External (WithID)_2020-05-14_real.xlsx")

### Load Last Cleaned files ###
## External
last_external_v1 <- read.xlsx("./output/external/CCCM_SiteReporting_V1 External_2020-05-12.xlsx")
last_external_v2 <- read.xlsx("./output/external/CCCM_SiteReporting_V2 External_2020-05-13.xlsx")

## Create V1 and V2 variable
last_external_v1$kobo_version <- "V1"
last_external_v2$kobo_version <- "V2"

unique(last_external_v1$uuid %in% master_all_ext$uuid)
unique(last_external_v2$uuid %in% master_all_ext$uuid)

## Take latest dataset and remove the duplicated entries using the master
new_v1 <- anti_join(last_external_v1, master_all_ext, "uuid")
new_v2 <- anti_join(last_external_v2, master_all_ext, "uuid")

new_ext <- plyr::rbind.fill(new_v1, new_v2)

## Append the unique new entries to the final Master ALL Internal
new_master_all_ext <- plyr::rbind.fill(master_all_ext, new_ext)
write.xlsx(new_master_all_ext, paste0("./output/external/CCCM_SiteReporting_All External_",today,".xlsx"))


### External Site ID code
new_master_all_ext$a4_site_name <- str_trim(new_master_all_ext$a4_site_name, "both")
new_master_all_ext$a4_site_code <- external_choices$a4_site_code[match(new_master_all_ext$a4_site_name, external_choices$a4_site_name)]
new_master_all_ext <- new_master_all_ext[order(new_master_all_ext$a4_site_code, new_master_all_ext$a4_site_name, na.last = FALSE),]

new_master_all_ext <- new_master_all_ext %>% mutate(a4_site_code = ifelse(is.na(a4_site_code) == TRUE, paste0(a2_district_code,"_",seq(1185,5000)), paste0(a4_site_code)))
new_master_all_ext$a4_site_code

## Fix codes based on master list
## Trim leading and trailing spaces
new_db$a4_site_name <- str_trim(new_db$a4_site_name)

## Replace the site ID with those in the master list 
## How to VlookUp in R: https://www.rforexcelusers.com/vlookup-in-r/
new_master_all_ext$master_list <- id_list$a4_site_code[match(new_master_all_ext$a4_site_name, id_list$a4_site_name)]
new_master_all_ext <- new_master_all_ext %>% mutate(id_check = ifelse(a4_site_code == master_list, a4_site_code, master_list)) %>%
  mutate(a4_site_code_final = ifelse(is.na(id_check), a4_site_code, id_check))

new_master_all_ext$a4_site_code <- new_master_all_ext$a4_site_code_final

new_master_all_ext$master_list <- NULL
new_master_all_ext$id_check <- NULL
new_master_all_ext$a4_site_code_final <- NULL


write.xlsx(new_master_all_ext, paste0("./output/external/CCCM_SiteReporting_All External (WithID)_",today,".xlsx"))

##################################### DASHBOARD ######################################################################

### Produce Dashboard Updated dataset ###
master_db <- read.xlsx("./output/dashboard/CCCM_SiteReporting_All DB (WithID)_2020-05-14_final.xlsx")

### Load Last Cleaned files ###
## Dashboard
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

#write.xlsx(new_db, paste0("./output/dashboard/CCCM_SiteReporting_All DB_",today,".xlsx"))

## Create the DB site code
new_db$a4_site_name <- str_trim(new_db$a4_site_name, "both")
new_db$a4_site_code <- external_choices$a4_site_code[match(new_db$a4_site_name, external_choices$a4_site_name)]
new_db <- new_db[order(new_db$a4_site_code, new_db$a4_site_name, na.last = FALSE),]

new_db <- new_db %>% mutate(a4_site_code = ifelse(is.na(a4_site_code) == TRUE, paste0(a2_district_code,"_",seq(1185,5000)), paste0(a4_site_code)))
new_db$a4_site_code <- str_replace(new_db$a4_site_code, "_", "-")

new_db$a4_site_code

## Fix codes based on master list
## Trim leading and trailing spaces
new_db$a4_site_name <- str_trim(new_db$a4_site_name)

## Replace the site ID with those in the master list 
## How to VlookUp in R: https://www.rforexcelusers.com/vlookup-in-r/
new_db$master_list <- id_list$a4_site_code[match(new_db$a4_site_name, id_list$a4_site_name)]
new_db <- new_db %>% mutate(id_check = ifelse(a4_site_code == master_list, a4_site_code, master_list)) %>%
                mutate(a4_site_code_final = ifelse(is.na(id_check), a4_site_code, id_check))

new_db$a4_site_code <- new_db$a4_site_code_final

new_db$master_list <- NULL
new_db$id_check <- NULL
new_db$a4_site_code_final <- NULL

write.xlsx(new_db, paste0("./output/dashboard/CCCM_SiteReporting_All DB (WithID)_",today,".xlsx"))
