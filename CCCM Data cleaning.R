# CCCM Site Monitoring Tool - Data Cleaning script
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V4
# 31/10/2019

rm(list=ls())
today <- Sys.Date()

## Download necessary packages
# devtools::install_github("mabafaba/clog", force = T)
# devtools::install_github("agualtieri/cleaninginspectoR", force = T)


## Install packages
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("openxlsx")

## Load libraries
require(tidyverse)
require(dataqualitycontrol)
require(cleaninginspectoR)
require(data.table)
require(openxlsx)
require(reshape2)
require(clog)

# browseVignettes("clog")

## Source
source("./R/cleanHead.R")
source("./R/add_locations.R")
source("./R/moveme.R")


## Upload data to be cleaned and fix some header names - the path may need to be updated based on where you stored your files
response <- read.xlsx("./data/cleaning/Master tbc/Master/CCCM_Site_Reporting (V1)_Master_tbc.xlsx")

names(response)[names(response) == "_index"] <- "index"
names(response)[names(response) == "_uuid"] <- "uuid"

## Upload updated cleaning log file - - the path may need to be updated based on where you stored your files
cleaning_log <- read.xlsx("./data/CCCM_SiteID_Cleaning log (V1)_Master_Script_V1.xlsx", sheet = "cleaning_log")


## Apply cleaning log
my_log <- cleaninglog(ids = cleaning_log$uuid,
                      variables = cleaning_log$variable,
                      new_values = cleaning_log$new_value,
                      name = cleaning_log$fix,
                      change = cleaning_log$change,
                      data_id_column_name = "uuid")

clean_data <- clog_clean(response, my_log)





### Using kobo to rename the site name and than merge the other column
choices <- read.csv("./data/kobo/choices.csv", check.names = F)
external_choices <- read.csv("./data/kobo/external_choices.csv", check.names = F)

external_choices <- filter(external_choices, external_choices$list_name == "sitename")
names(external_choices)[names(external_choices) == "name"] <- "a4_site_name"

clean_data$a4_site_name2 <- external_choices$`label::english`[match(clean_data$a4_site_name, external_choices$a4_site_name)]

clean_data<- mutate(clean_data, a4_site_name3 = ifelse(!is.na(a4_site_name2), as.character(a4_site_name2), a4_other_site))

### Create var from 1185 onwards and assing new site id
numeric_seq <- seq(from = 1185, to = 5000)
numeric_seq <- data.frame(numeric_seq)

clean_data <- mutate(clean_data, new_site_id = ifelse(a4_site_name != "other", a4_site_name, paste0(a2_district,"_",numeric_seq$numeric_seq)))


### Rename all variables for the dashboard and save file
clean_data <- add.location(clean_data)
clean_data <- clean_data[moveme(names(clean_data), "country_name before a1_governorate; country_id after country_name; a1_governorate_name before a1_governorate; 
                                              a2_district_name before a2_district; a3_sub_district_name before a3_sub_district; a4_site_name3 after a3_sub_district; new_site_id after a4_site_name3")]
# Delete unneccessary site columns
clean_data <- anonymise_dataset(clean_data, c("comments_001", "a4_site_name2", "a4_site_name", "deviceid", "subscriberid", "imei", "phonenumber", "_validation_status",
                                              "d2_2_second_most_common_district_of_idp_origin_001", "d2_2_most_common_governorate_of_idp_origin", "d2_governorate_of_idp_origin",
                                              "comments", "__version__", "calca11"))




# Rename site columns to match the DB file
names(clean_data)[names(clean_data) == "a4_site_name3"] <- "a4_site_name"
names(clean_data)[names(clean_data) == "new_site_id"] <- "a4_site_code"
names(clean_data)[names(clean_data) == "a1_governorate"] <- "a1_governorate_code"
names(clean_data)[names(clean_data) == "a2_district"] <- "a2_district_code"
names(clean_data)[names(clean_data) == "a3_sub_district"] <- "a3_sub_district_code"


## Prepare dashboard-ready file
dashboard <- clean_data
dashboard$a9_Site_Classification <- " "


dashboard$country_name <- NULL
dashboard$country_id <- NULL

names(dashboard)[names(dashboard) == "a1_governorate_name"] <- "a1_governorate"
names(dashboard)[names(dashboard) == "X_id"] <- "_id"
names(dashboard)[names(dashboard) == "uuid"] <- "_uuid"
names(dashboard)[names(dashboard) == "index"] <- "_index"
names(dashboard)[names(dashboard) == "X_submission_time"] <- "_submission_time"




db_rename <- select(dashboard, -c("q0_4_date", "a5_1_gps_longitude", "a5_2_gps_latitude", "a6_site_occupation_date_dd_mm_yy", 
                                  "a7_site_population_hh", "a7_site_population_individual", "a1_governorate", "a1_governorate_code",
                                  "a2_district_name", "a2_district_code", "a3_sub_district_name", "a3_sub_district_code",
                                  "a4_site_name", "a4_site_code"))

db_rename[] <- choices$`label::english`[match(unlist(db_rename), choices$name)]




db_norename <- select(dashboard, c("q0_4_date", "a5_1_gps_longitude", "a5_2_gps_latitude", "a6_site_occupation_date_dd_mm_yy", 
                                   "a7_site_population_hh", "a7_site_population_individual", "a1_governorate", "a1_governorate_code",
                                   "a2_district_name", "a2_district_code", "a3_sub_district_name", "a3_sub_district_code",
                                   "a4_site_name", "a4_site_code"))

final_dashboard <- cbind(db_rename, db_norename)

final_dashboard$a8_population_groups_other_than_idps_in_site_select_all_applicable <- str_replace_all(final_dashboard$a8_population_groups_other_than_idps_in_site_select_all_applicable, pattern = " ", replacement = " - ")
final_dashboard$a8_population_groups_other_than_idps_in_site_select_all_applicable <- str_replace_all(final_dashboard$a8_population_groups_other_than_idps_in_site_select_all_applicable, pattern = "_", replacement = " ")
final_dashboard$a4_site_code <- str_replace_all(final_dashboard$a4_site_code, pattern = "_", replacement = " - ")

final_dashboard <- final_dashboard[moveme(names(final_dashboard), "a1_governorate after q1_3_key_informat_mobile_number; a1_governorate_code after a1_governorate;
                                          a2_district_name after a1_governorate_code; a2_district_code after a2_district_name; a3_sub_district_name after a2_district_code;
                                          a3_sub_district_code after a3_sub_district_name; a4_site_name after a3_sub_district_code; a4_site_code after a4_site_name")]

final_dashboard <- final_dashboard[moveme(names(final_dashboard), "q0_4_date after q0_3_organization_other; a5_1_gps_longitude after a5_gps_coordinates; 
                                          a5_2_gps_latitude after a5_1_gps_longitude; a6_site_occupation_date_dd_mm_yy after a5_2_gps_latitude; 
                                          a7_site_population_hh after a6_site_occupation_date_dd_mm_yy; a7_site_population_individual after a7_site_population_hh;
                                          a9_Site_Classification after a9_formal_informal")]
 



write.xlsx(final_dashboard, paste0("./output/dashboard/CCCM_Site Reporting_V1_",today,".xlsx"))


#write.xlsx(final_dataset, paste0("./output/CCCM_SiteReporting_Week 1 Cleaned_",today,".xlsx"))


### Save Internal and External version of the files
#### After data is cleaning update the dataset by adding admin units names


#### INTERNAL
final_dataset_internal <- anonymise_dataset(clean_data, c("start", "end", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name",
                                                    "q1_2_key_informat_gender", "q1_3_key_informat_mobile_number", "a5_gps_coordinates", "_id", "_submission_time",
                                                    "q0_3_organization_other", "a4_site_name2", "comments", "comments_001", "a4_other_site", "q0_4_date", "b2_exu_fp_name",
                                                    "b3_exu_fp_mobile_number", "b5_smc_agency_fp_name", "b6_smc_agency_fp_mobile_number", "b8_community_committee_fp_name", "b9_community_committee_fp_cell"))


write.xlsx(final_dataset_internal, paste0("./output/internal/CCCM_SiteReporting_V1 Internal_",today,".xlsx"))

#### EXTERNAL
final_dataset_external <- anonymise_dataset(clean_data, c("start", "end", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name",
                                                             "q1_2_key_informat_gender", "q1_3_key_informat_mobile_number", "a5_gps_coordinates", "__version__", "_id", "_submission_time", "_validation_status",
                                                             "q0_3_organization_other", "a4_site_name2", "comments", "comments_001", "a4_other_site", "a5_1_gps_longitude", "a5_2_gps_latitude", "b2_exu_fp_name",
                                                             "b2_exu_fp_name", "b3_exu_fp_mobile_number", "b5_smc_agency_fp_name", "b6_smc_agency_fp_mobile_number", "b8_community_committee_fp_name", "b9_community_committee_fp_cell",
                                                             "q0_3_organization", "q0_4_date", "b3_smc_agency_fp_name", "b4_smc_agency_fp_mobile_number", "b6_community_committee_fp_name", "b7_community_committee_fp_cell in external",
                                                             "b4_site_smc_agency_name", "b7_community_committee_in_place"))

write.xlsx(final_dataset_external, paste0("./output/external/CCCM_SiteReporting_V1 External_",today,".xlsx"))    

