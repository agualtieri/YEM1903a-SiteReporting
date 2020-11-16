# CCCM Site Monitoring Tool - Data Merge Script
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V2
# 08/12/2019

rm(list=ls())
today <- Sys.Date()

## Install packaged
# install.packages("tidyverse")
# install.packages("stringr")

require(tidyverse)
require(stringr)
require(openxlsx)

## Load sources
source("./R/cleanHead.R")


## Load choice sheet
choices <- read.csv("./data/kobo/choices.csv")


## Load cleaned data
response <- read.xlsx("./data/CCCM_SiteReporting_V1 Internal_2020-11-10.xlsx")
#response <- cleanHead(response)
#response <- cleanHead(response)

## Filter out sites that are useless
#response <- response %>% filter(check == 1 & check.2 == 1)

## Merge safe cooking practices and fire safety methods
response <- response %>% mutate(fire_safety = paste0(Safe_cooking_practices," ", additional_fire_safety_measures))
response$fire_safety <- gsub("NA", "", response$fire_safety)
response$fire_safety <- gsub("none none", "none", response$fire_safety)



## Step 1: Data Merge - dots
lookup <- data.frame(colour = c("not_applicable", "inadequate", "non_existent", "adequate"), path = c("./merge/grey.png", "./merge/red.png", "./merge/orange.png",   
                                                                            "./merge/green.png"))


dots_df <- response %>% select("rrm_distributions":"waste_disposal_services")
dots_df[is.na(dots_df)] <- "not_applicable"


dots_merge <- as.data.frame(lapply(dots_df, function(col) lookup$path[match(col, lookup$colour)]))
colnames(dots_merge) <- paste0("@", colnames(dots_merge))

#write.csv(dots_merge, "./output/test_image_datamerge.csv", row.names = F)

## Step 2: Data Merge - rest pf the stuff
data_rename <- response %>% select("b4_site_smc_agency_name", "c2_landowner", "c1_1_type_of_site", "c1_2_type_of_site", "Primary_cooking_modality", "Primary_cooking_space", "fire_safety",
                                  "c3_tenancy_agreement_for_at_least_6_months", "b1_site_management", "b7_community_committee_in_place", "d1_most_common_reason_idps_left_place_of_origin",
                                  "a8_population_groups_other_than_idps_in_site_select_all_applicable", "d3_most_common_intention_in_next_three_months", "c4_general_market_in_site_close_proximity",
                                  "d2_1_most_common_district_of_idp_origin", "d2_2_second_most_common_district_of_idp_origin", "d2_3_third_most_common_district_of_idp_origin", 
                                  "c6_electricity_solar_power_available_in_site", "c5_fuel_available_in_site_close_proximity", "c8_primary_water_source", "c9_primary_shelter_type",
                                  "c10_primary_latrine_type", "i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need", starts_with("psp_"), starts_with("if_ngo_"),
                                  "c7_presence_of_particularly_vulnerable_groups.unaccompanied_separated_children", "c7_presence_of_particularly_vulnerable_groups.child_headed_hh", "c7_presence_of_particularly_vulnerable_groups.elderly",
                                  "c7_presence_of_particularly_vulnerable_groups.female_headed_hh", "c7_presence_of_particularly_vulnerable_groups.marginalized_people", "c7_presence_of_particularly_vulnerable_groups.persons_with_disabilities",
                                  "c7_presence_of_particularly_vulnerable_groups.persons_with_chronic_diseases_serious_medical_conditions", "c7_presence_of_particularly_vulnerable_groups.pregnant_and_lactating_women",
                                  "f1_threats_to_the_site.flooding", "f1_threats_to_the_site.eviction", "f1_threats_to_the_site.infectious_diseases", "f1_threats_to_the_site.friction_between_communities",
                                  "f1_threats_to_the_site.water_contamination", "f1_threats_to_the_site.conflict_related_incidents", "f1_threats_to_the_site.fire_related_incidents")



data_rename <- data_rename %>% select(-c(ends_with("_other")))

data_rename2 <- data_rename
data_rename2[] <- choices$label..english[match(unlist(data_rename2), choices$name)]

### Rename the varibles to keep the data merge template intact
data_norename <- response %>% select("a1_governorate_name", "a2_district_name", "a3_sub_district_name", "a4_site_name", "a6_site_occupation_date_dd_mm_yy", "a7_site_population_individual", "a7_site_population_hh")

#data_norename$a8_population_groups_other_than_idps_in_site_select_all_applicable <- str_replace_all(data_norename$a8_population_groups_other_than_idps_in_site_select_all_applicable, pattern = " ", replacement = " - ")
#data_norename$a8_population_groups_other_than_idps_in_site_select_all_applicable <- str_replace_all(data_norename$a8_population_groups_other_than_idps_in_site_select_all_applicable, pattern = "_", replacement = " ")
#data_norename$a4_site_code <- str_replace_all(data_norename$a4_site_code, pattern = "_", replacement = " - ")
  
## Step 3: merge into one dataset
data_merge <- cbind(data_rename2, data_norename, dots_merge)


## Step 4: Add maps and save as csv
data_merge$a4_site_name <- str_trim(data_merge$a4_site_name, "both")
data_merge$`@maps` <- paste0("./merge/test maps/YEM_CCCM_",data_merge$a4_site_name,"__",response$a3_sub_district_code,".pdf")


write.csv(data_merge, paste0("./merge/cccm_full_merge_",today,".csv"), row.names = F)                     
browseURL(paste0("./merge/cccm_full_merge_",today,".csv"))



