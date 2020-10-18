# CCCM Site Monitoring Tool - Clean update data from partners
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V1
# 20/07/2020

rm(list=ls())
today <- Sys.Date()

## Download necessary packages
# devtools::install_github("agualtieri/cleaninginspectoR", force = T)
# devtools::install_github("mabafaba/clog", force = T)


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

#browseVignettes("dataqualitycontrol")


## Source
source("./R/cleanHead.R")
source("./R/check_time.R")

#################################
#       Run data checks         #
#################################

## Upload data to be cleaned
response <- read.xlsx("data/CCCM Cluster_Site Reporting_All External (WithID)_2020-07-16.xlsx")


## Anonymize dataset
response <- response %>% select(-starts_with("X_"), -b4_site_smc_agency_name)
  

## Check 0: check that there are no site surveyed twice
n_occur <- data.frame(table(response$a4_site_name))
n_occur[n_occur$Freq > 1,]

duplicate_sites <- response[response$a4_site_name %in% n_occur$Var1[n_occur$Freq > 1],]



if (nrow(duplicate_sites) >= 1) {
  
  duplicate_sites <- duplicate_sites %>% select("uuid", "q0_3_organization", "a3_sub_district_name", "a4_site_name")
  duplicate_sites$issue <- "Duplicate site name:  check area code"
  duplicate_sites$new_value <- " "
  duplicate_sites$fix <- "Checked with partner"
  duplicate_sites$checked_by <- "ON"
  duplicate_sites$variable <- "a4_site_name"
  
  
  duplicates_log <-data.frame(uuid = duplicate_sites$uuid, 
                              agency = duplicate_sites$q0_3_organization, 
                              area = duplicate_sites$a3_sub_district_name,
                              variable = duplicate_sites$variable,
                              issue = duplicate_sites$issue, 
                              old_value = duplicate_sites$a4_site_name, 
                              new_value = duplicate_sites$new_value, 
                              fix = duplicate_sites$fix, 
                              checked_by = duplicate_sites$checked_by)
  
} else{
  
  duplicates_log <- data.frame(uuid = as.character(),
                               agency = as.character(),
                               area = as.character(),
                               variable = as.character(),
                               issue = as.character(),
                               old_value = as.character(),
                               new_value = as.character(),
                               fix = as.character(),
                               checked_by = as.character())
  
  print ("No sites with identical names have been detected. The dataset seems clean.")}


## Check 1: Check adequacy
### Check that a service defined as a priority need is not classified as adequate
check_adequacy <- select(response, "uuid", "q0_3_organization", "a4_site_name", c("rrm_distributions":"waste_disposal_services"), "i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need")

check_adequacy <- check_adequacy %>% mutate(food_check = ifelse((food_distribution == "adequate" & (i1_top_priority_need == "food" | i2_second_priority_need == "food" | i3_third_priority_need == "food")), 1,0))
check_adequacy <- check_adequacy %>% mutate(water_check = ifelse((wash_services == "adequate" & (i1_top_priority_need == "water" | i2_second_priority_need == "water" | i3_third_priority_need == "water")), 1,0))
check_adequacy <- check_adequacy %>% mutate(shelter_check = ifelse((shelter_maintenance_services == "adequate" & (i1_top_priority_need == "shelter_maintenance_assistance" | i2_second_priority_need == "shelter_maintenance_assistance" | i3_third_priority_need == "shelter_maintenance_assistance")), 1,0))
check_adequacy <- check_adequacy %>% mutate(nfi_check = ifelse((nfi_distribution == "adequate" & (i1_top_priority_need == "non_food_items" | i2_second_priority_need == "non_food_items" | i3_third_priority_need == "non_food_items")), 1,0))
check_adequacy <- check_adequacy %>% mutate(sanitation_check = ifelse((waste_disposal_services == "adequate" & (i1_top_priority_need == "sanitation_services" | i2_second_priority_need == "sanitation_services" | i3_third_priority_need == "sanitation_services")), 1,0))
check_adequacy <- check_adequacy %>% mutate(nutrition_check = ifelse((nutrition_services == "adequate" & (i1_top_priority_need == "nutrition_services" | i2_second_priority_need == "nutrition_services" | i3_third_priority_need == "nutrition_services")), 1,0))
check_adequacy <- check_adequacy %>% mutate(health_check = ifelse((health_services == "adequate" & (i1_top_priority_need == "medical_assistance" | i2_second_priority_need == "medical_assistance" | i3_third_priority_need == "medical_assistance")), 1,0))
check_adequacy <- check_adequacy %>% mutate(livelihoods_check = ifelse((livelihood_services == "adequate" & (i1_top_priority_need == "livelihood_assistance" | i2_second_priority_need == "livelihood_assistance" | i3_third_priority_need == "livelihood_assistance")), 1,0))
check_adequacy <- check_adequacy %>% mutate(cash_check = ifelse((cash_distributions == "adequate" & (i1_top_priority_need == "cash_assistance" | i2_second_priority_need == "cash_assistance" | i3_third_priority_need == "cash_assistance")), 1,0))
#check_adequacy <- check_adequacy %>% mutate(legal_check = ifelse((legal_services == "adequate" & (i1_top_priority_need == "Legal_services" | i2_second_priority_need == "Legal_services" | i3_third_priority_need == "Legal_services")), 1,0))
check_adequacy <- check_adequacy %>% mutate(protection_check = ifelse((protection_services == "adequate" & (i1_top_priority_need == "protection_services" | i2_second_priority_need == "protection_services" | i3_third_priority_need == "protection_services")), 1,0))
check_adequacy <- check_adequacy %>% mutate(education_check = ifelse((education_services == "adequate" & (i1_top_priority_need == "education" | i2_second_priority_need == "education" | i3_third_priority_need == "education")), 1,0))
check_adequacy <- check_adequacy %>% mutate(wash_sanitation_check = ifelse((wash_services == "adequate" & (i1_top_priority_need == "sanitation_services" | i2_second_priority_need == "sanitation_services" | i3_third_priority_need == "sanitation_services")), 1,0))


adequacy_red <- check_adequacy %>% select("uuid", "q0_3_organization", "a4_site_name", contains("_services"), contains("check"))

adequacy_melt <- adequacy_red %>% melt(id.vars = c("uuid", "q0_3_organization", "a4_site_name"), variable.name = "variable" , value.name = "old_value")
adequacy_melt <- filter(adequacy_melt, adequacy_melt$old_value == 1)

if(nrow(adequacy_melt)>=1){
  
  adequacy_melt$new_value <- " "
  adequacy_melt$fix <- "Checked with partner"
  adequacy_melt$checked_by <- "ON"
  adequacy_melt$variable <- "Multiple (from rrm_distribution to waste_disposal_services)"
  adequacy_melt$issue_type <- "Service marked both as 'adequate' and as a 'priority need'"
  
  adequacy_log <- data.frame(uuid = adequacy_melt$uuid, 
                             agency = adequacy_melt$q0_3_organization, 
                             area = adequacy_melt$a4_site_name, 
                             variable = adequacy_melt$variable, 
                             issue = adequacy_melt$issue_type, 
                             old_value = adequacy_melt$old_value, 
                             new_value = adequacy_melt$new_value, 
                             fix = adequacy_melt$fix, 
                             checked_by = adequacy_melt$checked_by)
  
} else { 
  
  adequacy_log <- data.frame(uuid = as.character(),
                             agency = as.character(),
                             area = as.character(),
                             variable = as.character(),
                             issue = as.character(),
                             old_value = as.character(),
                             new_value = as.character(),
                             fix = as.character(),
                             checked_by = as.character())
  
  
  
  print("No issues across avilability and service provisions have been detected. The dataset seems clean.")}



### Check that formal sites have more than 20 HH
formal <- select(response, "uuid", "q0_3_organization", "a4_site_name", "a7_site_population_hh", "a9_formal_informal")
formal <- formal %>% mutate(less_than_20 = ifelse((a9_formal_informal == "formal" & a7_site_population_hh < 20), 1,0))

formal_red <- filter(formal, formal$less_than_20 == 1)

if(nrow(formal_red) >=1) {
  
  formal_red$new_value <- " "
  formal_red$fix <- "Checked with partner"
  formal_red$checked_by <- "ON"
  formal_red$issue_type <- "Site identied as 'Formal' although the number of HHs is lower than 20"
  formal_red$variable <- "a7_site_population_hh"
  
  formal_log <- data.frame(uuid = formal_red$uuid, 
                           agency = formal_red$q0_3_organization, 
                           area = formal_red$a4_site_name, 
                           variable = formal_red$variable, 
                           issue = formal_red$issue_type, 
                           old_value = formal_red$a7_site_population_hh, 
                           new_value = formal_red$new_value, 
                           fix = formal_red$fix, 
                           checked_by = formal_red$checked_by)
  
  
} else {
  
  formal_log <- data.frame(uuid = as.character(),
                           agency = as.character(),
                           area = as.character(),
                           variable = as.character(),
                           issue = as.character(),
                           old_value = as.character(),
                           new_value = as.character(),
                           fix = as.character(),
                           checked_by = as.character())
  
  print("Formal site population size check not needed. The dataset seems clean.")}



### Check that collective centre is not a makeshift or emergency or transitional or open-air type of shelter
collective <- select(response, "uuid", "q0_3_organization", "a4_site_name", "c1_type_of_site", "c9_primary_shelter_type")
collective <- collective %>% mutate(collective_issue = ifelse((c1_type_of_site == "collective_centre" & (c9_primary_shelter_type == "emergency_shelter" |
                                                                                                           c9_primary_shelter_type == "makeshift_shelter" |
                                                                                                           c9_primary_shelter_type == "transitional_shelter" |
                                                                                                           c9_primary_shelter_type == "open_air_no_shelter")), 1, 0))


collective_red <- filter(collective, collective$collective_issue == 1)

if(nrow(collective_red) >= 1){
  
  collective_red$new_value <- " "
  collective_red$fix <- "Checked with partner"
  collective_red$checked_by <- "ON"
  collective_red$issue_type <- "Primary shelter type marked as 'makeshfit', 'emergency', 'transitional', or 'open-air' although type of site was identified as 'Collective centre'"
  collective_red$variable <- "c9_primary_shelter_type"
  
  collective_log <- data.frame(uuid = collective_red$uuid, 
                               agency = collective_red$q0_3_organization, 
                               area = collective_red$a4_site_name, 
                               variable = collective_red$variable, 
                               issue = collective_red$issue_type, 
                               old_value = collective_red$c9_primary_shelter_type, 
                               new_value = collective_red$new_value, 
                               fix = collective_red$fix, 
                               checked_by = collective_red$checked_by)
  
} else {
  
  collective_log <- data.frame(uuid = as.character(),
                               agency = as.character(),
                               area = as.character(),
                               variable = as.character(),
                               issue = as.character(),
                               old_value = as.character(),
                               new_value = as.character(),
                               fix = as.character(),
                               checked_by = as.character())
  
  print("Collective centre type checks not needed. The dataset seems clean.")}



### Check that waste disposal cannot be "non-existant" if they selected flush latrines
waste_disposal <- select(response, "uuid", "q0_3_organization", "a4_site_name", "c10_primary_latrine_type", "waste_disposal_services")
waste_disposal <- waste_disposal %>% mutate(waste_disposal_issue = ifelse((waste_disposal_services == "non_existant" & c10_primary_latrine_type == "flush_latrine_to_tank_sewage_system_pit"), 1, 0))


waste_disposal_red <- filter(waste_disposal, waste_disposal$waste_disposal_issue == 1)


if (nrow(waste_disposal_red) >= 1){
  
  waste_disposal_red$new_value <- " "
  waste_disposal_red$fix <- "Checked with partner"
  waste_disposal_red$checked_by <- "ON"
  waste_disposal_red$issue_type <- "Waste disposal marked as 'non-existant' although  'flush latrines' was selected as primary latrine type"
  waste_disposal_red$variable <- "waste_disposal_services"
  
  waste_disposal_log <- data.frame(uuid = waste_disposal_red$uuid, 
                                   agency = waste_disposal_red$q0_3_organization, 
                                   area = waste_disposal_red$a4_site_name, 
                                   variable = waste_disposal_red$variable, 
                                   issue = waste_disposal_red$issue_type, 
                                   old_value = waste_disposal_red$waste_disposal_services, 
                                   new_value = waste_disposal_red$new_value, 
                                   fix = waste_disposal_red$fix, 
                                   checked_by = waste_disposal_red$checked_by)
  
  
  
} else { 
  
  
  waste_disposal_log <- data.frame(uuid = as.character(),
                                   agency = as.character(),
                                   area = as.character(),
                                   variable = as.character(),
                                   issue = as.character(),
                                   old_value = as.character(),
                                   new_value = as.character(),
                                   fix = as.character(),
                                   checked_by = as.character())
  
  
  print("Waste disposal system checks not needed. The dataset looks clean.") }



### Check that adequate was services do not include: flush latrine to the open / open defecation / pit uncovered / illegal water connection / unprotected well / surface water
adequate_wash <- select(response, "uuid", "q0_3_organization", "a4_site_name", "c10_primary_latrine_type", "c8_primary_water_source", "wash_services")
adequate_wash <- adequate_wash %>% mutate(adequate_wash_issue = ifelse((wash_services == "adequate" & (c10_primary_latrine_type == "flush_latrine_to_the_open" |
                                                                                                         c10_primary_latrine_type == "open_air" |
                                                                                                         c10_primary_latrine_type == "pit_latrine_open" |
                                                                                                         c8_primary_water_source == "illegal_connection_to_piped_network" |
                                                                                                         c8_primary_water_source == "unprotected_well" |
                                                                                                         c8_primary_water_source == "surface_water")), 1, 0))

adequate_wash_red <- filter(adequate_wash, adequate_wash$adequate_wash_issue == 1)


if(nrow(adequate_wash_red)>=1){
  
  adequate_wash_red$new_value <- " "
  adequate_wash_red$fix <- "Checked with partner"
  adequate_wash_red$checked_by <- "ON"
  adequate_wash_red$issue_type <- "WASH services identified as adequate although water source or primary latrine type was marked as unsafe or unprotected"
  adequate_wash_red$variable <- "c10_primary_latrine_type"
  
  adequate_wash_log <- data.frame(uuid = adequate_wash_red$uuid, 
                                  agency = adequate_wash_red$q0_3_organization, 
                                  area = adequate_wash_red$a4_site_name, 
                                  variable = adequate_wash_red$variable, 
                                  issue = adequate_wash_red$issue_type, 
                                  old_value = adequate_wash_red$c10_primary_latrine_type, 
                                  new_value = adequate_wash_red$new_value, 
                                  fix = adequate_wash_red$fix, 
                                  checked_by = adequate_wash_red$checked_by)
  
} else {
  
  
  adequate_wash_log <- data.frame(uuid = as.character(),
                                  agency = as.character(),
                                  area = as.character(),
                                  variable = as.character(),
                                  issue = as.character(),
                                  old_value = as.character(),
                                  new_value = as.character(),
                                  fix = as.character(),
                                  checked_by = as.character())
  
  
  print("No issues with adequacy of wash services has been detected. The dataset seems clean.")}



### Check that eviction risk does not come with a tennacy agreement
eviction <- select(response, "uuid", "q0_3_organization", "a4_site_name", "c3_tenancy_agreement_for_at_least_6_months", "f1_threats_to_the_site.eviction")
eviction <- eviction %>% mutate(eviction_issue = ifelse((c3_tenancy_agreement_for_at_least_6_months == "yes" & f1_threats_to_the_site.eviction == 1), 1, 0))

eviction_red <- filter(eviction, eviction$eviction_issue == 1)

if (nrow(eviction_red) >=1){
  
  eviction_red$new_value <- " "
  eviction_red$fix <- "Checked with partner"
  eviction_red$checked_by <- "ON"
  eviction_red$issue_type <- "Eviction was identied as a risk although the site holds a tennacy agreement"
  eviction_red$variable <- "f1_threats_to_the_site.eviction"
  
  eviction_log <- data.frame(uuid = eviction_red$uuid, 
                             agency = eviction_red$q0_3_organization, 
                             area = eviction_red$a4_site_name, 
                             variable = eviction_red$variable, 
                             issue = eviction_red$issue_type, 
                             old_value = eviction_red$f1_threats_to_the_site.eviction, 
                             new_value = eviction_red$new_value, 
                             fix = eviction_red$fix, 
                             checked_by = eviction_red$checked_by)
  
  
  
} else {
  
  eviction_log <- data.frame(uuid = as.character(),
                             agency = as.character(),
                             area = as.character(),
                             variable = as.character(),
                             issue = as.character(),
                             old_value = as.character(),
                             new_value = as.character(),
                             fix = as.character(),
                             checked_by = as.character())
  
  
  print("No issues with eviction and type of tennacy has been detected. The dataset seems clean.")}

### Check cooking stuff
cooking <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "c5_fuel_available_in_site_close_proximity", "c6_electricity_solar_power_available_in_site", "primary_cooking_modality")
cooking <- cooking %>% mutate(cooking_issue = ifelse((c5_fuel_available_in_site_close_proximity == "yes" | c6_electricity_solar_power_available_in_site == "yes" & 
                                                        primary_cooking_modality == "Electrical_stove" | primary_cooking_modality == "Gas_stove"), 0, 1))


cooking_red <- filter(cooking, cooking$cooking_issue == 1)


if (nrow(cooking_red ) >=1){
  
  cooking_red$new_value <- " "
  cooking_red$fix <- "Checked with partner"
  cooking_red$checked_by <- "ON"
  cooking_red$issue_type <- "Eviction was identied as a risk although the site holds a tennacy agreement"
  cooking_red $variable <- "c5_fuel_available_in_site_close_proximity"
  
  cooking_log <- data.frame(uuid = cooking_red$uuid, 
                            agency = cooking_red$q0_3_organization, 
                            area = cooking_red$a4_site_name3, 
                            variable = cooking_red$variable, 
                            issue = cooking_red$issue_type, 
                            old_value = cooking_red$f1_threats_to_the_site.eviction, 
                            new_value = cooking_red$new_value, 
                            fix = cooking_red$fix, 
                            checked_by = cooking_red$checked_by)
  
  
  
} else {
  
  cooking_log <- data.frame(uuid = as.character(),
                            agency = as.character(),
                            area = as.character(),
                            variable = as.character(),
                            issue = as.character(),
                            old_value = as.character(),
                            new_value = as.character(),
                            fix = as.character(),
                            checked_by = as.character())
  
  
  print("No issues with cooking and availability of fuel. The dataset seems clean.")}




#### Rbind everything if they exist
cleaning_log <- plyr::rbind.fill(duplicates_log, 
                                 adequacy_log, 
                                 adequate_wash_log, 
                                 collective_log,
                                 eviction_log,
                                 formal_log,
                                 waste_disposal_log,
                                 cooking_log)


#### Save adequacy check file for easier user
final_log <- list("cleaning_log" = cleaning_log,
                  "Service adequacy vs needs" = check_adequacy)

write.xlsx(final_log, paste0("./output/CCCM_SiteID_cleaning log_",today,".xlsx"))
browseURL(paste0("./output/CCCM_SiteID_cleaning log_",today,".xlsx"))

         

##################################
#       Apply cleaning log       #
##################################

## Upload data to be cleaned and fix some header names
response <- read.xlsx("./data/CCCM Cluster_Site Reporting_All External (WithID)_2020-07-16.xlsx")



## Upload updated cleaning log file
cleaning_log <- read.xlsx("./data/", sheet = "cleaning_log")


## Apply cleaning log
my_log <- cleaninglog(ids = cleaning_log$uuid,
                      variables = cleaning_log$variable,
                      new_values = cleaning_log$new_value,
                      name = cleaning_log$fix,
                      change = cleaning_log$change,
                      data_id_column_name = "uuid")

clean_data <- clog_clean(response, my_log)

