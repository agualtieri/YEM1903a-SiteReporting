# CCCM Site Monitoring Tool - Data Cleaning script
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V8
# 09/11/2019

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
#require(xlsformfill)

#browseVignettes("dataqualitycontrol")


## Source
source("./R/cleanHead.R")
source("./R/check_time.R")


## Download data from kobo server
# datasets <- kobo_datasets("reach_yemen:KOBOyemREACH2017", "kobohr")
# kobo_data_downloader("412367", "reach_yemen:KOBOyemREACH2017", "kobohr")

## Fake data just for testing
#choices <- read.csv("./data/choices.csv", stringsAsFactors = F, check.names = F)
#questions <- read.csv("./data/questions.csv", stringsAsFactors = F, check.names = F)
#questions$name <- tolower(questions$name)

# response <- xlsform_fill(questions, choices,500)
 
## Upload data to be cleaned
response <- read.xlsx("data/cleaning/Master tbc/CCCM_Site_Reporting_Master_tbc_(V1) - NEW - 08102020.xlsx")


names(response)[names(response) == "_index"] <- "index"
names(response)[names(response) == "_uuid"] <- "uuid"

## Remove group name and reduce to all lowercase
# names(response) <- tolower(names(response))
#response <- cleanHead(response)
#response <- cleanHead(response)


## Anonymize dataset
response <- anonymise_dataset(response, c("deviceid", "subscriberid", "imei", "phonenumber", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name",
                                          "q1_2_key_informat_gender", "q1_3_key_informat_mobile_number", "a5_gps_coordinates", "calca11", "__version__", "_id", "_submission_time", "_validation_status"))

## Dataset manipulation
### Using kobo to rename the site name and than merge the other column
choices <- read.csv("./data/kobo/choices.csv", check.names = FALSE)
external_choices <- read.csv("./data/kobo/choices_external.csv", check.names = FALSE)

external_choices <- filter(external_choices, external_choices$list_name == "sitename")
names(external_choices)[names(external_choices) == "name"] <- "a4_site_name"

response$a4_site_name2 <- external_choices$`label::english`[match(response$a4_site_name, external_choices$a4_site_name)]

response <- mutate(response, a4_site_name3 = ifelse(!is.na(a4_site_name2), as.character(a4_site_name2), a4_other_site))

### Create var from 1185 onwards and assing new site id
#numeric_seq <- seq(from = 1185, to = 5000)
#numeric_seq <- data.frame(numeric_seq)

#response <- mutate(response, new_site_id = ifelse(a4_site_name != "other", a4_site_name, paste0(a2_district,"_",numeric_seq$numeric_seq)))



## Check 0: check that there are no site surveyed twice
n_occur <- data.frame(table(response$a4_site_name3))
n_occur[n_occur$Freq > 1,]

duplicate_sites <- response[response$a4_site_name3 %in% n_occur$Var1[n_occur$Freq > 1],]



if (nrow(duplicate_sites) >= 1) {
  
  duplicate_sites <- duplicate_sites %>% select("uuid", "q0_3_organization", "a3_sub_district", "a4_site_name3")
  duplicate_sites$issue <- "Duplicate site name:  check area code"
  duplicate_sites$new_value <- " "
  duplicate_sites$fix <- "Checked with partner"
  duplicate_sites$checked_by <- "ON"
  duplicate_sites$variable <- "a4_site_name3"
  
  
  duplicates_log <-data.frame(uuid = duplicate_sites$uuid, 
                              agency = duplicate_sites$q0_3_organization, 
                              area = duplicate_sites$a3_sub_district,
                              variable = duplicate_sites$variable,
                              issue = duplicate_sites$issue, 
                              old_value = duplicate_sites$a4_site_name3, 
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


#write.csv(duplicate_sites, paste0("./output/duplicated_sites_",today,".csv"), row.names = F)
#browseURL(paste0("./output/duplicated_sites_",today,".csv"))

## Check 1: run cleaninginspector
response_issue <- inspect_all(response, "uuid")

response_issue <- response_issue[!grepl("'other' response. may need recoding.", response_issue$issue_type),]
response_issue <- response_issue[!grepl("Potentially sensitive information.", response_issue$issue_type),]

  
  if(nrow(response_issue)>=1) {
    issue_table <- response_issue %>% mutate(uuid=response[.$index,"uuid",drop=TRUE], ngo=response[.$index,"q0_3_organization", drop=TRUE], area = response[.$index, "a4_site_name3", drop = TRUE])
    
  } else {issue_table <- response_issue}
                        


if(nrow(issue_table)>=1) {

issue_table$new_vaue <- " "
issue_table$fix <- "Checked with partner"
issue_table$checked_by <- "ON"

issue_log <- data.frame(uuid = issue_table$uuid, 
                        agency = issue_table$ngo, 
                        area = issue_table$area, 
                        variable = issue_table$variable,
                        issue = issue_table$issue_type, 
                        old_value = issue_table$value, 
                        new_value = issue_table$new_vaue, 
                        fix = issue_table$fix, 
                        checked_by = issue_table$checked_by)

} else {
    
  issue_log <- data.frame(uuid = as.character(),
                               agency = as.character(),
                               area = as.character(),
                               variable = as.character(),
                               issue = as.character(),
                               old_value = as.character(),
                               new_value = as.character(),
                               fix = as.character(),
                               checked_by = as.character())
  
  
  
  print("No outliers issues have been detected. The dataset seems clean.")}
  

#write.csv(response_issue, paste0("./output/dataset_issues_",today,".csv"), row.names = F)
#browseURL(paste0("./output/dataset_issues_",today,".csv"))

## Check 2: run extra cleaning analysis
### Check that the site name is in the correct location - TBD

## Check 3: Check adequacy
### Check that a service defined as a priority need is not classified as adequate
check_adequacy <- select(response, "uuid", "q0_3_organization", "a4_site_name3", c("rrm_distributions":"waste_disposal_services"), "i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need")

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


adequacy_red <- check_adequacy %>% select("uuid", "q0_3_organization", "a4_site_name3", contains("_services"), contains("check"))

adequacy_melt <- adequacy_red %>% melt(id.vars = c("uuid", "q0_3_organization", "a4_site_name3"), variable.name = "variable" , value.name = "old_value")
adequacy_melt <- filter(adequacy_melt, adequacy_melt$old_value == 1)

if(nrow(adequacy_melt)>=1){

adequacy_melt$new_value <- " "
adequacy_melt$fix <- "Checked with partner"
adequacy_melt$checked_by <- "ON"
adequacy_melt$variable <- "Multiple (from rrm_distribution to waste_disposal_services)"
adequacy_melt$issue_type <- "Service marked both as 'adequate' and as a 'priority need'"

adequacy_log <- data.frame(uuid = adequacy_melt$uuid, 
                           agency = adequacy_melt$q0_3_organization, 
                           area = adequacy_melt$a4_site_name3, 
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
   

#write.csv(check_adequacy, paste0("./output/adequacy_issues_",today,".csv"), row.names = F)
#browseURL(paste0("./output/adequacy_issues_",today,".csv"))

### Check longitude and latitude
wrong_lat_long <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "a5_1_gps_longitude", "a5_2_gps_latitude")
wrong_lat_long <- wrong_lat_long %>% mutate(wrong_longitude = ifelse((a5_1_gps_longitude > 54.73893746 | a5_1_gps_longitude < 41.60824994), 1,0))
wrong_lat_long <- wrong_lat_long %>% mutate(wrong_latitude = ifelse((a5_2_gps_latitude > 18.99999992 | a5_2_gps_latitude < 11.90848018), 1,0))

wrong_lat_long_melt <- wrong_lat_long %>% melt(id.vars = c("uuid", "q0_3_organization", "a4_site_name3", "a5_1_gps_longitude", "a5_2_gps_latitude"), variable.name = "variable" , value.name = "old_value")

wrong_lat_long_melt <- filter(wrong_lat_long_melt, wrong_lat_long_melt$old_value == 1)

if(nrow(wrong_lat_long_melt) >= 1) {

wrong_lat_long_melt$new_value <- " "
wrong_lat_long_melt$fix <- "Checked with partner"
wrong_lat_long_melt$checked_by <- "ON"
wrong_lat_long_melt$issue_type <- "The GPS coordinates provided may contain errors"

wrong_lat_long_melt$variable <- ifelse(wrong_lat_long_melt$variable == "wrong_longitude", "a5_1_gps_longitude", "a5_2_gps_latitude")
wrong_lat_long_melt$old_value <- ifelse(wrong_lat_long_melt$variable == "a5_1_gps_longitude", wrong_lat_long_melt$a5_1_gps_longitude, wrong_lat_long_melt$a5_2_gps_latitude)



gps_log <- data.frame(uuid = wrong_lat_long_melt$uuid, 
                      agency = wrong_lat_long_melt$q0_3_organization, 
                      area = wrong_lat_long_melt$a4_site_name3, 
                      variable = wrong_lat_long_melt$variable, 
                      issue = wrong_lat_long_melt$issue_type, 
                      old_value = wrong_lat_long_melt$old_value, 
                      new_value = wrong_lat_long_melt$new_value, 
                      fix = wrong_lat_long_melt$fix, 
                      checked_by = wrong_lat_long_melt$checked_by)

} else {
  
  gps_log <- data.frame(uuid = as.character(),
                             agency = as.character(),
                             area = as.character(),
                             variable = as.character(),
                             issue = as.character(),
                             old_value = as.character(),
                             new_value = as.character(),
                             fix = as.character(),
                             checked_by = as.character())
  

  
  print("No issues with GPS coodinates have been detected. The dataset seems clean.")}


#write.csv(wrong_lat_long, paste("./output/wrong_lat_long_",today,".csv"), row.names = F)
#browseURL(paste("./output/wrong_lat_long_",today,".csv"))

### Check phone number - still needs to be fixed
phonenumber <- select(response, "uuid","q0_3_organization", "a4_site_name3", "b3_exu_fp_mobile_number", "b6_smc_agency_fp_mobile_number")

            
phonenumber2 <- phonenumber %>% filter(!is.na(b3_exu_fp_mobile_number)) %>% 
                                         mutate(exu_fb_wrong_number = ifelse(grep("^[70|71|73|77|79]", b3_exu_fp_mobile_number), 0, 1))

phonenumber2[, c("a4_site_name3", "b3_exu_fp_mobile_number", "b6_smc_agency_fp_mobile_number")] <- NULL

phonenumber3 <- phonenumber %>% filter(!is.na(b6_smc_agency_fp_mobile_number)) %>% 
                                         mutate(smc_agency_wrong_number = ifelse(grep("^[70|71|73|77|79]", b6_smc_agency_fp_mobile_number), 0, 1))

phonenumber3[, c("a4_site_name3", "b3_exu_fp_mobile_number", "b6_smc_agency_fp_mobile_number")] <- NULL


phonenumber_df <- plyr::join_all(list(phonenumber, phonenumber2, phonenumber3), 
                                 by = "uuid", 
                                 type = "left")

phonenumber_df <- phonenumber_df[, !duplicated(colnames(phonenumber_df), fromLast = FALSE)] 

phone_melt <- phonenumber_df %>% melt(id.vars = c("uuid","q0_3_organization", "a4_site_name3", "b3_exu_fp_mobile_number", "b6_smc_agency_fp_mobile_number"))


phone_melt <- filter(phone_melt, phone_melt$value == 1)

 if(nrow(phone_melt)>=1) {

phone_melt$new_value <- " "
phone_melt$fix <- "Checked with partner"
phone_melt$checked_by <- "ON"
phone_melt$issue_type <- "The phone number provided may contain errors"



 phone_melt <- phone_melt %>% mutate(variable = ifelse(variable == "exu_fb_wrong_number", "b3_exu_fp_mobile_number",
                                                            ifelse(variable == "smc_agency_wrong_number", "b6_smc_agency_fp_mobile_number", NA)))
  
 phone_melt <- phone_melt %>% mutate(old_value = ifelse(value == ifelse((value == 1 & variable == "b3_exu_fp_mobile_number"), b3_exu_fp_mobile_number,
                                                        ifelse((value == 1 & variable == "b6_smc_agency_fp_mobile_number"), b6_smc_agency_fp_mobile_number, NA))))

 phone_log <- data.frame(uuid = phone_melt$uuid, 
                         agency = phone_melt$q0_3_organization, 
                         area = phone_melt$a4_site_name3, 
                         variable = phone_melt$variable, 
                         issue = phone_melt$issue_type, 
                         old_value = phone_melt$old_value, 
                         new_value = phone_melt$new_value, 
                         fix = phone_melt$fix, 
                         checked_by = phone_melt$checked_by)

  } else { 
  
  phone_log <- data.frame(uuid = as.character(),
                        agency = as.character(),
                        area = as.character(),
                        variable = as.character(),
                        issue = as.character(),
                        old_value = as.character(),
                        new_value = as.character(),
                        fix = as.character(),
                        checked_by = as.character())
  
  
  print("No issues with phone numbers. The dataset seems clean.")   }





### Check that formal sites have more than 20 HH
formal <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "a7_site_population_hh", "a9_formal_informal")
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
                          area = formal_red$a4_site_name3, 
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


#write.csv(formal, paste0("./output/formal_site_population_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/formal_site_population_issue_",today,".csv"))

### Check that collective centre is not a makeshift or emergency or transitional or open-air type of shelter
collective <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "c1_type_of_site", "c9_primary_shelter_type")
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
                           area = collective_red$a4_site_name3, 
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




#write.csv(collective, paste0("./output/collective_centre_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/collective_centre_issue_",today,".csv"))

### Check that waste disposal cannot be "non-existant" if they selected flush latrines
waste_disposal <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "c10_primary_latrine_type", "waste_disposal_services")
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
                               area = waste_disposal_red$a4_site_name3, 
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


#write.csv(waste_disposal, paste0("./output/waste_disposal_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/waste_disposal_issue_",today,".csv"))

### Check that adequate was services do not include: flush latrine to the open / open defecation / pit uncovered / illegal water connection / unprotected well / surface water
adequate_wash <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "c10_primary_latrine_type", "c8_primary_water_source", "wash_services")
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
                                   area = adequate_wash_red$a4_site_name3, 
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


#write.csv(adequate_wash, paste0("./output/adeqate_facility_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/adeqate_facility_issue_",today,".csv"))

### Check that eviction risk does not come with a tennacy agreement
eviction <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "c3_tenancy_agreement_for_at_least_6_months", "f1_threats_to_the_site.eviction")
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
                             area = eviction_red$a4_site_name3, 
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


#write.csv(eviction, paste0("./output/eviction_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/eviction_issue_",today,".csv"))

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



### Check lenght of the survey, 10 = minimum, 40 = maximum (can be changed)
time_stamp <- select(response, "uuid", "start", "end", "q0_3_organization", "a4_site_name3")


#check_time <- cleaninginspectoR::check_time(time_stamp, 5, 30)
source("./R/check_time.R")
check_time <- check_time(time_stamp, 5, 40)


names(check_time)[names(check_time) == "index"] <- "uuid"

check_time$q0_3_organization <- response$q0_3_organization[match(check_time$uuid, response$uuid)]
check_time$a4_site_name3 <- response$a4_site_name3[match(check_time$uuid, response$uuid)]

 if(nrow(check_time) >= 1){
  
  check_time$new_value <- " "
  check_time$fix <- "Checked with partner"
  check_time$checked_by <- "ON"
  check_time$issue_type <- "The survey was completed in less than 10 minutes or more than 40 minutes"
  check_time$variable <- "Lenght of survey"
  
  check_time_log <- data.frame(uuid = check_time$uuid, 
                               agency = check_time$q0_3_organization, 
                               area = check_time$a4_site_name3, 
                               variable = check_time$variable, 
                               issue = check_time$issue_type, 
                               old_value = check_time$value, 
                               new_value = check_time$new_value, 
                               fix = check_time$fix, 
                               checked_by = check_time$checked_by)
  
 } else {
  
  check_time_log <- data.frame(uuid = as.character(),
                        agency = as.character(),
                        area = as.character(),
                        variable = as.character(),
                        issue = as.character(),
                        old_value = as.character(),
                        new_value = as.character(),
                        fix = as.character(),
                        checked_by = as.character())
  
  
  print("The lenghts of the survey are within acceptable values. No cleaning needed.") }





#### Rbind everything if they exist
cleaning_log <- plyr::rbind.fill(duplicates_log, 
                           adequacy_log, 
                           adequate_wash_log, 
                           collective_log,
                           eviction_log,
                           formal_log,
                           issue_log,
                           waste_disposal_log,
                           gps_log,
                           phone_log,
                           cooking_log)

#write.csv(cleaning_log, paste0("./output/cleaning_log_",today,".csv"), row.names = F)
#browseURL(paste0("./output/cleaning_log_",today,".csv"))


#### Save adequacy check file for easier user
final_log <- list("cleaning_log" = cleaning_log,
                  "Service adequacy vs needs" = check_adequacy,
                  "Survey lenght" = check_time_log)

write.xlsx(final_log, paste0("./output/CCCM_SiteID_cleaning log_",today,".xlsx"))
browseURL(paste0("./output/CCCM_SiteID_cleaning log_",today,".xlsx"))

#if (nrow(check_time)>=1) {
 # write.xlsx(check_time_log, paste0("./output/CCCM_SiteID_time checks log_",today,".xlsx"))
#} else {print("Surveys were all between 5 and 40 minutes long.")}           



### Save everything in one file
#data_cleaning <- list("duplicated sites" = duplicate_sites,
                     # "Time stamp check" = check_time,
                     # "Wrong phone numbers" = phonenumber_df,
                     # "Outliers and others" = response_issue,
                     # "Service adequacy vs needs" = check_adequacy,
                     # "Longitude and latitude" = wrong_lat_long,
                     # "Number of HH in formal site" = formal,
                     # "Shelter type in collective shelter" = collective,
                     # "Waste disposal" = waste_disposal,
                     # "Adequate WASH facilities" = adequate_wash,
                     # "Tennecy agreement and eviction" = eviction)

#write.xlsx(data_cleaning, paste0("./output/CCCM_SiteID_data cleaning checks_",today,".xlsx"), colNames = TRUE)
#browseURL(paste0("./output/CCCM_SiteID_data cleaning checks_",today,".xlsx"))










#### After data is cleaning updated dataset by adding admin units names
## Load cleaned data
#response_clean <- read.csv("./data/CCCM_SiteID_24102019_cleaned.csv", stringsAsFactors = F)

#source("./R/add_locations.R")
#source("./R/moveme.R")

#response_clean <- add.location(response_clean)
