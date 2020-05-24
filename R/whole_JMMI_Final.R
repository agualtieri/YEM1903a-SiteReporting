## Main Yemen Market Monitoring Script ##
# clear R environment
rm(list=ls())

# set wd to this script's locations
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)

# Load dependencies
source("./Scripts/basic scripts/load_dependencies.R")

# Load data preparation scripts
source("./Scripts/basic scripts/add_pcodes.R")
source('./Scripts/basic scripts/add_locations.R')
source("./Scripts/basic scripts/moveme.R")
source("./Scripts/basic scripts/delete_observations.R")

source("./Scripts/basic scripts/outliers_for_mapping.R")
source("./Scripts/basic scripts/delete_observations.R")
source("./Scripts/basic scripts/delete_observations.R")

# Load calculators
source("./Scripts/basic scripts/calculate_smeb.R")
source("./Scripts/basic scripts/calculate_medians.R")
source("./Scripts/basic scripts/multiple_response.R")

library(openxlsx)
library(writexl)
library(purrr)
library(cleaninginspectoR)


hs1<-createStyle(fgFill = "#EE5859",
                fontColour = "white",
                textDecoration = "bold",
                border = "Bottom")

# Months -> update according to the month being analyzed
current_month <- "January_2020"

data.frame <- read.csv("Inputs/data_cleaned_january_2020.csv",
                    sep=",",
                    header=T, 
                    encoding="UTF-8", 
                    stringsAsFactors=FALSE)


## Delete the empty columns of price display ##
data.frame <- dplyr::select(data.frame, -c("X.U.FEFF.end_", "today_", "deviceid_", "enumerator_id_All", "select_one_organization_name_All", "org_name_other_All", "wash_list_All", "price_cubic_meter_All", "note_exchange_rate_All", contains("display"), contains("normalised"), "X_version__All", "X_id", "X_submission_time", "X_validation_status", "X_index"))

colnames(data.frame) <- gsub(colnames(data.frame), pattern = "_All", replacement = "")  
# Add Pcodes or Locatin names to data.frame and move them at the beginning of the data.frame
data.frame.named <- add.location(data.frame)
data.frame.named<- data.frame.named[moveme(names(data.frame.named), "country_name after date_survey; country_ID after country_name; governorate_name after country_ID; governorate_ID after governorate_name; district_name after governorate_ID; district_ID after district_name")]

## Delete districts that have less than 3 observations (see. methodology)
data.frame.validated <- delete.districts(data.frame.named, "district_ID", 3)%>%
  as_tibble()

## Save final dataset ###
write.csv(data.frame.validated, file = paste0("./Outputs/final_validated_",current_month,".csv"), row.names = FALSE)
write.csv(data.frame.validated, file = paste0("./Inputs/final_validated_",current_month,".csv"), row.names = FALSE)

# Medians for dataset
data_medians_district <- calculate.medians(data.frame.validated, "district_ID")%>%
  as_tibble()
data_medians_governorate <- calculate.medians(data_medians_district, "governorate_ID")%>%
  as_tibble()

# SMEB for dataset
data_smeb_district <- calculate.smeb(data_medians_district, "district_ID")%>%
  as_tibble()
data_smeb_governorate <- calculate.smeb(data_medians_governorate, "governorate_ID")%>%
  as_tibble()


###################
## Data Analysis ##
###################

## Select only consecutive months
# Import CVS file ##
# Load previous and current month from cleaned datasets -> needs to be updated
previous_month <- read_csv("Outputs/final_validated_December_2019.csv")
current_month_pull <- read_csv("Outputs/final_validated_January_2020.csv")

# Select unique ID from current month to match on previous month
uniqueID <- unique(previous_month$district_ID)

current.month.analysis <- current_month_pull %>% subset(district_ID %in% uniqueID)

# Save matched file
write.csv(current.month.analysis, file = paste0("./Outputs/analysis_",current_month,".csv"), row.names = FALSE)
write.csv(current.month.analysis, file = paste0("./Inputs/analysis_",current_month,".csv"), row.names = FALSE)

# Medians calculations #
# Calculate district, governorate, national medians using the matched file
district_medians_w_last_month <- calculate.medians(current.month.analysis, "district_ID")%>%
  as_tibble()
governorate_medians_w_last_month <- calculate.medians(district_medians_w_last_month, "governorate_ID")%>%
  as_tibble()
country_medians <- calculate.medians(governorate_medians_w_last_month, "country_ID")%>%
  as_tibble()

# SMEB calculations #
# Calculate district, governorate, national WASH SMEB using the matched file
district_smeb_w_last_month <- calculate.smeb(current.month.analysis, "district_ID")%>%
  as_tibble()
governorate_smeb_w_last_month <- calculate.smeb(district_smeb_w_last_month, "governorate_ID")%>%
  as_tibble()
country_smeb_w_last_month <- calculate.smeb(governorate_smeb_w_last_month, "country_ID")%>%
  as_tibble()


#make the data visaulzations file
write.csv(governorate_smeb_w_last_month, file="./Outputs/governorate_smeb_boxplot.csv")

# Calculate percentage change (I still need to create a function for this)
# Import CVS file - the file needs to be created manually by merging the governorate level medians ##
ts <- read_csv("Inputs/timeseries/JMMI_timeseries_R_v6.csv")
#ts$date <- as.Date(ts$date, format="%d/%m/%Y")
ts$date <- lubridate::dmy(ts$date)

## Calculate month to month percentage change ##
pct <- function(x) {((x/lag(x))-1)*100}


# District level #
district_ts <- ts %>%
            group_by(district_ID) %>%
            arrange(date) %>%
            mutate_at(vars(calc_price_petrol, calc_price_diesel,	calc_price_bottled_water,	calc_price_treated_water,	calc_price_soap,	calc_price_laundry,	calc_price_sanitary,	cost_cubic_meter,	exchange_rate), pct)%>%
            as_tibble()

# Governorate level #
governorate_ts <- ts %>% 
  group_by(governorate_ID) %>% 
  arrange(date) %>%
  mutate_at(vars(calc_price_petrol, calc_price_diesel, calc_price_bottled_water, calc_price_treated_water, calc_price_soap, calc_price_laundry, calc_price_sanitary, cost_cubic_meter, exchange_rate), pct)%>%
  as_tibble()

## Calculate national percentage change ##
national_ts <- read_csv("Inputs/timeseries/JMMI_national_timeseries.csv")

national_ts <- national_ts %>% 
  mutate_at(vars(calc_price_petrol, calc_price_diesel, calc_price_bottled_water, calc_price_treated_water, calc_price_soap, calc_price_laundry, calc_price_sanitary, cost_cubic_meter, exchange_rate), pct)%>%
  as_tibble()



################################################################################
## Other variables (not sure if it makes sense to create functions for these) ##
################################################################################

##################################################################################################
#restock, challenges, and cash
##################################################################################################


## Calculating avg restocking times per governorate 
## Pivoting to select the columns of interest, and pivoting by grouping them by name and calculating the avg ##
df_restock_avg <- data.frame.validated %>%
  dplyr::select(governorate_name, contains("restock")) %>%
  group_by(governorate_name) %>%
  dplyr::summarise_all(tibble::lst(mean), na.rm = TRUE)

## Proportion challenges per vendor 
mean_challenges_fuel <- multiple.response(data.frame.validated, c("fuel_constraints_multiple."))%>%
  as.data.frame()

mean_challenges_wash <- multiple.response(data.frame.validated, c("wash_constraints_multiple."))%>%
  as.data.frame()

mean_challenges_wt <- data.frame.validated%>%
  rename_at(vars(starts_with("constraints")), list( ~paste0("wt_",.) ))%>%
  multiple.response( c("wt_constraints_multiple."))%>%
  as.data.frame()

## Calculation of CASH proportions
df_cash <- data.frame.validated %>%
  dplyr::select(contains("cash_feasibility"))

cash_stuff <-as.matrix(colMeans(df_cash == TRUE, na.rm = TRUE))
mean_cash<-as.data.frame(cbind(rownames(cash_stuff),str_split_fixed(cash_stuff, " ",1)))

##################################################################################################
#Water Trucking
##################################################################################################

## Summary statistics for water trucking (median truck capacity, median distance from location, median additional costs)
df_water_trucking_median <- data.frame.validated %>%
  dplyr::select(capacity_truck,	location_source,	additional_cost_5,	additional_cost_10) %>%
  dplyr::summarise_all(funs(median), na.rm = TRUE)

## Proportion of water source mentioned ##
df_water_source <- data.frame.validated %>%
  dplyr::select(type_water) %>%
  filter(!is.na(type_water))%>%
  filter(type_water != "") %>%
  group_by(type_water) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

## Proportion of vendors applying an increase due to distance
df_delivery_costs <- data.frame.validated %>%
  dplyr::select(distance_price) %>%
  filter(!is.na(distance_price))%>%
  filter(distance_price != "") %>%
  group_by(distance_price) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

## Proportion of chlorinated water
df_chlorinated_water <- data.frame.validated %>%
  dplyr::select(water_chlorinated) %>%
  filter(!is.na(water_chlorinated))%>%
  filter(water_chlorinated != "") %>%
  group_by(water_chlorinated) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

## Proportion of type of owner ##
df_owner <- data.frame.validated %>% 
  dplyr::select(type_owner) %>%
  filter(!is.na(type_owner))%>%
  filter(type_owner != "") %>%
  group_by(type_owner) %>%
  dplyr::summarise(n=n()) %>%
  mutate(freq = n/sum(n)) 

## Issues affecting supply chain ##
## % of Vendor KIs reporting issues affecting supply chain
supply_yesno <- data.frame.validated %>% 
  dplyr::select(mrk_supply_routes) %>%
  filter(!is.na(mrk_supply_routes))%>%
  filter(mrk_supply_routes != "") %>%
  group_by(mrk_supply_routes) %>%
  dplyr::summarise(n=n()) %>%
  mutate(freq = n/sum(n)) 

##################################################################################################
#Infrastructure and supply issues
##################################################################################################

source("./Scripts/basic scripts/multiple_response_other_underscore.R")
source("./Scripts/basic scripts/multiple_response.R")

## Multiple response - issues affecting supply chain
names(data.frame.validated)<-str_replace_all(names(data.frame.validated), "\\.","/")
supply_pull<-dplyr::select(data.frame.validated, c(starts_with("mrk_supply_issues/")))
issues_supply <- multiple.response(supply_pull, c("mrk_supply_issues/"))%>%
  as.data.frame()

## Infrastrcutural damage ##
damage_pull<-dplyr::select(data.frame.validated, c(starts_with("mrk_dmg_infra/")))
infra_damage <- multiple.response(damage_pull, c("mrk_dmg_infra/"))%>%
  as.data.frame()

##############################################################################################
#Supply increase section
##############################################################################################

## Supply increase: FUEL ##
supply.increase.fuel50 <- data.frame.validated %>%
              dplyr::select(mrk_increse_fuel_50) %>%
              filter(!is.na(mrk_increse_fuel_50))%>%
              filter(mrk_increse_fuel_50 != "") %>%
              group_by(mrk_increse_fuel_50) %>%
              dplyr::summarise(n=n()) %>%
              mutate(freq = n/sum(n)) 

supply.increase.fuel100 <- data.frame.validated %>%
              dplyr::select(mrk_increse_fuel_100) %>%
              filter(!is.na(mrk_increse_fuel_100))%>%
              filter(mrk_increse_fuel_100 != "") %>%
              group_by(mrk_increse_fuel_100) %>%
              dplyr::summarise(n=n()) %>%
              mutate(freq = n/sum(n)) 

supply_increase_fuel <- as_tibble(bind_rows(supply.increase.fuel50, supply.increase.fuel100))

## Supply increase: SMEB ##
supply.increase.WASH50 <- data.frame.validated %>%
  dplyr::select(mrk_increse_WASH_50) %>%
  filter(!is.na(mrk_increse_WASH_50))%>%
  filter(mrk_increse_WASH_50 != "") %>%
  group_by(mrk_increse_WASH_50) %>%
  dplyr::summarise(n=n()) %>%
  mutate(freq = n/sum(n)) 

supply.increase.WASH100 <- data.frame.validated %>%
  dplyr::select(mrk_increse_WASH_100) %>%
  filter(!is.na(mrk_increse_WASH_100))%>%
  filter(mrk_increse_WASH_100 != "") %>%
  group_by(mrk_increse_WASH_100) %>%
  dplyr::summarise(n=n()) %>%
  mutate(freq = n/sum(n)) 

supply_increase_wash <- as_tibble(bind_rows(supply.increase.WASH50, supply.increase.WASH100))

## Supply increase: WATER ##
supply.increase.water50 <- data.frame.validated %>%
  dplyr::select(mrk_increse_water_50) %>%
  filter(!is.na(mrk_increse_water_50))%>%
  filter(mrk_increse_water_50 != "") %>%
  group_by(mrk_increse_water_50) %>%
  dplyr::summarise(n=n()) %>%
  mutate(freq = n/sum(n)) 

supply.increase.water100 <- data.frame.validated %>%
  dplyr::select(mrk_increse_water_100) %>%
  filter(!is.na(mrk_increse_water_100))%>%
  filter(mrk_increse_water_100 != "") %>%
  group_by(mrk_increse_water_100) %>%
  dplyr::summarise(n=n()) %>%
  mutate(freq = n/sum(n))

supply_increase_water <- as_tibble(bind_rows(supply.increase.water50, supply.increase.water100))

#######################################
#Mapping Outliers
#######################################
outliers_mapping<-as_tibble(map_outlier(data_smeb_district))

#########################################
#Trade Origins
#########################################




########################################
#Final Issues Check
########################################
#Fucntions
inspect_all_new<-function (df, uuid.column.name = NULL){
  uuid.provided <- !is.null(uuid.column.name)
  if (uuid.provided) {
    duplicate_uuids <- find_duplicates(df, duplicate.column.name = uuid.column.name)
  }
  if (!uuid.provided) {
    duplicate_uuids <- find_duplicates_uuid(df)
  }
  outliers <- find_outliers(df)
  other_responses <- find_other_responses_new(df)
  list <- list(outliers, other_responses, duplicate_uuids, 
    sensitive_columns(df, T))
  issues <- lapply(list, function(x) {
    if (nrow(x) > 0) {
      x$value <- as.character(x$value)
    }
    return(x)
  })
  rbind(sensitive_columns(df, T), duplicate_uuids, find_outliers(df), 
    find_other_responses_new(df))
  
  issues <- do.call(rbind, issues)
  
  issues$district_name<-0
  issues$uuid<-0
  #issues$org<-0
  dist_name<-colnames(dplyr::select(df,contains("district_ID")))  
  uuid_num<-colnames(dplyr::select(df,contains("uuid")))
  org_num<-which(colnames(df)==(colnames(dplyr::select(df,contains("select_one_organization_name")))))
  for (i in 1:nrow(issues)){
    #is the issue type is not ;other' response. may need recoding continue.
    if (issues$issue_type[i] != "'other' response. may need recoding."){
      #get the row number for which the observation occurs within the data frame
      rownum <- issues$index[i]
      #append the organization which has the faulty issue into the issues table
      issues$district_name[i] <- as.character(df[rownum,which(colnames(df)==dist_name)])
      #find the colnum of the varialbe which produced teh issue using the match function with the name
      issues$uuid[i]<- as.character(df[rownum,which(colnames(df)==uuid_num)])
      #issues$org[i]<-df[rownum,org_num]
    }
    
  } 
  return(issues)
}
find_other_responses_new<-function (data) {
  counts <- data %>% select_other_columns_new
  if (ncol(counts) == 0) {
    return(empty_issues_table())
  }
  counts <- counts %>% (tidyr::gather)
  if (ncol(counts) == 0) {
    return(empty_issues_table())
  }
  else {
    counts %<>% dplyr::filter(!is.na(value)) %>% dplyr::filter(!value %in% 
        c("", TRUE, FALSE, 1, 0, "VRAI", "FAUX", 
          "TRUE", "FALSE", "<NA>", "NA"))
    if(nrow(counts)!=0){
      counts %<>% dplyr::group_by(key, value) %>% 
        dplyr::summarise(count2 = length(value)) %>%
        dplyr::filter(!is.na(value))
      others <- counts %>% as.data.frame
      if (nrow(others) == 0) {
        return(empty_issues_table())
      }
      others <- others %>% dplyr::mutate(value = paste0(value, 
        " /// instances: ", count2)) %>% dplyr::select(variable = key, 
          value)
      others <- data.frame(index = NA, others[, c("value", 
        "variable")], has_issue = T, issue_type = "'other' response. may need recoding.", 
        stringsAsFactors = F)
      return(others)
    }
  }
}
select_other_columns_new<-function (data) {
  othernames <- grep("other$|Other$|other_All|autre$|Autre$", names(data), 
    value = T)
  data[othernames]
}

issues_check<-inspect_all_new(data.frame.validated)

#########################################
#Create workbooks
#########################################

list_of_datasets_analysis <- list("Raw Data" = data.frame.validated,
  "Outlier Issues" = issues_check,
  "District Medians" = data_medians_district,
  "District SMEB" = data_smeb_district,
  "Governorate Medians" = data_medians_governorate,
  "Governorate SMEB" = data_smeb_governorate,
  "National Medians" = country_medians,
  "National SMEB" = country_smeb_w_last_month,
  "District TS" = district_ts,
  "Governorate TS"= governorate_ts,
  "National TS" = national_ts,
  "Water Truck Median" = df_water_trucking_median,
  "Water Source" = df_water_source,
  "Delivery Costs" = df_delivery_costs,
  "Chlorinated Water" = df_chlorinated_water,
  "Owner" = df_owner,
  "Supply Yes-No" = supply_yesno,
  "Supply Issues" = issues_supply,
  "Infrastruct Damage" = infra_damage,
  "Fuel Constraints" = mean_challenges_fuel,
  "WASH Constraints" = mean_challenges_wash,
  "Water Constraints" = mean_challenges_wt,
  "Fuel Markets" = supply_increase_fuel,
  "WASH Markets" = supply_increase_wash,
  "Water Markets" = supply_increase_water,
  "Cash Feasibility" = mean_cash,
  "Restock Average" = df_restock_avg)
title_analysis <- paste0("./Outputs/Analysis_Full_",current_month,".xlsx")
write_xlsx(list_of_datasets_analysis, title_analysis)

list_of_datasets_maps <- list("Raw Data" = data.frame.validated,
  "District Medians" = data_medians_district,
  "District SMEB" = data_smeb_district,
  "Governorate Medians" = data_medians_governorate,
  "Governorate SMEB" = data_smeb_governorate,
  "Outliers for Maps" = outliers_mapping)
title_maps <- paste0("./Outputs/Maps_Full_",current_month,".xlsx")
write_xlsx(list_of_datasets_maps,title_maps)


list_of_datasets_final <- list("Raw Data" = data.frame.validated,
  "District Medians" = data_medians_district,
  "District SMEB" = data_smeb_district,
  "Governorate Medians" = data_medians_governorate,
  "Governorate SMEB" = data_smeb_governorate,
  "Outlier Issues" = issues_check)
title_Final <- paste0("./Outputs/Final_Full_",current_month,".xlsx")
write_xlsx(list_of_datasets_final,title_Final)
