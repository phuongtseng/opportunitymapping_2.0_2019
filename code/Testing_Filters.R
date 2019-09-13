#Credit to Phuong Tseng
library(data.table)
library(dplyr)
library(readr)
library(readxl)
library(tidyverse)
library(tidycensus)
library(tidyr)

#save.image("~/Documents/GitHub/opportunitymapping-master/opportunitymapping2019/index_filters_2019.RData")
load("/Users/ptseng/Documents/GitHub/opportunitymapping-master/opportunitymapping2019/data/index_filters_2019.RData")


#Starts here
load(file = "all_data.RData")

edu_indicators <- all_data %>% select("fips","CountyID.x","TOTPOP.x","math_prof","read_prof","grad_rate","pct_not_frpm","z_math_prof","z_read_prof","z_grad_rate","az_pct_not_frpm","county_name.x", "HD01_VD04","HD01_VD03","ratio","ratio2","z_preK") %>% mutate(edu_domain=(edu_indicators$z_preK+edu_indicators$z_math_prof+edu_indicators$z_read_prof+edu_indicators$z_grad_rate+edu_indicators$az_pct_not_frpm)/5)

econ_indicators <- all_data %>% select("fips","CountyID.x", "TOTPOP.x","total_pop_2017","below_200_pov_2017.x","moe_below_200_pov_2017.x"   ,"pct_below_pov_2017","moe_pct_below_pov_2017","pct_below_200_pov_2017.x", "pct_assist_2017","med_hhincome_2017" ,"moe_med_hhincome_2017" ,"employed_pop_20to60_2017","pct_employed_20to60_2017","home_value_2017" ,"moe_home_value_2017","pct_bachelors_plus_2017","above_200_pov_2017","pct_above_200_pov_2017","tot_hh_2017","moe_tot_hh_2017","moe_pct_long_commute_2017","moe_assist_2017", "moe_long_commute_pct","long_commute_pct","low_wage_med_distance" ,"jobs_lowed" ,"rural_flag","az_pct_assist_2017" ,"az_pct_employed_20to60_2017", "z_home_value_2017" ,"z_pct_bachelors_plus_2017" ,"az_pct_long_commute_2017", "z_jobs_lowed" ,"Econ_Domain", "z_sdrevpcap","sdrev","sdrevpcap","sd_totpop") %>% mutate(econ_domain=(econ_indicators$z_sdrevpcap + econ_indicators$z_jobs_lowed + econ_indicators$az_pct_long_commute_2017 + econ_indicators$z_pct_bachelors_plus_2017 + econ_indicators$z_home_value_2017+ econ_indicators$az_pct_employed_20to60_2017 + econ_indicators$az_pct_assist_2017)/7)

housing_indicators <- all_data %>% select("fips","CountyID.x" ,"TOTPOP.x","county_name.x", "below_200_pov_2017.y","moe_below_200_pov_2017.y","pct_below_200_pov_2017.y","pm25","pct_pm25","toxRelease","pct_toxRelease","lead_pctl","pct_lead_pctl" ,"Grocery", "z_Grocery" ,"az_Grocery","P_INSURED" , "az_insurance" ,"H_Crime", "pct_parks" ,"az_pct_below_200_pov_2017","az_pct_below_200_pov_20172","az_pct_pm25","az_pct_toxRelease","az_pct_lead_pctl" ,"Housing_Env_Domain","test_azcrime" ,"azhealthcare" ,"zparks" ) %>% mutate(housing_domain=(housing_indicators$az_insurance + housing_indicators$az_Grocery+ housing_indicators$az_pct_below_200_pov_2017+ housing_indicators$az_pct_pm25+ housing_indicators$az_pct_toxRelease+ housing_indicators$az_pct_lead_pctl+ housing_indicators$test_azcrime+ housing_indicators$azhealthcare+ housing_indicators$zparks)/9)

#Conduit taking broadband and single parent fam
socap_indicators <- all_data %>% select("fips", "CountyID.x","TOTPOP.x", "pct_singleparent_hh_2017.y", "moe_pct_singleparent_hh_2017.y", "az_pct_singleparent_hh_2017", "TOTPOP.y","Median_bb","z_broadband","Clubs","AVGDIS_REL","reg_vote","z_broadband2","Conduit","SOCIAL_CAP","z_regvoter","zreligious","zclubs") %>% mutate(Conduit=(socap_indicators$z_broadband + socap_indicators$az_pct_singleparent_hh_2017)/2) %>% mutate(SOCAP_domain=(socap_indicators$zreligious + socap_indicators$zclubs + socap_indicators$z_regvoter)/3)

#after merging all datasets assign it to a new vector
index <- index_scores

index$edu_domain <- (index$z_preK + index$z_math_prof + index$z_read_prof + index$z_grad_rate + index$az_pct_not_frpm)/5

index$SOCAP_domain <- (index$z_regvoter + index$zclubs + index$zreligious)/3

index$Conduit <- (index$az_pct_singleparent_hh_2017 + index$z_broadband)/2

index$econ_domain <- (index$z_jobs_lowed + index$az_pct_long_commute_2017 + index$z_sdrevpcap + index$z_home_value_2017 + index$az_pct_assist_2017+ index$z_pct_bachelors_plus_2017 + index$az_pct_employed_20to60_2017)/7

index$housing_domain <- (index$test_azcrime + index$zparks + index$az_Grocery+index$az_pct_toxRelease + index$az_insurance + index$az_pct_lead_pctl+index$pct_below_200_pov_2017.x + index$az_pct_pm25 + index$azhealthcare)/9

index$index <- (index$housing_domain + index$edu_domain + index$econ_domain + index$SOCAP_domain + index$Conduit)/5

#index$housing_domain[is.na(index$housing_domain)] <- 0
#save(all_data,file="all_data.RData")

#Filtering Single parent families >= 30% returns 471 records with 8 NAs
index_filters$SPF_GT_30[which(index_filters$pct_singleparent_hh_2017.y<0.3)] <- 0
index_filters$SPF_GT_30[which(index_filters$pct_singleparent_hh_2017.y>=0.3)] <- -1
index_filters$flag_spf <- ifelse(is.na(index_filters$SPF_GT_30),0,index_filters$SPF_GT_30)
sum(index_filters$flag_spf) #fixed NAs to 0

#Filtering Poverty (below 200 FPL) >= 30% returns 418 records with 3 NAs
index_filters$POVR200_GT_30[which(index_filters$pct_below_200_pov_2017.x<0.3)] <- 0
index_filters$POVR200_GT_30[which(index_filters$pct_below_200_pov_2017.x>=0.3)] <- -1 #418
index_filters$POVR200_GT_30 <- ifelse(is.na(index_filters$POVR200_GT_30),0,index_filters$POVR200_GT_30)
summary(index_filters$POVR200_GT_30) #fixed NAs to 0
sum(index_filters$POVR200_GT_30)

#These are records with NAs or missing values
#fips 06081984300 has NaN in pct_pov_below_200 and pct_singleparent_hh
#fips 06081984300 (Mod) need to change to NAs
#fips 06095253000 has NaN in pct_pov_below_200 and pct_singleparent_hh
#fips 06095253000 (Highest) need to change to NAs
#fips 06095980000 has NaN in pct_pov_below_200 and pct_singleparent_hh
#fips 06095980000 (High) need to change to NAs
#fips <- c('6095980000','6095253000','6081984300')
#for test$fips in c('6095980000','6095253000','6081984300'){
#  categories$DI_Blk_Lat_POV30 <- NaN}

categories$SPF30_P30 <- ifelse(categories$fips=='6095980000', 'NaN', categories$SPF30_P30)

#fips 06001981900 has NA in pct_singleparent_hh
#fips 06001981900 (High),
#fips 06013351101 has NA in pct_singleparent_hh
#fips 06013351101 (Mod),
#fips 06013351102 has NA in pct_singleparent_hh
#fips 06013351102 (Mod),
#fips 06013351103 has NA in pct_singleparent_hh
#fips 06013351103 (High),
#fips 06075980300 has NA in pct_singleparent_hh
#fips 06075980300 (High)

#Filtering single parent >= 30% AND poverty (below 200 FPL) >= 30% returns 292 with 313 NAs
index_filters$SPF30_P30[which(index_filters$flag_spf==0 | index_filters$POVR200_GT_30==0)] <- 0
index_filters$SPF30_P30[which(index_filters$flag_spf==-1 & index_filters$POVR200_GT_30==-1)] <- -1
summary(index_filters$SPF30_P30) #fixed NAs to 0
sum(index_filters$SPF30_P30) #292

#High Divergence and population of Black and Latinx > 50%
input_DI$Flag_HighDI_Blk_Lat[which(input_DI$Black_Latinx<=0.5 | input_DI$divergence_thresh<3)] <- 0
input_DI$Flag_HighDI_Blk_Lat[which(input_DI$Black_Latinx>0.5 & input_DI$divergence_thresh==3)] <- -1
sum(index_filters$Flag_HighDI_Blk_Lat) #201 no NAs

#High Divergence with population of Black and Latinx > 50% and poverty (below 200 FPL) >= 30%
index_filters$DI_Blk_Lat_POV30[which(index_filters$Flag_HighDI_Blk_Lat==0 | index_filters$POVR200_GT_30==0)] <- 0
index_filters$DI_Blk_Lat_POV30[which(index_filters$Flag_HighDI_Blk_Lat==-1 & index_filters$POVR200_GT_30==-1)] <- -1
#320
sum(index_filters$DI_Blk_Lat_POV30) #171 no NAs
sum(index_filters$POVR200_GT_30) #418 no NAs

#Filter
#High Divergence with population of Black and Latinx > 50% and poverty (below 200 FPL) >= 30% OR
#Poverty (below 200 FPL) >= 30% and Single-parent family >= 30%
index_filters$DI_Blk_Lat_POV30_OR_POV30_SPF30[which((index_filters$Flag_HighDI_Blk_Lat==0 & index_filters$POVR200_GT_30==0) | (index_filters$POVR200_GT_30==0 & index_filters$pct_singleparent_hh_2017.y<0.3))] <- 0
index_filters$DI_Blk_Lat_POV30_OR_POV30_SPF30[which((index_filters$Flag_HighDI_Blk_Lat==-1 & index_filters$POVR200_GT_30==-1) | (index_filters$POVR200_GT_30==-1 & index_filters$pct_singleparent_hh_2017.y>=0.3))] <- -1
#returns 109 NAs
index_filters$DI_Blk_Lat_POV30_OR_POV30_SPF30 <- ifelse(is.na(index_filters$DI_Blk_Lat_POV30_OR_POV30_SPF30),0,index_filters$DI_Blk_Lat_POV30_OR_POV30_SPF30) #fixed NAs

summary(index_filters$DI_Blk_Lat_POV30_OR_POV30_SPF30) #320 records

#Check filters
summary(index_filters$Flag_HighDI_Blk_Lat) #no NAs

sum(index_filters$flag_high_DI) #no NAs

#na_pov <- index %>% select("fips","pct_below_200_pov_2017.x") %>% filter(is.na(index$pct_below_200_pov_2017.x)) #3 NAs

#na_spf <- index %>% select("fips", "SPF_GT_30" , "pct_singleparent_hh_2017.y") %>% filter(is.na(index$pct_singleparent_hh_2017.y)) #8 NAs

library(dplyr)
categorize <- index_filters %>% dplyr::select("fips","index","DI_Blk_Lat_POV30", "SPF30_P30","DI_Blk_Lat_POV30_OR_POV30_SPF30") %>% filter(DI_Blk_Lat_POV30_OR_POV30_SPF30==-1) %>% mutate(category="Lowest Opportunity") #returns 320

remaining <- categories %>% dplyr::select("fips","index.x","DI_Blk_Lat_POV30","SPF30_P30","DI_Blk_Lat_POV30_OR_POV30_SPF30.x","category") %>% filter(DI_Blk_Lat_POV30_OR_POV30_SPF30.x==0)

nan_records <- remaining %>% dplyr::select("fips","index.x","DI_Blk_Lat_POV30","SPF30_P30","DI_Blk_Lat_POV30_OR_POV30_SPF30.x","category") %>% filter(DI_Blk_Lat_POV30_OR_POV30_SPF30.x==NaN)
nan_records$category_2 <- NA

#1256 records instead of 1259 records after removing 3 NAs
quantile(remaining$index.x, prob = c(0.25, .5, .75))
#categories not filtered breakdown
#25%  -0.13186716  | 50% -0.01013241 | 75% 0.11480394
#categories without filters breakdown
#20% -0.15917358  | 40%  -0.05659378 | 60% 0.04262045 | 80% 0.15259539
remaining$category_2[which(remaining$index.x>0.11480394)] <- "Highest Opportunity" #314
remaining$category_2[which(remaining$index.x>-0.01013241 & remaining$index.x<=0.11480394)] <- "High Opportunity" #314
remaining$category_2[which(remaining$index.x>-0.13186716 & remaining$index.x<=-0.01013241)] <- "Moderate Opportunity" #314
remaining$category_2[which(remaining$index.x<=-0.13186716)] <- "Low Opportunity" #314

quantile(index_filters$index, prob = c(0.20, .40, .60, .80))
#25% -0.130332961 | 50% -0.006063838 | 70% 0.119676113
#20% -0.15917358  | 40%  -0.05659378 | 60% 0.04262045 | 80% 0.15259539
index_filters$category_wo_filters[which(index_filters$index>0.15259539)] <- "Highest Opportunity"
index_filters$category_wo_filters[which(index_filters$index>=0.04262045 & index_filters$index<=0.15259539)] <- "High Opportunity"
index_filters$category_wo_filters[which(index_filters$index <= -0.15917358)] <- "Lowest Opportunity"
index_filters$category_wo_filters[which(index_filters$index < 0.04262045 & index_filters$index > -0.05659378)] <- "Moderate Opportunity"
index_filters$category_wo_filters[which(index_filters$index <= -0.05659378 & index_filters$index >= -0.15917358)] <- "Low Opportunity"

ftable(index_filters$category_wo_filters, index_filters$category)
merging <- full_join(nan_records, remaining)
categories <- full_join(remaining, categorize)
#na_records <- index %>% select("fips", "SPF_GT_30" , "pct_singleparent_hh_2017.y") %>% filter(is.na(index$pct_singleparent_hh_2017.y)) #8 NAs
index_w_filters <- full_join(remaining, categorize)
write.csv(index_w_filters, "index_w_filters.csv")


# Geography
#The Opportunity Mapping framework focuses the bay area region which encompases 9 counties. So, there are 9 counties of the bay area in the **counties** vector.

counties <- c("Alameda", "Contra Costa", "Marin", "Napa","San Mateo", "Santa Clara", "Solano", "Sonoma", "San Francisco")


## Load the American Community Survey (ACS)
#Some opportunity indicators in the model are pulled from the Census' American Community Survey, 5-year-estimates. We are using the most recent year, which is 2017.

v17 <- load_variables(year=2017,
dataset="acs5",
cache=FALSE)
glimpse(v17)


## Pick your ACS variables and/or table
#The **grab_acs_variables** function below makes table and variables selection process easier for users. This function has several arguments:
#1) **geography**,
#2) **variables** or **table**,
#3) **year** of the acs dataset,
#4) **output** format can be set to *wide* or *long*,
#5) **state** is set to California,
#6) **county** reads in the bay area counties, and
#7) **geometry** is set to TRUE so that the tigris shapefiles will be read in

## G. Overlays
#grab_acs_variables <- function(a){
#  overlay_indicators <- get_acs(geography="tract",
#                                variables = a,
#                                year = 2017,
#                                output = "wide",
#                                state = "CA",
#                                county = counties,
#                                geometry=TRUE,
#                                survey="acs5")
#  write_csv(overlay_indicators, "overlay_indicators.csv")
#  View(overlay_indicators)
#}
#grab_acs_variables("B19013_001")
