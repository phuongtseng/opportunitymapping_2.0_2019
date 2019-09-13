---
  title: "CensusFinancialData"
author: "Phuong Tseng"
date: "5/22/2018"
output:
  html_document:
  toc: yes
toc_float: yes
pdf_document: default
---
  
  ## Information about the Script:
  
#Do file created by Heather Bromfield in Stata, Finalized on March 9, 2018 Opportunity Mapping Project 2.0, Haas Institute for a Fair and Inclusive Society. 
# It was revised & updated by Phuong Tseng in May 2018 in R with notes labeled as #@
# This do file contains procedures and explanations for cleaning and calculating the tax base capacity indicator.
---
  
  ## 1R. Set-up the libraries and directory in R
#{r setup, include=FALSE}
knitr::opts_chunk$set(eval = FALSE, include = FALSE)

library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(plyr)
library(data.table)


## 2R. Read the Financial Data
X2012_FinData_df <- read_excel("/Users/ptseng/Documents/GitHub/opportunitymapping/taxbase/2012_FinData copy.xlsx", col_names = FALSE, col_types = c("text", "text", "text", "text", "text", "text", "numeric", "numeric", "text"))
colnames(X2012_FinData_df) <- c("state", "gov_type", "county", "unit", "auto", "item", "amt", "year", "flag")
glimpse(X2012_FinData_df)
#write.csv(X2012_FinData_df, "X2012_Fin_Data_df.csv")
#@1 million records of all tax codes in CA
#get city and county revenues outside the city (dataset says about county revenues and includes or excludes municipality), then later pick apart these 

## 3R. Filter the data by type and state
#@ The gov_type code refers to the level of data, so "2" is municipalities/cities, and "5" is school-district level data
#X2012_municp <- dplyr::filter(X2012_FinData_df, state =="05", gov_type=="2")
X2012_FinData_df <- dplyr::filter(X2012_FinData_df, state=="05", gov_type=="2"|gov_type=="3"|gov_type=="4"|gov_type=="5") #64,443 records
X2016_FinEst <- dplyr::filter(X2016_FinEst, state =="05", gov_type=="1")


## 4R. Filter by revenue codes 
df <- filter(X2012_FinData_df, grepl('^[AD-DT-U]', item)) #23,823 records
#X2012_municp2 <- X2012_municp[!grepl("[E-S;W-Z]", X2012_municp$item),]

#The ones below did not work but it was good try
#@ X2012_FinData_dftest3 <- X2012_FinData_df4[which(startsWith(X2012_FinData_df4$item, c("A","B","C","D","T","U"))),]
#@ X2012_FinData_dftest3 <- X2012_FinData_df4[which(endsWith(X2012_FinData_df4$item, c("T","U"))),]
#@ X2012_FinData_dftest <- (-(endsWith(X2012_FinData_dftest2$item, "[U-T]")))

#@ Multiple tax revenue codes
#@ This is where there is discrepancies in Heather's data vs mine. Her data resulted in a larger amount of revenue than that of my results. Ask her for the table.
#@ This resulted in 1,879 records or Alameda with 67 records, this should leave us with df and df3

## 3. Generate a variable that concatenates all of the ID items together.
df$county <- str_pad(df$county, 3, side=c("left"), pad = "0")
df$unit <- str_pad(df$unit, 3, side=c("left"), pad = "0")
df$state <- str_pad(df$state, 2, side=c("left"), pad = "0")
### gen ID = state + gov_type + cnty + unit + auto ;
df$ID <-paste(df$state, df$gov_type, df$county, df$unit, sep = "")

###### Finally, I'm aggregating all of the revenue items for each jurisdiction. The "collapse" command additionally eliminates all of the other fields (which are extranneous for our purposes.
### collapse (sum) revenueitems=amt, by(ID) ;
#@ Make sure to remove column item because it prevents R from aggregating all these rows due to the different categorization
df <- ddply(df,.(ID, state, gov_type, county, unit, auto, year, flag),summarize,revenueitems=sum(amt),number=length(ID))
# There are now 4465 records

## As a quick check to make sure that this worked properly, I'm entering in a command that displays the data to make sure that things seem right. The 3rd character in the ID code indicates what type of jurisdiction this is; a value of 2 indicates that the jurisdiction is a municipality. Sure enough, there are 14 observations with 2 as their 3rd character, and there are 14 incorporated cities in Alameda County.

### list ID revenueitems ;

## Because these numbers represent thousands of dollars, we have to multiply by
### gen revenueitems_final = revenueitems * 1000 ;
df$amount_in_thousands <- df$revenueitems * 1000

#STEP 2 to work with the government integrated directory
GID_2012 <- read_csv("~ptseng/Downloads/2012_Individual_Unit_File_Revised/Fin_GID_2012.txt", col_names = FALSE)
GID_2012$state <- substring(GID_2012$X1, 1,2)
GID_2012$jrsd_type <- substring(GID_2012$X1, 3,3)
GID_2012$cnty <- substring(GID_2012$X1, 4,6)
GID_2012$etc <- substring(GID_2012$X1, 7,14)
GID_2012$jrdsct_name <- substring(GID_2012$X1, 15,78)
GID_2012$county <- substring(GID_2012$X1, 79,113)
GID_2012$unknown <- substring(GID_2012$X1, 114,134)
#@ There are 90,315 records

#@ This is to filter Governments Integrated Directory file to get the bay area records
GID_2012_df <-dplyr::filter(GID_2012, state =="05", cnty=="001" | cnty=="007"| cnty=="021"| cnty=="028"| cnty=="038"| cnty=="041"| cnty=="043"| cnty=="048"| cnty=="049") %>% mutate(ID=paste0(state,jrsd_type,cnty,etc))
## In order to make sure that we'll be able to join this data to the crosswalk data, there are a couple of changes that need to be made: (1) removing all jurisdiction types that are not cities; (2) creating a new variable for the field containing the city names that matches the crosswalk's variable name; and (3) dropping the variable "county", which also exists in the crosswalk data but is of a different data type.
drop <- c("X1", "county")
GID_2012_df2 = GID_2012_df[,!(names(GID_2012_df) %in% drop)]
GID_2012_df2[-c("X1","county"),]
GID_2012_df2 <- GID_2012_df2[!grepl("AUTH", GID_2012_df2$jrdsct_name),]
GID_2012_df2 <- GID_2012_df2[!grepl("AGENCY", GID_2012_df2$jrdsct_name),]
GID_2012_df2$ID2 <- substring(GID_2012_df2$ID, 0,9)
GID_2012_df2 <- GID_2012_df2[,-c(7),drop=F]
View(GID_2012_df2)

#@ Do this for the municipal revenue mutate(ID=paste0(state,jrsd_type,cnty,etc))
#df$ID <-paste(df$state, df$gov_type, df$county, df$unit, sep = "")
#GID_2012_df <-dplyr::filter(GID_2012, state =="05", cnty=="001" | cnty=="007"| cnty=="021"| cnty=="028"| cnty=="038"| cnty=="041"| cnty=="043"| cnty=="048"| cnty=="049") %>% mutate(ID=paste0(state,jrsd_type,etc))

# GID_2012_df file shows that county 001 is alameda, 007 is contracosta, 021 -marin, 048-solano, 049-sonoma, encompases 001/041/038 san francisco, 041-sanmateo, 028-napa, 043-santa clara
merge <- merge(df, GID_2012_df2, by.x="ID", by.y="ID2")

setwd("/Users/ptseng/Documents/Research/tax capacity/2016_Individual_Unit_file")
X2016_FinEst <-
  read_table("2016FinEstDAT_12042018modp_pu.txt",
             col_names = FALSE)
View(X2016_FinEst)
#X2016_FinEstDATA <- X2016FinEstDAT_12042018modp_pu

X2016_FinEst$state <- substr(X2016_FinEst$X1, 1, 2)
X2016_FinEst$gov_type <- substr(X2016_FinEst$X1, 3, 3)
X2016_FinEst$cnty <- substr(X2016_FinEst$X1, 4, 6)

X2016_FinEst <- X2016_FinEst %>% select(X1, X2, state, cnty, gov_type) %>% filter(state == "05", gov_type=="2"|gov_type=="3"|gov_type=="4"|gov_type=="5") #26,159 records
#Since I took these codes from city, township, special district, and independent school district, it would make sense to aggregate it at the city level for the county. However, the jurisdiction names are so complex, I'm going to aggregate it to the county level as these tax codes are 

X2016_FinEst$unit <- substr(X2016_FinEst$X1, 7, 9)
X2016_FinEst$own_unit <- substr(X2016_FinEst$X1, 10, 14)
X2016_FinEst$taxcode <- substr(X2016_FinEst$X1, 15, 17)
X2016_FinEst$flag <- str_sub(X2016_FinEst$X2, start= -1)
X2016_FinEst$test_amount <- str_replace_all(X2016_FinEst$X2, fixed("2016"), " ") 
X2016_FinEst$amount <- str_sub(X2016_FinEst$test_amount, end=-2)

#Now remove column number 10 test_amount 
X2016_FinEst <- X2016_FinEst[,-c(10), drop=F]

#Some have a "0" value amount so make sure to adjust these numbers
X2016_FinEst <- X2016_FinEst %>% mutate(amount=as.numeric(amount), amount_2016=amount*1000)

#Keep A, D, T, U
X2016_FinEst_df <- filter(X2016_FinEst, grepl('^[AD-DT-U]', taxcode)) #8992 records
X2016_FinEst_df$ID <- paste0(X2016_FinEst_df$state,X2016_FinEst_df$gov_type,X2016_FinEst_df$unit,X2016_FinEst_df$own_unit)
merge_2016 <- merge(X2016_FinEst_df, Fin_GID_2016, by.x="ID", by.y="ID")

setwd("/Users/ptseng/Documents/Research/tax capacity/2016_Individual_Unit_file")
merge_2016 <- fread("merge_2016.csv")
merge_2016$Population <- str_sub(merge_2016$X__5, end=-3)
merge_2016 <- merge_2016 %>% mutate(Population=as.numeric(Population))
head(merge_2016)

# Aggregating all of the revenue items for each jurisdiction
merge_2016 <-
  merge_2016 %>% group_by(ID, state.x, gov_type, unit, own_unit, statecode, countycode, jrsd_type, etc, jrdsct_name, X__3) %>% 
  summarize(amount_total_2016 = sum(amount_2016), Population_total_2016 = sum(Population), number = length(ID))

#note that some records had NAs in the Population column so I had to go to census and manually enter these numbers using ACS 5-yr 2017 estimates
###NOTE THE NUMBER OF NAs was 252, mannually entered X numbers
#https://censusreporter.org/profiles/97000US0636670-shoreline-unified-school-district-ca/
merge_2016$rev_p_cap <- merge_2016$amount_total_2016/merge_2016$Population_total_2016
write.csv(rev_p_cap, "Rev_p_cap_2016.csv")

rev_p_cap <- read_csv("/Users/ptseng/Documents/Research/tax capacity/2016_Individual_Unit_file/Rev_p_cap.csv")
rev_p_cap$rev_p_cap <- rev_p_cap$amount_total_2016/rev_p_cap$Population_total_2016

#264 - 252 = 12 entries (municipalities all had populations), school districts 252-99 = 153 entered manually
library(readr)
geocorr2014 <- read_csv("geocorr2014.csv")
geocorr2014 <- geocorr2014[-c(1), , drop = F]
geocorr2014 <- mutate(
  geocorr2014,
  pop10 = as.numeric(pop10))
  #fix a bad read-in resulting from spanish characters
 # placename = gsub('Caqada', 'Canada', placenm),
  #placename =
   # substring(placenm, 1, nchar(placenm) - 4),
  #jrdsct_name =
   # str_to_upper(placenm)
#)

#@Try to change the abbreviated labels to the longname since school names are inconsistent in the finance data
#@gsubfn("X2012_school4_BayArea", list("ELEM"="ELEMENTARY", "UNIF"="UNIFIED", "SCH"="SCHOOL", "DIST"="DISTRICT"), c("x","y","z"))
#create a dummy variable to rename the names

names <- rev_p_cap
names$name <- names$jrdsct_name
names$name = gsub(' SCH\\w* ', ' SCHOOL ', names$name)
names$name = gsub(' EL\\w* ', ' ELEMENTARY ', names$name)
names$name = gsub(' ELEM\\w* ', ' ELEMENTARY ', names$name)
names$name = gsub(' UNI\\w* ', ' UNIFIED ', names$name)
names$name = gsub(' UN\\w* ', ' UNION ', names$name)
names$name = gsub(' DIST\\w* ', ' DISTRICT ', names$name)
#names$name <- str_replace_all(names$name, fixed("DIST"), "RICT")
names$name = gsub(' D$\\w* ', ' DISTRICT ', names$name)

names$name = gsub(' JT\\w* ', ' JOINT ', names$name)
names$name = gsub(' VAL\\w* ', ' VALLEY ', names$name)

#Special district (Water) create a municipality and the water district has a boundary where 80% of people are inside the city vs outside. 10 units 80% of its area is within it boundary, apportion s

library(knitr)
purl("taxbase_capacity.Rmd",
     output = "/Users/ptseng/Documents/GitHub/opportunitymapping/CensusFinancialData.R",
     documentation = 1)
#take the 100% of special districts into the calculation. What happened to tracts outside of municipalities?
#data comparable 
#get county numbers from 2016, is that data only for the county part excluding municipalities or including municipalities. county revenue (35) - municipality revenue (25) = extract city from county


county_pop <- fread("ACS_17_5YR_B01003_with_ann.csv") 
county_pop <- county_pop[-c(1), , drop = F]
names(county_pop) <- gsub(x = names(county_pop),
                        pattern = "\\.",
                        replacement = "_")
county_pop$cntycode <- str_sub(county_pop$GEO_id2, 3,5)
rev_p_cap$cntycode <- paste0("0", rev_p_cap$countycode)
merge_county_pop <- merge(county_pop, rev_p_cap, by="cntycode")
merge <- merge(merge_county, merge_county_pop, by="jrdsct_name")

merge$city_county_amount <- merge$amount_county_2016 - merge$amount_total_2016
merge$Population_county <- merge %>% mutate(HD01_VD01=as.numeric(HD01_VD01))
merge$city_county_pop <- merge$Population_total_2016 - merge$Population_county
merge <- city_county_amount/city_county_pop



##@ firstrow(variables) replace

## We're now ready to map the data.

## Note: There are several census tracts that have no jurisdiction name listed. This seems to be because these tracts partially cover an incorporated Alameda County jurisdiction, and also partially cover an unincorporated area.---#Created: November 26, 2018 at 10:59AM
#Modified: March 26, 2019 at 12:54AM
#Last Opened: December 5, 2018 at 1:05PM
#working directory:
#Lines 166 and below are revised methodology that transfers municipal's revenues to block groups ---output:html_document
editor_options:chunk_output_type:console---## Information about the Script:
  # This script contains procedures and explanations for cleaning and calculating the tax base/ capacity indicator.
  # There are 4 important steps and 3 different datasets in this analysis. The first step is to clean and filter the Finance data. Then clean the Government Integrated Directory (GID) datatset, which has the jurisdictions, and remove all cities that are not within the bay area. The third step merges both finance and GID with crosswalks. Lastly, analysis and aggregation of municipal revenues.
  # Credit to Heather Bromfield for developing this analysis in March 2018 in Stata. Revised & updated procedures by Phuong Tseng in May 2018 in R. Reviewer and editor: Arthur Gailes in November 2018
  
  # Load all libraries in R
  knitr::opts_chunk$set(
    eval = FALSE,
    include = FALSE,
    warning = FALSE,
    message = FALSE
  )
x <- c("dplyr", "data.table", "readr", "readxl", "stringr", "RCurl")
lapply(x, require, character.only = TRUE)

#This part did not work well, and therefore, I had to export and manually remove revenue codes that are not A-D and T-V in Excel.*
#Note: For unincoporated areas take the county revenue to account for these areas' services not covered by municipalities since it falls outside.
#Raj chetty used
#1) local tax base of total tax revenue per capita/mean household income per capita for working age adults
#2) local tax rate per capita
#3) local government expenditure per capita from 1992 opportunity as resources because municipality has to be collected for the state as to what flows into the municipality

#Difference between expenditure and capacity metropolitan
#Calculating expenditure and tax rate (from Chetty's work, pg 57-59 of 2014 paper)

### Generate a variable that concatenates all of the columns together
X2012_municp3 <- X2012_municp2
X2012_municp3$county <-
  str_pad(X2012_municp3$county,
          3,
          side = c("left"),
          pad = "0")
X2012_municp3$unit <-
  str_pad(X2012_municp3$unit,
          3,
          side = c("left"),
          pad = "0")
X2012_municp3$state <-
  str_pad(X2012_municp3$state,
          2,
          side = c("left"),
          pad = "0")
X2012_municp3$ID <-
  paste(
    X2012_municp3$state,
    X2012_municp3$gov_type,
    X2012_municp3$county,
    X2012_municp3$unit,
    sep = ""
  )


### Aggregating all of the revenue items for each jurisdiction
X2012_municp4 <-
  X2012_municp3 %>% group_by(ID, state, gov_type, county,
                             unit, auto, year, flag) %>%
  summarize(revenueitems = sum(amt), number = length(ID))

X2012_municp4$revenueitems_total <-
  X2012_municp4$revenueitems * 1000


## Part 2: Clean the Government Integrated Directory dataset
GID_2012 <-
  read_tsv("data/input/Fin_GID_2012.txt", col_names = FALSE)

GID_2012$state <- substring(GID_2012$X1, 1, 2)
GID_2012$jrsd_type <- substring(GID_2012$X1, 3, 3)
GID_2012$cnty <- substring(GID_2012$X1, 4, 6)

GID_2012$etc <- substring(GID_2012$X1, 7, 14)
GID_2012$jrdsct_name <- substring(GID_2012$X1, 15, 78)
GID_2012$county <- substring(GID_2012$X1, 79, 113)
GID_2012$unknown <- substring(GID_2012$X1, 114, 134)

### Filter the data to get the bay area records
GID_2012_df <-
  dplyr::filter(
    GID_2012,
    state == "05",
    cnty == "001" |
      cnty == "007" |
      cnty == "021" |
      cnty == "028" |
      cnty == "038" |
      cnty == "041" |
      cnty == "043" |
      cnty == "048" |
      cnty == "049"
  ) %>% mutate(ID = paste0(state, jrsd_type, cnty, etc))

### Drop the first two columns
drop <- c("X1", "county")
GID_2012_df2 = GID_2012_df[, !(names(GID_2012_df) %in% drop)]

### Remove all jurisdiction types that are not cities

#df replace
GID_2012_df2 <-
  GID_2012_df2[!grepl("AUTH", GID_2012_df2$jrdsct_name), ]
GID_2012_df2 <-
  GID_2012_df2[!grepl("AGENC", GID_2012_df2$jrdsct_name), ]
head(GID_2012_df2)

### Create a new ID field and drop a column
GID_2012_df2$ID2 <- substring(GID_2012_df2$ID, 0, 9)
GID_2012_df2 <- GID_2012_df2[, -c(7), drop = F]
# View(GID_2012_df2)

## Part 3A: Merge datasets (municipal revenue with the GID)
X2012_municp4_BayArea <-
  merge(X2012_municp4, GID_2012_df2, by.x = "ID",
        by.y = "ID2")

## Part 3B: Download and clean the block to city crosswalk file
#Geocoor's allocation factor shows that census block to city ratio is 1:1. Therefore, this approach takes the block to city crosswalk population. <b>Note that the geocorr14 dataset might need a new URL.</b>
#downloading doesn't work, fils can be downloaded from Missouri geocorr14, specifying a block to city crosswalk
library (RCurl)
download <- getURL("http://mcdc.missouri.edu/tmpscratch/20JUN1910122.geocorr14/geocorr14.csv")
download <- read.csv(text = download, header=TRUE)
download <-
  read_csv("geocorr2014.csv", locale = locale(encoding = 'ASCII'))
block_city <- download[-c(1), , drop = F]
block_city <- mutate(
  block_city,
  pop10 = as.numeric(pop10),
  placename = as.character(placenm14),
  #fix a bad read-in resulting from spanish characters
  placename = gsub('Caqada', 'Canada', placename),
  placename =
    substring(placename, 1, nchar(placename) - 4),
  jrdsct_name =
    str_to_upper(placename)
)

### Assign the variable block_city2 the sum of population of blocks value by jurisdictions
block_city2 <-
  block_city %>% group_by(jrdsct_name) %>% summarise(juris_pop = sum(pop10)) %>% ungroup()


#We want to merge it back to each block so that when we take the city revenue and divide it by the sum of pop in a jurisdiction, we will get the block revenue per person value

# give each block a variable for the sum of it's jurisdiction's population
block_city3 <- left_join(block_city, block_city2, by = "jrdsct_name")
block_city_walk <-
  left_join(X2012_municp4_BayArea, block_city2, by = "jrdsct_name")

## Export the files
#write_csv(block_city4, "block_city4.csv", na ="NA")
#save(list=c("block_city4", "block_city3"), file="../Users/ptseng/Documents/Outputs/taxbase/block_city4.csv")
