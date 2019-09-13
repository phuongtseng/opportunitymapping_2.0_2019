---
  title: "taxbase_capacity"
author: "Phuong Tseng"
date: "5/22/2018"
output:
  html_document:
  toc: yes
toc_float: yes
pdf_document: default
---
  
  ## Information about the Script:
  
  > Do file created by Heather Bromfield in Stata, Finalized on March 9, 2018 Opportunity Mapping Project 2.0, Haas Institute for a Fair and Inclusive Society. 
> It was revised & updated by Phuong Tseng in May 2018 in R with notes labeled as #@
> This do file contains procedures and explanations for cleaning and calculating the tax base capacity indicator.
---
## 1R. Set-up the libraries and directory in R
```{r echo=TRUE}
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(plyr)
library(data.table)
```
## 2R. Read the Financial Data
```{r echo=FALSE}
X2012_FinData_df <- read_excel("/Users/ptseng/Documents/Outputs/taxbase/2012_FinData copy.xlsx", col_names = FALSE, col_types = c("text", "text", "text", "text", "text", "text", "numeric", "numeric", "text"))
colnames(X2012_FinData_df) <- c("state", "gov_type", "county", "unit", "auto", "item", "amt", "year", "flag")
glimpse(X2012_FinData_df)

#@ *Skips to line 89
#@1 million records of all tax codes in CA
```
## 3R. Filter the data by type and state
```{r echo=TRUE}
#@ The gov_type code refers to the level of data, so "2" is municipalities/cities, and "5" is school-district level data
X2012_municp <- dplyr::filter(X2012_FinData_df, state =="05", gov_type=="2")
#@ This resulted in 25,613 records in CA, but by county of Alameda returns 890
View(X2012_municp)

## 4R. Filter by revenue codes
X2012_municp2 <- X2012_municp[!grepl("[E-S;W-Z]", X2012_municp$item),]

#@ Had to export and remove it manually in Excel then import it back to R with 375 records
X2012_municp3 <- fread("/Users/ptseng/Documents/Outputs/taxbase/X2012_municp2.xls")

## 3. Generate a variable that concatenates all of the ID items together.
```{r echo=FALSE}
X2012_municp3$county <- str_pad(X2012_municp3$county, 3, side=c("left"), pad = "0")
X2012_municp3$unit <- str_pad(X2012_municp3$unit, 3, side=c("left"), pad = "0")
X2012_municp3$state <- str_pad(X2012_municp3$state, 2, side=c("left"), pad = "0")
### gen ID = state + gov_type + cnty + unit + auto ;
X2012_municp3$ID <-paste(X2012_municp3$state, X2012_municp3$gov_type, X2012_municp3$county, X2012_municp3$unit, sep = "")
#@ 11,326 records
head(X2012_municp3$ID) 

#@ Make sure to remove column item because it prevents R from aggregating all these rows due to the different categorization
X2012_municp4 <- ddply(X2012_municp3,.(ID, state, gov_type, county, unit, auto, year, flag),summarize,revenueitems=sum(amt),number=length(ID))
#@ There are now 484 records

#@ This generates the city revenue is the same as revenueitems * 1000
X2012_municp4$city_rev <- X2012_municp4$revenueitems * 1000

## Cleaning the Governments Integrated Directory file to obtain the jurisdiction names
library(readr)
GID_2012 <- read_csv("~/Downloads/2012_Individual_Unit_File_Revised/Fin_GID_2012.txt", col_names = FALSE)
GID_2012$state <- substring(GID_2012$X1, 1,2)
GID_2012$jrsd_type <- substring(GID_2012$X1, 3,3)
GID_2012$cnty <- substring(GID_2012$X1, 4,6)
GID_2012$etc <- substring(GID_2012$X1, 7,14)
GID_2012$jrdsct_name <- substring(GID_2012$X1, 15,78)
GID_2012$county <- substring(GID_2012$X1, 79,113)
GID_2012$unknown <- substring(GID_2012$X1, 114,134)
#@ There are 90,315 records

#@ Filter this by Bay Area counties within California and create an ID
GID_2012_df <-dplyr::filter(GID_2012, state =="05", cnty=="001" | cnty=="007"| cnty=="021"| cnty=="028"| cnty=="038"| cnty=="041"| cnty=="043"| cnty=="048"| cnty=="049") %>% mutate(ID=paste0(state,jrsd_type,cnty,etc)) 

## In order to make sure that we'll be able to join this data to the crosswalk data, there are a couple of changes that need to be made: (1) removing all jurisdiction types that are not cities; (2) creating a new variable for the field containing the city names that matches the crosswalk's variable name; and (3) dropping the variable "county", which also exists in the crosswalk data but is of a different data type.*/
# keep if strpos(jrdsct_name,"CITY")
# drop if strpos(jrdsct_name,"DIST")
# drop if strpos(jrdsct_name,"AUTH")
# drop if strpos(jrdsct_name,"AGENCY")
# drop county
drop <- c("X1", "county")
GID_2012_df2 = GID_2012_df[,!(names(GID_2012_df) %in% drop)]
GID_2012_df2 <- GID_2012_df2[!grepl("AUTH", GID_2012_df2$jrdsct_name),]
GID_2012_df2 <- GID_2012_df2[!grepl("AGENCY", GID_2012_df2$jrdsct_name),]
GID_2012_df2 <- GID_2012_df2[!grepl("DIST", GID_2012_df2$jrdsct_name),] #@ Do not run this with schoolcrosswalk
#@ This resulted in 182 entries out of 4,485 records in the state 
#@ And 571 left (by keeping DIST) for merge with schools
View(GID_2012_df2)

#@ the Fin_GID_2012_df2 is the same as Heather's 2012_Clean_GID.dta, so since the data is already in R, I don't have to read the data in. The Fin_GID_2012_df2 has all records, not just Alameda
GID_2012_df2$ID2 <- substring(GID_2012_df2$ID, 0,9)
GID_2012_df2 <- GID_2012_df2[,-c(7),drop=F]
View(GID_2012_df2)
#@ This has 159 places with "sch"; 571 records

### merge the Governments Integrated Directory with the jurisdiction names by IDs
X2012_municp4_BayArea <- merge(X2012_municp4, GID_2012_df2, by.x="ID", by.y="ID2")#@This returns 101 records

#@ Now, we need to merge it with the census block crosswalks
library (RCurl)
download <- getURL("http://mcdc.missouri.edu/tmpscratch/20JUN1910122.geocorr14/geocorr14.csv")
block_city <- read.csv(text = download, header=TRUE) #@ It has 403,399 block records
block_city2 <- block_city[-c(1),,drop=F] #@ remove the 2nd row with the names

block_city2$pop10 <- as.numeric(as.integer(block_city2$pop10))
block_city2$placename <- as.character(block_city2$placenm14)
block_city2$placename <- substring(block_city2$placename, 1, nchar(block_city2$placename)-4) #@has to remove the last 4 ", CA"
block_city2$jrdsct_name <- str_to_upper(block_city2$placename)

#@ Taking a slightly different approach that the original version by using census blocks instead of tracts, but the final geographic outputs will be tract. To determine what the total population is of each jurisdiction, the missouri crosswalk file shows that the allocation factor of census blocks to city's ratio is 1:1. To calculate the tax revenues per capita then allocate the revenue per capita value to census tracts

#@ assign the variable block_city2 the sum of population of blocks value by jurisdictions
block_city3 <- block_city2 %>% group_by(jrdsct_name) %>% summarise_at(vars(pop10), sum) %>% ungroup()

#@ We want to merge it back to each block so that when we take the city revenue and divide it by the sum of pop in a jurisdiction, we will get the block revenue per person value
block_city4 <- left_join(block_city2, block_city3, by="jrdsct_name") #@ resulted in 9,564 tracts, 403,398 blocks

#To calculate the revenue per capita takes the city revenue which is the X2012_municp4_BayArea variable to divide it by the sum of blocks population per city

block_city4$Rev_p_capita <- X2012_municp4_BayArea$revenueitems_final/block_city4$pop10.y

block_city4$block_rev <- block_city4$Rev_p_capita * block_city4$pop10.x

block_city4$TractID <- paste0(block_city4$state, block_city4$county, block_city4$tract)

# Due to this aggregation, the other fields are lost and thus the results need to merge back to the jurisdiction names
block_city5 <- block_city4 %>% group_by(TractID) %>% summarise_at(vars(block_rev, pop10.x), sum) %>% ungroup()

#@ block merges with tract crosswalk to get the sum of block revenue at the tract level value then divide it by the tract population in the tract.
block_city5$tract_rev_p_capita <- block_city5$block_rev/block_city5$pop10.x

block_city4$TractID <- paste0(block_city4$county, block_city4$tract)

# Merge the bay area municipality names to block_city4 and then to block_city5
block_city_walk <- left_join(block_city5, block_city4, by="TractID") #@ Finally merged the municipalities revenue with population crosswalk to obtain 1430 tracts, but with blocks there are 51,410 blocks

View(tract_block_city5)
#@$$ aggtctrev <- sum of city_rev/California pop

### sort tract ;
#@ arrange(X2012_FinData$tract)
### by tract: egen aggtctrev = total(tract_revenue) ;
municp4_walk_rev <- ddply(municp4_walk,.(ID, year, revenueitems, revenueitems_final, jrdsct_name, jrsd_type, cnty, tract, cntyname, placenm, placename, pop10.x, afact, AFACT2, pop10.y, tract_revenue),summarize,aggtrev=sum(tract_revenue),number=length(ID))
#@ municp5 <- municp4_walk %>% group_by(tract) %>% summarise_at(vars(tract_revenue), sum) %>% ungroup()
unique(municp4_walk_rev, incomparables = FALSE)

### We're now ready to map the data.

### Note: There are several census tracts that have no jurisdiction name listed. This seems to be because these tracts partially cover an incorporated Alameda County jurisdiction, and also partially cover an unincorporated area.