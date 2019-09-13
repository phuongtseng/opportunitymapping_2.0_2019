#Final Filters

#Credit to Phuong Tseng and Arthur Gailes
library(data.table, dplyr, readxl)

setwd("/Users/ptseng/Downloads/fwdtractdataforbayarea")
input <- fread("BayAreaTract_2015_Filters_cSept2018.csv", header=TRUE)
colnames(input)
input_df <- input
# View(input)

setwd("/Users/ptseng/Documents/GitHub/opportunitymapping-master")
input_DI <- read_xlsx("bay_area_divergence_index_data.xlsx")
colnames(input_DI)

#Apply filters to each subset of data
#Method B: Poverty rate >= 30% and high segregation in divergence index OR single-parent families >= 30% and poverty rate >= 30%
input$P30_DI_HighSeg[which(input$EC_POV2>=0.30 & input_DI$`Segregation ranking based on Divergence Index`=="High segregation")] <- -1
input$P30_S30[which(input$EC_POV2>=0.30 & input$p_spf>=0.30)] <- -1
input$Final_Filters[which(input$P30_DI_HighSeg==-1 | input$P30_S30 == -1)] <- -1

#To check how many tracts are captured so that we could decide whether to divide it into two or leave it as the lowest category
sum(input$Final_Filters==-1, na.rm=TRUE)

# Assign the index scores to another variable so that it would be easier to restore them after turning them into "NAs" in the next step (#2)
indexDup <-  input$IndexScore
#You only need to do this if you want to restore index scores after manipulating them as in step 2.
input$IndexScore[which(input$Final_Filters == -1)] <- NA
#This makes sure that your filtered tracts won't be caught in the highest percentiles, which could make you assign less than 20% of your tracts to "very high". See below for more.
#If you want to keep your index scores, just do step 1.
input <- dplyr::mutate(input, percentile = rank(input$IndexScore, ties.method = 'min', na.last = F)/n(), oppcat = NA)
#Rank sorts by value, so dividing by n() produces percentile rankings.
#I like this method because you can sort NAs into the lowest (0th percentile) ranking. Quantile and ntile ignore NAs completely, but that means that you'll end up with slightly less than 40% of total tracts in your filter. In other words, you'll get 40% of your non-NA tracts, not 40% of your total tracts. Heather and Elizabeth did this last year with TCAC, so we ended up with fewer High resource tracts than we should've had.
input$oppcat[which(input$percentile > 0.8)] <- 'Very High Opportunity'
input$oppcat[which(input$percentile > 0.6 & input$percentile <= 0.8)] <- 'High Opportunity'
input$oppcat[which(input$Final_Filters == -1)] <- 'Very Low Opportunity'
#These are all just assigning categories
remaining <- input$IndexScore[which(is.na(input$oppcat))]
#Creates a vector for the remainder of your tracts
remaining <- ntile(remaining, 2)
#Splits the remained into the final two groups
#Note that your NA index scores will return an NA value. You can assign that to "Missing" or whatever text you want later
remaining <- ifelse(remaining == 2, 'Moderate Opportunity', 'Low Opportunity')
#Assigns categories, retains NA values
input$oppcat[which(is.na(input$oppcat))]  <- remaining
#Restores index scores
input$IndexScore <- indexDup

#Make sure the directory is set back to the opportunitymapping-master folder before exporting
setwd("/Users/ptseng/Documents/GitHub/opportunitymapping-master")
# Export it to csv
write.csv(input, "input.csv")

