# =============================================================
# SUMMARISE SIGNIFICANT TREND RESULTS BY CATCHMENT AND REGION =
# =============================================================

# ======================
# Addition 8-Oct-2015 - Remove Trend data based on Council-specified exclusions
# 
# Before the trends are summarised for delivery to IT Effect, a number of specific
# exclusions must be applied based on requests by Councils. These exclusions can
# based on Agency, Region or Site.
#
# The rules driving these exclusions need to be loaded from a csv file with the
# following structure

#Type	   |Identifier     |Measurement|Trend5 |Trend10|State
# -------+---------------+-----------+-------+-------+-------
#Agency	 |WaikatoRegion	 |DRP        |FALSE	 |TRUE   |FALSE     <- Agency
#Region	 |Taranaki	     |TN	       |TRUE	 |TRUE   |FALSE     <- Region
#Site	   |ES-00045	     |DRP	       |TRUE   |TRUE   |FALSE     <- Sites

# Values in the Trend5, Trend10 and State columns specify whether the values should
# be excluded or not. Where values are TRUE, these values are to be excluded for that analysis.

# The process here will only exclude data for 5 yearand Trend10.

# The exclusion is processed AFTER the 5 year and 10 year trends have been combined
# into one dataframe.


# Set working directory
od<-getwd()
setwd("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state")

#/* -===Include required function libraries===- */ 

source("scripts/WQualityStateTrend/lawa_state_functions.R")


# ===============================================
# Load trend data - monthly, bimonthly, quarterly

# 5 YEAR TREND
#Reference Dates
StartYear <- 2012
EndYear <- 2016

# Trend pass/fail logs
vm<-read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_monthly_",StartYear,"-",EndYear,".csv",sep=""))
#vb<-read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_bimonthly_",StartYear,"-",EndYear,".csv",sep=""))
vq<-read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_quarterly_",StartYear,"-",EndYear,".csv",sep=""))

# Trend results
m<-read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_monthly_",StartYear,"-",EndYear,".csv",sep=""))
#b<-read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_bimonthly_",StartYear,"-",EndYear,".csv",sep=""))
q<-read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_quarterly_",StartYear,"-",EndYear,".csv",sep=""))

m$m<-12
#b$m<-6
q$m<-4

# Append Dataframes
#vmbq5  <- rbind.data.frame(vm,vb,vq)
vmbq5  <- rbind.data.frame(vm,vq)
vmbq5$period<-5

#mbq5 <- rbind.data.frame(m,b,q)
# Dropping bimonthly from finalised trend - NIWA recommendation
mbq5 <- rbind.data.frame(m,q)
mbq5$period<-5


# Load trend data - monthly, bimonthly, quarterly

# 10 YEAR TREND
StartYear <- 2007
EndYear <- 2016

vm<-read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_monthly_",StartYear,"-",EndYear,".csv",sep=""))
#vb<-read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_bimonthly_",StartYear,"-",EndYear,".csv",sep=""))
vq<-read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_quarterly_",StartYear,"-",EndYear,".csv",sep=""))

m<-read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_monthly_",StartYear,"-",EndYear,".csv",sep=""))
#b<-read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_bimonthly_",StartYear,"-",EndYear,".csv",sep=""))
q<-read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_quarterly_",StartYear,"-",EndYear,".csv",sep=""))

m$m<-12
#b$m<-6
q$m<-4

# Append Dataframes
#vmbq9 <- rbind.data.frame(vm,vb,vq)
vmbq9 <- rbind.data.frame(vm,vq)
vmbq9$period<-10

#mbq9 <- rbind.data.frame(m,b,q)
# Dropping bimonthly from finalised trend - NIWA recommendation
mbq9 <- rbind.data.frame(m,q)
mbq9$period<-10
rm(m,q,vm,vq)

mbq <- rbind.data.frame(mbq5,mbq9)
vmbq <- rbind.data.frame(vmbq5,vmbq9)


# ========================
# Processing exclusions
# ========================

# Load exclusion file
exclusions <- read.csv(file="//file/herman/R/OA/08/02/2017/Water Quality/R/lawa_state/TrendExclusion2017.csv",stringsAsFactors=FALSE)

## BEGIN CODE ADDITION   ID==CawthronSwitchTrendOutput ------------
## ID:   CawthronSwitchTrendOutput
## DATE: 2018-02-14 
## BY:   Sean Hodges
##       Horizons Regional Council
## DESC: Councils may choose not to have trend data presented on LAWA. The LAWA code
##       uses an exclusion file to drop data from agencies, regions and sites at 
##       each Councils request. This code addition introduces a switch to turn off
##       exclusions based on regions.
## OPTIONAL STEP
## Created to address request from Cawthron, and endorsed by EMAR Project Manager, 
## for all trend data from all councils, irrespective of Taranaki's exclusion
##
## Create a variable to control execution of this additional code.
## This would best be handled by a config or ini file for this code, but for
## time-being, we'll hardcode directly in this script.
## OPTIONAL STEP STATUS
CawthronSwitchTrendOutput <- TRUE

if(CawthronSwitchTrendOutput==TRUE){
exclusions <- exclusions[exclusions$Type!="Region",]
}
## END CODE ADDITION

# Mark records for exclusion------------
mbq$Exclusion <- FALSE
xagency<-exclusions[exclusions$Type=="Agency",]
xregion<-exclusions[exclusions$Type=="Region",]
xlawaid<-exclusions[exclusions$Type=="Site",]

for(x in 1:nrow(xagency)){
  # Exlusion 5 year
  mbq$Exclusion[mbq$Agency==xagency$Identifier[x] & mbq$Parameter==xagency$Measurement[x] & mbq$period==5] <- xagency$Trend5[x]
  # Excusion 10 year
  mbq$Exclusion[mbq$Agency==xagency$Identifier[x] & mbq$Parameter==xagency$Measurement[x] & mbq$period==10] <- xagency$Trend10[x]
}

for(x in 1:nrow(xregion)){
  # Exlusion 5 year
  mbq$Exclusion[mbq$Region==xregion$Identifier[x] & mbq$Parameter==xregion$Measurement[x] & mbq$period==5] <- xregion$Trend5[x]
  # Excusion 10 year
  mbq$Exclusion[mbq$Region==xregion$Identifier[x] & mbq$Parameter==xregion$Measurement[x] & mbq$period==10] <- xregion$Trend10[x]
}

for(x in 1:nrow(xlawaid)){
  # Exlusion 5 year
  mbq$Exclusion[mbq$LAWAID==xlawaid$Identifier[x] & mbq$Parameter==xlawaid$Measurement[x] & mbq$period==5] <- xlawaid$Trend5[x]
  # Excusion 10 year
  mbq$Exclusion[mbq$LAWAID==xlawaid$Identifier[x] & mbq$Parameter==xlawaid$Measurement[x] & mbq$period==10] <- xlawaid$Trend10[x]  
}

rm(xagency,xregion,xlawaid)

# Exclude records
cat("Dropping records from trends as requested by Councils:\n",sum(mbq$Exclusion),"Site/parameter combinations dropped out of a total of",nrow(mbq),"\n")
mbq <- mbq[mbq$Exclusion==FALSE,]  # <-- only retain those records where the exclusion column contains a FALSE value.

# ========================
# Significant results now a deprecated approach based on Larned et al (2015)
# Flag significant results
#mbq$p.sig <- ifelse(mbq$p.value<=0.05,1,0)

# Summary for significant results at a catchment level
# s <- summaryBy(p.sig~period+Catchment+Parameter+m,  
#                data=mbq, 
#                FUN=c(sum), na.rm=TRUE, keep.name=TRUE)
# t <- summaryBy(p.sig~period+Catchment+Parameter+m,  
#                data=mbq, 
#                FUN=c(length), keep.name=TRUE)
# u <- summaryBy(Sen.Pct~period+Catchment+Parameter+m,  
#                data=mbq, 
#                FUN=c(mean), keep.name=TRUE)

# Flag results outside the 100(1–2α)% confidence interval for trend direction-testing
mbq$trend.flag <- ifelse(mbq$zeroLocationCL90,0,1)

# For catchments, this summaryBy statement sums up how many
# sites have a trend direction (but does not indicate what direction)
# The trend.flag column is populate with 1's and 0's
s <- summaryBy(trend.flag~period+Catchment+Parameter+m,  
               data=mbq, 
               FUN=c(sum), na.rm=TRUE, keep.name=TRUE)

# For catchments, this summaryBy statement determines how many (vector length)
# site/measurement combos have had trend estimation applied.
t <- summaryBy(trend.flag~period+Catchment+Parameter+m,  
               data=mbq, 
               FUN=c(length), keep.name=TRUE)

# For catchments, this summaryBy statement determines the median
# index value for -1, 0 or 1 trend scores  ## CONFIRM APPROACH
# The sign of the resulting median will then give direction of catchment trend
u <- summaryBy(TrendScore~period+Catchment+Parameter+m,  
               data=mbq, 
               FUN=c(median), keep.name=TRUE)


s$Samples<-t$trend.flag # How many sites have trends estimates applied in Catchment
s$Trend.Proportion <- s$trend.flag/s$Samples # proportion of site/parameter combos with trend direction in catchment
s$Direction <- u$TrendScore # sign of this value gives direction for trend in catchment

v<-calcTrendScoreAggregate(s)

# # Summary for significant results at a regional level
# s <- summaryBy(p.sig~period+Region+Parameter+m,  
#                data=mbq, 
#                FUN=c(sum), na.rm=TRUE, keep.name=TRUE)
# t <- summaryBy(p.sig~period+Region+Parameter+m,  
#                data=mbq, 
#                FUN=c(length), keep.name=TRUE)
# u <- summaryBy(Sen.Pct~period+Region+Parameter+m,  
#                data=mbq, 
#                FUN=c(mean), keep.name=TRUE)
# s$count<-t$p.sig
# s$p.flag <- s$p.sig/s$count
# s$Sen.Pct <- u$Sen.Pct

# # Flag results outside the 100(1–2α)% confidence interval for trend direction-testing
# For regions, this summaryBy statement sums up how many
# sites have a trend direction (but does not indicate what direction)
# The trend.flag column is populate with 1's and 0's
s <- summaryBy(trend.flag~period+Region+Parameter+m,  
               data=mbq, 
               FUN=c(sum), na.rm=TRUE, keep.name=TRUE)

# For regions, this summaryBy statement determines how many (vector length)
# site/measurement combos have had trend estimation applied.
t <- summaryBy(trend.flag~period+Region+Parameter+m,  
               data=mbq, 
               FUN=c(length), keep.name=TRUE)

# For regions, this summaryBy statement determines the median
# index value for -1, 0 or 1 trend scores  ## CONFIRM APPROACH
# The sign of the resulting median will then give direction of catchment trend
u <- summaryBy(TrendScore~period+Region+Parameter+m,  
               data=mbq, 
               FUN=c(median), keep.name=TRUE)


s$Samples<-t$trend.flag # How many sites have trends estimates applied in Region
s$Trend.Proportion <- s$trend.flag/s$Samples # proportion of site/parameter combos with trend direction in Region
s$Direction <- u$TrendScore # sign of this value gives direction for trend in Region

w<-calcTrendScoreAggregate(s)

w$Catchment <- ""
v$Region <- ""
z<-rbind.data.frame(v,w)

# OUTPUT data to CSVs
write.csv(vmbq,file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_combined.csv")  ## LOG PASS-FAIL for Trend analysis
write.csv(vmbq,file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/SitesMeetingTrendCriteria.csv")   ## LOG PASS-FAIL for Trend analysis   ## A more sensible name to use when sharing this file

rm(vmbq,vmbq5,vmbq9)
write.csv(mbq,file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_sites_freq_combined.csv")    ## COMBINED TREND DATA FOR 5 AND 10 YEARS
write.csv(z,file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_catchment_region.csv")         ## 

# rearrange data to: 
# Location  Parameter  Altitude	Landuse	TrendScore	TrendFrequency 	Region	TrendPeriodYrs

mbq$Location <- mbq$LAWAID
z$Location <- paste(z$Region,as.character(z$Catchment),sep="")

## Adding in applied quarterly/monthly frequency data
#c <- mbq[c(2,3,8,27,28,7,13,14,17,16)]

c <- mbq[c(36,3,12,32,33,11,18,19,22,21)]

names(c) <- c("Location","Parameter","TrendScore","m","period","Freq",
              "Altitude","Landuse","Region","Frequency")
#names(c) <- c("Location","Parameter","Altitude","Landuse","TrendScore","m","Frequency","Region","period")

# This vector needs checking if fields added or removed
d <- z[c(11,3,9,4,1)]  ## Location,Parameter,TrendScore,m,period
names(d) <- c("Location","Parameter","TrendScore","m","period")

d$Freq<-""
d$Altitude <- "ALL"
d$Landuse <- "ALL"
d$Region <- ""
d$Frequency <- ""

# Getting list of unique catchments by region
#l1 <- l[c(10,5)]
#l2 <- unique(l1)

#l2$CatchString <- as.character(l2$Catchment)
#d1 <- merge(d, l2, by.x="Location",by.y="CatchString")
e <- rbind.data.frame(c,d)

## SAVE FINAL FILE SUMMARISING TREND RESULTS
write.csv(e,file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_sites_catchment_region.csv")

setwd(od)