# =============================================================
# PREPARE STATE, TREND & RAW DATA FOR DELIVERY TO IT EFFECT      =
# =============================================================

# ======================
# Created 8-Oct-2015 

# Based on the process Maree follows to prepare data, the following
# script attempts to emulate the steps for preparing data for
# delivery and outputing to csv and xlsx files.

# ======================
ANALYSIS<-"DELIVERY"
# Set working directory
od<-getwd()
setwd("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state")

#/* -===Include required function libraries===- */ 
source("lawa_state_functions.R")

wqparam <- c("BDISC","TURB","PH","NH4","TON","TN","DRP","TP","ECOLI") 
wqparam <- as.data.frame(as.matrix(wqparam))
names(wqparam)<-c("Parameter")

# ======================
# TRENDS

# ---------------------
# [2015-10-08] Load final trend file
trends<-read.csv(file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_sites_catchment_region.csv",stringsAsFactors = FALSE)

# ---------------------
# [2015-10-08] Remove DIN results - not required for LAWA upload - only there for River Awards & Morgan Foundation 
trends <- trends[trends$Parameter!="DIN",]
trends$X <- c(1:nrow(trends)) # updating row counter in data - only needed for debugging code

# ---------------------
# [2015-10-08] Load data to determine frequency's by region, catchment and site and retain those data that match
StartYear <- 2007
EndYear <- 2016
StartMonth <- 1
EndMonth <- 12
load(file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trenddata2007-2016.RData")

lawadata <- samplesByInterval(StartYear,EndYear,StartMonth,EndMonth,lawadata)

# Determine counts by site/parameter/year
df_minfreq_length <- summaryBy(Value~Region+Catchment+SiteName+parameter+year,data=lawadata,
                                     id=~LAWAID+FrequencyAll,
                                     FUN=c(length), keep.name=TRUE)

# Determine annual medians counts by site/parameter
df_minfreq_Parameter <- summaryBy(Value~Region+Catchment+SiteName+parameter,data=df_minfreq_length,
                               id=~LAWAID,
                               FUN=c(median), keep.name=TRUE)

# # Determine annual medians counts by site
# df_minfreq_Site <- summaryBy(Value~Region+Catchment+SiteName,data=df_minfreq_length,
#                                id=~LAWAID,
#                                FUN=c(median), keep.name=TRUE)

# Determine annual medians counts by catchment
df_minfreq_Catchment <- summaryBy(Value~Region+Catchment+parameter,data=df_minfreq_length,
                               FUN=c(median), keep.name=TRUE)

# Determine annual medians counts by region
df_minfreq_Region <- summaryBy(Value~Region+parameter,data=df_minfreq_length,
                                         FUN=c(median), keep.name=TRUE)

# Determining minimum frequency's
# Region
df_minfreq_Region$Frequency<-"Quarterly"
df_minfreq_Region$Frequency[df_minfreq_Region$Value==12]<-"Monthly"
# recode value here to 12 or 4 as well
df_minfreq_Region$Value[df_minfreq_Region$Value!=12]<-4

df_minfreq_Region <- df_minfreq_Region[is.na(df_minfreq_Region$Region)==FALSE,]
n <- names(df_minfreq_Region)
n[1]<-"Location"
names(df_minfreq_Region)<-n

df_minfreq_Region$Region<-df_minfreq_Region$Location
df_minfreq_Region$Catchment<-""
df_minfreq_Region$SiteName<-""

# Cross-join regions with parameters to have each parameter for each region
#df_minfreq_Region <- merge(x = df_minfreq_Region, y = wqparam, by = NULL)


# Catchment
df_minfreq_Catchment$Frequency<-"Quarterly"
df_minfreq_Catchment$Frequency[df_minfreq_Catchment$Value>=10]<-"Monthly"
# recode value here to 12 or 4 as well
df_minfreq_Catchment$Value[df_minfreq_Catchment$Value>=10]<-12
df_minfreq_Catchment$Value[df_minfreq_Catchment$Value<10]<-4

df_minfreq_Catchment <- df_minfreq_Catchment[is.na(df_minfreq_Catchment$Region)==FALSE,]
n <- names(df_minfreq_Catchment)
n[2] <-"Location"
names(df_minfreq_Catchment)<-n

df_minfreq_Catchment$Catchment<-""
df_minfreq_Catchment$SiteName<-""

# Cross-join regions with parameters to have each parameter for each region
#df_minfreq_Catchment <- merge(x = df_minfreq_Catchment, y = wqparam, by = NULL)

# Reordering columns
df_minfreq_Catchment <- df_minfreq_Catchment[,c(2,3,4,1,5,6,7)]

# Converting location column to character
df_minfreq_Catchment$Location <- as.character(df_minfreq_Catchment$Location)

# Parameter
df_minfreq_Parameter$Frequency<-"Quarterly"
df_minfreq_Parameter$Frequency[df_minfreq_Parameter$Value>=10]<-"Monthly"
# recode value here to 12 or 4 as well
df_minfreq_Parameter$Value[df_minfreq_Parameter$Value>=10]<-12
df_minfreq_Parameter$Value[df_minfreq_Parameter$Value<10]<-4


df_minfreq_Parameter <- df_minfreq_Parameter[is.na(df_minfreq_Parameter$Region)==FALSE,]
n <- names(df_minfreq_Parameter)
n[4]<-"parameter"
n[6]<-"Location"
names(df_minfreq_Parameter) <- n

# [2015-10-14] Reordering columns
df_minfreq_Parameter <- df_minfreq_Parameter[,c(6,5,7,1:4)]


df_minfreq_Parameter <- df_minfreq_Parameter[,c(1,7,2,3:6)]
df_minfreq_Catchment <- df_minfreq_Catchment[,c(1:3,5,4,6:7)]


# [2015-10-14] Binding tables together in order to join results to TRENDS table
df_minfreq <- rbind.data.frame(df_minfreq_Region,df_minfreq_Catchment,df_minfreq_Parameter)

n <- names(df_minfreq)
n[2]<-"Parameter"
names(df_minfreq) <- n


#issue with join below
tmp <- merge(trends,df_minfreq,by=c("Location","Parameter"),all.x=TRUE)

# [2015-10-14] Joining data to select min frequency matches
#tmp<-dplyr::left_join(trends,df_minfreq,by=c("Location","Parameter"))
tmp1<-tmp[tmp$m==tmp$Value,]



# ---------------------
# [2015-10-14] Drop columns
# Columns to keep
# Location	Parameter	Altitude	Landuse	TrendScore	Frequency	Region	period
# ARC-00014	DRP	      Lowland	  Urban         	-2	Monthly	  Auckland	5
# ARC-00014	ECOLI    	Lowland	  Urban           0	  Monthly	  Auckland	5
# ARC-00014	TN	      Lowland	  Urban         	0	  Monthly	  Auckland	5


tmp1<-tmp1[,c(1,2,7,8,4,10,9,6)]
names(tmp1) <-c("Location",	"Parameter",	"Altitude",	"Landuse",	"TrendScore",	"Frequency",	"Region",	"period")
# remove any NA rows
tmp1 <- tmp1[!is.na(tmp1$Location),]

# ---------------------
# Export to XL workbook
write.csv(tmp1,file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_fordelivery.csv")         ## 

# ---------------------
# Housekeeping
rm(trends,tmp,tmp1,df_minfreq,df_minfreq_Region,df_minfreq_Parameter,df_minfreq_Catchment,df_minfreq_length,lawadata,n,wqparam)


# ======================
# STATE

# ---------------------
# 1. Load final state file
state<-read.csv(file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/LAWA_STATE_FINAL_2012-2016.csv",stringsAsFactors = FALSE)

# 2. join the agency information back to the LAWAIDs.
# Reload the site table [should still be loaded if all scripts run together, but in case this script is run in isolation, this will ensure you get the
# the last-saved site table back]
load(file="//file/herman/r/oa/08/02/2017/Water Quality/ROutput/lawa_sitetable.RData")

# Fields have  been renamed from WFS feed to match what the code is
# expecting, as code originally written expecting data from Hilltop Site Table.
newFieldNames <- c("X","LAWAID","ID", "CouncilSiteID","UsedInLAWA","AltitudeGroup","LanduseGroup","FrequencyAll","Frequency",
                   "Region","Agency","ISLAND","CatchID","CatchType",
                   "NZREACH","Catchment","Comments","LawaCatchm","CatchLbl")

names(l) <- newFieldNames

# keep LAWAID and Agency
la<-l[,c(2,11)]
state <- merge(state, la, by.x="Location",by.y="LAWAID", all.x=TRUE) # Using Hilltop sitenames to match site information

#3. Combinations to keep
state$Filter<-FALSE

## Region combos

# All	    All	      Region|All
state$Filter[state$StateGroup=="Region|All" & state$AltitudeGroup=="All" & state$LanduseGroup=="All"] <- TRUE
# Upland	All	      Region|Upland
state$Filter[state$StateGroup=="Region|Upland" & state$AltitudeGroup=="Upland" & state$LanduseGroup=="All"] <- TRUE
# Lowland	All	      Region|Lowland
state$Filter[state$StateGroup=="Region|Lowland" & state$AltitudeGroup=="Lowland" & state$LanduseGroup=="All"] <- TRUE
# All	    Rural	    Region|Rural
state$Filter[state$StateGroup=="Region|Rural" & state$AltitudeGroup=="All" & state$LanduseGroup=="Rural"] <- TRUE
# All	    Forest  	Region|Forest
state$Filter[state$StateGroup=="Region|Forest" & state$AltitudeGroup=="All" & state$LanduseGroup=="Forest"] <- TRUE
# All   	Urban	    Region|Urban
state$Filter[state$StateGroup=="Region|Urban" & state$AltitudeGroup=="All" & state$LanduseGroup=="Urban"] <- TRUE
# Upland	Rural	    Region|Upland|Rural
state$Filter[state$StateGroup==" Region|Upland|Rural" ] <- TRUE
# Upland	Forest   	Region|Upland|Forest
state$Filter[state$StateGroup=="Region|Upland|Forest"] <- TRUE
# Lowland	Rural	    Region|Lowland|Rural
state$Filter[state$StateGroup=="Region|Lowland|Rural"] <- TRUE
# Lowland	Forest	  Region|Lowland|Forest
state$Filter[state$StateGroup=="Region|Lowland|Forest"] <- TRUE
# Lowland	Urban	    Region|Lowland|Urban
state$Filter[state$StateGroup=="Region|Lowland|Urban"] <- TRUE

## Catchment combos

# All	    All	      Catchment|All
state$Filter[state$StateGroup=="Catchment|All" & state$AltitudeGroup=="All" & state$LanduseGroup=="All"] <- TRUE
# Upland	All     	Catchment|Upland
state$Filter[state$StateGroup=="Catchment|Upland" & state$AltitudeGroup=="Upland" & state$LanduseGroup=="All"] <- TRUE
# Lowland	All	      Catchment|Lowland
state$Filter[state$StateGroup=="Catchment|Lowland" & state$AltitudeGroup=="Lowland" & state$LanduseGroup=="All"] <- TRUE
# All	    Rural	    Catchment|Rural
state$Filter[state$StateGroup=="Catchment|Rural" & state$AltitudeGroup=="All" & state$LanduseGroup=="Rural"] <- TRUE
# All	    Forest  	Catchment|Forest
state$Filter[state$StateGroup=="Catchment|Forest" & state$AltitudeGroup=="All" & state$LanduseGroup=="Forest"] <- TRUE
# All	    Urban	    Catchment|Urban
state$Filter[state$StateGroup=="Catchment|Urban" & state$AltitudeGroup=="All" & state$LanduseGroup=="Urban"] <- TRUE
# Upland	Rural	    Catchment|Upland|Rural
state$Filter[state$StateGroup==" Catchment|Upland|Rural" ] <- TRUE
# Upland	Forest   	Catchment|Upland|Forest
state$Filter[state$StateGroup=="Catchment|Upland|Forest"] <- TRUE
# Upland	Urban	    Catchment|Upland|Urban  
#state$Filter[state$StateGroup=="Catchment|Upland|Urban"] EXCLUDED AS ONLY ONE SITE
# Lowland	Rural	    Catchment|Lowland|Rural
state$Filter[state$StateGroup=="Catchment|Lowland|Rural"] <- TRUE
# Lowland	Forest	  Catchment|Lowland|Forest
state$Filter[state$StateGroup=="Catchment|Lowland|Forest"] <- TRUE
# Lowland	Urban	    Catchment|Lowland|Urban
state$Filter[state$StateGroup=="Catchment|Lowland|Urban"] <- TRUE


## Site combos
state$AltitudeGroup[state$StateGroup=="Site|All"] <- "All"
state$LanduseGroup[state$StateGroup=="Site|All"] <- "All"
state$Filter[state$StateGroup=="Site|All"] <- TRUE

state$LanduseGroup[state$StateGroup=="Site|Lowland"] <- "All"
state$Filter[state$StateGroup=="Site|Lowland"] <- TRUE
state$LanduseGroup[state$StateGroup=="Site|Upland"] <- "All"
state$Filter[state$StateGroup=="Site|Upland"] <- TRUE

state$AltitudeGroup[state$StateGroup=="Site|Rural"] <- "All"
state$Filter[state$StateGroup=="Site|Rural"] <- TRUE
state$AltitudeGroup[state$StateGroup=="Site|Forest"] <- "All"
state$Filter[state$StateGroup=="Site|Forest"] <- TRUE
state$AltitudeGroup[state$StateGroup=="Site|Urban"] <- "All"
state$Filter[state$StateGroup=="Site|Urban"] <- TRUE

state$Filter[state$StateGroup=="Site|Upland|Forest"] <- TRUE
state$Filter[state$StateGroup=="Site|Upland|Rural"] <- TRUE
# state$Filter[state$StateGroup=="Site|Upland|Urban"]  EXCLUDED AS ONLY ONE SITE

state$Filter[state$StateGroup=="Site|Lowland|Forest"] <- TRUE
state$Filter[state$StateGroup=="Site|Lowland|Rural"] <- TRUE
state$Filter[state$StateGroup=="Site|Lowland|Urban"] <- TRUE



## 4. Remove Upland|Urban combination as only one site contributes
p<-grep(pattern = "Upland|Urban",x = state$StateGroup,fixed=TRUE)
#state<-state[-p,]

## 5. Round values to appropriate DP's

# Note that for rounding off a 5, the IEC 60559 standard is expected to be used, ‘go to the even digit’. Therefore round(0.5) is 0 and round(-1.5) is -2
# This is not the desired behaviour here. It is expected that 0.5 rounds to 1, 2.5 rounds, to 3 and so on.
# Therefore for all even values followed by exactly .5 needs to have a small number added (like 0.00001) to get them rounding in the right direction (unless there is 
# a flag in the function that provides for this behaviour), or to redefine the round function. 
# (see http://theobligatescientist.blogspot.co.nz/2010/02/r-i-still-love-you-but-i-hate-your.html)

# As all values are positive, we'll add a small number, related to the degree of rounding required.
# If I was smarter, I would redefine the round function

# Rounding to 4 digits
state$Q50 <- state$Q50 + 0.000001
state$Q50 <- round(state$Q50,digits = 4)

# Rounding to 2 digits
state$Q50[state$Parameter=="BDISC" | state$Parameter=="PH" | state$Parameter=="TURB"] <- state$Q50[state$Parameter=="BDISC" | state$Parameter=="PH" | state$Parameter=="TURB"] + 0.0001
state$Q50[state$Parameter=="BDISC" | state$Parameter=="PH" | state$Parameter=="TURB"] <- round(state$Q50[state$Parameter=="BDISC" | state$Parameter=="PH" | state$Parameter=="TURB"],2)

# Rounding to 0 digits
state$Q50[state$Parameter=="ECOLI"] <- state$Q50[state$Parameter=="ECOLI"] + 0.01
state$Q50[state$Parameter=="ECOLI"] <- round(state$Q50[state$Parameter=="ECOLI"],0)



## 6. Detection limit correction
# Load detection limit correction data - this should be updated each year
dl<-read.csv(file="//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/AgencyDLs.csv",stringsAsFactors = FALSE)

# Removing NA rows i.e. rows without detection limit values
dl<-dl[!is.na(dl$Value),]

#converting Q50 to text to allow less than strings to be added
state$Q50 <- as.character(state$Q50)

for(i in 1:nrow(dl)){
  state$Q50[state$Agency==dl$Agency[i] & state$Parameter==dl$Parameter[i] & as.numeric(state$Q50)<dl$Value[i]] <- dl$DL[i]
}

## 7. Tidy up
# Filtering dataset by state$Filter
state <- state[state$Filter==TRUE,]

# Dropping original row counter and agency field
state<-state[,c(1,3:10)]

# Renaming Median column
c<-names(state)
c[5]<-"Median"
names(state)<-c

# ---------------------
# Export to XL workbook
write.csv(state,file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/state_fordelivery.csv",row.names = FALSE)         ## 


# ---------------------
# Housekeeping
#rm(state)

# ======================
# RAW DATA


# Load 10 year data pull - data.frame "lawadata"
load("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trenddata2007-2016.RData",verbose=TRUE)
raw<-lawadata[,c(1,16,5,2,13,4,7,15,22,19,18,28,23)]
#rm(lawadata)

raw<-raw[!is.na(raw$LAWAID),]
#head(raw)
names(raw)
raw$Symbol<-""
raw$Symbol[raw$CenType=="Left"]<-"<"
raw$Symbol[raw$CenType=="Right"]<-">"

raw$RawValue <- paste(raw$Symbol,raw$OriginalValue,sep="")
raw$QC <- ""
raw$Agency[grepl(pattern = "^NIWA",x=raw$Agency )] <- "NIWA"
raw$Agency[grepl(pattern = "^Christchurch",x=raw$Agency )] <- "Christchurch"
raw$License[!(raw$Agency=="NIWA" | raw$Agency=="Christchurch")] <- "CC BY 4.0"
raw$Ownership[!(raw$Agency=="NIWA" | raw$Agency=="Christchurch")] <- "The Regional Council or Unitary Authority specified under the agency attribute"
raw$Disclaimer[!(raw$Agency=="Canterbury" | raw$Agency=="Horizons" | raw$Agency=="Waikato")] <- "LAWA Partners shall not be liable, whether in contract, tort, equity or otherwise, for any loss or damage of any type (including consequential losses) arising directly or indirectly from the inadequacy, inaccuracy or any other deficiency in information supplied irrespective of the cause.  Use of information supplied is entirely at the risk of the recipient and shall be deemed to be acceptance of this liability exclusion."
raw$Disclaimer[raw$Agency=="Canterbury"]  <- "http://data.ecan.govt.nz/Catalogue/Agreement"
raw$Disclaimer[raw$Agency=="Horizons"]    <- "The enclosed information is supplied, within the framework of the Manawatu-Wanganui Regional Councils quality system, from the best data currently available.
However, as we endeavour to continuously improve our products, we reserve the right to amend the data on which this information is based, where necessary and without notice, at any time."
raw$Disclaimer[raw$Agency=="Waikato"]     <- "Waikato Regional Council provides this information in good faith and has exercised all reasonable skill and care in controlling the content of this information, and accepts no liability in contract, tort or otherwise, for any loss, damage, injury or expense (whether direct, indirect or consequential) arising out of the provision of this information or its use by you."

raw<-raw[,c(1:6,8:19)]

c <- c("Site", "SiteID",	"Measurement",	"DateTime",	"Original.Value", "Method",	"LAWAID", "Region",	"Landuse",	"Altitude",	"Catchment",	
       "Agency",	"Symbol","Raw.Value","QC", "License",	"Ownership",	"Disclaimer")
names(raw) <- c


# c <- c("Site",	"Measurement",	"DateTime",	"Date",	"Raw Value",	"Symbol",	"Value",	"Method",	"QC",	
#        "LAWAID",	"Region",	"Landuse",	"Altitude",	"WSGS84_X",	"WSGS84_Y",	"Catchment",	"CatchmentNameLAWA",	"Agency",	
#        "License",	"Ownership",	"Disclaimer")

# Rearrange columns
#raw<-raw[,c(1:4,18,17,6,19,8:16,20:22)]

# +++ WITHOUT NIWA / CCC
raw_sans <- raw[!(grepl("^CCC",x = raw$LAWAID,perl = TRUE) | grepl("^NRWQN",x = raw$LAWAID,perl = TRUE)) ,]

# ---------------------
# Export to XL workbook
write.csv(raw,file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/rawdata.csv",row.names = FALSE)         ## 
write.csv(raw_sans,file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/rawdata_minus_niwa_ccc.csv",row.names = FALSE)         ## rename to rawdata_download

rm(raw,raw_sans)

setwd(od)