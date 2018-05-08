#===================================================================================================
#  LAWA TREND ANALYSIS
#  Horizons Regional Council
#
# 17 September 2014
#
#  Maree Clark
#  Sean Hodges
#  Horizons Regional Council
#===================================================================================================

rm(list = ls())
TRENDPERIOD <- 5 # years


# Clearing workspace


ANALYSIS<-"TREND"
# Set working directory
od <- getwd()
wd <- "//file/herman/R/OA/08/02/2017/Water Quality/R/lawa_state"
setwd(wd)

# Clean up output folder before starting script.
# cleanup <- FALSE
# if(cleanup){
#   rOutput <- "//file/herman/r/oa/08/02/2017/Water Quality/ROutput"
#   files <- list.files(rOutput)
#   if(length(files) >0){
#     for(i in 1:length(files)){
#       file.remove(paste(rOutput,"/",files[i],sep=""))
#     }
#   }
# }

x <- Sys.time()


#Reference Dates
EndYear <- 2016


StartYear <- EndYear - TRENDPERIOD + 1

#if(!exists(foo, mode="function")) source("lawa_state_functions.R")

#/* -===Include required function libraries===- */ 

source("scripts/WQualityStateTrend/lawa_state_functions.R")
HilltopLibrary<-FALSE
library(Hilltop)
HilltopLibrary<-TRUE
#-- Specifying source files - note that these connections are pulling from SOE folder on HilltopDEV

if(HilltopLibrary==TRUE){
  lawa            <- Hilltop::HilltopData("//hilltopdev/data/lawa2017/state/lawa_provisional_2017.dsn")
}

#/* -===Global variable/constant definitions===- */ 
vendor <- c("52NORTH","AQUATIC","HILLTOP","KISTERS")

seasons <- read.csv("seasons.csv")

#/* -===Local variable/constant definitions===- */
wqparam <- c("BDISC","TURB","NH4","TON","TN","DRP","TP","ECOLI") 

tss <- 3  # tss = time series server
# tss_url <- "http://hilltopdev.horizons.govt.nz/lawa2014trend05.hts?"
# tss_url <- "http://hilltopdev.horizons.govt.nz:8080/lawa2017trend05.lawa?"
tss_url <- "http://hilltopdev.horizons.govt.nz:8080/lawa2017.lawa?"

hts <- c("service=Hilltop",
       "&request=SiteList",
       "&request=MeasurementList",
       "&request=GetData&collection=LAWA_",
       paste("&from=",StartYear,"-01-01&to=",EndYear+1,"-01-01",sep="")
) 
#_52N
#_kqs
#_sos <- c("service=SOS&request=GetObservation&featureOfInterest=","&observedProperty=","&temporalFilter=om:phenomenom,")


#/* -===Subroutine===- 
#// void main(){}
#*/

# Site data request

#l <- SiteTable(databasePathFileName="//ares/waterquality/LAWA/2013/hilltop.mdb",sqlID=2) ## Assumes all sites have hilltop.mdb site names
if(HilltopLibrary!=TRUE) requestData(vendor[tss],tss_url,"service=Hilltop&request=Reset")

# Replace database call here with call to previously loaded WFS Site data
#l <- SiteTable(databasePathFileName="//file/herman/R/OA/08/02/2017/MASTER SiteList/lawa_2017.mdb",sqlID=3) ## Allows for different sitenames in hilltop.mdb - requires assessment and population of the database.
l <- read.csv("LAWA_Site_Table1.csv",stringsAsFactors=FALSE)

l$SWQLanduse[l$SWQLanduse=="Native"|l$SWQLanduse=="Exotic"|l$SWQLanduse=="Natural"] <- "Forest"

## fixing gaps in Christchurch city council data
l$SiteID[l$SiteID==""] <- l$CouncilSiteID[l$SiteID==""]

if(HilltopLibrary==TRUE){
  s <- Hilltop::SiteList(lawa)
} else {
  r <- requestData(vendor[tss],tss_url,request=paste(hts[1],hts[2],sep=""))
  s <- SiteList(r)
}


# Load Reference data for Trends --- NO LONGER REQUIRED WITH FUNCTIONS FROM TON
#                                --- SNELDER TO IMPUTE CENSORED VALUES 
#trendRules_csv <- read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/RScript/lawa_state/trendrules.csv",sep=""),header=TRUE,sep=",",quote = "\"")

cat("LAWA Water QUality TREND Analysis\n","Number of sites returned:",length(s))


# -=== WQ PARAMETERS ===-
#requestData(vendor[tss],tss_url,"service=Hilltop&request=Reset")
for(i in 1:length(wqparam)){
  ## Added Hilltop library 2017-09-07
  if(HilltopLibrary==FALSE){
    requestData(vendor[tss],tss_url,"service=Hilltop&request=Reset")
  }
  cat("Starting",wqparam[i],"\n")
  
  if(HilltopLibrary==TRUE){
    lawa_collection <- GetCollection.HilltopData(lawa, "//hilltopdev/c$/HilltopServer/LAWA_collections.xml",paste("LAWA",wqparam[i],sep="_"))
    mySites<-unique(lawa_collection[,1])
    myMeas<-unique(lawa_collection[,2])
    for(ii in 1:length(mySites)){
      dx <- try(GetData(lawa,mySites[ii], myMeas, startTime=paste(StartYear,"-01-01",sep=""), endTime=paste(EndYear + 1,"-01-01",sep=""), WQParams=FALSE),silent=TRUE)
      if(attr(dx,"class")!="try-error"){
        if(ii==1){
          x1 <- unlist(attr(dx,"dimnames"))
          x1df <- data.frame(index(dx),as.character(coredata(dx)),stringsAsFactors = FALSE)
          x1df$SiteName<-mySites[ii];x1df$parameter<-wqparam[i];x1df$Method<-""
          x1df <- x1df[,c(3,1,2,5,4)]
          names(x1df) <- c("SiteName" , "Date"  ,    "Value"   ,  "Method"  ,  "parameter")
          wqdata <- x1df
        } else {
          x1 <- unlist(attr(dx,"dimnames"))
          x1df <- data.frame(index(dx),as.character(coredata(dx)),stringsAsFactors = FALSE)
          x1df$SiteName<-mySites[ii];x1df$parameter<-wqparam[i];x1df$Method<-""
          x1df <- x1df[,c(3,1,2,5,4)]
          names(x1df) <- c("SiteName" , "Date"  ,    "Value"   ,  "Method"  ,  "parameter")
          wqdata <- rbind.data.frame(wqdata,x1df,stringsAsFactors = FALSE)
        }
        rm(x1,x1df)
      }
    }
    
  } else {
    r <- readUrl(vendor[tss],tss_url,paste(hts[1],hts[4],wqparam[i],hts[length(hts)],sep=""))
    #r <- requestData(vendor[tss],tss_url,paste(hts[1],hts[4],wqparam[i],hts[length(hts)],sep=""))
    wqdata <- MeasurementList(xmlmdata=r,requestType="Hilltop")
    wqdata$Value <- as.character(wqdata$Value)
    wqdata$parameter <- wqparam[i]
  }  
  
  # ------------------------
  # Handling censored data
  # ------------------------
  
  #1. Detect censored data
  wqdata_cen <-flagCensoredDataDF(wqdata)
  wqdata_cen$Value <- as.numeric(wqdata_cen$Value)
  
  # Reduce dataset to complete cases only - removes NA's etc
  ok <- complete.cases(wqdata_cen[,3])
  wqdata_cen <- wqdata_cen[ok,]  
  
  # 2. Handle Left Censored (<)
  # For STATE, half value where CenType==Left
  cat("Left Censored\n")
  
  if(ANALYSIS=="STATE"){
    wqdata_left <- qualifiedValues2(wqdata_cen)
  } else {
    # For TREND, apply leftCensored()
    
    if(exists("wqdata_left")){
      rm(wqdata_left)
    }
    for(x in 1:length(s)){
      # cat(x,s[x],"\n",sep=" ")
      tmp<-wqdata_cen[wqdata_cen$SiteName==s[x],]
      ok <- complete.cases(tmp[,3])
      tmp <- tmp[ok,]
      
      # Only process sites that have data
      if(length(tmp[,1])!=0){
        if(!exists("wqdata_left")){
          
          tmp1<-leftCensored(tmp)
          if(tmp1!=FALSE){
            wqdata_left <- tmp1
            rm(tmp1)
          }
          
        } else {
          tmp_left<-leftCensored(tmp)
          if(tmp_left!=FALSE){
            wqdata_left <- rbind.data.frame(wqdata_left,tmp_left)
          }
        }
        #cat("Found",length(tmp[,1]),"values for",wqparam[i],"at",s[x],"\n")
        
      } else {
        #cat("No",wqparam[i],"at",s[x],"\n")
      }
    }
  }
  # 3. Handle Right censored (>)
  cat("Right Censored\n")
  if(exists("wqdata_right")){
    rm(wqdata_right)
  }
  for(x in 1:length(s)){
    tmp<-wqdata_left[wqdata_left$SiteName==s[x],]
    ok <- complete.cases(tmp[,3])
    tmp <- tmp[ok,]  
    # Only process sites that have data
    if(length(tmp[,1])!=0){
      if(!exists("wqdata_right")){
        
        wqdata_right<-rightCensored(tmp)
        
      } else {
        tmp_right<-rightCensored(tmp)
        wqdata_right <- rbind.data.frame(wqdata_right,tmp_right)
      }
      #cat("Found",length(tmp[,1]),"values for",wqparam[i],"at",s[x],"\n")
      
    } else {
      #cat("No",wqparam[i],"at",s[x],"\n")
    }
  }
  
  
  # 4. Jitter tied data
  cat("Jitter\n")
  if(exists("wqdata_jitter")){
    rm(wqdata_jitter)
  }
  for(x in 1:length(s)){
    tmp<-wqdata_right[wqdata_right$SiteName==s[x],]
    ok <- complete.cases(tmp[,3])
    tmp <- tmp[ok,]  
    # Only process sites that have data
    if(length(tmp[,1])!=0){
      #cat("Jitter",s[x],"\n")
      if(!exists("wqdata_jitter")){
        
        wqdata_jitter<-addJitter(tmp)
        
      } else {
        tmp_jitter<-addJitter(tmp)
        wqdata_jitter <- rbind.data.frame(wqdata_jitter,tmp_jitter)
      }
      
    } else {
      #cat("No",wqparam[i],"at",s[x],"\n")
    }
  }
  
  
  #wqdata <- merge(wqdata, l, by.x="SiteName",by.y="Site", all.x=TRUE) # using native sitetable sitenames to match
  wqdata_jitter$OriginalValue <- wqdata_jitter$Value 
  wqdata_jitter$Value <- wqdata_jitter$i3Values
  
  wqdata <- merge(wqdata_right, l, by.x="SiteName",by.y="CouncilSiteID", all.x=TRUE) # Using Hilltop sitenames to match site information
  
  #wqdata$parameter <- wqparam[i]
  
  wqdata_q <- wqdata                  # retaining original data retrieved from webservice
  
  #wqdata <- merge(wqdata, l, by.x="SiteName",by.y="Site", all.x=TRUE) # using native sitetable sitenames to match
  #   wqdata <- merge(wqdata, l, by.x="SiteName",by.y="Site", all.x=TRUE) # Using Hilltop sitenames to match site information
  #   wqdata$parameter <- wqparam[i]
  
  # Reduce dataset to complete cases only - removes sites that have data in the hilltop
  # files, but are not explicitly included in the LAWA site table.
  ok <- complete.cases(wqdata[,3])
  wqdata <- wqdata[ok,]  
  
  # Fields have  been renamed from WFS feed to match what the code is
  # expecting, as code originally written expecting data from Hilltop Site Table.
  newFieldNames <- c("SiteName","Date","Value","Method","parameter", "Censored",
                     "CenType","converted_values","dflag","i1Values","i2Values","X",
                     "LAWAID","ID", "UsedInLAWA","AltitudeGroup","LanduseGroup","FrequencyAll",
                     "Frequency","Region","Agency","ISLAND","CatchID","CatchType",
                     "NZREACH","Catchment","Comments","LawaCatchm","CatchLbl")
  
  names(wqdata) <- newFieldNames
  
  
  
  # Deprecated 18-Sep-2016 as censored data handled by functions
  # supplied by Ton Snelder.
  #wqdata <- merge(wqdata, tr, by.x="LAWAID",by.y="LAWAID", all.y=TRUE)
  
  ## --- WHAT IS BEING REORDER HERE AND WHY? ---
  # Reorder items in data.frame
  #wqdata <- wqdata[order(wqdata[,1],wqdata[,3]),]
  
  # Building dataframe to save at the end of this step 
  if(i==1){
    lawadata <- wqdata
    lawadata_q <- wqdata_q
  } else {
    lawadata <- rbind(lawadata,wqdata)
    lawadata_q <- rbind(lawadata_q,wqdata_q)
  }    
  
  
  print(Sys.time() - x)


}

# Housekeeping
# - Saving the lawadata table
save(lawadata,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trenddata",StartYear,"-",EndYear,".RData",sep=""))
#write.csv(lawadata,"//file/herman/R/OA/08/02/2017/Water Quality/ROutput/LAWA_RAW_DATA_TREND05yr.csv")

save(lawadata_q,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trenddata_q_",StartYear,"-",EndYear,".RData",sep=""))

# disconnect from Hilltop object
Hilltop::disconnect(lawa)


# DATA CLEANSE #

#Calculating the Long term LAWA Trends in water quality.

#Reference Dates
#StartYear <- Set at start of script
#EndYear <-   Set at start of script
years <- EndYear - StartYear + 1
StartMonth <- 1
EndMonth <- 12
if(years==5){
  rate<-1
} else{
  rate<-0.9
}
load(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trenddata",StartYear,"-",EndYear,".RData",sep=""))

###################################################################################
#Step 0 Data Summary
###################################################################################
#b "  Ensure one value per sampling interval
#b "  Calculate the number of samples per sampling interval and select/calculate/
#     determine representative values

lawadata <- samplesByInterval(StartYear,EndYear,StartMonth,EndMonth,lawadata)

#drop sites with no LAWA ID
lawadata <- lawadata[!is.na(lawadata$LAWAID),]

df_count <- summaryBy(Value~SiteName+parameter+yearMon,data=lawadata, FUN=c(length), keep.name=TRUE)


multipleResultsByMonth <- subset(df_count,Value>1)

# ---- DOCUMENTATION ----
# Values are now derived for the three sampling frequencies.
# Medians are derived, rather than nearest values. The original
# justification was the number of samples that Councils would 
# deliver in their data for a month - SOE samples were not
# necessarily separated out from other project data. A decision
# was made early on to simply take a median. That decision has
# not been reviewed, but should have been, in the light of 
# of the progressive improvements in data supply.

## Default median calculation
df_value_monthly <- subset(summaryBy(Value~SiteName+parameter+yearMon,data=lawadata,
                                     id=~LAWAID+LanduseGroup+AltitudeGroup+Catchment+Region+Frequency+year+mon+bimon+Qtr+depth,
                                     FUN=c(quantile), prob=c(0.5), type=5, keep.name=TRUE),Frequency=="Monthly")
# 
# df_value_bimonthly <- subset(summaryBy(Value~SiteName+parameter+yearBimon,data=lawadata,
#                                        id=~LAWAID+LanduseGroup+AltitudeGroup+Catchment+Region+Frequency+year+mon+bimon+Qtr+depth,
#                                        FUN=c(quantile), prob=c(0.5), type=5, keep.name=TRUE),Frequency=="Monthly" | Frequency=="Bimonthly")

#df_value_quarterly <- subset(summaryBy(Value~SiteName+parameter+yearQtr,data=lawadata,
#                                       id=~LAWAID+LanduseGroup+AltitudeGroup+Catchment+Region+Frequency+year+mon+bimon+Qtr+depth,
#                                       FUN=c(quantile), prob=c(0.5), type=5, keep.name=TRUE),Frequency=="Monthly" | Frequency=="Bimonthly" | Frequency=="Quarterly")


## A slightly less fraught method is to use Aggregate() to return first value in each group
df_value_quarterly <- aggregate(lawadata, list(lawadata$SiteName,
                                               lawadata$parameter,
                                               lawadata$yearQtr), FUN=head, 1)
df_value_quarterly <- df_value_quarterly[,c(4,8,40,6,16,20,19,29,23,22,34,35,37,39,33)] # matching columns to what's generated by summaryBy above.


###################################################################################


#require(wq)

#======================================================
#SEASONAL KENDALL ANALYSIS
#======================================================

t <- Sys.time()


######################################################################################################
# Seasonal Kendall for Monthly Sampling for each site, by each parameter

# Returning unique LAWAIDs for processing data subsets
#lawa_ids <- as.character(unique(df_value_monthly$LAWAID))
uLAWAID <- unique(df_value_monthly$LAWAID)
months<-12   # Monthly data
monthLbl <- "Monthly"
k <- 1       # counter for sites/parameters meeting minimum N
rbindTrendCheck<-FALSE # Flagging condition for rbinding dataframe that keeps track of sites, measurements and checks for trend inclusion
for(i in 1:length(uLAWAID)){
  # Store current LAWAID
  l <- uLAWAID[i]
  # Store vector of unique parameters for current LAWAID
  uWQParam <- unique(df_value_monthly$parameter[df_value_monthly$LAWAID==l])
  
  #l <- lawa_ids[i]
  #df_value_monthly1 <- subset(df_value_monthly, LAWAID==l)
  #parameters <- as.character(unique(df_value_monthly1$parameter))
  # this step is to double check output with TimeTrends
  
  ## The following two lines of code have now been rendered unnecessary by changing
  ## the library used for the trend analysis from "wq" to "EnvStats".
  ## The original "wq" library (now no longer available on CRAN) required a
  ## timeseries object to be passed to the seasonal kendall functdion; the
  ## EnvStats library only requires a dataframe.
  
  #lawa <- wqData(data=df_value_monthly1,locus=c(3,5,15),c(2,4),site.order=TRUE,time.format="%Y-%m-%d",type="long")
  #x <- tsMake(object=lawa,focus=gsub("-",".",l))
  
  # calculating seasonal Kendall statistics for individual parameters for an individual site
  
  for(j in 1:length(uWQParam)){
    # subsetting dataframe based on lawaid and parameter
    x <- df_value_monthly[df_value_monthly$LAWAID==l&df_value_monthly$parameter==uWQParam[j],]  
    
    ### TREND INCLUSION CRITERIA
    # 1. Count samples in the first or last 12 month periods to compare to entry criteria for trend
    first_year<-length(x$year[x$year==StartYear])
    last__year<-length(x$year[x$year==EndYear])
    
    # 2. Count samples in order to compare to entry criteria for trend
    num_samples <- length(x[,1])
    
    # building dataframe to report out data to assess pass-fails for trend inclusion
    v <- matrix(data=c(l,uWQParam[j],first_year,last__year,num_samples),nrow=1,ncol=5,byrow=TRUE)
    if(!rbindTrendCheck){ 
      validDataForTrend <-as.data.frame(v,stringsAsFactors=FALSE)
      rbindTrendCheck<-TRUE
    } else {
      validDataForTrend <- rbind(validDataForTrend, as.data.frame(v,stringsAsFactors=FALSE))
    }
    
    # Check Trend Criteria - Assess pass-fail for trends analysis
    PassTrendCriteria <- TrendCriteria(first_year, last__year, num_samples, rate, years, months, monthMultiplier = 1)
    
    # Processing Trends for sites/parameters pass trend criteria
    if(PassTrendCriteria){
      
      s <- seaKenEPA(x)
      list(sen.slope     = sk.sen.slope, 
           sen.slope.pct = sk.sen.slope.pct, 
           p.value       = sk.p.value,
           sen.z         = sk.sen.z,
           sen.z.prob    = sk.sen.z.prob,
           slope.lcl90   = sk.slope.LCL90,
           slope.ucl90   = sk.slope.UCL90)
      
      m <-data.frame(l,
                     uWQParam[j],
                     s$sen.slope.pct,
                     s$sen.slope,
                     s$p.value,
                     sen.z,
                     sen.z.prob,
                     s$slope.lcl90,
                     s$slope.ucl90,
                     stringsAsFactors = FALSE)
      if(k==1){
        seasonalkendall <- m
        #cat("seasonalkendal dataframe created\n")
      } else {
        seasonalkendall <- rbind(seasonalkendall, m)
        #cat("Appending to seasonalkendall dataframe\n")
      }
      
      rm(s,m)
      k <- k + 1
      
    }
  }
  
}

names(seasonalkendall) <- c("LAWAID",
                            "Parameter",
                            "Sen.Pct",
                            "Sen.Slope",
                            "p.value",
                            "sen.z",
                            "sen.z.prob",
                            "Sen.Slope.LCL90",
                            "Sen.Slope.UCL90")

seasonalkendall$freq<- monthLbl
trendscores <- calcTrendScore(seasonalkendall)

names(validDataForTrend)  <- c("LAWAID","Parameter","N.Months.StartYear","N.Months.EndYear","Num.Samples")
validDataForTrend$freq<- monthLbl


rm(m)

load(file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/lawa_sitetable.RData")


# Fields have  been renamed from WFS feed to match what the code is
# expecting, as code originally written expecting data from Hilltop Site Table.
newFieldNames <- c("X","LAWAID","ID", "CouncilSiteID", "UsedInLAWA","AltitudeGroup","LanduseGroup","FrequencyAll","Frequency",
                   "Region","Agency","ISLAND","CatchID","CatchType",
                   "NZREACH","Catchment","Comments","LawaCatchm")

names(l) <- newFieldNames

trends <- merge(trendscores, l, by.x="LAWAID",by.y="LAWAID",all.x=TRUE) # Using LAWAIDs to join tables
rm(seasonalkendall)
write.csv(trends,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_monthly_",StartYear,"-",EndYear,".csv",sep=""))
write.csv(validDataForTrend,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_monthly_",StartYear,"-",EndYear,".csv",sep=""))

cat("LAWA Water QUality Trend Analysis\nCompleted assigning Trend Results for Monthly Data\n")
cat(paste(k,"Sites/Parameter combinations meet 90 percent sampling occasion requirements\n"))
rm(trends,validDataForTrend)

# ######################################################################################################
# # Seasonal Kendall for Bimonthly Sampling for each site, by each parameter
# 
# # Returning unique LAWAIDs for processing data subsets
# lawa_ids <- as.character(unique(df_value_bimonthly$LAWAID))
# 
# k <- 1 # counter for sites/parameters meeting minimum N
# rbindTrendCheck<-FALSE # Flagging condition for rbinding dataframe that keeps track of sites, measurements and checks for trend inclusion
# for(i in 1:length(lawa_ids)){
#   
#   months<-6
#   l <- lawa_ids[i]
#   df_value_bimonthly1 <- subset(df_value_bimonthly, LAWAID==l)
#   parameters <- as.character(unique(df_value_bimonthly1$parameter))
#   # this step is to double check output with TimeTrends
#   #Uncomment if needed
#   #write.csv(df_value_monthly1,file=paste("c:/data/MWR_2013/2013/ES-00022.csv",sep=""))
#   
#   lawa <- wqData(data=df_value_bimonthly1,locus=c(3,5,15),c(2,4),site.order=TRUE,time.format="%Y-%m-%d",type="long")
#   x <- tsMake(object=lawa,focus=gsub("-",".",l))
#   #cat("length(parameters)",length(parameters),"\n")
#   #cat(parameters,"\n")
#   
#   # calculating seasonal Kendall statistics for individual parameters for an individual site
#   for(j in 1:length(parameters)){
#     ### TREND INCLUSION CRITERIA
#     # 1. Count samples in the first or last 20 percent of the 5 year window to compare to entry criteria for trend
#     first_20pct<-length(df_value_bimonthly1$year[df_value_bimonthly1$year==StartYear & df_value_bimonthly1$parameter==parameters[j]])
#     last__20pct<-length(df_value_bimonthly1$year[df_value_bimonthly1$year==EndYear & df_value_bimonthly1$parameter==parameters[j]])
#     # 2. Count samples in order to compare to entry criteria for trend                                                                                          
#     num_samples <- length(subset(df_value_bimonthly1,parameter==parameters[j])[,1])                                                                             
#     
#     # building dataframe to report out data to assess pass-fails for trend inclusion                                                                            
#     v <- matrix(data=c(l,parameters[j],first_20pct,last__20pct,num_samples),nrow=1,ncol=5,byrow=TRUE)                                                           
#     if(!rbindTrendCheck){                                                                                                                                       
#       validDataForTrend <-as.data.frame(v,stringsAsFactors=FALSE)                                                                                               
#       rbindTrendCheck<-TRUE                                                                                                                                     
#     } else {                                                                                                                                                    
#       validDataForTrend <- rbind(validDataForTrend, as.data.frame(v,stringsAsFactors=FALSE))                                                                    
#     }                                                                                                                                                           
#     
#     # Check Trend Criteria - Assess pass-fail for trends analysis                                                                                               
#     PassTrendCriteria <- TrendCriteria(first_20pct, last__20pct, num_samples, rate, years, months,monthMultiplier = 1) # This month multiplier is linked to the 
#     # the number of months selected here
#     # Processing Trends for sites/parameters pass trend criteria
#     if(PassTrendCriteria){
#       
#       if(length(parameters)==1){
#         s<-seaKenLAWA(x,"median")              # x has a different structure where there is only one item
#       } else{
#         s<-seaKenLAWA(x[,j],"median")
#       }
#       #cat(i,lawa_ids[i],length(lawa$time),parameters[j],s$p.value,s$sen.slope.pct,"\n")
#       
#       #s$sen.slope.pct  #  <---- required for LAWA
#       #s$sen.slope      #  <----
#       #s$p.value        #  <---- required for LAWA
#       
#       
#       m <-matrix(data=c(l,parameters[j],s$sen.slope.pct,s$sen.slope,s$p.value),nrow=1,ncol=5,byrow=TRUE)
#       if(k==1){   # removed i==i condition - causing errors where first site doesn't meet criteria for trend analysis
#         seasonalkendall <-as.data.frame(m,stringsAsFactors=FALSE)
#         #cat("seasonalkendal dataframe created\n")
#       } else {
#         seasonalkendall <- rbind(seasonalkendall, as.data.frame(m,stringsAsFactors=FALSE))
#         #cat("Appending to seasonalkendall dataframe\n")
#       }
#       k <- k + 1
#     }
#   }
#   
# }
# 
# names(seasonalkendall) <- c("LAWAID","Parameter","Sen.Pct","Sen.Slope","p.value")
# names(validDataForTrend)  <- c("LAWAID","Parameter","N.Months.StartYear","N.Months.EndYear","Num.Samples")
# validDataForTrend$freq<- "Bimonthly"
# 
# seasonalkendall$Sen.Pct <-as.numeric(as.character(seasonalkendall$Sen.Pct))
# seasonalkendall$Sen.Slope <-as.numeric(as.character(seasonalkendall$Sen.Slope))
# seasonalkendall$p.value <-as.numeric(as.character(seasonalkendall$p.value))
# 
# seasonalkendall$freq<- "Bimonthly"
# trendscores <- calcTrendScore(seasonalkendall)
# 
# rm(m)
# 
# load(file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/lawa_sitetable.RData")
# 
# # Fields have  been renamed from WFS feed to match what the code is
# # expecting, as code originally written expecting data from Hilltop Site Table.
# newFieldNames <- c("X","LAWAID","ID", "CouncilSiteID", "UsedInLAWA","AltitudeGroup","LanduseGroup","FrequencyAll","Frequency",
#                    "Region","Agency","ISLAND","CatchID","CatchType",
#                    "NZREACH","Catchment","Comments","LawaCatchm")
# 
# names(l) <- newFieldNames
# 
# trends <- merge(trendscores, l, by.x="LAWAID",by.y="LAWAID",all.x=TRUE) # Using LAWAIDs to join tables
# rm(seasonalkendall)
# write.csv(trends,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_bimonthly_",StartYear,"-",EndYear,".csv",sep=""))
# write.csv(validDataForTrend,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_bimonthly_",StartYear,"-",EndYear,".csv",sep=""))
# 
# cat("LAWA Water QUality Trend Analysis\nCompleted assigning Trend Results for Bimonthly Data\n")
# cat(paste(k,"Sites/Parameter combinations meet 90 percent sampling occasion requirements\n"))
# rm(trends,validDataForTrend)

######################################################################################################
# Seasonal Kendall for Quarterly Sampling for each site, by each parameter

# Returning unique LAWAIDs for processing data subsets
#lawa_ids <- as.character(unique(df_value_monthly$LAWAID))
uLAWAID <- unique(df_value_quarterly$LAWAID)
months<-4   # Quarterly data
monthLbl <- "Quarterly"
k <- 1       # counter for sites/parameters meeting minimum N
rbindTrendCheck<-FALSE # Flagging condition for rbinding dataframe that keeps track of sites, measurements and checks for trend inclusion
for(i in 1:length(uLAWAID)){
  # Store current LAWAID
  l <- uLAWAID[i]
  # Store vector of unique parameters for current LAWAID
  uWQParam <- unique(df_value_quarterly$parameter[df_value_quarterly$LAWAID==l])
  
  #l <- lawa_ids[i]
  #df_value_monthly1 <- subset(df_value_monthly, LAWAID==l)
  #parameters <- as.character(unique(df_value_monthly1$parameter))
  # this step is to double check output with TimeTrends
  
  ## The following two lines of code have now been rendered unnecessary by changing
  ## the library used for the trend analysis from "wq" to "EnvStats".
  ## The original "wq" library (now no longer available on CRAN) required a
  ## timeseries object to be passed to the seasonal kendall functdion; the
  ## EnvStats library only requires a dataframe.
  
  #lawa <- wqData(data=df_value_monthly1,locus=c(3,5,15),c(2,4),site.order=TRUE,time.format="%Y-%m-%d",type="long")
  #x <- tsMake(object=lawa,focus=gsub("-",".",l))
  
  # calculating seasonal Kendall statistics for individual parameters for an individual site
  
  for(j in 1:length(uWQParam)){
    # subsetting dataframe based on lawaid and parameter
    x <- df_value_quarterly[df_value_quarterly$LAWAID==l&df_value_quarterly$parameter==uWQParam[j],]  
    
    ### TREND INCLUSION CRITERIA
    # 1. Count samples in the first or last 12 month periods to compare to entry criteria for trend
    first_year<-length(x$year[x$year==StartYear])
    last__year<-length(x$year[x$year==EndYear])
    
    # 2. Count samples in order to compare to entry criteria for trend
    num_samples <- length(x[,1])
    
    # building dataframe to report out data to assess pass-fails for trend inclusion
    v <- matrix(data=c(l,uWQParam[j],first_year,last__year,num_samples),nrow=1,ncol=5,byrow=TRUE)
    if(!rbindTrendCheck){ 
      validDataForTrend <-as.data.frame(v,stringsAsFactors=FALSE)
      rbindTrendCheck<-TRUE
    } else {
      validDataForTrend <- rbind(validDataForTrend, as.data.frame(v,stringsAsFactors=FALSE))
    }
    
    # Check Trend Criteria - Assess pass-fail for trends analysis
    PassTrendCriteria <- TrendCriteria(first_year, last__year, num_samples, rate, years, months, monthMultiplier = 1)
    
    # Processing Trends for sites/parameters pass trend criteria
    if(PassTrendCriteria){
      
      s <- seaKenEPA(x)
      list(sen.slope     = sk.sen.slope, 
           sen.slope.pct = sk.sen.slope.pct, 
           p.value       = sk.p.value,
           sen.z         = sk.sen.z,
           sen.z.prob    = sk.sen.z.prob,
           slope.lcl90   = sk.slope.LCL90,
           slope.ucl90   = sk.slope.UCL90)
      
      m <-data.frame(l,
                     uWQParam[j],
                     s$sen.slope.pct,
                     s$sen.slope,
                     s$p.value,
                     sen.z,
                     sen.z.prob,
                     s$slope.lcl90,
                     s$slope.ucl90,
                     stringsAsFactors = FALSE)
      if(k==1){
        seasonalkendall <- m
        #cat("seasonalkendal dataframe created\n")
      } else {
        seasonalkendall <- rbind(seasonalkendall, m)
        #cat("Appending to seasonalkendall dataframe\n")
      }
      
      rm(s,m)
      k <- k + 1
      
    }
  }
  
}

names(seasonalkendall) <- c("LAWAID",
                            "Parameter",
                            "Sen.Pct",
                            "Sen.Slope",
                            "p.value",
                            "sen.z",
                            "sen.z.prob",
                            "Sen.Slope.LCL90",
                            "Sen.Slope.UCL90")

seasonalkendall$freq<- monthLbl
trendscores <- calcTrendScore(seasonalkendall)

names(validDataForTrend)  <- c("LAWAID","Parameter","N.Months.StartYear","N.Months.EndYear","Num.Samples")
validDataForTrend$freq<- monthLbl


rm(m)

load(file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/lawa_sitetable.RData")

# Fields have  been renamed from WFS feed to match what the code is
# expecting, as code originally written expecting data from Hilltop Site Table.
#newFieldNames <- c("X","LAWAID","ID", "UsedInLAWA","AltitudeGroup","LanduseGroupAll","LanduseGroup","FrequencyAll","Frequency",
#                   "Region","Agency","ISLAND","CatchID","CatchType",
#                   "NZREACH","Catchment","Comments","LawaCatchm")

newFieldNames <- c("X","LAWAID","ID", "CouncilSiteID", "UsedInLAWA","AltitudeGroup","LanduseGroup","FrequencyAll","Frequency",
                   "Region","Agency","ISLAND","CatchID","CatchType",
                   "NZREACH","Catchment","Comments","LawaCatchm")

names(l) <- newFieldNames

trends <- merge(trendscores, l, by.x="LAWAID",by.y="LAWAID",all.x=TRUE) # Using LAWAIDs to join tables
rm(seasonalkendall)
write.csv(trends,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_quarterly_",StartYear,"-",EndYear,".csv",sep=""))
write.csv(validDataForTrend,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_quarterly_",StartYear,"-",EndYear,".csv",sep=""))

cat("LAWA Water QUality Trend Analysis\nCompleted assigning Trend Results for Quarterly Data\n")
cat(paste(k,"Sites/Parameter combinations meet 90 percent sampling occasion requirements\n"))
rm(trends,validDataForTrend)

######################################################################################################


print(Sys.time()-t)
setwd(od)


#===================================================================================================
#  LAWA TREND ANALYSIS
#  Horizons Regional Council
#
# 17 September 2014
#
#  Maree Clark
#  Sean Hodges
#  Horizons Regional Council
#===================================================================================================

rm(list = ls())
TRENDPERIOD <- 10 # years


# Clearing workspace


ANALYSIS<-"TREND"
# Set working directory
od <- getwd()
wd <- "//file/herman/R/OA/08/02/2017/Water Quality/R/lawa_state"
setwd(wd)

# Clean up output folder before starting script.
# cleanup <- FALSE
# if(cleanup){
#   rOutput <- "//file/herman/r/oa/08/02/2017/Water Quality/ROutput"
#   files <- list.files(rOutput)
#   if(length(files) >0){
#     for(i in 1:length(files)){
#       file.remove(paste(rOutput,"/",files[i],sep=""))
#     }
#   }
# }

x <- Sys.time()


#Reference Dates
EndYear <- 2016


StartYear <- EndYear - TRENDPERIOD + 1

#if(!exists(foo, mode="function")) source("lawa_state_functions.R")

#/* -===Include required function libraries===- */ 

source("scripts/WQualityStateTrend/lawa_state_functions.R")
HilltopLibrary<-FALSE
library(Hilltop)
HilltopLibrary<-TRUE
#-- Specifying source files - note that these connections are pulling from SOE folder on HilltopDEV

if(HilltopLibrary==TRUE){
  lawa            <- Hilltop::HilltopData("//hilltopdev/data/lawa2017/state/lawa_provisional_2017.dsn")
}

#/* -===Global variable/constant definitions===- */ 
vendor <- c("52NORTH","AQUATIC","HILLTOP","KISTERS")

seasons <- read.csv("seasons.csv")

#/* -===Local variable/constant definitions===- */
wqparam <- c("BDISC","TURB","NH4","TON","TN","DRP","TP","ECOLI") 

tss <- 3  # tss = time series server
# tss_url <- "http://hilltopdev.horizons.govt.nz/lawa2014trend05.hts?"
# tss_url <- "http://hilltopdev.horizons.govt.nz:8080/lawa2017trend05.lawa?"
tss_url <- "http://hilltopdev.horizons.govt.nz:8080/lawa2017.lawa?"

hts <- c("service=Hilltop",
         "&request=SiteList",
         "&request=MeasurementList",
         "&request=GetData&collection=LAWA_",
         paste("&from=",StartYear,"-01-01&to=",EndYear+1,"-01-01",sep="")
) 
#_52N
#_kqs
#_sos <- c("service=SOS&request=GetObservation&featureOfInterest=","&observedProperty=","&temporalFilter=om:phenomenom,")


#/* -===Subroutine===- 
#// void main(){}
#*/

# Site data request

#l <- SiteTable(databasePathFileName="//ares/waterquality/LAWA/2013/hilltop.mdb",sqlID=2) ## Assumes all sites have hilltop.mdb site names
if(HilltopLibrary!=TRUE) requestData(vendor[tss],tss_url,"service=Hilltop&request=Reset")

# Replace database call here with call to previously loaded WFS Site data
#l <- SiteTable(databasePathFileName="//file/herman/R/OA/08/02/2017/MASTER SiteList/lawa_2017.mdb",sqlID=3) ## Allows for different sitenames in hilltop.mdb - requires assessment and population of the database.
l <- read.csv("LAWA_Site_Table1.csv",stringsAsFactors=FALSE)

l$SWQLanduse[l$SWQLanduse=="Native"|l$SWQLanduse=="Exotic"|l$SWQLanduse=="Natural"] <- "Forest"


## fixing gaps in Christchurch city council data
l$SiteID[l$SiteID==""] <- l$CouncilSiteID[l$SiteID==""]


if(HilltopLibrary==TRUE){
  s <- Hilltop::SiteList(lawa)
} else {
  r <- requestData(vendor[tss],tss_url,request=paste(hts[1],hts[2],sep=""))
  s <- SiteList(r)
}


# Load Reference data for Trends --- NO LONGER REQUIRED WITH FUNCTIONS FROM TON
#                                --- SNELDER TO IMPUTE CENSORED VALUES 
#trendRules_csv <- read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/RScript/lawa_state/trendrules.csv",sep=""),header=TRUE,sep=",",quote = "\"")

cat("LAWA Water QUality TREND Analysis\n","Number of sites returned:",length(s))


# -=== WQ PARAMETERS ===-
#requestData(vendor[tss],tss_url,"service=Hilltop&request=Reset")
for(i in 1:length(wqparam)){
  ## Added Hilltop library 2017-09-07
  if(HilltopLibrary==FALSE){
    requestData(vendor[tss],tss_url,"service=Hilltop&request=Reset")
  }
  cat("Starting",wqparam[i],"\n")
  
  if(HilltopLibrary==TRUE){
    lawa_collection <- GetCollection.HilltopData(lawa, "//hilltopdev/c$/HilltopServer/LAWA_collections.xml",paste("LAWA",wqparam[i],sep="_"))
    mySites<-unique(lawa_collection[,1])
    myMeas<-unique(lawa_collection[,2])
    for(ii in 1:length(mySites)){
      dx <- try(GetData(lawa,mySites[ii], myMeas, startTime=paste(StartYear,"-01-01",sep=""), endTime=paste(EndYear + 1,"-01-01",sep=""), WQParams=FALSE),silent=TRUE)
      if(attr(dx,"class")!="try-error"){
        if(ii==1){
          x1 <- unlist(attr(dx,"dimnames"))
          x1df <- data.frame(index(dx),as.character(coredata(dx)),stringsAsFactors = FALSE)
          x1df$SiteName<-mySites[ii];x1df$parameter<-wqparam[i];x1df$Method<-""
          x1df <- x1df[,c(3,1,2,5,4)]
          names(x1df) <- c("SiteName" , "Date"  ,    "Value"   ,  "Method"  ,  "parameter")
          wqdata <- x1df
        } else {
          x1 <- unlist(attr(dx,"dimnames"))
          x1df <- data.frame(index(dx),as.character(coredata(dx)),stringsAsFactors = FALSE)
          x1df$SiteName<-mySites[ii];x1df$parameter<-wqparam[i];x1df$Method<-""
          x1df <- x1df[,c(3,1,2,5,4)]
          names(x1df) <- c("SiteName" , "Date"  ,    "Value"   ,  "Method"  ,  "parameter")
          wqdata <- rbind.data.frame(wqdata,x1df,stringsAsFactors = FALSE)
        }
        rm(x1,x1df)
      }
    }
    
  } else {
    r <- readUrl(vendor[tss],tss_url,paste(hts[1],hts[4],wqparam[i],hts[length(hts)],sep=""))
    #r <- requestData(vendor[tss],tss_url,paste(hts[1],hts[4],wqparam[i],hts[length(hts)],sep=""))
    wqdata <- MeasurementList(xmlmdata=r,requestType="Hilltop")
    wqdata$Value <- as.character(wqdata$Value)
    wqdata$parameter <- wqparam[i]
  }  
  
  # ------------------------
  # Handling censored data
  # ------------------------
  
  #1. Detect censored data
  wqdata_cen <-flagCensoredDataDF(wqdata)
  wqdata_cen$Value <- as.numeric(wqdata_cen$Value)
  
  # Reduce dataset to complete cases only - removes NA's etc
  ok <- complete.cases(wqdata_cen[,3])
  wqdata_cen <- wqdata_cen[ok,]  
  
  # 2. Handle Left Censored (<)
  # For STATE, half value where CenType==Left
  cat("Left Censored\n")
  
  if(ANALYSIS=="STATE"){
    wqdata_left <- qualifiedValues2(wqdata_cen)
  } else {
    # For TREND, apply leftCensored()
    
    if(exists("wqdata_left")){
      rm(wqdata_left)
    }
    for(x in 1:length(s)){
      # cat(x,s[x],"\n",sep=" ")
      tmp<-wqdata_cen[wqdata_cen$SiteName==s[x],]
      ok <- complete.cases(tmp[,3])
      tmp <- tmp[ok,]
      
      # Only process sites that have data
      if(length(tmp[,1])!=0){
        if(!exists("wqdata_left")){
          
          tmp1<-leftCensored(tmp)
          if(tmp1!=FALSE){
            wqdata_left <- tmp1
            rm(tmp1)
          }
          
        } else {
          tmp_left<-leftCensored(tmp)
          if(tmp_left!=FALSE){
            wqdata_left <- rbind.data.frame(wqdata_left,tmp_left)
          }
        }
        #cat("Found",length(tmp[,1]),"values for",wqparam[i],"at",s[x],"\n")
        
      } else {
        #cat("No",wqparam[i],"at",s[x],"\n")
      }
    }
  }
  # 3. Handle Right censored (>)
  cat("Right Censored\n")
  if(exists("wqdata_right")){
    rm(wqdata_right)
  }
  for(x in 1:length(s)){
    tmp<-wqdata_left[wqdata_left$SiteName==s[x],]
    ok <- complete.cases(tmp[,3])
    tmp <- tmp[ok,]  
    # Only process sites that have data
    if(length(tmp[,1])!=0){
      if(!exists("wqdata_right")){
        
        wqdata_right<-rightCensored(tmp)
        
      } else {
        tmp_right<-rightCensored(tmp)
        wqdata_right <- rbind.data.frame(wqdata_right,tmp_right)
      }
      #cat("Found",length(tmp[,1]),"values for",wqparam[i],"at",s[x],"\n")
      
    } else {
      #cat("No",wqparam[i],"at",s[x],"\n")
    }
  }
  
  
  # 4. Jitter tied data
  cat("Jitter\n")
  if(exists("wqdata_jitter")){
    rm(wqdata_jitter)
  }
  for(x in 1:length(s)){
    tmp<-wqdata_right[wqdata_right$SiteName==s[x],]
    ok <- complete.cases(tmp[,3])
    tmp <- tmp[ok,]  
    # Only process sites that have data
    if(length(tmp[,1])!=0){
      #cat("Jitter",s[x],"\n")
      if(!exists("wqdata_jitter")){
        
        wqdata_jitter<-addJitter(tmp)
        
      } else {
        tmp_jitter<-addJitter(tmp)
        wqdata_jitter <- rbind.data.frame(wqdata_jitter,tmp_jitter)
      }
      
    } else {
      #cat("No",wqparam[i],"at",s[x],"\n")
    }
  }
  
  
  #wqdata <- merge(wqdata, l, by.x="SiteName",by.y="Site", all.x=TRUE) # using native sitetable sitenames to match
  wqdata_jitter$OriginalValue <- wqdata_jitter$Value 
  wqdata_jitter$Value <- wqdata_jitter$i3Values
  
  wqdata <- merge(wqdata_right, l, by.x="SiteName",by.y="CouncilSiteID", all.x=TRUE) # Using Hilltop sitenames to match site information
  
  #wqdata$parameter <- wqparam[i]
  
  wqdata_q <- wqdata                  # retaining original data retrieved from webservice
  
  #wqdata <- merge(wqdata, l, by.x="SiteName",by.y="Site", all.x=TRUE) # using native sitetable sitenames to match
  #   wqdata <- merge(wqdata, l, by.x="SiteName",by.y="Site", all.x=TRUE) # Using Hilltop sitenames to match site information
  #   wqdata$parameter <- wqparam[i]
  
  # Reduce dataset to complete cases only - removes sites that have data in the hilltop
  # files, but are not explicitly included in the LAWA site table.
  ok <- complete.cases(wqdata[,3])
  wqdata <- wqdata[ok,]  
  
  # Fields have  been renamed from WFS feed to match what the code is
  # expecting, as code originally written expecting data from Hilltop Site Table.
  newFieldNames <- c("SiteName","Date","Value","Method","parameter", "Censored",
                     "CenType","converted_values","dflag","i1Values","i2Values","X",
                     "LAWAID","ID", "UsedInLAWA","AltitudeGroup","LanduseGroup","FrequencyAll",
                     "Frequency","Region","Agency","ISLAND","CatchID","CatchType",
                     "NZREACH","Catchment","Comments","LawaCatchm","CatchLbl")
  
  names(wqdata) <- newFieldNames
  
  
  
  # Deprecated 18-Sep-2016 as censored data handled by functions
  # supplied by Ton Snelder.
  #wqdata <- merge(wqdata, tr, by.x="LAWAID",by.y="LAWAID", all.y=TRUE)
  
  ## --- WHAT IS BEING REORDER HERE AND WHY? ---
  # Reorder items in data.frame
  #wqdata <- wqdata[order(wqdata[,1],wqdata[,3]),]
  
  # Building dataframe to save at the end of this step 
  if(i==1){
    lawadata <- wqdata
    lawadata_q <- wqdata_q
  } else {
    lawadata <- rbind(lawadata,wqdata)
    lawadata_q <- rbind(lawadata_q,wqdata_q)
  }    
  
  
  print(Sys.time() - x)
  
  
}

# Housekeeping
# - Saving the lawadata table
save(lawadata,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trenddata",StartYear,"-",EndYear,".RData",sep=""))
#write.csv(lawadata,"//file/herman/R/OA/08/02/2017/Water Quality/ROutput/LAWA_RAW_DATA_TREND05yr.csv")

save(lawadata_q,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trenddata_q_",StartYear,"-",EndYear,".RData",sep=""))

# disconnect from Hilltop object
Hilltop::disconnect(lawa)


# DATA CLEANSE #

#Calculating the Long term LAWA Trends in water quality.

#Reference Dates
#StartYear <- Set at start of script
#EndYear <-   Set at start of script
years <- EndYear - StartYear + 1
StartMonth <- 1
EndMonth <- 12
if(years==5){
  rate<-1
} else{
  rate<-0.9
}
load(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trenddata",StartYear,"-",EndYear,".RData",sep=""))

###################################################################################
#Step 0 Data Summary
###################################################################################
#b "  Ensure one value per sampling interval
#b "  Calculate the number of samples per sampling interval and select/calculate/
#     determine representative values

lawadata <- samplesByInterval(StartYear,EndYear,StartMonth,EndMonth,lawadata)

#drop sites with no LAWA ID
lawadata <- lawadata[!is.na(lawadata$LAWAID),]

df_count <- summaryBy(Value~SiteName+parameter+yearMon,data=lawadata, FUN=c(length), keep.name=TRUE)


multipleResultsByMonth <- subset(df_count,Value>1)

# ---- DOCUMENTATION ----
# Values are now derived for the three sampling frequencies.
# Medians are derived, rather than nearest values. The original
# justification was the number of samples that Councils would 
# deliver in their data for a month - SOE samples were not
# necessarily separated out from other project data. A decision
# was made early on to simply take a median. That decision has
# not been reviewed, but should have been, in the light of 
# of the progressive improvements in data supply.

## Default median calculation
df_value_monthly <- subset(summaryBy(Value~SiteName+parameter+yearMon,data=lawadata,
                                     id=~LAWAID+LanduseGroup+AltitudeGroup+Catchment+Region+Frequency+year+mon+bimon+Qtr+depth,
                                     FUN=c(quantile), prob=c(0.5), type=5, keep.name=TRUE),Frequency=="Monthly")
# 
# df_value_bimonthly <- subset(summaryBy(Value~SiteName+parameter+yearBimon,data=lawadata,
#                                        id=~LAWAID+LanduseGroup+AltitudeGroup+Catchment+Region+Frequency+year+mon+bimon+Qtr+depth,
#                                        FUN=c(quantile), prob=c(0.5), type=5, keep.name=TRUE),Frequency=="Monthly" | Frequency=="Bimonthly")

#df_value_quarterly <- subset(summaryBy(Value~SiteName+parameter+yearQtr,data=lawadata,
#                                       id=~LAWAID+LanduseGroup+AltitudeGroup+Catchment+Region+Frequency+year+mon+bimon+Qtr+depth,
#                                       FUN=c(quantile), prob=c(0.5), type=5, keep.name=TRUE),Frequency=="Monthly" | Frequency=="Bimonthly" | Frequency=="Quarterly")


## A slightly less fraught method is to use Aggregate() to return first value in each group
df_value_quarterly <- aggregate(lawadata, list(lawadata$SiteName,
                                               lawadata$parameter,
                                               lawadata$yearQtr), FUN=head, 1)
df_value_quarterly <- df_value_quarterly[,c(4,8,40,6,16,20,19,29,23,22,34,35,37,39,33)] # matching columns to what's generated by summaryBy above.


###################################################################################


#require(wq)

#======================================================
#SEASONAL KENDALL ANALYSIS
#======================================================

t <- Sys.time()


######################################################################################################
# Seasonal Kendall for Monthly Sampling for each site, by each parameter

# Returning unique LAWAIDs for processing data subsets
#lawa_ids <- as.character(unique(df_value_monthly$LAWAID))
uLAWAID <- unique(df_value_monthly$LAWAID)
months<-12   # Monthly data
monthLbl <- "Monthly"
k <- 1       # counter for sites/parameters meeting minimum N
rbindTrendCheck<-FALSE # Flagging condition for rbinding dataframe that keeps track of sites, measurements and checks for trend inclusion
for(i in 1:length(uLAWAID)){
  # Store current LAWAID
  l <- uLAWAID[i]
  # Store vector of unique parameters for current LAWAID
  uWQParam <- unique(df_value_monthly$parameter[df_value_monthly$LAWAID==l])
  
  #l <- lawa_ids[i]
  #df_value_monthly1 <- subset(df_value_monthly, LAWAID==l)
  #parameters <- as.character(unique(df_value_monthly1$parameter))
  # this step is to double check output with TimeTrends
  
  ## The following two lines of code have now been rendered unnecessary by changing
  ## the library used for the trend analysis from "wq" to "EnvStats".
  ## The original "wq" library (now no longer available on CRAN) required a
  ## timeseries object to be passed to the seasonal kendall functdion; the
  ## EnvStats library only requires a dataframe.
  
  #lawa <- wqData(data=df_value_monthly1,locus=c(3,5,15),c(2,4),site.order=TRUE,time.format="%Y-%m-%d",type="long")
  #x <- tsMake(object=lawa,focus=gsub("-",".",l))
  
  # calculating seasonal Kendall statistics for individual parameters for an individual site
  
  for(j in 1:length(uWQParam)){
    # subsetting dataframe based on lawaid and parameter
    x <- df_value_monthly[df_value_monthly$LAWAID==l&df_value_monthly$parameter==uWQParam[j],]  
    
    ### TREND INCLUSION CRITERIA
    # 1. Count samples in the first or last 12 month periods to compare to entry criteria for trend
    first_year<-length(x$year[x$year==StartYear])
    last__year<-length(x$year[x$year==EndYear])
    
    # 2. Count samples in order to compare to entry criteria for trend
    num_samples <- length(x[,1])
    
    # building dataframe to report out data to assess pass-fails for trend inclusion
    v <- matrix(data=c(l,uWQParam[j],first_year,last__year,num_samples),nrow=1,ncol=5,byrow=TRUE)
    if(!rbindTrendCheck){ 
      validDataForTrend <-as.data.frame(v,stringsAsFactors=FALSE)
      rbindTrendCheck<-TRUE
    } else {
      validDataForTrend <- rbind(validDataForTrend, as.data.frame(v,stringsAsFactors=FALSE))
    }
    
    # Check Trend Criteria - Assess pass-fail for trends analysis
    PassTrendCriteria <- TrendCriteria(first_year, last__year, num_samples, rate, years, months, monthMultiplier = 1)
    
    # Processing Trends for sites/parameters pass trend criteria
    if(PassTrendCriteria){
      
      s <- seaKenEPA(x)
      list(sen.slope     = sk.sen.slope, 
           sen.slope.pct = sk.sen.slope.pct, 
           p.value       = sk.p.value,
           sen.z         = sk.sen.z,
           sen.z.prob    = sk.sen.z.prob,
           slope.lcl90   = sk.slope.LCL90,
           slope.ucl90   = sk.slope.UCL90)
      
      m <-data.frame(l,
                     uWQParam[j],
                     s$sen.slope.pct,
                     s$sen.slope,
                     s$p.value,
                     sen.z,
                     sen.z.prob,
                     s$slope.lcl90,
                     s$slope.ucl90,
                     stringsAsFactors = FALSE)
      if(k==1){
        seasonalkendall <- m
        #cat("seasonalkendal dataframe created\n")
      } else {
        seasonalkendall <- rbind(seasonalkendall, m)
        #cat("Appending to seasonalkendall dataframe\n")
      }
      
      rm(s,m)
      k <- k + 1
      
    }
  }
  
}

names(seasonalkendall) <- c("LAWAID",
                            "Parameter",
                            "Sen.Pct",
                            "Sen.Slope",
                            "p.value",
                            "sen.z",
                            "sen.z.prob",
                            "Sen.Slope.LCL90",
                            "Sen.Slope.UCL90")

seasonalkendall$freq<- monthLbl
trendscores <- calcTrendScore(seasonalkendall)

names(validDataForTrend)  <- c("LAWAID","Parameter","N.Months.StartYear","N.Months.EndYear","Num.Samples")
validDataForTrend$freq<- monthLbl


#names(seasonalkendall)    <- c("LAWAID","Parameter","Sen.Pct","Sen.Slope","p.value")

#seasonalkendall$Sen.Pct <-as.numeric(as.character(seasonalkendall$Sen.Pct))
#seasonalkendall$Sen.Slope <-as.numeric(as.character(seasonalkendall$Sen.Slope))
#seasonalkendall$p.value <-as.numeric(as.character(seasonalkendall$p.value))


rm(m)

load(file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/lawa_sitetable.RData")


# Fields have  been renamed from WFS feed to match what the code is
# expecting, as code originally written expecting data from Hilltop Site Table.
newFieldNames <- c("X","LAWAID","ID", "CouncilSiteID", "UsedInLAWA","AltitudeGroup","LanduseGroup","FrequencyAll","Frequency",
                   "Region","Agency","ISLAND","CatchID","CatchType",
                   "NZREACH","Catchment","Comments","LawaCatchm")

names(l) <- newFieldNames

trends <- merge(trendscores, l, by.x="LAWAID",by.y="LAWAID",all.x=TRUE) # Using LAWAIDs to join tables
rm(seasonalkendall)
write.csv(trends,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_monthly_",StartYear,"-",EndYear,".csv",sep=""))
write.csv(validDataForTrend,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_monthly_",StartYear,"-",EndYear,".csv",sep=""))

cat("LAWA Water QUality Trend Analysis\nCompleted assigning Trend Results for Monthly Data\n")
cat(paste(k,"Sites/Parameter combinations meet 90 percent sampling occasion requirements\n"))
rm(trends,validDataForTrend)

# ######################################################################################################
# # Seasonal Kendall for Bimonthly Sampling for each site, by each parameter
# 
# # Returning unique LAWAIDs for processing data subsets
# lawa_ids <- as.character(unique(df_value_bimonthly$LAWAID))
# 
# k <- 1 # counter for sites/parameters meeting minimum N
# rbindTrendCheck<-FALSE # Flagging condition for rbinding dataframe that keeps track of sites, measurements and checks for trend inclusion
# for(i in 1:length(lawa_ids)){
#   
#   months<-6
#   l <- lawa_ids[i]
#   df_value_bimonthly1 <- subset(df_value_bimonthly, LAWAID==l)
#   parameters <- as.character(unique(df_value_bimonthly1$parameter))
#   # this step is to double check output with TimeTrends
#   #Uncomment if needed
#   #write.csv(df_value_monthly1,file=paste("c:/data/MWR_2013/2013/ES-00022.csv",sep=""))
#   
#   lawa <- wqData(data=df_value_bimonthly1,locus=c(3,5,15),c(2,4),site.order=TRUE,time.format="%Y-%m-%d",type="long")
#   x <- tsMake(object=lawa,focus=gsub("-",".",l))
#   #cat("length(parameters)",length(parameters),"\n")
#   #cat(parameters,"\n")
#   
#   # calculating seasonal Kendall statistics for individual parameters for an individual site
#   for(j in 1:length(parameters)){
#     ### TREND INCLUSION CRITERIA
#     # 1. Count samples in the first or last 20 percent of the 5 year window to compare to entry criteria for trend
#     first_20pct<-length(df_value_bimonthly1$year[df_value_bimonthly1$year==StartYear & df_value_bimonthly1$parameter==parameters[j]])
#     last__20pct<-length(df_value_bimonthly1$year[df_value_bimonthly1$year==EndYear & df_value_bimonthly1$parameter==parameters[j]])
#     # 2. Count samples in order to compare to entry criteria for trend                                                                                          
#     num_samples <- length(subset(df_value_bimonthly1,parameter==parameters[j])[,1])                                                                             
#     
#     # building dataframe to report out data to assess pass-fails for trend inclusion                                                                            
#     v <- matrix(data=c(l,parameters[j],first_20pct,last__20pct,num_samples),nrow=1,ncol=5,byrow=TRUE)                                                           
#     if(!rbindTrendCheck){                                                                                                                                       
#       validDataForTrend <-as.data.frame(v,stringsAsFactors=FALSE)                                                                                               
#       rbindTrendCheck<-TRUE                                                                                                                                     
#     } else {                                                                                                                                                    
#       validDataForTrend <- rbind(validDataForTrend, as.data.frame(v,stringsAsFactors=FALSE))                                                                    
#     }                                                                                                                                                           
#     
#     # Check Trend Criteria - Assess pass-fail for trends analysis                                                                                               
#     PassTrendCriteria <- TrendCriteria(first_20pct, last__20pct, num_samples, rate, years, months,monthMultiplier = 1) # This month multiplier is linked to the 
#     # the number of months selected here
#     # Processing Trends for sites/parameters pass trend criteria
#     if(PassTrendCriteria){
#       
#       if(length(parameters)==1){
#         s<-seaKenLAWA(x,"median")              # x has a different structure where there is only one item
#       } else{
#         s<-seaKenLAWA(x[,j],"median")
#       }
#       #cat(i,lawa_ids[i],length(lawa$time),parameters[j],s$p.value,s$sen.slope.pct,"\n")
#       
#       #s$sen.slope.pct  #  <---- required for LAWA
#       #s$sen.slope      #  <----
#       #s$p.value        #  <---- required for LAWA
#       
#       
#       m <-matrix(data=c(l,parameters[j],s$sen.slope.pct,s$sen.slope,s$p.value),nrow=1,ncol=5,byrow=TRUE)
#       if(k==1){   # removed i==i condition - causing errors where first site doesn't meet criteria for trend analysis
#         seasonalkendall <-as.data.frame(m,stringsAsFactors=FALSE)
#         #cat("seasonalkendal dataframe created\n")
#       } else {
#         seasonalkendall <- rbind(seasonalkendall, as.data.frame(m,stringsAsFactors=FALSE))
#         #cat("Appending to seasonalkendall dataframe\n")
#       }
#       k <- k + 1
#     }
#   }
#   
# }
# 
# names(seasonalkendall) <- c("LAWAID","Parameter","Sen.Pct","Sen.Slope","p.value")
# names(validDataForTrend)  <- c("LAWAID","Parameter","N.Months.StartYear","N.Months.EndYear","Num.Samples")
# validDataForTrend$freq<- "Bimonthly"
# 
# seasonalkendall$Sen.Pct <-as.numeric(as.character(seasonalkendall$Sen.Pct))
# seasonalkendall$Sen.Slope <-as.numeric(as.character(seasonalkendall$Sen.Slope))
# seasonalkendall$p.value <-as.numeric(as.character(seasonalkendall$p.value))
# 
# seasonalkendall$freq<- "Bimonthly"
# trendscores <- calcTrendScore(seasonalkendall)
# 
# rm(m)
# 
# load(file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/lawa_sitetable.RData")
# 
# # Fields have  been renamed from WFS feed to match what the code is
# # expecting, as code originally written expecting data from Hilltop Site Table.
# newFieldNames <- c("X","LAWAID","ID", "CouncilSiteID", "UsedInLAWA","AltitudeGroup","LanduseGroup","FrequencyAll","Frequency",
#                    "Region","Agency","ISLAND","CatchID","CatchType",
#                    "NZREACH","Catchment","Comments","LawaCatchm")
# 
# names(l) <- newFieldNames
# 
# trends <- merge(trendscores, l, by.x="LAWAID",by.y="LAWAID",all.x=TRUE) # Using LAWAIDs to join tables
# rm(seasonalkendall)
# write.csv(trends,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_bimonthly_",StartYear,"-",EndYear,".csv",sep=""))
# write.csv(validDataForTrend,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_bimonthly_",StartYear,"-",EndYear,".csv",sep=""))
# 
# cat("LAWA Water QUality Trend Analysis\nCompleted assigning Trend Results for Bimonthly Data\n")
# cat(paste(k,"Sites/Parameter combinations meet 90 percent sampling occasion requirements\n"))
# rm(trends,validDataForTrend)

######################################################################################################
# Seasonal Kendall for Quarterly Sampling for each site, by each parameter

# Returning unique LAWAIDs for processing data subsets
#lawa_ids <- as.character(unique(df_value_monthly$LAWAID))
uLAWAID <- unique(df_value_quarterly$LAWAID)
months<-4   # Quarterly data
monthLbl <- "Quarterly"
k <- 1       # counter for sites/parameters meeting minimum N
rbindTrendCheck<-FALSE # Flagging condition for rbinding dataframe that keeps track of sites, measurements and checks for trend inclusion
for(i in 1:length(uLAWAID)){
  # Store current LAWAID
  l <- uLAWAID[i]
  # Store vector of unique parameters for current LAWAID
  uWQParam <- unique(df_value_quarterly$parameter[df_value_quarterly$LAWAID==l])
  
  #l <- lawa_ids[i]
  #df_value_monthly1 <- subset(df_value_monthly, LAWAID==l)
  #parameters <- as.character(unique(df_value_monthly1$parameter))
  # this step is to double check output with TimeTrends
  
  ## The following two lines of code have now been rendered unnecessary by changing
  ## the library used for the trend analysis from "wq" to "EnvStats".
  ## The original "wq" library (now no longer available on CRAN) required a
  ## timeseries object to be passed to the seasonal kendall functdion; the
  ## EnvStats library only requires a dataframe.
  
  #lawa <- wqData(data=df_value_monthly1,locus=c(3,5,15),c(2,4),site.order=TRUE,time.format="%Y-%m-%d",type="long")
  #x <- tsMake(object=lawa,focus=gsub("-",".",l))
  
  # calculating seasonal Kendall statistics for individual parameters for an individual site
  
  for(j in 1:length(uWQParam)){
    # subsetting dataframe based on lawaid and parameter
    x <- df_value_quarterly[df_value_quarterly$LAWAID==l&df_value_quarterly$parameter==uWQParam[j],]  
    
    ### TREND INCLUSION CRITERIA
    # 1. Count samples in the first or last 12 month periods to compare to entry criteria for trend
    first_year<-length(x$year[x$year==StartYear])
    last__year<-length(x$year[x$year==EndYear])
    
    # 2. Count samples in order to compare to entry criteria for trend
    num_samples <- length(x[,1])
    
    # building dataframe to report out data to assess pass-fails for trend inclusion
    v <- matrix(data=c(l,uWQParam[j],first_year,last__year,num_samples),nrow=1,ncol=5,byrow=TRUE)
    if(!rbindTrendCheck){ 
      validDataForTrend <-as.data.frame(v,stringsAsFactors=FALSE)
      rbindTrendCheck<-TRUE
    } else {
      validDataForTrend <- rbind(validDataForTrend, as.data.frame(v,stringsAsFactors=FALSE))
    }
    
    # Check Trend Criteria - Assess pass-fail for trends analysis
    PassTrendCriteria <- TrendCriteria(first_year, last__year, num_samples, rate, years, months, monthMultiplier = 1)
    
    # Processing Trends for sites/parameters pass trend criteria
    if(PassTrendCriteria){
      
      s <- seaKenEPA(x)
      list(sen.slope     = sk.sen.slope, 
           sen.slope.pct = sk.sen.slope.pct, 
           p.value       = sk.p.value,
           sen.z         = sk.sen.z,
           sen.z.prob    = sk.sen.z.prob,
           slope.lcl90   = sk.slope.LCL90,
           slope.ucl90   = sk.slope.UCL90)
      
      m <-data.frame(l,
                     uWQParam[j],
                     s$sen.slope.pct,
                     s$sen.slope,
                     s$p.value,
                     sen.z,
                     sen.z.prob,
                     s$slope.lcl90,
                     s$slope.ucl90,
                     stringsAsFactors = FALSE)
      if(k==1){
        seasonalkendall <- m
        #cat("seasonalkendal dataframe created\n")
      } else {
        seasonalkendall <- rbind(seasonalkendall, m)
        #cat("Appending to seasonalkendall dataframe\n")
      }
      
      rm(s,m)
      k <- k + 1
      
    }
  }
  
}

names(seasonalkendall) <- c("LAWAID",
                            "Parameter",
                            "Sen.Pct",
                            "Sen.Slope",
                            "p.value",
                            "sen.z",
                            "sen.z.prob",
                            "Sen.Slope.LCL90",
                            "Sen.Slope.UCL90")

seasonalkendall$freq<- monthLbl
trendscores <- calcTrendScore(seasonalkendall)

names(validDataForTrend)  <- c("LAWAID","Parameter","N.Months.StartYear","N.Months.EndYear","Num.Samples")
validDataForTrend$freq<- monthLbl


#names(seasonalkendall)    <- c("LAWAID","Parameter","Sen.Pct","Sen.Slope","p.value")

#seasonalkendall$Sen.Pct <-as.numeric(as.character(seasonalkendall$Sen.Pct))
#seasonalkendall$Sen.Slope <-as.numeric(as.character(seasonalkendall$Sen.Slope))
#seasonalkendall$p.value <-as.numeric(as.character(seasonalkendall$p.value))


rm(m)

load(file="//file/herman/R/OA/08/02/2017/Water Quality/ROutput/lawa_sitetable.RData")

# Fields have  been renamed from WFS feed to match what the code is
# expecting, as code originally written expecting data from Hilltop Site Table.
#newFieldNames <- c("X","LAWAID","ID", "UsedInLAWA","AltitudeGroup","LanduseGroupAll","LanduseGroup","FrequencyAll","Frequency",
#                   "Region","Agency","ISLAND","CatchID","CatchType",
#                   "NZREACH","Catchment","Comments","LawaCatchm")

newFieldNames <- c("X","LAWAID","ID", "CouncilSiteID", "UsedInLAWA","AltitudeGroup","LanduseGroup","FrequencyAll","Frequency",
                   "Region","Agency","ISLAND","CatchID","CatchType",
                   "NZREACH","Catchment","Comments","LawaCatchm")

names(l) <- newFieldNames

trends <- merge(trendscores, l, by.x="LAWAID",by.y="LAWAID",all.x=TRUE) # Using LAWAIDs to join tables
rm(seasonalkendall)
write.csv(trends,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/trend_quarterly_",StartYear,"-",EndYear,".csv",sep=""))
write.csv(validDataForTrend,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/validDataForTrend_quarterly_",StartYear,"-",EndYear,".csv",sep=""))

cat("LAWA Water QUality Trend Analysis\nCompleted assigning Trend Results for Quarterly Data\n")
cat(paste(k,"Sites/Parameter combinations meet 90 percent sampling occasion requirements\n"))
rm(trends,validDataForTrend)

######################################################################################################


print(Sys.time()-t)
setwd(od)

