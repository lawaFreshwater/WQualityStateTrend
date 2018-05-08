#===================================================================================================
#  LAWA TREND ANALYSIS
#  Horizons Regional Council
#
#  3 September 2016
#
#  Maree Clark
#  Staci Boyte
#  Sean Hodges
#  Horizons Regional Council
#===================================================================================================

# Clearing workspace
rm(list = ls())

# Set working directory
od <- getwd()
wd <- "//file/herman/R/OA/08/02/2017/Water Quality/R/lawa_state"
setwd(wd)

# Clean up output folder before starting script.
cleanup <- FALSE
if(cleanup){
  rOutput <- "//file/herman/r/oa/08/02/2017/Water Quality/ROutput"
  files <- list.files(rOutput)
  if(length(files) >0){
    for(i in 1:length(files)){
      file.remove(paste(rOutput,"/",files[i],sep=""))
    }
  }
}

x <- Sys.time()
#Reference Dates
StartYear <- 2007
EndYear <- 2016

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


#/* -===Local variable/constant definitions===- */
wqparam <- c("BDISC","TURB","NH4","TON","TN","DRP","TP","ECOLI") 
#wqparam <- c("BDISC") 
tss <- 3  # tss = time series server
# tss_url <- "http://hilltopdev.horizons.govt.nz/lawa2014trend10.hts?"
# tss_url <- "http://hilltopdev.horizons.govt.nz:8080/lawa2016trend10.lawa?"
tss_url <- "http://hilltopdev.horizons.govt.nz:8080/LAWA2017.lawa?"

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
#l <- SiteTable(databasePathFileName="//file/herman/R/OA/08/02/2017/MASTER SiteList/lawa_2016.mdb",sqlID=3) ## Allows for different sitenames in hilltop.mdb - requires assessment and population of the database.
l <- read.csv("LAWA_Site_Table1.csv",stringsAsFactors=FALSE)

l$SWQLanduse[l$SWQLanduse=="Native"|l$SWQLanduse=="Exotic"|l$SWQLanduse=="Natural"] <- "Forest"

r <- requestData(vendor[tss],tss_url,request=paste(hts[1],hts[2],sep=""))
s <- SiteList(r)


# Load Reference data for Trends --- NO LONGER REQUIRED WITH FUNCTIONS FROM TON
#                                --- SNELDER TO IMPUTE CENSORED VALUES 
#trendRules_csv <- read.csv(file=paste("//file/herman/R/OA/08/02/2017/Water Quality/RScript/lawa_state/trendrules.csv",sep=""),header=TRUE,sep=",",quote = "\"")

cat("LAWA Water QUality TREND Analysis\n","Number of sites returned:",length(s))


# -=== WQ PARAMETERS ===-
#requestData(vendor[tss],tss_url,"service=Hilltop&request=Reset")
for(i in 1:length(wqparam)){
  
  ## Added Hilltop library 2017-09-07
  if(HilltopLibrary!=TRUE){
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
  
  wqdata <- merge( wqdata_cen, l, by.x="SiteName",by.y="CouncilSiteID", all.x=TRUE) # Using Hilltop sitenames to match site information
  wqdata <- wqdata[complete.cases(wqdata[,3]),]  # removing any rows with null result values
  
  # Fields have  been renamed from WFS feed to match what the code is
  # expecting, as code originally written expecting data from Hilltop Site Table.
  newFieldNames <- c("SiteName","Date","Value","Method","parameter",
                     "Censored","CenType","X","LAWAID","ID",
                     "UsedInLAWA","AltitudeGroup","LanduseGroup","FrequencyAll","Frequency",
                     "Region","Agency","ISLAND","CatchID","CatchType",
                     "NZREACH","Catchment","Comments","LawaCatchm","CatchLbl")
  
  
  names(wqdata) <- newFieldNames

  # Building dataframe to save at the end of this step 
  if(i==1){
    lawadata <- wqdata
  } else {
    lawadata <- rbind(lawadata,wqdata)
  }    
}

lawadata <- lawadata[!is.na(lawadata$Region),]
wd <- "//file/herman/R/OA/08/02/2017/Water Quality/ROutput"
setwd(wd)

r <- unique(lawadata$Region)

# disconnect from Hilltop object
Hilltop::disconnect(lawa)


for(i in 1:length(r)){
  write.csv(lawadata[lawadata$Region==r[i],],paste(r[i],".csv",sep=""),row.names = FALSE)
}


# Housekeeping
# - Saving the lawadata table
save(lawadata,file=paste("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/RC-Supplied-Data",StartYear,"-",EndYear,".RData",sep=""))
#write.csv(lawadata,"//file/herman/R/OA/08/02/2017/Water Quality/ROutput/LAWA_RAW_DATA_TREND10yr.csv")