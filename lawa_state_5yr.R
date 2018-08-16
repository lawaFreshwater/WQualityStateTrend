#===================================================================================================
#  LAWA STATE ANALYSIS
#  Horizons Regional Council
#
#  2 September 2016
#
#  Purpose: Water Quality State Analysis Service Definition
#
#  Processing of council water quality monitoring data for the LAWNZ website
#  has been completed by Horizons council staff between 2011 and 2015. To
#  reduce the dependancy on council staff and to increase transparency to
#  all participants, this script file has been prepared to automate the STATE
#  assessment portion of LAWA's State and Trend Analysis.
#
#  To make the data collation component of this script as flexible as possible,
#  proprietary file formats or RDBMS systems are not used. Instead, data is
#  accessed using standards-based requests to Council time series servers that
#  deliver WaterML 2.0 XML files. WaterML 2.0 is an Open Geospatial Consortium
#  standard that encodes water data time series into an XML file. These data
#  can be accessed using standard XML libraries provided by many programming
#  languages.
#
#  Maree Clark
#  Sean Hodges
#  Horizons Regional Council
#===================================================================================================

# Clearing workspace
rm(list = ls())

ANALYSIS<-"STATE"
# Set working directory
od <- getwd()
# wd <- "//file/herman/R/OA/08/02/2017/Water Quality/R/lawa_state"
# setwd(wd)

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
StartYear <- 2012
EndYear <- 2018

#if(!exists(foo, mode="function")) source("lawa_state_functions.R")

#/* -===Include required function libraries===- */ 

source("h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")

HilltopLibrary<-FALSE
library(Hilltop)
HilltopLibrary<-TRUE
#-- Specifying source files - note that these connections are pulling from SOE folder on HilltopDEV

if(HilltopLibrary==TRUE){
  lawa            <- Hilltop::HilltopData("//hilltopdev/data/lawa2017/state/lawa_provisional_2017.dsn")
  #returns a data file object - a link to the server.
}

#/* -===Global variable/constant definitions===- */ 
vendor <- c("52NORTH","AQUATIC","HILLTOP","KISTERS")


#/* -===Local variable/constant definitions===- */
# Need to retain PH in this list for NOF Calculations later
wqparam <- c("BDISC","TURB","NH4","PH","TON","TN","DRP","TP","ECOLI") 
#wqparam <- c("BDISC") 
tss <- 3  # tss = time series server
#tss_url <- "http://hilltopdev.horizons.govt.nz/lawa2014.hts?"
tss_url <- "http://hilltopdev.horizons.govt.nz:8080/lawa2018.lawa?"

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
if(HilltopLibrary==FALSE){
  requestData(vendor[tss],tss_url,"service=Hilltop&request=Reset")
}
# Replace database call here with call to previously loaded WFS Site data
#l <- SiteTable(databasePathFileName="//file/herman/R/OA/08/02/2016/MASTER SiteList/lawa_2016.mdb",sqlID=3) ## Allows for different sitenames in hilltop.mdb - requires assessment and population of the database.
l <- read.csv("LAWA_Site_Table1.csv",stringsAsFactors=FALSE) #This is intersected with catchment info
#l2 <- read.csv("provisional_LAWA_Site_Table.csv",stringsAsFactors=FALSE)
#l<-rbind.data.frame(l1,l2,stringsAsFactors=FALSE)

l$SWQLanduse[l$SWQLanduse=="Native"|l$SWQLanduse=="Exotic"|l$SWQLanduse=="Natural"] <- "Forest"

#Get a list of sites on the lawa server connection
#The result is a data frame that has the site name, location and time range of each site
if(HilltopLibrary==TRUE){
  s <- Hilltop::SiteList(lawa)
} else {
  r <- requestData(vendor[tss],tss_url,request=paste(hts[1],hts[2],sep=""))
  s <- SiteList(r)
}

cat("LAWA Water QUality State Analysis\n","Number of sites returned:",length(s))


# 
# for(i in 1:length(wqparam)){
#   #Quick Check of number of sites with data
#   #requestData(vendor[tss],tss_url,"service=Hilltop&request=Reset")
# 
#   r <- readUrl(vendor[tss],tss_url,paste(hts[1],hts[4],wqparam[i],hts[length(hts)],sep=""))
#   wqdata <- MeasurementList(xmlmdata=r,requestType="Hilltop")
#   wqdata$Value <- as.character(wqdata$Value)
#   cat(wqparam[i]," sites returned: ",length(unique(wqdata$SiteName)),"\n",sep="")
# }


# -=== WQ PARAMETERS ===-
#requestData(vendor[tss],tss_url,"service=Hilltop&request=Reset")
for(i in 1:length(wqparam)){
  # for(i in 4:4){
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
      dx <- try(GetData(lawa,
                        mySites[ii],
                        myMeas,
                        startTime=paste(StartYear,"-01-01",sep=""),
                        endTime=paste(EndYear + 1,"-01-01",sep=""),
                        WQParams=FALSE),silent=TRUE)
      if(attr(dx,"class")!="try-error"){
        if(!exists("wqdata")){
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
    r <- readUrl(vendor[tss],tss_url,paste(hts[1],hts[4],wqparam[i],hts[length(hts)],sep="")) #Calls for all data for a wqparam.  All sites?
    #r <- requestData(vendor[tss],tss_url,paste(hts[1],hts[4],wqparam[i],hts[length(hts)],sep=""))
    wqdata <- MeasurementList(xmlmdata=r,requestType="Hilltop") #Returns dataframe of Date, SiteName, Value, Method
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
  cat("Left Censored data\n")
  
  if(ANALYSIS=="STATE"){
    wqdata_left <- qualifiedValues2(wqdata_cen)
  } else {
    # For TREND, apply leftCensored()
    
    if(exists("wqdata_left")){
      rm(wqdata_left)
    }
    for(x in 1:length(s)){
      
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
  # 3. Handle Right censored (>) in state and trend
  cat("Right Censored data\n")
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
  
  
  #wqdata <- merge(wqdata, l, by.x="SiteName",by.y="Site", all.x=TRUE) # using native sitetable sitenames to match
  wqdata_right$OriginalValue <- wqdata_right$Value 
  wqdata_right$Value <- wqdata_right$i2Values
  countSites_afterROS   <- length(unique(wqdata_right$SiteName))
  countSites            <- length(unique(wqdata$SiteName))
  cat(wqparam[i],": Site count before ROS   - ",countSites,"\n",
      wqparam[i],": Site count after ROS    - ",countSites_afterROS,"\n",sep="")
  
  wqdata <- merge(wqdata_right, l, by.x="SiteName",by.y="CouncilSiteID", all.x=TRUE) # Using Hilltop sitenames to match site information
  #wqdata$parameter <- wqparam[i]
  countSites_AfterMerge <- length(unique(wqdata$SiteName))
  cat(wqparam[i],": Site count before merge - ",countSites,"\n",
      wqparam[i],": Site count after merge  - ",countSites_afterROS,"\n",sep="")
  
  
  # Water quality data includes <, > and * at this point. A decision is need here regarding
  # the method for turning these qualified values into a numeric of some form.
  wqdata_q <- wqdata                  # retaining original data retrieved from webservice
  
  
  #wqdata <- qualifiedValues(wqdata)   # processing less than values
  #wqdata <- subset(wqdata,Value>=0)
  
  # Reduce dataset to complete cases only - removes sites that have data in the hilltop
  # files, but are not explicitly included in the LAWA site table.
  ok <- complete.cases(wqdata[,3])
  wqdata <- wqdata[ok,]
  
  # There are some data that have been provide with duplicate daily values
  #   - values at mid-night and later during the day.
  # To resolve this issue for the moment, we are simply calculating a median
  # daily value.
  
  # Date/times are reduced to Dates with no times, and then the median value for each day
  # is generated, ensuring that all other data is kept for each record.
  
  # Fields have  been renamed from WFS feed to match what the code is
  # expecting, as code originally written expecting data from Hilltop Site Table.
  newFieldNames <- c("SiteName","Date","Value","Method","parameter",
                     "Censored","CenType","ROS","iValues","converted_values","dflag",
                     "i2Values","OriginalValues","X","LAWAID","ID",
                     "UsedInLAWA","AltitudeGroup","LanduseGroup","FrequencyAll","Frequency",
                     "Region","Agency","ISLAND","CatchID","CatchType",
                     "NZREACH","Catchment","Comments","LawaCatchm","CatchLbl")
  
  
  names(wqdata) <- newFieldNames
  
  ### ADD IN THE LAWA SITE TABLE ATTRIBUTES
  
  wqdata_A <- wqdata
  wqdata_A$dayDate <- trunc(wqdata_A$Date,"days")
  
  # This summaryBy configured for site data fields available from WFS feeds
  # Fields have also been renamed from WFS feed to match what the code is
  # expecting, as code originally written expecting data from Hilltop Site Table.
  
  #     wqdata_med <- summaryBy(Value~LAWAID+SiteName+parameter+dayDate,
  #                           id=~ID+Agency+Site_Type+Region+NZREACH+
  #                             LanduseGroup+AltitudeGroup+Catchment+Frequency+
  #                             NZTM_X+NZTM_Y+WSGS84_X+WSGS84_Y+Comments+
  #                             NIWASITE+NZMS260+MFEX+MFEY+Catchment.Name.LAWA+
  #                             InHilltopFile+UsedInLAWA,
  #                           data=wqdata_A, 
  #                           FUN=c(median), na.rm=TRUE, keep.name=TRUE)
  
  #     ## using median() function
  #     wqdata_med <- summaryBy(Value~LAWAID+SiteName+parameter+dayDate,
  #                             id=~ID+Agency+Region+NZREACH+
  #                               LanduseGroup+AltitudeGroup+Catchment+Frequency+
  #                               Comments+UsedInLAWA,
  #                             data=wqdata_A, 
  #                             FUN=c(median), na.rm=TRUE, keep.name=TRUE)
  
  ## using hazen method for median - quantile(prob=c(0.5),type=5)
  wqdata_med <- summaryBy(Value~LAWAID+SiteName+parameter+dayDate,
                          id=~ID+Agency+Region+NZREACH+
                            LanduseGroup+AltitudeGroup+Catchment+CatchLbl+Frequency+
                            Comments+UsedInLAWA,
                          data=wqdata_A, 
                          FUN=c(quantile), prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
  
  # Building dataframe to save at the end of this step 
  if(i==1){
    #lawadata <- wqdata
    lawadata <- wqdata_med
    lawadata_q <- wqdata_q
  } else {
    #lawadata <- rbind(lawadata,wqdata)
    lawadata <- rbind(lawadata,wqdata_med)
    lawadata_q <- rbind(lawadata_q,wqdata_q)
  }   
  
  
  # =======================================================
  # Water Quality State Analysis
  # =======================================================
  
  # All data for the current parameter is passed through to the StateAnalysis
  # Function.
  #   The output of this function is a data.frame with site medians, 
  # with the grouping variables of landuse, altitude, catchment and local
  # local authority name. This data.frame forms the basis for calculating
  # State for each site, based on the median of the last sampled values
  #   This step also excludes those sites that meets the following exclusion
  # criteria:
  #
  # Exclusion criteria
  #   - less than 30 samples for monthly samples
  #   - less than 80 percent of samples for bimonthly/quarterly
  
  # Exclude PH
  if(wqparam[i]!="PH0"){
    cat("LAWA Water Quality State Analysis\n",wqparam[i])
    
    print(Sys.time() - x)
    
    cat("\nLAWA Water QUality State Analysis\nCalculating reference quartiles\n")
    
    state <- c("Site","Catchment","Region","NZ")
    level <- c("LandUseAltitude","LandUse","Altitude","None")
    
    sa11 <- StateAnalysis(wqdata,state[1],level[1])
    
    sa21 <- StateAnalysis(wqdata,state[2],level[1])
    sa22 <- StateAnalysis(wqdata,state[2],level[2])
    sa23 <- StateAnalysis(wqdata,state[2],level[3])
    sa24 <- StateAnalysis(wqdata,state[2],level[4])
    
    sa31 <- StateAnalysis(wqdata,state[3],level[1])
    sa32 <- StateAnalysis(wqdata,state[3],level[2])
    sa33 <- StateAnalysis(wqdata,state[3],level[3])
    sa34 <- StateAnalysis(wqdata,state[3],level[4])
    
    sa41 <- StateAnalysis(wqdata,state[4],level[1])
    sa42 <- StateAnalysis(wqdata,state[4],level[2])
    sa43 <- StateAnalysis(wqdata,state[4],level[3])
    sa44 <- StateAnalysis(wqdata,state[4],level[4])
    
    cat("LAWA Water QUality State Analysis\n","Binding ",wqparam[i]," data together for measurement\n")
    
    if(i==1){
      sa <- rbind(sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)
    } else {
      sa <- rbind(sa,sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)
    }
  }
  rm(wqdata)
}

# disconnect from Hilltop object
Hilltop::disconnect(lawa)

# Housekeeping
# - Saving the lawadata table
save(lawadata,file=paste("//file/herman/r/oa/08/02/2017/Water Quality/ROutput/lawadata",StartYear,"-",EndYear,".RData",sep=""))
save(lawadata_q,file=paste("//file/herman/r/oa/08/02/2017/Water Quality/ROutput/lawadata_q_",StartYear,"-",EndYear,".RData",sep=""))
save(l,file="//file/herman/r/oa/08/02/2017/Water Quality/ROutput/lawa_sitetable.RData")

load(file=paste("//file/herman/r/oa/08/02/2017/Water Quality/ROutput/lawadata",StartYear,"-",EndYear,".RData",sep=""))
#Sites that are missing LAWAIDs
chkbb<- unique(lawadata$SiteName[is.na(lawadata$LAWAID)])
# -- WORTH CHECKING _WHY_ SITES ARE MISSING LAWA IDs
# Check:
# 1. There may be differences in siteIds and Council siteids when data is joined, resulting
#    in unintended omissions.


cat("Number of unique sites",length(unique(lawadata$SiteName)))
cat("Number of unique LAWAIDs",length(unique(lawadata$LAWAID)))
cat("Number of sites missing LAWAIDS from state analysis at this point: ",length(chkbb),"\n")


# - Remove extraneous objects
rm(sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)

# State Analysis output contains quantiles for each parameter by site.
# - Rename data.frame headings
names(sa) <- c("AltitudeGroup","LanduseGroup","Region","Catchment","SiteName","LAWAID","Parameter","Q0","Q25","Q50","Q75","Q100","N","Scope")
# - Write data.frame to a csv file for inspection

# filter sa to remove any LAWAIDS that are NA
sa <- sa[!is.na(sa$LAWAID),]
write.csv(sa,file=paste("//file/herman/r/oa/08/02/2017/Water Quality/ROutput/sa",StartYear,"-",EndYear,".csv",sep=""))

#sa<-read.csv(file=paste("//file/herman/r/oa/08/02/2017/Water Quality/ROutput/sa",StartYear,"-",EndYear,".csv",sep=""),stringsAsFactors=FALSE)
#sa <- sa[,-1]


cat("LAWA Water QUality State Analysis\nAssigning State Scores\n")
# ' //   In assigning state scores, the routine needs to process each combination of altitude
# ' // and landuse and compare to the National levels for the same combinations.
# ' //   These combinations are:

# ' //   National data set - no factors
# ' //       Each site (all altitude and landuses) compared to overall National medians

# ' //   Single factor comparisons
# ' //       Each upland site (all landuses) compared to upland National medians
# ' //       Each lowland site (all landuses) compared to lowland National medians
# ' //       Each rural site (all altitudes) compared to rural National medians
# ' //       Each forest site (all altitudes) compared to forest National medians
# ' //       Each urban site (all altitudes) compared to urban National medians

# ' //   Multiple factor comparisons
# ' //      For each Altitude
# ' //        Each rural site compared to rural National medians
# ' //        Each forest site compared to forest National medians
# ' //        Each urban site compared to urban National medians

# ' //      For each LandUse
# ' //        Each upland site compared to upland National medians
# ' //        Each lowland site compared to lowland National medians


scope <- c("Site","Catchment","Region") 

for(i in 1:3){
  
  ss1 <- StateScore(sa,scope[i],"","",wqparam,comparison=1)  
  ss21 <- StateScore(sa,scope[i],"Upland","",wqparam,comparison=2)
  ss22 <- StateScore(sa,scope[i],"Lowland","",wqparam,comparison=2)
  ss31 <- StateScore(sa,scope[i],"","Rural",wqparam,comparison=3)
  ss32 <- StateScore(sa,scope[i],"","Forest",wqparam,comparison=3)
  ss33 <- StateScore(sa,scope[i],"","Urban",wqparam,comparison=3)
  ss411 <- StateScore(sa,scope[i],"Upland","Rural",wqparam,comparison=4)
  ss412 <- StateScore(sa,scope[i],"Upland","Forest",wqparam,comparison=4)
  
  # The following line will fail if there are no sites with Upland Urban classification
  # Need to put a test into the StateScore function to return an empty dataframe
  
  # RE-ENABLE THIS ONCE BOPRC data available
  ss413 <- StateScore(sa,scope[i],"Upland","Urban",wqparam,comparison=4)
  
  ss421 <- StateScore(sa,scope[i],"Lowland","Rural",wqparam,comparison=4)
  ss422 <- StateScore(sa,scope[i],"Lowland","Forest",wqparam,comparison=4)
  ss423 <- StateScore(sa,scope[i],"Lowland","Urban",wqparam,comparison=4)
  
  
  # RE-ENABLE THIS ONCE BOPRC data available
  #   if(i==1){
  #     ss <- rbind(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss413,ss421,ss422,ss423)
  #   } else{
  #     ss <- rbind(ss,ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss413,ss421,ss422,ss423)
  #   }
  #   
  if(i==1){
    ss <- rbind(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)
  } else{
    ss <- rbind(ss,ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)
  }
  
}

# Housekeeping
# - Remove extraneous objects
#rm(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss413,ss421,ss422,ss423)
rm(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)



write.csv(ss,file=paste("//file/herman/r/oa/08/02/2017/Water Quality/ROutput/state",StartYear,"-",EndYear,".csv",sep=""))



cat("LAWA Water QUality State Analysis\nCompleted assigning State Scores\n")


print(Sys.time() - x)


ss_csv <- read.csv(file=paste("//file/herman/r/oa/08/02/2017/Water Quality/ROutput/state",StartYear,"-",EndYear,".csv",sep=""),header=TRUE,sep=",",quote = "\"")

ss.1 <- subset(ss_csv,Scope=="Region")
ss.1$Location <- ss.1$Region
ss.2 <- subset(ss_csv,Scope=="Catchment")
ss.2$Location <- ss.2$Catchment
ss.3 <- subset(ss_csv,Scope=="Site")
ss.3$Location <- ss.3$LAWAID

ss.4 <- rbind.data.frame(ss.1,ss.2,ss.3)
unique(ss.4$Location)

ss.5 <- ss.4[c(18,8,2,3,11,17,4,15,16)]  # Location, Parameter, Altitude, Landuse, Q50, LAWAState, Region, Scope, StateGroup

write.csv(ss.5,file=paste("//file/herman/r/oa/08/02/2017/Water Quality/ROutput/LAWA_STATE_FINAL_",StartYear,"-",EndYear,".csv",sep=""))
lawadata_without_niwa <- subset(lawadata,Agency!="NIWA")
lawadata_q_without_niwa <- subset(lawadata_q,Agency!="NIWA")

write.csv(lawadata_without_niwa,"//file/herman/r/oa/08/02/2017/Water Quality/ROutput/LAWA_DATA.csv")
write.csv(lawadata_without_niwa,"//file/herman/r/oa/08/02/2017/Water Quality/ROutput/LAWA_RAW_DATA.csv")

setwd(od)
