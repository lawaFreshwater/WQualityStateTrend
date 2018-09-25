rm(list=ls())
library(lubridate)
StartYear <- 2013
EndYear <- 2017
source("h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")
source("h:/ericg/16666LAWA/2018/LAWAFunctionsEG.R")


riverSiteTable=read.csv(file="h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors = F)
#get the catSiteTable, jsut fo rthe catchment info, for hte outpunt to ITE
# load("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawa_sitetable.RData",verbose=T)

#Load the latest made (might need to check it works nicely across month folders) 
if(!exists('wqdata')){
  combowqdata=tail(dir(path = "H:/ericg/16666LAWA/2018/WaterQuality/1.Imported",pattern = "AllCouncils.csv",recursive = T,full.names = T),1)
  cat(combowqdata)
  wqdata=read.csv(combowqdata,stringsAsFactors = F)
  wqdata$SWQLanduse[is.na(wqdata$SWQLanduse)]=riverSiteTable$SWQLanduse[match(wqdata$LawaSiteID[is.na(wqdata$SWQLanduse)],riverSiteTable$LawaSiteID)]
  wqdata$SWQAltitude[is.na(wqdata$SWQAltitude)]=riverSiteTable$SWQAltitude[match(wqdata$LawaSiteID[is.na(wqdata$SWQAltitude)],riverSiteTable$LawaSiteID)]
  rm(combowqdata)
  wqdata$SWQLanduse[tolower(wqdata$SWQLanduse)%in%c("unstated","")] <- NA
  wqdata$SWQLanduse[tolower(wqdata$SWQLanduse)%in%c("forest","native","exotic","natural")] <- "forest"
}

#Export for ITE LAWAID, SITE, Measurement, DATETIME, DATE, Raw Value, Symbol, Value, Region
wqdata$Symbol=""
wqdata$Symbol[wqdata$CenType=="Left"]='<'
wqdata$Symbol[wqdata$CenType=="Right"]='>'
wqdata$RawValue=paste0(wqdata$Symbol,wqdata$Value)

# wqdata$Catchment=catSiteTable$LAWA_CATCH[match(wqdata$LawaSiteID,catSiteTable$LawaSiteID)]

write.csv(wqdata%>%select(LawaSiteID,CouncilSiteID,parameter,Date,RawValue,Symbol,Value,Region),
          file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/RiverWQ_GraphData",
                             format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),row.names = F)

wqdYear=year(dmy(wqdata$Date))
wqdata <- wqdata[which(wqdYear>=StartYear & wqdYear<=EndYear),]
rm(wqdYear)
#928841 to 434698


# #You know?  Should be the SiteName matches with CouncilSiteID.  Are those in these tables?
# wqdata <- left_join(wqdata,catSiteTable,by="LawaSiteID",suffix=c("",".y"))

wqparam <- c("BDISC","TURB","NH4","PH","TON","TN","DRP","TP","ECOLI") 


suppressWarnings(rm(wqdata_A,wqdata_med,wqdata_n,lawadata,lawadata_q))
for(i in 1:length(wqparam)){
  
  wqdata_A = wqdata[tolower(wqdata$parameter)==tolower(wqparam[i]),]
  #CENSORING
  #Previously for state, left-censored was repalced by half the limit value, and right-censored was imputed
  wqdata_A$origValue=wqdata_A$Value
  if(any(wqdata_A$CenType=='Left')){
    wqdata_A$Value[wqdata_A$CenType=="Left"] <- wqdata_A$Value[wqdata_A$CenType=="Left"]/2
    #left-censored replacement value is the maximum of left-censored values, per site
    suppressWarnings(wqdata_A <- wqdata_A%>%group_by(LawaSiteID)%>%mutate(lcenrep=max(Value[CenType=='Left'],na.rm=T))%>%ungroup)
    wqdata_A$Value[wqdata_A$CenType=='Left'] <- wqdata_A$lcenrep[wqdata_A$CenType=='Left']
    wqdata_A <- wqdata_A%>%select(-lcenrep)
  }
  if(any(wqdata_A$CenType=='Right')){
    wqdata_A$Value[wqdata_A$CenType=="Right"] <- wqdata_A$Value[wqdata_A$CenType=="Right"]*1.1
    suppressWarnings(wqdata_A <- wqdata_A%>%group_by(LawaSiteID)%>%mutate(rcenrep=min(Value[CenType=='Right'],na.rm=T))%>%ungroup)
    wqdata_A$Value[wqdata_A$CenType=='Right'] <- wqdata_A$rcenrep[wqdata_A$CenType=='Right']
    wqdata_A <- wqdata_A%>%select(-rcenrep)
  }
  wqdata_A$LAWAID=wqdata_A$LawaSiteID
  wqdata_A$LanduseGroup=wqdata_A$SWQLanduse
  wqdata_A$AltitudeGroup=wqdata_A$SWQAltitude
  # wqdata_A$Catchment=wqdata_A$LAWA_CATCH
  wqdata_A$Frequency=wqdata_A$SWQFrequencyLast5
wqdata_A <- as.data.frame(wqdata_A)  
  #Medians per sampling occasion are then reduced within StateAnalysis to a median per site to reflect state
  wqdata_med <- summaryBy(formula=Value~SiteName+parameter+Date,
                          id=~LawaSiteID+SiteID+CouncilSiteID+Agency+Region+SWQLanduse+
                            SWQAltitude+SWQFrequencyLast5+Catchment,#+TermReach+LAWA_CATCH+CATCH_LBL+Comment+SWQuality,
                          data=wqdata_A,
                          FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
  wqdata_med$LAWAID=wqdata_med$LawaSiteID
  wqdata_med$LanduseGroup=wqdata_med$SWQLanduse
  wqdata_med$AltitudeGroup=wqdata_med$SWQAltitude
  wqdata_med$Catchment=wqdata_med$LAWA_CATCH
  wqdata_med$Frequency=wqdata_med$SWQFrequencyLast5

  wqdata_n <- summaryBy(formula=Value~SiteName+parameter+Date,
                        id=~LawaSiteID+SiteID+CouncilSiteID+Agency+Region+SWQLanduse+
                          SWQAltitude+SWQFrequencyLast5+Catchment,#+TermReach+LAWA_CATCH+CATCH_LBL+Comment+SWQuality,
                        data=wqdata_A, 
                        FUN=length,keep.names=T)
  wqdata_c <- summaryBy(formula=Censored~SiteName+parameter+Date,
                        id=~LawaSiteID+SiteID+CouncilSiteID+Agency+Region+SWQLanduse+
                          SWQAltitude+SWQFrequencyLast5+Catchment,#+TermReach+LAWA_CATCH+CATCH_LBL+Comment+SWQuality,
                        data=wqdata_A, 
                        FUN=any,keep.names=T)
  wqdata_ct <- summaryBy(formula=CenType~SiteName+parameter+Date,
                         id=~LawaSiteID+SiteID+CouncilSiteID+Agency+Region+SWQLanduse+
                           SWQAltitude+SWQFrequencyLast5+Catchment,#+TermReach+LAWA_CATCH+CATCH_LBL+Comment+SWQuality,
                         data=wqdata_A, 
                         FUN=function(x)paste(unique(x)),keep.names=T)
  wqdata_med$n=wqdata_n$Value
  wqdata_med$Censored=wqdata_c$Censored
  if(!is.null(wqdata_ct$CenType)){
    wqdata_med$CenType=wqdata_ct$CenType
  }else{
    wqdata_med$CenType="FALSE"
  }
  rm(wqdata_n)
  rm(wqdata_c,wqdata_ct)
  gc()
  # Building dataframe to save at the end of this step 
  if(i==1){
    lawadata <- wqdata_med
    # lawadata_q <- wqdata
  } else {
    lawadata <- rbind(lawadata,wqdata_med)
    # lawadata_q <- rbind(lawadata_q,wqdata)
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
    cat("LAWA Water Quality State Analysis\t",wqparam[i],'\n')
    cat("LAWA Water QUality State Analysis\nCalculating reference quartiles\n")
    
    state <- c("Site","Catchment","Region","NZ")
    level <- c("LandUseAltitude","LandUse","Altitude","None")
    
    sa11 <- StateAnalysis(df = wqdata_A,type = state[1],level = level[1])
    
    sa21 <- StateAnalysis(wqdata_A,state[2],level[1])
    sa22 <- StateAnalysis(wqdata_A,state[2],level[2])
    sa23 <- StateAnalysis(wqdata_A,state[2],level[3])
    sa24 <- StateAnalysis(wqdata_A,state[2],level[4])
    
    sa31 <- StateAnalysis(wqdata_A,state[3],level[1])
    sa32 <- StateAnalysis(wqdata_A,state[3],level[2])
    sa33 <- StateAnalysis(wqdata_A,state[3],level[3])
    sa34 <- StateAnalysis(wqdata_A,state[3],level[4])
    
    sa41 <- StateAnalysis(wqdata_A,state[4],level[1])
    sa42 <- StateAnalysis(wqdata_A,state[4],level[2])
    sa43 <- StateAnalysis(wqdata_A,state[4],level[3])
    sa44 <- StateAnalysis(wqdata_A,state[4],level[4])
    
    cat("LAWA Water QUality State Analysis\n","Binding ",wqparam[i]," data together for measurement\n")
    
    if(i==1){
      sa <- rbind(sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)
    } else {
      sa <- rbind(sa,sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)
    }
  }
  rm(wqdata_A)
  rm(sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)
}

rm(state)

# Housekeeping
# - Saving the lawadata table  USED in NOF calculations
save(lawadata,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/lawadata",StartYear,"-",EndYear,".RData"))
#load(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/lawadata",StartYear,"-",EndYear,".RData"),verbose=T)
# save(lawadata_q,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/lawadata_q_",StartYear,"-",EndYear,".RData"))

# State Analysis output contains quantiles for each parameter by site.
# - Rename data.frame headings
names(sa) <-  c("AltitudeGroup","LanduseGroup","Region","Catchment","SiteName","LAWAID","Parameter",
                "Q0","Q25","Q50","Q75","Q100","N","Scope")

# filter sa to remove any LAWAIDS that are NA
sa <- sa[!is.na(sa$LAWAID),]
write.csv(sa,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/sa",StartYear,"-",EndYear,".csv"),row.names = F)
# sa <- read.csv(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/sa",StartYear,"-",EndYear,".csv"),stringsAsFactors = F)

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


#Now as of 2018 we're only doing it at site
scope <- c("Site","Catchment","Region") 
i=1
# for(i in 1:3){
#  comparision = 1,2,3,4
#              1 = All -                 AltitudeGroup=="All"    & LanduseGroup=="All"   & Scope=="NZ"
#              2 = Altitude -            AltitudeGroup==altitude & LanduseGroup=="All"   & Scope=="NZ"
#              3 = Land use -            AltitudeGroup=="All"    & LanduseGroup==landuse & Scope=="NZ"
#              4 = Altitude & Land use - AltitudeGroup==altitude & LanduseGroup==landuse & Scope=="NZ"

ss1 <-   StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = "",landuse = "",wqparam = wqparam,comparison=1)
ss21 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = "upland",landuse = "",wqparam,comparison=2)
ss22 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = "lowland",landuse = "",wqparam,comparison=2)
ss31 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = "",landuse = "rural",wqparam,comparison=3)
ss32 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = "",landuse = "forest",wqparam,comparison=3)
ss33 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = "",landuse = "urban",wqparam,comparison=3)
ss411 <- StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = "upland",landuse = "rural",wqparam,comparison=4)
ss412 <- StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = "upland",landuse = "forest",wqparam,comparison=4)
ss413 <- StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = "upland",landuse = "urban",wqparam = wqparam,comparison=4)
ss421 <- StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = "lowland",landuse = "rural", wqparam = wqparam,comparison=4)
ss422 <- StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = "lowland",landuse = "forest",wqparam = wqparam,comparison=4)
ss423 <- StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = "lowland",landuse = "urban", wqparam = wqparam,comparison=4)

# if(i==1){
  # ss <- rbind(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412)
  ss <- rbind(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)
# } else{
#   ss <- rbind(ss,ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)
# }
rm(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)
# }
ss=ss[order(ss$LAWAID),]

write.csv(ss,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/state",StartYear,"-",EndYear,".csv"),row.names = F)
# ss=read.csv(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/state",StartYear,"-",EndYear,".csv"),stringsAsFactors = F)
cat("LAWA Water QUality State Analysis\nCompleted assigning State Scores\n")

ss_csv <- read.csv(file=paste("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/state",StartYear,"-",EndYear,".csv",sep=""),header=TRUE,sep=",",quote = "\"")

# ss.1 <- subset(ss_csv,Scope=="Region")
# ss.1$Location <- ss.1$Region
# ss.2 <- subset(ss_csv,Scope=="Catchment")
# ss.2$Location <- ss.2$Catchment
ss.3 <- subset(ss_csv,Scope=="Site")
ss.3$Location <- ss.3$LAWAID

if(exists("ss.1")){
  ss.4 <- rbind.data.frame(ss.1,ss.2,ss.3)
# unique(ss.4$Location)
}else{
  ss.4 <- ss.3
}
ss.5 <- ss.4%>%select(Location,Parameter,AltitudeGroup,LanduseGroup,Q50,LAWAState,Region,Scope,StateGroup)
ss.5 <- ss.5%>%rename(Median=Q50)
# ss.5 <- ss.4[c(18,8,2,3,11,17,4,15,16)-1]  # Location, Parameter, Altitude, Landuse, Q50, LAWAState, Region, Scope, StateGroup
 
#The altitude and landgroup columns should reflect the grouping being used for comparison, and not the characteristics of the site. 
#Allowing the appearance of 'all' as a landuse or altitude
altGroups=unique(ss.5$AltitudeGroup)
luGroups=unique(ss.5$LanduseGroup)
ss.5$StateGroupB=gsub(pattern = 'site\\|',replacement = "",x = ss.5$StateGroup)
ss5sg=strsplit(ss.5$StateGroupB,'\\|')
replaceAlt = rep('all',dim(ss.5)[1])
replaceAlt[which(unlist(lapply(ss5sg,FUN=function(x)any('lowland'%in%x))))]='lowland'
replaceAlt[which(unlist(lapply(ss5sg,FUN=function(x)any('upland'%in%x))))]='upland'

replaceLU = rep('all',dim(ss.5)[1])
replaceLU[which(unlist(lapply(ss5sg,FUN=function(x)any('forest'%in%x))))]='forest'
replaceLU[which(unlist(lapply(ss5sg,FUN=function(x)any('rural'%in%x))))]='rural'
replaceLU[which(unlist(lapply(ss5sg,FUN=function(x)any('urban'%in%x))))]='urban'

ss.5$AltitudeGroup=replaceAlt
ss.5$LanduseGroup=replaceLU
ss.5 <- ss.5%>%select(-StateGroupB)
rm(replaceAlt,replaceLU)

pseudo.titlecase = function(str)
{
  substr(str, 1, 1) = toupper(substr(str, 1, 1))
  return(str)
}
ss.5$AltitudeGroup=pseudo.titlecase(ss.5$AltitudeGroup)
ss.5$LanduseGroup=pseudo.titlecase(ss.5$LanduseGroup)
ss.5$StateGroup <- as.character(ss.5$StateGroup)%>%lapply(FUN=function(x){strsplit(x,'\\|')%>%
    unlist%>%pseudo.titlecase%>%paste0(collapse='|')})%>%unlist

write.csv(ss.5,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/RiverWQ_STATE_",
                           StartYear,"-",EndYear,"forITE",format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),row.names = F)
ss.5 <- read.csv(tail(dir(path = paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/"),
                          pattern = 'RiverWQ_STATE_',full.names = T))[1],stringsAsFactors = F)

# lawadata_without_niwa <- subset(lawadata,Agency!="NIWA")
# lawadata_q_without_niwa <- subset(lawadata_q,Agency!="NIWA")

# write.csv(lawadata_without_niwa,"h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/LAWA_DATA.csv",row.names = F)
# write.csv(lawadata_without_niwa,"h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/LAWA_RAW_DATA.csv",row.names = F)


