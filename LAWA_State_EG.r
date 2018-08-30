rm(list=ls())

StartYear <- 2013
EndYear <- 2017
source("h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")

loadLatestCSV <- function(council=NULL,maxHistory=100){
  stepBack=0
  while(stepBack<maxHistory){
    if(dir.exists(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                         format(Sys.Date()-stepBack,"%Y-%m-%d"),'/'))){
      if(file.exists(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                            format(Sys.Date()-stepBack,"%Y-%m-%d"),'/',council,'.csv'))){
        cat('\nloading',council,'from',stepBack,'days ago,',format(Sys.Date()-stepBack,"%Y-%m-%d"),'\t')
        return(read.csv(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                               format(Sys.Date()-stepBack,"%Y-%m-%d"),'/',council,'.csv'),
                        stringsAsFactors = F))
      }
    }
    stepBack=stepBack+1
  }
  cat('\n',council,"Not found")
  return(NULL)
}



#Load latest siteTable1, which is intersected with Catchment
#NOTE  THIS IS NOT JUST THE SITE  TABLE!
stbl=tail(dir(path="h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",pattern="LAWA_Site_Table1.csv",recursive=T,full.names=T),1)
catSiteTable <- read.csv(stbl,stringsAsFactors = F)
rm(stbl)
catSiteTable$SWQLanduse[catSiteTable$SWQLanduse=="Native"|catSiteTable$SWQLanduse=="Exotic"|catSiteTable$SWQLanduse=="Natural"] <- "Forest"
catSiteTable$SiteID[catSiteTable$SiteID=="karapiro stm at hickey rd bridge - cambridge"] <- "karapiro stm at hickey rd bridge"
catSiteTable$SiteID=trimws(catSiteTable$SiteID)
catSiteTable$CouncilSiteID=trimws(catSiteTable$CouncilSiteID)
catSiteTable$LawaSiteID=trimws(catSiteTable$LawaSiteID)
catSiteTable$SWQAltitude=tolower(catSiteTable$SWQAltitude)
catSiteTable$SWQLanduse=tolower(catSiteTable$SWQLanduse)
catSiteTable$SWQFrequencyAll=tolower(catSiteTable$SWQFrequencyAll)
catSiteTable$SWQFrequencyLast5=tolower(catSiteTable$SWQFrequencyLast5)
catSiteTable$Region=tolower(catSiteTable$Region)
catSiteTable$Agency=tolower(catSiteTable$Agency)

save(catSiteTable,file="h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawa_sitetable.RData")


for(council in c("ac","boprc","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  mfl=loadLatestCSV(council)
  while(grepl(pattern = '^X',x = names(mfl)[1])){
    mfl=mfl[,-1]
  }
  if(names(mfl)[14]=='SWQFrequencyAll'){
    names(mfl)[14] <- 'Frequency'
  }
  if('accessDate'%in%names(mfl)){
    # cat('\t it has access date')
    # mfl=mfl[,-which(names(mfl)=='accessDate')]
  }
  if(sum(is.na(mfl$Agency))>0){
    cat(sum(is.na(mfl$Agency)),'non agency')
    cat('\t',paste(collapse=', ',unique(mfl$SiteName[mfl$Agency==''|is.na(mfl$Agency)])))
  }
  if(sum(!tolower(mfl$CouncilSiteID)%in%tolower(catSiteTable$CouncilSiteID))>0){
    cat(sum(!unique(tolower(mfl$CouncilSiteID))%in%catSiteTable$CouncilSiteID),'not in site table\n')
  }
  eval(parse(text=paste0(council,'=mfl')))
  rm(mfl)
}

niwa=read.csv('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/NIWAwqData.csv',stringsAsFactors=F)
if('SWQFrequencyAll'%in%names(niwa)){
  names(niwa)[which(names(niwa)=='SWQFrequencyAll')] <- "Frequency"
}
if(!'accessDate'%in%names(niwa)){
  niwa$accessDate = format(file.info("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/NIWAwqData.xml")$mtime,"%d-%b-%Y")
}

niwa <- niwa%>%select(names(ncc))  #Rearrange niwa columsn to match order of others

niwa$Value[niwa$parameter%in%c("NH4","DRP","TN","TP","TON")]=niwa$Value[niwa$parameter%in%c("NH4","DRP","TN","TP","TON")]/1000

# store=niwa$SiteID
# niwa$SiteID=niwa$SiteName
# niwa$SiteName=store
# rm(store)
# *** wqdata$SiteName[wqdata$Agency=="NIWA"]=wqdata$CouncilSiteID[wqdata$Agency=="NIWA"]

#The one site not found in BOP is found in NIWA data, so that's cool.
# theseBOP=grep(boprc$SiteName,pattern = 'Motu at waitangirua',ignore.case = T)
# thisNiwa=grep(niwa$CouncilSiteID,pattern = "Motu at Waitangirua",ignore.case = T)[1]
# boprc$SiteID[theseBOP]=niwa$CouncilSiteID[thisNiwa]
# boprc$CouncilSiteID[theseBOP]=niwa$CouncilSiteID[thisNiwa]
# boprc$SWQuality[theseBOP]='yes'
# boprc$SWQAltitude[theseBOP]=niwa$SWQAltitude[thisNiwa]
# boprc$SWQLanduse[theseBOP]=niwa$SWQLanduse[thisNiwa]
# boprc$Region[theseBOP]="bay of plenty"
# rm(theseBOP,thisNiwa)





wqdata=rbind.data.frame(boprc,ecan,es,gdc,gwrc,hbrc,hrc,mdc,ncc,nrc,orc,tdc,trc,wcrc,wrc,niwa,make.row.names = F)
wqdata$SiteID=trimws(wqdata$SiteID)
wqdata$CouncilSiteID=trimws(wqdata$CouncilSiteID)
wqdata$LawaSiteID=trimws(wqdata$LawaSiteID)
wqdata$SWQAltitude=tolower(wqdata$SWQAltitude)
wqdata$SWQLanduse=tolower(wqdata$SWQLanduse)
wqdata$Frequency=tolower(wqdata$Frequency)
wqdata$SWQFrequencyLast5=tolower(wqdata$SWQFrequencyLast5)
wqdata$Region=tolower(wqdata$Region)
wqdata$Agency=tolower(wqdata$Agency)

try(dir.create(paste0("H:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"))))
write.csv(wqdata,paste0("H:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/AllCouncils.csv"),row.names = F)
rm(boprc,ecan,es,gdc,gwrc,hbrc,hrc,mdc,ncc,nrc,orc,tdc,trc,wcrc,wrc,council)

#Load the latest made (might need to check it works nicely across month folders) 
if(!exists('wqdata')){
  acwqdata=tail(dir(path = "H:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",pattern = "AllCouncils.csv",recursive = T,full.names = T),1)
  wqdata=read.csv(acwqdata,stringsAsFactors = F)
  rm(acwqdata)
}



#Audit plots to allow comparison between agencies - check units consistency etc
wqd=summaryBy(data=wqdata,formula=Value~LawaSiteID+parameter+Date,id=~Agency,FUN=median)
wqds=spread(wqd,parameter,Value.median)
params=unique(wqdata$parameter)
for(param in 1:length(params)){
  tiff(filename = paste0('h:/ericg/16666LAWA/2018/WaterQuality/QA/',names(wqds)[param+3],'.tif'),
       width = 15,height=12,units='in',res=300,compression='lzw',type='cairo')
  if(names(wqds)[param+3]!="PH"){
    plot(as.factor(wqds$Agency[wqds[,param+3]>0]),wqds[wqds[,param+3]>0,param+3],ylab=names(wqds)[param+3],log='y')
  }else{
    plot(as.factor(wqds$Agency),wqds[,param+3],ylab=names(wqds)[param+3])
  }
  if(names(dev.cur())=='tiff'){dev.off()}
}


#You know?  Should be the SiteName matches with CouncilSiteID.  Are those in these tables?
wqdata <- left_join(wqdata,catSiteTable,by="LawaSiteID",suffix=c("",".y"))


unique(wqdata$LawaSiteID[is.na(wqdata$SWQLanduse.y)])
# "NRWQN-00012" "ES-00010"    "EBOP-00223"  "NRWQN-00035" "ECAN-10028"  "LAWA-00669"




#These names were used to be applied onto the WQdata load
c("SiteName","Date","Value","Method","parameter",
  "Censored","CenType","ROS","iValues","converted_values","dflag",
  "i2Values","OriginalValues","X","LAWAID","ID",
  "UsedInLAWA","AltitudeGroup","LanduseGroup","FrequencyAll","Frequency",
  "Region","Agency","ISLAND","CatchID","CatchType",
  "NZREACH","Catchment","Comments","LawaCatchm","CatchLbl")

names(wqdata)
[1] "SiteName"          "Date"              "Value"             "Method"            "parameter"        
[6] "Censored"          "CenType"           "CouncilSiteID"     "LawaSiteID"        "SiteID"           
[11] "SWQuality"         "SWQAltitude"       "SWQLanduse"        "Frequency"         "SWQFrequencyLast5"
[16] "Region"            "Agency"            "Lat"               "Long"              "X"                
[21] "SWQFrequencyAll"   "ISLAND"            "CatchID"           "CatchType"         
"TermReach"         "LAWA_CATCH"        "Comment"           "SOE_FW_RIV"        "CATCH_LBL"        

wqparam <- c("BDISC","TURB","NH4","PH","TON","TN","DRP","TP","ECOLI") 

suppressWarnings(rm(wqdata_A,wqdata_med,wqdata_n,lawadata,lawadata_q))
for(i in 1:length(wqparam)){
  
  wqdata_A = wqdata[wqdata$parameter==tolower(wqparam[i]),]
  # ADD tracking of n, to see how many values the medians were calculated from
  wqdata_med <- summaryBy(formula=Value~SiteName+parameter+Date,
                          id=~LawaSiteID+SiteID+CouncilSiteID+Agency+Region+TermReach+
                            SWQLanduse+SWQAltitude+LAWA_CATCH+CATCH_LBL+SWQFrequencyLast5+
                            Comment+SWQuality,
                          data=wqdata_A, 
                          FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
  wqdata_med$LAWAID=wqdata_med$LawaSiteID
  wqdata_med$LanduseGroup=wqdata_med$SWQLanduse
  wqdata_med$AltitudeGroup=wqdata_med$SWQAltitude
  wqdata_med$Catchment=wqdata_med$LAWA_CATCH
  wqdata_med$Frequency=wqdata_med$SWQFrequencyLast5

    wqdata_n <- summaryBy(formula=Value~SiteName+parameter+Date,
                        id=~LawaSiteID+SiteID+CouncilSiteID+Agency+Region+TermReach+
                          SWQLanduse+SWQAltitude+LAWA_CATCH+CATCH_LBL+SWQFrequencyLast5+
                          Comment+SWQuality,
                        data=wqdata_A, 
                        FUN=length,keep.names=T)
  rm(wqdata_A)
  wqdata_med$n=wqdata_n$Value
  rm(wqdata_n)
  gc()
  # Building dataframe to save at the end of this step 
  if(i==1){
    #lawadata <- wqdata
    lawadata <- wqdata_med
    lawadata_q <- wqdata
  } else {
    #lawadata <- rbind(lawadata,wqdata)
    lawadata <- rbind(lawadata,wqdata_med)
    lawadata_q <- rbind(lawadata_q,wqdata)
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
    
    sa11 <- StateAnalysis(df = wqdata_med,type = state[1],level = level[1])
    
    sa21 <- StateAnalysis(wqdata_med,state[2],level[1])
    sa22 <- StateAnalysis(wqdata_med,state[2],level[2])
    sa23 <- StateAnalysis(wqdata_med,state[2],level[3])
    sa24 <- StateAnalysis(wqdata_med,state[2],level[4])
    
    sa31 <- StateAnalysis(wqdata_med,state[3],level[1])
    sa32 <- StateAnalysis(wqdata_med,state[3],level[2])
    sa33 <- StateAnalysis(wqdata_med,state[3],level[3])
    sa34 <- StateAnalysis(wqdata_med,state[3],level[4])
    
    sa41 <- StateAnalysis(wqdata_med,state[4],level[1])
    sa42 <- StateAnalysis(wqdata_med,state[4],level[2])
    sa43 <- StateAnalysis(wqdata_med,state[4],level[3])
    sa44 <- StateAnalysis(wqdata_med,state[4],level[4])
    
    cat("LAWA Water QUality State Analysis\n","Binding ",wqparam[i]," data together for measurement\n")
    
    if(i==1){
      sa <- rbind(sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)
    } else {
      sa <- rbind(sa,sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)
    }
  }
rm(sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)
}
rm(state)

# Housekeeping
# - Saving the lawadata table  USED in NOF calculations
save(lawadata,file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawadata",StartYear,"-",EndYear,".RData",sep=""))
save(lawadata_q,file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawadata_q_",StartYear,"-",EndYear,".RData",sep=""))

# State Analysis output contains quantiles for each parameter by site.
# - Rename data.frame headings
names(sa) <-  c("AltitudeGroup","LanduseGroup","Region","Catchment","SiteName","LAWAID","Parameter","Q0","Q25","Q50","Q75","Q100","N","Scope")

# filter sa to remove any LAWAIDS that are NA
sa <- sa[!is.na(sa$LAWAID),]  #drops 45
write.csv(sa,file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/sa",StartYear,"-",EndYear,".csv",sep=""),row.names = F)


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
i=1
# for(i in 1:3){

ss1 <-   StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower(""),landuse = tolower(""),wqparam = wqparam,comparison=1)
ss21 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower("Upland"),landuse = tolower(""),wqparam,comparison=2)
ss22 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower("Lowland"),landuse = tolower(""),wqparam,comparison=2)
ss31 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower(""),landuse = tolower("Rural"),wqparam,comparison=3)
ss32 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower(""),landuse = tolower("Forest"),wqparam,comparison=3)
ss33 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower(""),landuse = tolower("Urban"),wqparam,comparison=3)
ss411 <- StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower("Upland"),landuse = tolower("Rural"),wqparam,comparison=4)
ss412 <- StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower("Upland"),landuse = "forest",wqparam,comparison=4)

# The following line will fail if there are no sites with Upland Urban classification
# Need to put a test into the StateScore function to return an empty dataframe

# RE-ENABLE THIS ONCE BOPRC data available
if(0){
  ss413 <- StateScore(df = sa,scopeIn=scope[i],altitude = "upland",landuse = "urban",wqparam = wqparam,comparison=4)
  ss421 <- StateScore(sa,scope[i],"lowland","rural",wqparam,comparison=4)
  ss422 <- StateScore(sa,scope[i],"lowland","forest",wqparam,comparison=4)
  ss423 <- StateScore(sa,scope[i],"lowland","urban",wqparam,comparison=4)
}

if(i==1){
  ss <- rbind(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)
} else{
  ss <- rbind(ss,ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)
}
rm(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)
# }


write.csv(ss,file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/state",StartYear,"-",EndYear,".csv",sep=""),row.names = F)



cat("LAWA Water QUality State Analysis\nCompleted assigning State Scores\n")

ss_csv <- read.csv(file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/state",StartYear,"-",EndYear,".csv",sep=""),header=TRUE,sep=",",quote = "\"")

ss.1 <- subset(ss_csv,Scope=="Region")
ss.1$Location <- ss.1$Region
ss.2 <- subset(ss_csv,Scope=="Catchment")
ss.2$Location <- ss.2$Catchment
ss.3 <- subset(ss_csv,Scope=="Site")
ss.3$Location <- ss.3$LAWAID

ss.4 <- rbind.data.frame(ss.1,ss.2,ss.3)
# unique(ss.4$Location)

ss.5 <- ss.4[c(18,8,2,3,11,17,4,15,16)-1]  # Location, Parameter, Altitude, Landuse, Q50, LAWAState, Region, Scope, StateGroup



write.csv(ss.5,file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/LAWA_STATE_FINAL_",StartYear,"-",EndYear,".csv",sep=""),row.names = F)
# lawadata_without_niwa <- subset(lawadata,Agency!="NIWA")
# lawadata_q_without_niwa <- subset(lawadata_q,Agency!="NIWA")

# write.csv(lawadata_without_niwa,"h:/ericg/16666LAWA/2018/WaterQuality/ROutput/LAWA_DATA.csv",row.names = F)
# write.csv(lawadata_without_niwa,"h:/ericg/16666LAWA/2018/WaterQuality/ROutput/LAWA_RAW_DATA.csv",row.names = F)


