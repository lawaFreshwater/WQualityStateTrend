rm(list=ls())

StartYear <- 2013
EndYear <- 2017

loadLatestCSV <- function(council=NULL,maxHistory=100){
  stepBack=0
  while(stepBack<maxHistory){
    if(dir.exists(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                         format(Sys.Date()-stepBack,"%Y-%m-%d"),'/'))){
      if(file.exists(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                            format(Sys.Date()-stepBack,"%Y-%m-%d"),'/',council,'.csv'))){
        cat('loading',council,'from',stepBack,'days ago,',format(Sys.Date()-stepBack,"%Y-%m-%d"),'\n')
        return(read.csv(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                               format(Sys.Date()-stepBack,"%Y-%m-%d"),'/',council,'.csv'),
                        stringsAsFactors = F))
      }
    }
    stepBack=stepBack+1
  }
  return(NULL)
}
source("h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")

for(council in c("ac","boprc","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  mfl=loadLatestCSV(council)
  while(grepl(pattern = '^X',x = names(mfl)[1])){
    mfl=mfl[,-1]
  }
  if(names(mfl)[14]=='SWQFrequencyAll'){
    names(mfl)[14] <- 'Frequency'
  }
  if('accessDate'%in%names(mfl)){
    mfl=mfl[,-which(names(mfl)=='accessDate')]
  }
  cat('\n',sum(is.na(mfl$Agency)))
  if(sum(is.na(mfl$Agency))>0){
    cat(council)
    browser()
  }
  
  
  #Could switch namings here, to maximise match with siteTable, with COuncilSiteID longest name?
  
  
  eval(parse(text=paste0(council,'=mfl')))
  rm(mfl)
}
#boprc, gdc were missing some agency
niwa=read.csv('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/NIWAwqDataB.csv')


wqdata=rbind.data.frame(boprc,ecan,es,gdc,gwrc,hbrc,hrc,mdc,ncc,nrc,orc,tdc,trc,wcrc,wrc,niwa,make.row.names = F)
rm(boprc,ecan,es,gdc,gwrc,hbrc,hrc,mdc,ncc,nrc,orc,tdc,trc,wcrc,wrc,niwa)


wqdata$SiteID=trimws(wqdata$SiteID)
wqdata$CouncilSiteID=trimws(wqdata$CouncilSiteID)
wqdata$LawaSiteID=trimws(wqdata$LawaSiteID)

params=unique(wqdata$parameter)
agencies=unique(wqdata$Agency)
for(param in 1:length(params)){
  tiff(filename = paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),'%Y-%m-%d'),'/',params[param],'.tif'),
       width = 12,height=15,units='in',res=300,compression='lzw',type='cairo')
  if(param%in%c(1,2,6,7)){
    par(mfrow=c(4,5))
  }else{
    par(mfrow=c(4,4))
  }
  for(age in 1:length(agencies)){
    if(sum(!is.na(wqdata$Value[wqdata$Agency==agencies[age]&wqdata$parameter==params[param]]))>2){
      plot(density(na.rm=T,wqdata$Value[wqdata$Agency==agencies[age]&wqdata$parameter==params[param]]),
           main=paste(params[param],agencies[age]))
      }
  }
  if(names(dev.cur())=='tiff'){dev.off()}
}

write.csv(wqdata,paste0("H:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/AllCouncils.csv"),row.names = F)

wqdata=read.csv(paste0("H:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/AllCouncils.csv"),stringsAsFactors = F)
# rm(boprc,ecan,es,gdc,gwrc,hbrc,hrc,mdc,ncc,nrc,orc,tdc,trc,wcrc,wrc,council)


#PrepWFS does not use "SiteName", it gets "CouncilSiteID","LawaSiteID","SiteID" as variables from WFS, into siteTable

#CouncilSiteID   }                             In GDC, matches SiteName
#LawaSiteID      }    in the wfs xml
#SiteID          }                             In GDC, matches SiteName

#SiteName             In the SOS XML, saved into local XML

# XML2CSV tries to match SiteName to CouncilSiteID, the LawaSiteID, then SiteID.

#where does siteName get into our CSV files?  It's what's in the XML from SOS.  The others come in by matched in siteTable
#So don't change the siteName, that's what's come from SOS.  We could switch up CouncilSiteID and SiteID until they match the SiteName

#For NIWA, we'll overwrite CouncilSITEID onto SIteName
wqdata$SiteName[wqdata$Agency=="NIWA"]=wqdata$CouncilSiteID[wqdata$Agency=="NIWA"]

table(tolower(wqdata$SiteName)==tolower(wqdata$SiteID))         #half half
table(tolower(wqdata$SiteName)==tolower(wqdata$CouncilSiteID))  #most do  <--------------  but most NIWA didnt
table(tolower(wqdata$SiteName)==tolower(wqdata$LawaSiteID))     #none
table(tolower(wqdata$SiteID)==  tolower(wqdata$CouncilSiteID))  #one third do

#Switch in the WQDATA table, the nonmatching CouncilSiteID and SiteID
toSpin=which(tolower(wqdata$SiteName)!=tolower(wqdata$CouncilSiteID))
table(tolower(wqdata$SiteName[toSpin])==tolower(wqdata$SiteID[toSpin]))  #The ones that dont match on CouncilSiteID do match on siteID
storchenegg = wqdata$SiteID[toSpin]                           #So Switch the SiteID and CouncilSiteID for these ones
wqdata$SiteID[toSpin] = wqdata$CouncilSiteID[toSpin]
wqdata$CouncilSiteID[toSpin] = storchenegg
table(wqdata$Agency[toSpin])  #All were boprc
rm(toSpin)

table(tolower(wqdata$SiteName)==tolower(wqdata$CouncilSiteID))  #all do  <--------------  




#Load latest siteTable1, which is intersected with Catchment
#NOTE  THIS IS NOT JUST THE SITE  TABLE!
stepBack=0
while(stepBack<20){
  if(dir.exists(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date()-stepBack,"%Y-%m-%d")))){
    if(file.exists(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date()-stepBack,"%Y-%m-%d"),"/LAWA_Site_Table1.csv"))){
      catSiteTable <- read.csv(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date()-stepBack,"%Y-%m-%d"),"/LAWA_Site_Table1.csv"),stringsAsFactors=FALSE) #This is intersected with catchment info
    while(grepl(pattern = '^X',x = names(catSiteTable)[1])){
      catSiteTable=catSiteTable[,-1]
    }
    
    cat("loading catSiteTable with Catchment info from",stepBack,'days ago\n')
    stepBack=1000
    break
  }
  }
  stepBack=stepBack+1
}
rm(stepBack)

#Some LawaIDs are missing for boprc and gdc
table(wqdata$Agency[is.na(wqdata$LawaSiteID)])

table(tolower(catSiteTable$SiteID)==tolower(catSiteTable$CouncilSiteID))  #two thirds no
table(catSiteTable$Agency[tolower(catSiteTable$SiteID)==tolower(catSiteTable$CouncilSiteID)]) #evenly spread amoung councils
table(catSiteTable$Agency[tolower(catSiteTable$SiteID)!=tolower(catSiteTable$CouncilSiteID)]) #evenly spread

#These names used in catSiteTable, mostly CouncilSiteID should match against SOS data's SiteName
#We havent touched WQData's siteName, but we have switched some of their SiteID and CouncilSiteIDs, and should do the same in the catSiteTable
#"SiteID"            "CouncilSiteID"     "LawaSiteID" 
table(tolower(unique(tolower(wqdata$LawaSiteID)))%in%tolower(catSiteTable$LawaSiteID)) #7 lawaSiteIDs sitenames aren't in catSiteTable$CouncilSiteID
# table(tolower(unique(wqdata$SiteID))%in%tolower(catSiteTable$SiteID)) #334 wqdata sitenames aren't in catSiteTable$CouncilSiteID
# table(tolower(unique(wqdata$SiteName))%in%tolower(catSiteTable$CouncilSiteID)) #334 wqdata sitenames aren't in catSiteTable$CouncilSiteID
# table(tolower(unique(wqdata$CouncilSiteID))%in%tolower(catSiteTable$CouncilSiteID)) #334 wqdata sitenames aren't in catSiteTable$CouncilSiteID
# table(unique(tolower(wqdata$SiteName))[which(!tolower(unique(wqdata$SiteName))%in%tolower(catSiteTable$CouncilSiteID))]%in%tolower(catSiteTable$SiteID)) #326 of those are found in the SiteID instead
# 
# these=match(unique(storchenegg),catSiteTable$)
# unique(catSiteTable$Agency[these]) #and again, it's all BOPRC.
# catSiteTable$SiteID[these] <- catSiteTable$CouncilSiteID[these]
# catSiteTable$CouncilSiteID[these] <- unique(storchenegg)
# rm(storchenegg)

table(tolower(unique(wqdata$SiteName))%in%tolower(catSiteTable$CouncilSiteID)) #334 wqData$SiteNames aren't found in the catSiteTable$CouncilSiteID
table(unique(tolower(wqdata$SiteName))[which(!tolower(unique(wqdata$SiteName))%in%tolower(catSiteTable$CouncilSiteID))]%in%tolower(catSiteTable$SiteID)) #326 of those are found in the SiteID instead
missings <- unique(wqdata$SiteName)[!tolower(unique(wqdata$SiteName))%in%tolower(catSiteTable$CouncilSiteID)]
unique(wqdata$Agency)[!tolower(unique(wqdata$SiteName))%in%tolower(catSiteTable$CouncilSiteID)] #lots of na
unique(wqdata$Region)[!tolower(unique(wqdata$SiteName))%in%tolower(catSiteTable$CouncilSiteID)] #lots of na
missings[missings%in%tolower(catSiteTable$SiteID)]  #These are WQ data site names that aren't in the CouncilSiteID, but are in the SiteID
toSpin <- which(tolower(catSiteTable$SiteID)%in%missings[missings%in%tolower(catSiteTable$SiteID)])

store <- catSiteTable$CouncilSiteID[toSpin]
catSiteTable$CouncilSiteID[toSpin] <- catSiteTable$SiteID[toSpin]
catSiteTable$SiteID[toSpin] <- store
rm(store,toSpin)

table(unique(tolower(wqdata$SiteName))[which(!tolower(unique(wqdata$SiteName))%in%tolower(catSiteTable$CouncilSiteID))]%in%tolower(catSiteTable$SiteID)) #8 just arent found at all 
missings <- unique(tolower(wqdata$SiteName))[which(!tolower(unique(wqdata$SiteName))%in%tolower(catSiteTable$CouncilSiteID))]
# "motu at waitangirua"           "tarawera at sh30 bridge"       "waimapu 100m d/s sh29"         "avon02"                       
# "out01"                         "sq33476"                       "waiomoko river at sh35 bridge" "101753"   
for(mm in missings){
  cat(grep(pattern=mm,x = tolower(catSiteTable$SiteID),ignore.case = T),'\t')
  cat(grep(pattern=mm,x = tolower(catSiteTable$CouncilSiteID),ignore.case = T),'\t')
  cat(grep(pattern=mm,x = tolower(catSiteTable$LawaSiteID),ignore.case = T),'\t')
}

#Where are they
table(wqdata$Agency[wqdata$SiteName%in%missings])
table(wqdata$SiteName[wqdata$SiteName%in%missings&!is.na(wqdata$Agency)])
table(wqdata$SiteName[wqdata$SiteName%in%missings&is.na(wqdata$Agency)])



table(tolower(unique(wqdata$CouncilSiteID))%in%tolower(catSiteTable$SiteID))
table(tolower(unique(wqdata$LawaSiteID))%in%tolower(catSiteTable$SiteID))
table(tolower(unique(wqdata$SiteID))%in%tolower(catSiteTable$SiteID))  #5 missing
table(tolower(unique(wqdata$SiteID))%in%tolower(catSiteTable$CouncilSiteID))  

#Some data table siteIDs are not in catSiteTable siteID
which(!tolower(unique(wqdata$SiteID))%in%tolower(catSiteTable$SiteID))
missingIDs <- unique(wqdata$SiteID)[which(!tolower(unique(wqdata$SiteID))%in%tolower(catSiteTable$SiteID))]
#  [1] NA                                 "Avon River at Bridge Street"      "Linwood Canal/City Outfall Drain"
 # "Lyell Creek above mouth"          "Wairua at Purua"    
table(wqdata$Agency[which(!tolower(wqdata$SiteID)%in%tolower(catSiteTable$SiteID))])

tolower(missingIDs)%in%tolower(catSiteTable$CouncilSiteID)
grep('purua',catSiteTable$SiteID,ignore.case = T)  #451
grep('purua',catSiteTable$CouncilSiteID,ignore.case = T) #451
grep('purua',catSiteTable$LawaSiteID,ignore.case = T)

# Ah.  The bulk of them are Auckland council.
#Drop them
cat("WARNING,  DROPPING ALL AUCKLAND COUNCIL DATA AT THE MOMENT")
wqdata=wqdata[-which(wqdata$SiteName%in%missings),]


save(wqdata,file = paste0("H:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/WQDataCompiled.rData"))




catSiteTable$SWQLanduse[catSiteTable$SWQLanduse=="Native"|catSiteTable$SWQLanduse=="Exotic"|catSiteTable$SWQLanduse=="Natural"] <- "Forest"

wqdata <- left_join(wqdata,catSiteTable,by="LawaSiteID",suffix=c("",".y"))


CHECK SUCCESS OF MERGE

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

allSiteIDs=unique(catSiteTable$SiteID)

for(i in 1:length(wqparam)){
  
  wqdata_A = wqdata[wqdata$parameter==wqparam[i],]
  
  wqdata_med <- summaryBy(formula=Value~SiteName+parameter+Date,
                          id=~LawaSiteID+SiteID+CouncilSiteID+Agency+Region+TermReach+
                            SWQLanduse+SWQAltitude+LAWA_CATCH+CATCH_LBL+SWQFrequencyLast5+
                            Comment+SWQuality,
                          data=wqdata_A, 
                          FUN=c(quantile), prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
  wqdata_med$LAWAID=wqdata_med$LawaSiteID
  wqdata_med$LanduseGroup=wqdata_med$SWQLanduse
  wqdata_med$AltitudeGroup=wqdata_med$SWQAltitude
  wqdata_med$Catchment=wqdata_med$LAWA_CATCH
  wqdata_med$Frequency=wqdata_med$SWQFrequencyLast5
    rm(wqdata_A)
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
}


# Housekeeping
# - Saving the lawadata table  USED in NOF calculations
save(lawadata,file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawadata",StartYear,"-",EndYear,".RData",sep=""))
save(lawadata_q,file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawadata_q_",StartYear,"-",EndYear,".RData",sep=""))
save(catSiteTable,file="h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawa_sitetable.RData")


# - Remove extraneous objects
rm(sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)

# State Analysis output contains quantiles for each parameter by site.
# - Rename data.frame headings
names(sa) <- c("AltitudeGroup","LanduseGroup","Region","Catchment","SiteName","LAWAID","Parameter","Q0","Q25","Q50","Q75","Q100","N","Scope")
# - Write data.frame to a csv file for inspection

# filter sa to remove any LAWAIDS that are NA
sa <- sa[!is.na(sa$LAWAID),]
write.csv(sa,file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/sa",StartYear,"-",EndYear,".csv",sep=""),row.names = F)

#sa<-read.csv(file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/sa",StartYear,"-",EndYear,".csv",sep=""),stringsAsFactors=FALSE)
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
i=1
# for(i in 1:3){
  
  ss1 <-   StateScore(df = sa,scope = tolower(scope[i]),altitude = tolower(""),landuse = tolower(""),wqparam,comparison=1)
  ss21 <-  StateScore(df = sa,scope = tolower(scope[i]),altitude = tolower("Upland"),landuse = tolower(""),wqparam,comparison=2)
  ss22 <-  StateScore(df = sa,scope = tolower(scope[i]),altitude = tolower("Lowland"),landuse = tolower(""),wqparam,comparison=2)
  ss31 <-  StateScore(df = sa,scope = tolower(scope[i]),altitude = tolower(""),landuse = tolower("Rural"),wqparam,comparison=3)
  ss32 <-  StateScore(df = sa,scope = tolower(scope[i]),altitude = tolower(""),landuse = tolower("Forest"),wqparam,comparison=3)
  ss33 <-  StateScore(df = sa,scope = tolower(scope[i]),altitude = tolower(""),landuse = tolower("Urban"),wqparam,comparison=3)
  ss411 <- StateScore(df = sa,scope = tolower(scope[i]),altitude = tolower("Upland"),landuse = tolower("Rural"),wqparam,comparison=4)
  ss412 <- StateScore(df = sa,scope = tolower(scope[i]),altitude = tolower("Upland"),landuse = "forest",wqparam,comparison=4)
  
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
  
# }

# Housekeeping
# - Remove extraneous objects
#rm(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss413,ss421,ss422,ss423)
rm(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)



write.csv(ss,file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/state",StartYear,"-",EndYear,".csv",sep=""),row.names = F)



cat("LAWA Water QUality State Analysis\nCompleted assigning State Scores\n")


print(Sys.time() - x)


ss_csv <- read.csv(file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/state",StartYear,"-",EndYear,".csv",sep=""),header=TRUE,sep=",",quote = "\"")

ss.1 <- subset(ss_csv,Scope=="Region")
ss.1$Location <- ss.1$Region
ss.2 <- subset(ss_csv,Scope=="Catchment")
ss.2$Location <- ss.2$Catchment
ss.3 <- subset(ss_csv,Scope=="Site")
ss.3$Location <- ss.3$LAWAID

ss.4 <- rbind.data.frame(ss.1,ss.2,ss.3)
unique(ss.4$Location)

ss.5 <- ss.4[c(18,8,2,3,11,17,4,15,16)-1]  # Location, Parameter, Altitude, Landuse, Q50, LAWAState, Region, Scope, StateGroup

write.csv(ss.5,file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/LAWA_STATE_FINAL_",StartYear,"-",EndYear,".csv",sep=""),row.names = F)
# lawadata_without_niwa <- subset(lawadata,Agency!="NIWA")
# lawadata_q_without_niwa <- subset(lawadata_q,Agency!="NIWA")

# write.csv(lawadata_without_niwa,"h:/ericg/16666LAWA/2018/WaterQuality/ROutput/LAWA_DATA.csv",row.names = F)
# write.csv(lawadata_without_niwa,"h:/ericg/16666LAWA/2018/WaterQuality/ROutput/LAWA_RAW_DATA.csv",row.names = F)


