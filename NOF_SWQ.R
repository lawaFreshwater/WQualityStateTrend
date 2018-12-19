#===================================================================================================
#  LAWA NATIONAL OBJECTIVES FRAMEWORK
#  Horizons Regional Council
#
#  4 September 2016
#
#  Creator: Kelvin Phan  2014
#
#  Updated by: Maree Patterson 2016
#              Sean Hodges
#             Eric Goodwin 2018 Cawthron Institute
#  Horizons Regional Council
#===================================================================================================

#Test does this need doing?
file.info("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOF_STATE_2018_Rounded_NAs.csv")$mtime<
  file.info(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/lawadata",StartYear,"-",EndYear,".RData"))$mtime


rm(list = ls())
library(tidyr)
ANALYSIS<-"NOF"
# Set working directory
od <- getwd()
wd <- "h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state"
setwd(wd)

source("scripts/WQualityStateTrend/NOF_Functions.R")
NOF_PERIOD <- 5 # years

## Load NOF Bands
NOFbandDefinitions <- read.csv("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/NOFbandDefinitions3.csv", header = TRUE, stringsAsFactors=FALSE)
# Band Median.Nitrate X95th.Percentile.Nitrate Median.Ammoniacal.N Max.Ammoniacal.N E..coli Ecoli95 EcoliRec540 EcoliRec260
# 1    A         x<=1.0                   x<=1.5             x<=0.03          x<=0.05  x<=130   <=540         x<5        x<20
# 2    B     x<1&x<=2.4             x>1.5&x<=3.5      x>0.03&x<=0.24    x>0.05&x<=0.4  x<=130 x<=1000  x>=5&x<=10 x>=20&x<=30
# 3    C   x>2.4&x<=6.9             x>3.5&x<=9.8       x>0.24&x<=1.3     x>0.4&x<=2.2  x<=130 x<=1200 x>=10&x<=20 x>=20&x<=34
# 4    D          x>6.9                    x>9.8              x>1.30            x>2.2   x>130  x>1200 x>=20&x<=30        x>34
# 5    E          x>Inf                    x>Inf               x>Inf            x>Inf   x>260  x>1200        x>30        x>50

# This is for setting the years you would like to undertake the assessment on for the National objectives Framework.
# Add more years using comma seperation as needed 
yr  <-c("2013", "2014","2015","2016","2017","Overall")
reps  <-length(yr)



#===================================================================================================
## Load LAWA Data
#Reference Dates
EndYear <- 2017
StartYear <- EndYear - NOF_PERIOD + 1

# loads lawadata dataframe  from LAWA_State.r  - has altered values from censoring, and calculated medians

load(file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/lawadata",StartYear,"-",EndYear,".RData"),verbose=T)

lawadata$parameter=toupper(lawadata$parameter)
# Subset to just have NH4 etc
# Date column in lawadata already POSIXct data type
sub_swq <- lawadata%>%select(c("LawaSiteID","SiteName","Date","parameter","Value"))%>%
  filter(tolower(parameter)%in%tolower(c("NH4","TON","ECOLI","PH")))
#+++++++++++++++++++++++++++++ Dealing with data in dfswq table++++++++++++++++++++++++++++++++++++
Name_swq<- unique(sub_swq$SiteName)
uLAWAids <- unique(sub_swq$LawaSiteID)
#+++++++++++++++++++++++++++++ Ammonia adjustment for pH++++++++++++++++++++++++++++++++++++
csv="H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/NOFAmmoniaAdjustment.csv"
adjnh4=NH4adj(sub_swq,c("NH4","PH"),csv)
sub_swq<-rbind(sub_swq,adjnh4)
rm(adjnh4,csv)

names(sub_swq)[names(sub_swq)=='parameter'] <- "Parameter"
sub_swq$Date=strptime(sub_swq$Date,"%d-%b-%Y")

if(exists("NOFSummaryTable")) { rm("NOFSummaryTable") }
i=1
cat(length(uLAWAids),'\t')
for(i in i:length(uLAWAids)){
  cat('.')
  if(as.integer(i/100)==(i/100)){cat('\n')}
  #regex find replace  sub_swq\$((.*))\[sub_swq\$Parameter=="TON" & sub_swq\$SiteName==uLAWAids\[i\]\]
  suppressWarnings(rm(tonsite,nh4site,ecosite,rightSite,value,Value)  )
  rightSite=sub_swq[(sub_swq$LawaSiteID==uLAWAids[i]),]
  rightSite=rightSite[!is.na(rightSite$Value),]
  # create table of compliance with proposed National Objectives Frameqork
  Com_NOF <- data.frame (Year = yr,
                         Median_Nitrate           = as.numeric(rep(NA,reps)),
                         Med_Nitrate_Band         = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Per_Nitrate              = as.numeric(rep(NA,reps)),
                         Per_Nitrate_Band         = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Nitrate_Toxicity_Band    = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Median_Ammoniacal        = as.numeric(rep(NA,reps)),
                         Med_Ammoniacal_Band      = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Max_Ammoniacal           = as.numeric(rep(NA,reps)),
                         Max_Ammoniacal_Band      = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Ammonia_Toxicity_Band    = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         E_coli                   = as.numeric(rep(NA,reps)),
                         E_coli_band              = rep(NA,reps),
                         E_coli95                 = as.numeric(rep(NA,reps)),
                         E_coli95_band            = rep(NA,reps),
                         E_coliRecHealth540       = as.numeric(rep(NA,reps)),
                         E_coliRecHealth540_Band  = rep(NA,reps),
                         E_coliRecHealth260       = as.numeric(rep(NA,reps)),
                         E_coliRecHealth260_Band  = rep(NA,reps),
                         E_coliSummaryband        = factor(rep(NA,reps),levels=c("A","B","C","D","E")),
                         stringsAsFactors = FALSE)
  
  
  ###################### Nitrate Toxicity  ########################################
  tonsite=rightSite[rightSite$Parameter=="TON",]
  
  #--------Median Nitrate--------------------------------------
  Value <- tapply(tonsite$Value, format(tonsite$Date, '%Y'), na.rm=TRUE, quantile,prob=c(0.5),type=5)
  
  if(length(Value)!=0){
    #adding values into Com_NOF table
    Com_NOF$Median_Nitrate <- Value[match(Com_NOF$Year,names(Value))]
    
    if(length(tonsite$Value)>=30){
      Com_NOF$Median_Nitrate[nrow(Com_NOF)] <- quantile(tonsite$Value,prob=c(0.5),type=5,na.rm=T)
    }
    #find the band which each value belong to
    Com_NOF$Med_Nitrate_Band <- unlist(lapply(Com_NOF$Median_Nitrate,NOF_FindBand,bandColumn = NOFbandDefinitions$Median.Nitrate))
    Com_NOF$Med_Nitrate_Band <- unlist(lapply(Com_NOF$Med_Nitrate_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    
    #-------95th percentage Nitrate--------------------------------------
    Value <- tapply(tonsite$Value, 
                    format(tonsite$Date, '%Y'), 
                    na.rm=TRUE, quantile, probs = 0.95, type =5)
    
    #adding values into Com_NOF table
    Com_NOF$Per_Nitrate = Value[match(Com_NOF$Year,names(Value))]
    
    #calculate the overall 95th percentage
    if(length(tonsite$Value)>=30){
      Com_NOF$Per_Nitrate[nrow(Com_NOF)] <- quantile(tonsite$Value,prob=c(0.95),type=5)
    }
    
    #find the band which each value belong to
    Com_NOF$Per_Nitrate_Band <- unlist(lapply(Com_NOF$Per_Nitrate,NOF_FindBand,bandColumn = NOFbandDefinitions$X95th.Percentile.Nitrate))
    Com_NOF$Per_Nitrate_Band <- unlist(lapply(Com_NOF$Per_Nitrate_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    
    #---------------------Finding the band for Nitrate Toxicity --------------------------
    #The worse of the two nitrate bands
    Com_NOF$Nitrate_Toxicity_Band = apply(select(Com_NOF, Med_Nitrate_Band, Per_Nitrate_Band),1,max,na.rm=T)
  }
  ###################### Ammonia Toxicity  ############################
  nh4site=rightSite[rightSite$Parameter=="NH4adj",]
  #--------Median Ammoniacal Nitrogen--------------------------------------
  Value <- tapply(nh4site$Value, 
                  format(nh4site$Date, '%Y'), 
                  na.rm=TRUE, quantile,prob=c(0.5),type=5)
  
  if(length(Value)!=0){
    #adding values into Com_NOF table
    Com_NOF$Median_Ammoniacal = Value[match(Com_NOF$Year,names(Value))]
    # Com_NOF$Median_Ammoniacal <- NOF_AddValue(Value, Com_NOF$Median_Ammoniacal, Com_NOF$Year)
    
    #calculate the overall median
    if(length(nh4site$Value)>=30){
      Com_NOF$Median_Ammoniacal[nrow(Com_NOF)] <- quantile(nh4site$Value,prob=c(0.5),type=5)
    }
    
    #find the band which each value belong to
    Com_NOF$Med_Ammoniacal_Band <- unlist(lapply(Com_NOF$Median_Ammoniacal,NOF_FindBand,
                                                 bandColumn=NOFbandDefinitions$Median.Ammoniacal.N)) 
    Com_NOF$Med_Ammoniacal_Band <- unlist(lapply(Com_NOF$Med_Ammoniacal_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    
    #-------95th percentage Ammoniacal Nitrogen--------------------------------------
    Value <- tapply(nh4site$Value, format(nh4site$Date, '%Y'), na.rm=TRUE, max)
    
    #adding values into Com_NOF table
    Com_NOF$Max_Ammoniacal <- Value[match(Com_NOF$Year,names(Value))]

    #calculate the overall median
    if(length(nh4site$Value)>=30){
      Com_NOF$Max_Ammoniacal[nrow(Com_NOF)] <- quantile(nh4site$Value,prob=c(0.95),type=5)
    }
    
    #find the band which each value belong to
    Com_NOF$Max_Ammoniacal_Band <-unlist(lapply(Com_NOF$Max_Ammoniacal,NOF_FindBand,
                                                bandColumn=NOFbandDefinitions$Max.Ammoniacal.N)) 
    Com_NOF$Max_Ammoniacal_Band <- unlist(lapply(Com_NOF$Max_Ammoniacal_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    
        #------------------Finding the band for Ammonia Toxicity-------------------------------
    Com_NOF$Ammonia_Toxicity_Band=apply(select(Com_NOF,Med_Ammoniacal_Band, Max_Ammoniacal_Band),1,max,na.rm=T)
   }  
  
  ######################  E.Coli #########################################
  suppressWarnings(rm(cnEc_band,cnEc95_band,cnEcRecHealth540_Band,cnEcRecHealth260_Band,ecosite,rawEcoli))
  ecosite=rightSite[rightSite$Parameter=="ECOLI",]
  rawEcoli=data.frame(year=format(ecosite$Date,'%Y'),
                      value=ecosite$Value)
  rawEcoli=rawEcoli[!is.na(rawEcoli$value),]
  if(dim(rawEcoli)[1]>=60){ #data requirement for band determination, footnote 1, table  NPS
    for(yy in 1:length(Com_NOF$Year)){
      ecv=rawEcoli$value[which(rawEcoli$year==Com_NOF$Year[yy])]
      if(length(ecv)>0){
        Com_NOF$E_coliRecHealth540[yy]=sum(ecv>540)/length(ecv)*100
        Com_NOF$E_coliRecHealth260[yy]=sum(ecv>260)/length(ecv)*100
      }
    }
    
    #Calculate overall exceedance percentage
    Com_NOF$E_coliRecHealth540[nrow(Com_NOF)] <- sum(rawEcoli$value>540)/length(rawEcoli$value)*100
    Com_NOF$E_coliRecHealth260[nrow(Com_NOF)] <- sum(rawEcoli$value>260)/length(rawEcoli$value)*100
    
    Com_NOF$E_coliRecHealth540_Band <- unlist(lapply(Com_NOF$E_coliRecHealth540,NOF_FindBand,bandColumn=NOFbandDefinitions$EcoliRec540))
    cnEcRecHealth540_Band <- unlist(lapply(Com_NOF$E_coliRecHealth540_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    
    Com_NOF$E_coliRecHealth260_Band <- unlist(lapply(Com_NOF$E_coliRecHealth260,NOF_FindBand,bandColumn=NOFbandDefinitions$EcoliRec260))
    cnEcRecHealth260_Band <- unlist(lapply(Com_NOF$E_coliRecHealth260_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))  
  } #else they're left as NA
  
  #E coli median ####
  if(dim(ecosite)[1]>=60){
    Value <- tapply(ecosite$Value, format(ecosite$Date, '%Y'), na.rm=TRUE, quantile,prob=c(0.5),type=5)
    if(length(Value)!=0){
      #adding values into Com_NOF table
      Com_NOF$E_coli <- Value[match(Com_NOF$Year,names(Value))]
      #calculate the overall median
      Com_NOF$E_coli[nrow(Com_NOF)] <- quantile(ecosite$Value,prob=c(0.5),type=5,na.rm=T)
      #find the band which each value belong to
      Com_NOF$E_coli_band <- unlist(lapply(Com_NOF$E_coli,NOF_FindBand,bandColumn=NOFbandDefinitions$E..coli))
      #This median EColi can meet multiple bands
      cnEc_band <- unlist(lapply(Com_NOF$E_coli_band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    }
    #else they're left as NA

    #-------95th percentile Ecoli-----------------------------------------------------
    Value <- tapply(ecosite$Value,format(ecosite$Date, '%Y'),na.rm=TRUE, quantile,prob=c(0.95),type=5)
    
    if(length(Value)!=0){
      #adding values into Com_NOF table
      Com_NOF$E_coli95 <- Value[match(Com_NOF$Year,names(Value))]
      #calculate the overall 95 percentile
      Com_NOF$E_coli95[nrow(Com_NOF)] <- quantile(ecosite$Value,prob=c(0.95),type=5,na.rm=T)
      #find the band which each value belong to
      Com_NOF$E_coli95_band <- unlist(lapply(Com_NOF$E_coli95,NOF_FindBand,bandColumn=NOFbandDefinitions$Ecoli95))
      #Ecoli95 can meet multiple bands
      cnEc95_band <- unlist(lapply(Com_NOF$E_coli95_band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    }  
  }
  #---------------------------------------------------------------------------------
  #These contain the best case out of these scorings, the worst of which contributes.
  if(all(exists(c("cnEc_band","cnEc95_band","cnEcRecHealth540_Band","cnEcRecHealth260_Band")))){
    Com_NOF$E_coliSummaryband = apply(cbind(pmax(cnEc_band,cnEc95_band,cnEcRecHealth540_Band,cnEcRecHealth260_Band)),1,max)
  }else{
    Com_NOF$E_coliSummaryband = NA
  }
  
  Com_NOF$LawaSiteID <- uLAWAids[i]
  if(!exists("NOFSummaryTable")){
    NOFSummaryTable <- Com_NOF
  } else {
    NOFSummaryTable <- rbind.data.frame(NOFSummaryTable,Com_NOF,stringsAsFactors = FALSE)
  }
  rm(ecosite,nh4site,rightSite,tonsite)
}

if(0){
  with(NOFSummaryTable,plot(as.factor(Med_Nitrate_Band),Median_Nitrate))
  with(NOFSummaryTable,plot(as.factor(Per_Nitrate_Band),Per_Nitrate))
  with(NOFSummaryTable,plot(as.factor(Nitrate_Toxicity_Band),Per_Nitrate))
  with(NOFSummaryTable,plot(as.factor(Med_Ammoniacal_Band),Median_Ammoniacal))
  with(NOFSummaryTable,plot(as.factor(Med_Ammoniacal_Band),Median_Ammoniacal))
  table(NOFSummaryTable$Med_Nitrate_Band,NOFSummaryTable$Nitrate_Toxicity_Band)
  table(NOFSummaryTable$Per_Nitrate_Band,NOFSummaryTable$Nitrate_Toxicity_Band)
  table(NOFSummaryTable$Med_Ammoniacal_Band,NOFSummaryTable$Ammonia_Toxicity_Band)
  table(NOFSummaryTable$Max_Ammoniacal_Band,NOFSummaryTable$Nitrate_Toxicity_Band)
  table(NOFSummaryTable$E_coli95_band,NOFSummaryTable$E_coliSummaryband)
}

#############################Save the output table ############################
riverSiteTable=read.csv(file="h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors = F) 
# load(file="h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/lawa_sitetable.RData") #From LAWA_State_EG, it's with catchment info


NOFSummaryTable$CouncilSiteID=riverSiteTable$CouncilSiteID[match(NOFSummaryTable$LawaSiteID,riverSiteTable$LawaSiteID)]
NOFSummaryTable$SiteID=riverSiteTable$SiteID[match(NOFSummaryTable$LawaSiteID,riverSiteTable$LawaSiteID)]
NOFSummaryTable <- NOFSummaryTable%>%select(LawaSiteID:SiteID,Year:E_coliSummaryband)
write.csv(NOFSummaryTable, file = paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/NOFSummaryTable.csv"),row.names=F)

NOFSummaryTable <- merge(NOFSummaryTable, riverSiteTable) 
NOFSummaryTableSubset <- NOFSummaryTable[NOFSummaryTable$Year=="Overall",]
NOFSummaryTableSubset <- NOFSummaryTableSubset%>%select("LawaSiteID","CouncilSiteID","SiteID",
                                                        Year:E_coliSummaryband,
                                                        "SWQAltitude","SWQLanduse","Agency")
write.csv(NOFSummaryTableSubset, file = paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/NOFSummaryTable_Overall.csv"),row.names=F)

# Reshape Output
require(reshape2)
NOFSummaryTableLong <- melt(data=NOFSummaryTable,#%>%select(-Comment),
                            id.vars=c("LawaSiteID","CouncilSiteID","SiteID","Agency","Year","Region","Long","Lat",
                                      "SWQAltitude","SWQLanduse","SWQuality","SWQFrequencyAll","SWQFrequencyLast5",
                                      "accessDate"))#,"TermReach","LAWA_CATCH","CATCH_LBL","CatchID","CatchType","SOE_FW_RIV"))
write.csv(NOFSummaryTableLong, file = paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/NOFSummaryTableLong.csv"),row.names=F)

NOFSummaryTableLongSubset <- NOFSummaryTableLong[NOFSummaryTableLong$Year=="Overall",]
NOFSummaryTableLongSubset <- NOFSummaryTableLongSubset[!is.na(NOFSummaryTableLongSubset$LawaSiteID),]
write.csv(NOFSummaryTableLongSubset, file = paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/NOF_STATE_2018.csv"),row.names=F)



#Round them off.  For web display?
NOFRound <- NOFSummaryTableLongSubset
NOFRound$variable <- as.character(NOFRound$variable)

variables<-as.character(unique(NOFSummaryTableLongSubset$variable))
variables <- variables[order(variables)]
# [1] "Ammonia_Toxicity_Band"   "E_coli"                  "E_coli_band"             "E_coli95"                "E_coli95_band"          
# [6] "E_coliRecHealth260"      "E_coliRecHealth260_Band" "E_coliRecHealth540"      "E_coliRecHealth540_Band" "E_coliSummaryband"      
# [11] "Max_Ammoniacal"          "Max_Ammoniacal_Band"     "Med_Ammoniacal_Band"     "Med_Nitrate_Band"        "Median_Ammoniacal"      
# [16] "Median_Nitrate"          "Nitrate_Toxicity_Band"   "Per_Nitrate"             "Per_Nitrate_Band"       

# Decimal places for variables
dp <- rep(NA,length(variables))
dp[variables%in%c("E_coli", "E_coli95", "E_coliRecHealth260", "E_coliRecHealth540")] <- 0
dp[variables%in%c("Max_Ammoniacal", "Median_Ammoniacal", "Median_Nitrate", "Per_Nitrate")] <- 4

parameterInvolved <- variables
parameterInvolved <- gsub(pattern = "Band",replacement = "",x = parameterInvolved)
parameterInvolved <- gsub(pattern = "band",replacement = "",x = parameterInvolved)
parameterInvolved <- gsub(pattern = "_$",replacement = "",x = parameterInvolved)
parameterInvolved <- gsub(pattern = "Med_",replacement = "Median_",x = parameterInvolved)
parameterInvolved <- gsub(pattern = "E_coli$",replacement = "E_coliMedian",x = parameterInvolved)

desc = rep('value',length(variables))
desc[grepl(variables,pattern = 'band',ignore.case = T)] <- 'band'
desc[variables%in%c("Agency", "SWQAltitude","SWQLanduse","SiteID","CATCH_LBL","CatchID",
               "CatchType","Comment","LAWA_CATCH","Region","SOE_FW_RIV",
               "SWQFrequencyAll","SWQFrequencyLast5","SWQuality","TermReach")] <- 'meta'

dfp <- data.frame(variables,parameterInvolved,desc,dp,stringsAsFactors=FALSE,row.names=NULL)
NOFRound <- merge(NOFRound,dfp,by.x="variable",by.y="variables",all=TRUE)

rm(variables,parameterInvolved,desc,dp)
# POST PROCESSING NOF RESULTS
# Round values to appropriate DP's

# Note that for rounding off a 5, the IEC 60559 standard is expected to be used, ‘go to the even digit’. Therefore round(0.5) is 0 and round(-1.5) is -2
# This is not the desired behaviour here. It is expected that 0.5 rounds to 1, 2.5 rounds, to 3 and so on.
# Therefore for all even values followed by exactly .5 needs to have a small number added (like 0.00001) to get them rounding in the right direction (unless there is 
# a flag in the function that provides for this behaviour), or to redefine the round function. 
# (see http://theobligatescientist.blogspot.co.nz/2010/02/r-i-still-love-you-but-i-hate-your.html)

# As all values are positive, we'll add a small number, related to the degree of rounding required.
# If I was smarter, I would redefine the round function


for(i in 1:length(dfp$variables)){
  if(!is.na(dfp$dp[i])){
    NOFRound$value[NOFRound$variable==dfp$variables[i]] <- as.character(as.numeric(NOFRound$value[NOFRound$variable==dfp$variables[i]]) + 0.000001)
    NOFRound$value[NOFRound$variable==dfp$variables[i]] <- as.character(round(as.numeric(NOFRound$value[NOFRound$variable==dfp$variables[i]]),digits = dfp$dp[i]))
  }
}


NOFRound$value[is.na(NOFRound$value)] <- "NA"
NOFRound <- NOFRound[order(NOFRound$LawaSiteID,NOFRound$parameterInvolved,NOFRound$desc),]
write.csv(NOFRound, file = paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/NOF_STATE_2018_Rounded_NAs.csv"),row.names=F)

# Transform (tidyr::spread) data in NOFRound to the following form to supply to IT Effect
# LawaSiteID,SiteName,Year,Parameter,value,Band
# ARC-00001,44603,Overall,Max_AmmoniacalN,NA,NA
# ARC-00001,44603,Overall,Median_Ammoniacal,NA,NA
# ARC-00001,44603,Overall,Median_Ecoli,28,A
# ARC-00001,44603,Overall,Median_Nitrate,0.0079,A

NOF_value <- NOFRound%>%filter(desc=="value")%>%select("LawaSiteID","CouncilSiteID","SiteID","Agency","Year","variable","value","parameterInvolved")
names(NOF_value) <- c("LawaSiteID","CouncilSiteID","SiteID","Agency","Year","Parameter","Value",'parameterInvolved')
NOF_band  <- NOFRound%>%filter(desc=="band")%>%select("LawaSiteID","CouncilSiteID","SiteID","Agency","Year","variable","value","parameterInvolved")
names(NOF_band) <- c("LawaSiteID","CouncilSiteID","SiteID","Agency","Year","BandingRule","BandScore",'parameterInvolved')

NOF_wide <- dplyr::left_join(NOF_band,NOF_value,by = c("LawaSiteID","CouncilSiteID","SiteID","Agency", "Year", "parameterInvolved"))
NOF_wide <- unique(NOF_wide)

write.csv(NOF_wide, file = paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/RiverWQ_NOF_forITE_",
                                  format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),row.names = F)

# NOF_wide=read.csv(tail(dir(path="h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis",pattern="RiverWQ_NOF_forITE_",recursive = T,full.names = T),1),stringsAsFactors = F)
