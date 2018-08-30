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
# A          <=1.0                    <=1.5              <=0.03           <=0.05   <=130   <=540          <5         <20
# B          <=2.4                    <=3.5              <=0.24            <=0.4   <=130  <=1000        <=10        <=30
# C          <=6.9                    <=9.8               <=1.3            <=2.2   <=130  <=1200        <=20        <=34
# D           >6.9                     >9.8               >1.30             >2.2    >130   >1200        <=30        <=50
# E           >Inf                     >Inf                >Inf             >Inf    >260   >1200         >30         >50


# This is for setting the years you would like to undertake the assessment on for the National objectives Framework.
# Add more years using comma seperation as needed 
yr  <-c("2013", "2014","2015","2016","2017","Overall")
reps  <-length(yr)



#===================================================================================================
## Load LAWA Data
#Reference Dates
EndYear <- 2017
StartYear <- EndYear - NOF_PERIOD + 1


# loads lawadata dataframe
file.info(paste0("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawadata",StartYear,"-",EndYear,".RData"))$mtime

load(file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawadata",StartYear,"-",EndYear,".RData"))

lawadata$parameter=toupper(lawadata$parameter)
# Subset to just have NH4
# Date column in lawadata already POSIXct data type
sub_swq <- lawadata%>%select(c("LawaSiteID","SiteName","Date","parameter","Value"))%>%filter(tolower(parameter)%in%tolower(c("NH4","TON","ECOLI","PH")))
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
  #regex find replace  sub_swq\$((.*))\[sub_swq\$Parameter=="TON" & sub_swq\$SiteName==uLAWAids\[i\]\]
  suppressWarnings(rm(tonsite,nh4site,ecosite,rightSite,value,Value)  )
  rightSite=sub_swq[(sub_swq$LawaSiteID==uLAWAids[i]),]
  rightSite=rightSite[!is.na(rightSite$Value),]
  # create table of compliance with proposed National Objectives Frameqork
  Com_NOF <- data.frame (Year = yr,
                         Median_Nitrate        = as.numeric(rep(NA,reps)),
                         Med_Nitrate_Band      = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Per_Nitrate           = as.numeric(rep(NA,reps)),
                         Per_Nitrate_Band      = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Nitrate_Toxicity_Band = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Median_Ammoniacal     = as.numeric(rep(NA,reps)),
                         Med_Ammoniacal_Band   = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Max_Ammoniacal        = as.numeric(rep(NA,reps)),
                         Max_Ammoniacal_Band   = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Ammonia_Toxicity_Band = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         E_coli                = as.numeric(rep(NA,reps)),
                         E_coli_band           = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         E_coli95              = as.numeric(rep(NA,reps)),
                         E_coli95_band         = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         E_coliRecHealth540    = as.numeric(rep(NA,reps)),
                         E_coliRecHealth540band= factor(rep(NA,reps),levels = c("A","B","C","D","E")),
                         E_coliRecHealth260    = as.numeric(rep(NA,reps)),
                         E_coliRecHealth260band= factor(rep(NA,reps),levels = c("A","B","C","D","E")),
                         E_coliSummaryband         = factor(rep(NA,reps),levels=c("A","B","C","D","E")),
                         stringsAsFactors = FALSE)
  
  
  ###################### Nitrate Toxicity  ########################################
  tonsite=rightSite[rightSite$Parameter=="TON",]
  
  #--------Median Nitrate--------------------------------------
  Value <- tapply(tonsite$Value, 
                  format(tonsite$Date, '%Y'), 
                  na.rm=TRUE, quantile,prob=c(0.5),type=5)
  
  if(length(Value)!=0){
    #adding values into Com_NOF table
    Com_NOF$Median_Nitrate <- Value[match(Com_NOF$Year,names(Value))]
    # Com_NOF$Median_Nitrate <- NOF_AddValue(Value, Com_NOF$Median_Nitrate, Com_NOF$Year)
    
    if(length(tonsite$Value)>=30){
      Com_NOF$Median_Nitrate[nrow(Com_NOF)] <- quantile(tonsite$Value,prob=c(0.5),type=5,na.rm=T)
    }
    #find the band which each value belong to
    Com_NOF$Med_Nitrate_Band <-unlist(lapply(Com_NOF$Median_Nitrate,NOF_FindBand,bandColumn = NOFbandDefinitions$Median.Nitrate))
    Com_NOF$Med_Nitrate_Band <- unlist(lapply(Com_NOF$Med_Nitrate_Band,FUN=function(x){min(unlist(strsplit(x,split = ',')))}))
    
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
    Com_NOF$Per_Nitrate_Band <- unlist(lapply(Com_NOF$Per_Nitrate_Band,FUN=function(x){min(unlist(strsplit(x,split = ',')))}))
    
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
    Com_NOF$Med_Ammoniacal_Band <- unlist(lapply(Com_NOF$Med_Ammoniacal_Band,FUN=function(x){min(unlist(strsplit(x,split = ',')))}))
    
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
    Com_NOF$Max_Ammoniacal_Band <- unlist(lapply(Com_NOF$Max_Ammoniacal_Band,FUN=function(x){min(unlist(strsplit(x,split = ',')))}))
    
        #------------------Finding the band for Ammonia Toxicity-------------------------------
    Com_NOF$Ammonia_Toxicity_Band=apply(select(Com_NOF,Med_Ammoniacal_Band, Max_Ammoniacal_Band),1,max,na.rm=T)
   }  
  
  ######################  E.Coli #########################################
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
    
    Com_NOF$E_coliRecHealth540band <- unlist(lapply(Com_NOF$E_coliRecHealth540,NOF_FindBand,bandColumn=NOFbandDefinitions$EcoliRec540))
    Com_NOF$E_coliRecHealth540band <- unlist(lapply(Com_NOF$E_coliRecHealth540band,
                                                    FUN=function(x){min(unlist(strsplit(x,split = ',')))}))
    
    Com_NOF$E_coliRecHealth260band <- unlist(lapply(Com_NOF$E_coliRecHealth260,NOF_FindBand,bandColumn=NOFbandDefinitions$EcoliRec260))
    Com_NOF$E_coliRecHealth260band <- unlist(lapply(Com_NOF$E_coliRecHealth260band,
                                                    FUN=function(x){min(unlist(strsplit(x,split = ',')))}))  
  } #else they're left as NA
  
  #E coli median ####
  if(dim(ecosite)[1]>60){
    Value <- tapply(ecosite$Value, format(ecosite$Date, '%Y'), na.rm=TRUE, quantile,prob=c(0.5),type=5)
    if(length(Value)!=0){
      #adding values into Com_NOF table
      Com_NOF$E_coli <- Value[match(Com_NOF$Year,names(Value))]
      #calculate the overall median
      Com_NOF$E_coli[nrow(Com_NOF)] <- quantile(ecosite$Value,prob=c(0.5),type=5,na.rm=T)
      #find the band which each value belong to
      Com_NOF$E_coli_band <- unlist(lapply(Com_NOF$E_coli,NOF_FindBand,bandColumn=NOFbandDefinitions$E..coli))
      #This median EColi can meet multiple bands
      # Com_NOF$E_coli_band <- unlist(lapply(Com_NOF$E_coli_band,FUN=function(x){min(unlist(strsplit(x,split = ',')))}))
    }
    #else they're left as NA

    #-------95th percentile Ecoli-----------------------------------------------------
    Value <- tapply(ecosite$Value,format(ecosite$Date, '%Y'),na.rm=TRUE, quantile,prob=c(0.95),type=5)
    
    if(length(Value)!=0){
      #adding values into Com_NOF table
      Com_NOF$E_coli95 <- Value[match(Com_NOF$Year,names(Value))]
      # Com_NOF$E_coli95 <- NOF_AddValue(Value, Com_NOF$E_coli95, Com_NOF$Year)
      #calculate the overall 95 percentile
      Com_NOF$E_coli95[nrow(Com_NOF)] <- quantile(ecosite$Value,prob=c(0.95),type=5,na.rm=T)
      #find the band which each value belong to
      Com_NOF$E_coli95_band <- unlist(lapply(Com_NOF$E_coli95,NOF_FindBand,bandColumn=NOFbandDefinitions$Ecoli95))
      #Ecoli95 can meet multiple bands
      # Com_NOF$E_coli95_band <- unlist(lapply(Com_NOF$E_coli95_band,FUN=function(x){min(unlist(strsplit(x,split = ',')))}))
    }  
  }
  #---------------------------------------------------------------------------------
  Com_NOF$E_coliSummaryband = apply(select(Com_NOF,E_coli_band, E_coli95_band ,E_coliRecHealth540band,E_coliRecHealth260band),1,
                                max)
  
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
load(file="h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawa_sitetable.RData") #From LAWA_State_EG, it's with catchment info


NOFSummaryTable$CouncilSiteID=catSiteTable$CouncilSiteID[match(NOFSummaryTable$LawaSiteID,catSiteTable$LawaSiteID)]
NOFSummaryTable$SiteID=catSiteTable$SiteID[match(NOFSummaryTable$LawaSiteID,catSiteTable$LawaSiteID)]
NOFSummaryTable <- NOFSummaryTable%>%select(LawaSiteID:SiteID,Year:E_coliSummaryband)
write.csv(NOFSummaryTable, file = "h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOFSummaryTable.csv",row.names=F)

NOFSummaryTable <- merge(NOFSummaryTable, catSiteTable) 
NOFSummaryTableSubset <- NOFSummaryTable[NOFSummaryTable$Year=="Overall",]
NOFSummaryTableSubset <- NOFSummaryTableSubset%>%select("LawaSiteID","CouncilSiteID","SiteID",
                                                        Year:E_coliSummaryband,
                                                        "SWQAltitude","SWQLanduse","Agency")
write.csv(NOFSummaryTableSubset, file = "h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOFSummaryTable_Overall.csv")

# Reshape Output
require(reshape2)
NOFSummaryTableLong <- melt(data=NOFSummaryTable,id.vars=c("LawaSiteID","CouncilSiteID","SiteID","SWQAltitude","SWQLanduse","Agency","Year"))
write.csv(NOFSummaryTableLong, file = "h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOF Summary Table Long.csv")

NOFSummaryTableLongSubset <- NOFSummaryTableLong[NOFSummaryTableLong$Year=="Overall",]
NOFSummaryTableLongSubset <- NOFSummaryTableLongSubset[!is.na(NOFSummaryTableLongSubset$LawaSiteID),]
write.csv(NOFSummaryTableLongSubset, file = "h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOF_STATE_2018.csv")



#Round them off.  For web display?
nofRound <- NOFSummaryTableLongSubset
vars<-as.character(unique(NOFSummaryTableLongSubset$variable))
vars <- vars[order(vars)]
#vars
# [1] "accessDate"             "Ammonia_Toxicity_Band"  "CATCH_LBL"              "CatchID"                "CatchType"             
# [6] "Comment"                "E_coli"                 "E_coli_band"            "E_coli95"               "E_coli95_band"         
# [11] "E_coliRecHealth260"     "E_coliRecHealth260band" "E_coliRecHealth540"     "E_coliRecHealth540band" "E_coliSummaryband"         
# [16] "LAWA_CATCH"             "Max_Ammoniacal"         "Max_Ammoniacal_Band"    "Med_Ammoniacal_Band"    "Med_Nitrate_Band"      
# [21] "Median_Ammoniacal"      "Median_Nitrate"         "Nitrate_Toxicity_Band"  "Per_Nitrate"            "Per_Nitrate_Band"      
# [26] "Region"                 "SOE_FW_RIV"             "SWQFrequencyAll"        "SWQFrequencyLast5"      "SWQuality"             
# [31] "TermReach"     

nofRound$variable <- as.character(nofRound$variable)
# Decimal places for variables
#dp <- c(4,0,NA,NA,NA,4,NA,4,NA,4)
dp <- rep(NA,length(vars))
dp[vars%in%c("E_coli", "E_coli95", "E_coliRecHealth260", "E_coliRecHealth540")] <- 0
dp[vars%in%c("Max_Ammoniacal", "Median_Ammoniacal", "Median_Nitrate", "Per_Nitrate")] <- 4

# p <- c("Median_Ecoli","Median_Ecoli","Per_Ecoli","Per_Ecoli","Median_Ammoniacal","Median_Nitrate","Max_AmmoniacalN","Max_AmmoniacalN","Median_Ammoniacal","Median_Nitrate","Per_Nitrate","Per_Nitrate")
p=vars #EG: I dont see why this should be different!?

desc = rep('value',length(vars))
desc[grepl(vars,pattern = 'band',ignore.case = T)] <- 'band'
desc[vars%in%c("Agency", "SWQAltitude","SWQLanduse","SiteID","CATCH_LBL","CatchID",
               "CatchType","Comment","LAWA_CATCH","Region","SOE_FW_RIV",
               "SWQFrequencyAll","SWQFrequencyLast5","SWQuality","TermReach")] <- 'meta'

dfp <- data.frame(vars,p,desc,stringsAsFactors=FALSE,row.names=NULL)
nofRound <- merge(nofRound,dfp,by.x="variable",by.y="vars",all=TRUE)


# POST PROCESSING NOF RESULTS
# Round values to appropriate DP's

# Note that for rounding off a 5, the IEC 60559 standard is expected to be used, ‘go to the even digit’. Therefore round(0.5) is 0 and round(-1.5) is -2
# This is not the desired behaviour here. It is expected that 0.5 rounds to 1, 2.5 rounds, to 3 and so on.
# Therefore for all even values followed by exactly .5 needs to have a small number added (like 0.00001) to get them rounding in the right direction (unless there is 
# a flag in the function that provides for this behaviour), or to redefine the round function. 
# (see http://theobligatescientist.blogspot.co.nz/2010/02/r-i-still-love-you-but-i-hate-your.html)

# As all values are positive, we'll add a small number, related to the degree of rounding required.
# If I was smarter, I would redefine the round function


for(i in 1:length(vars)){
  if(!is.na(dp[i])){
    nofRound$value[nofRound$variable==vars[i]] <- as.character(as.numeric(nofRound$value[nofRound$variable==vars[i]]) + 0.000001)
    nofRound$value[nofRound$variable==vars[i]] <- as.character(round(as.numeric(nofRound$value[nofRound$variable==vars[i]]),digits = dp[i]))
  } else {
    cat('meta column\t')
  }
}


nofRound$value[is.na(nofRound$value)] <- "NA"
nofRound <- nofRound[order(nofRound$LawaSiteID,nofRound$p,nofRound$desc),]
write.csv(nofRound, file = "h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOF_STATE_2018_Rounded_NAs.csv")

# Transform (tidyr::spread) data in nofRound to the following form to supply to IT Effect
# LawaSiteID,SiteName,Year,Parameter,value,Band
# ARC-00001,44603,Overall,Max_AmmoniacalN,NA,NA
# ARC-00001,44603,Overall,Median_Ammoniacal,NA,NA
# ARC-00001,44603,Overall,Median_Ecoli,28,A
# ARC-00001,44603,Overall,Median_Nitrate,0.0079,A

nof_value <- nofRound[nofRound$desc=="value",c(2,3,4,8,1,9)]
names(nof_value) <- c("LawaSiteID","CouncilSiteID","SiteID","Year","Parameter","Value")
nof_band  <- nofRound[nofRound$desc=="band",c(2,3,4,8,1,9)]
names(nof_band) <- c("LawaSiteID","CouncilSiteID","SiteID","Year","Parameter","Band")

nof_wide <- dplyr::left_join(nof_value,nof_band,by = c("LawaSiteID", "Year", "Parameter"))
nof_wide <- unique(nof_wide)

write.csv(nof_wide, file = "h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOF_STATE_2018_ITEFFECT.csv")
