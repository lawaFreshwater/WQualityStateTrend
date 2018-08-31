rm(list=ls())
library(XML)
source('H:/ericg/16666LAWA/2018/LAWAFunctionsEG.R')
lawaset=c("NH4", "TURB", "BDISC",  "DRP",  "ECOLI",  "TN",  "TP",  "TON",  "PH")


# 
# for(agency in c("boprc","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
#   cat('\n',agency,'\n')
#   checkReturnNames(agency=agency)
# }
#     

siteTable=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors=FALSE)
while(grepl(pattern = '^X',x = names(siteTable)[1])){
  siteTable=siteTable[,-1]
}
siteTable$SiteID=trimws(siteTable$SiteID)
rownames(siteTable)=NULL

# lawaIDs=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/LAWAMasterSiteListasatMarch2018.csv",stringsAsFactors = F)
# lawaIDs=lawaIDs[lawaIDs$Module=="Freshwater Quality",]
# lawaIDs$Lat=as.numeric(lawaIDs$Latitude)
# lawaIDs$Long=as.numeric(lawaIDs$Longitude)
# stlid=full_join(x = siteTable,y = lawaIDs,by=c("LawaSiteID"="LawaID"))



transfers=read.table("h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/transfers_plain_english_view.txt",
                     sep=',',header = F,stringsAsFactors = F)
names(transfers)=c("agency","nameFrom","nameTo","code")
transfers$nameTo=gsub(pattern = "\\(HRC\\)",replacement = "(LAWA)",x = transfers$nameTo)
transfers$agency[transfers$agency=='Horizons'] <- "HRC"

transfers$nameTo[transfers$nameTo=="Ammoniacal-N (LAWA)"] <- "NH4"
transfers$nameTo[transfers$nameTo=="Turbidity EPA (LAWA)"] <- "TURB"
transfers$nameTo[transfers$nameTo=="Black Disc (LAWA)"] <- "BDISC"
transfers$nameTo[transfers$nameTo=="DRP (LAWA)"] <- "DRP"
transfers$nameTo[transfers$nameTo=="E. coli by MPN (LAWA)"] <- "ECOLI"
transfers$nameTo[transfers$nameTo=="TN (LAWA)"] <- "TN"
transfers$nameTo[transfers$nameTo=="TP (LAWA)"] <- "TP"
transfers$nameTo[transfers$nameTo=="TON (LAWA)"] <- "TON"
transfers$nameTo[transfers$nameTo=="pH (LAWA)"] <- "PH"

lawaset=c("NH4", "TURB", "BDISC",  "DRP",  "ECOLI",  "TN",  "TP",  "TON",  "PH")



#Auckland
accsv=xml2csvRiver(agency="AC")
write.csv(accsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/ac.csv'),row.names=F)
# boprccsv$SiteID=ucSIDs[match(boprccsv$CouncilSiteID,uSIDs)]

#Bay of Plenty
boprccsv=xml2csvRiver(agency="boprc")
write.csv(boprccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/boprc.csv'),row.names=F)

#Canterbury
ecancsv=xml2csvRiver(agency="ECan")
write.csv(ecancsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/ecan.csv'),row.names=F)

#Gisborne 
gdcsv=xml2csvRiver(agency='gdc',maxHistory = 20)
write.csv(gdcsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/gdc.csv'),row.names=F)

#Horizons
hrccsv=xml2csvRiver(agency="hrc")
write.csv(hrccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/hrc.csv'),row.names=F)

#Northland 
nrccsv=xml2csvRiver(agency="nrc")
write.csv(nrccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/nrc.csv'),row.names=F)

#Taranaki 
trccsv=xml2csvRiver(agency='TRC')
write.csv(trccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/trc.csv'),row.names=F)

#Hawkes Bay 
hbrccsv=xml2csvRiver(agency='HBRC')
write.csv(hbrccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/hbrc.csv'),row.names=F)

#Greater Wellington 
gwrccsv=xml2csvRiver(agency="GWRC")
write.csv(gwrccsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/gwrc.csv'),row.names=F)

#Nelson
ncccsv=xml2csvRiver(agency="NCC")
write.csv(ncccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/ncc.csv'),row.names=F)

#Tasman 
tdcsv=xml2csvRiver(agency='TDC')
write.csv(tdcsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/tdc.csv'),row.names = F)

#Marlborough 
mdccsv=xml2csvRiver(agency="mdc")
write.csv(mdccsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/mdc.csv'),row.names=F)

#Otago 
orccsv=xml2csvRiver(agency = 'orc')
write.csv(orccsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/orc.csv'),row.names=F)

#Environment Southland 
escsv=xml2csvRiver(agency="ES")
write.csv(escsv,file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/es.csv'),row.names=F)

#West Coast 
wcrcsv=xml2csvRiver(agency="WCRC")
write.csv(wcrcsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/wcrc.csv'),row.names=F)

#Waikato
wrcsv=xml2csvRiver(agency='WRC')
write.csv(wrcsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/wrc.csv'),row.names=F)







#Per agency/measure xmlAge, start, stop, n, nSite, mean, max, min audit
library(lubridate)
nms=data.frame(agency=NULL,xmlAge=NULL,var=NULL,earliest=NULL,latest=NULL,nMeas=NULL,nSite=NULL,meanMeas=NULL,maxMeas=NULL,minMeas=NULL,nNA=NULL)
for(agency in c("ac","boprc","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  xmlAge = checkXMLage(agency)
  mfl=loadLatestCSVRiver(agency)
  if(!is.null(mfl)){
    newRows=data.frame(agency=rep(agency,length(unique(mfl$parameter))),
                       xmlAge=rep(xmlAge,length(unique(mfl$parameter))),
                       var=sort(unique(mfl$parameter)),
                       earliest=rep("",length(unique(mfl$parameter))),
                       latest=rep("",length(unique(mfl$parameter))),
                       nMeas=rep(NA,length(unique(mfl$parameter))),
                       nSite=rep(NA,length(unique(mfl$parameter))),
                       meanMeas=rep(NA,length(unique(mfl$parameter))),
                       maxMeas=rep(NA,length(unique(mfl$parameter))),
                       minMeas=rep(NA,length(unique(mfl$parameter))),
                       nNA=rep(NA,length(unique(mfl$parameter))),
                       stringsAsFactors = F)
    for(v in 1:dim(newRows)[1]){
      newRows$earliest[v]=format(min(dmy(mfl$Date[which(mfl$parameter==newRows$var[v])])),'%d-%b-%Y')
      newRows$latest[v]=format(max(dmy(mfl$Date[which(mfl$parameter==newRows$var[v])])),'%d-%b-%Y')
      newRows$nMeas[v]=sum(mfl$parameter==newRows$var[v])
      newRows$nSite[v]=length(unique(mfl$SiteName[which(mfl$parameter==newRows$var[v] & !is.na(mfl$Value))]))
      newRows$meanMeas[v]=round(mean(mfl$Value[mfl$parameter==newRows$var[v]],na.rm=T),1)
      newRows$maxMeas[v]=round(max(mfl$Value[mfl$parameter==newRows$var[v]],na.rm=T),1)
      newRows$minMeas[v]=round(min(mfl$Value[mfl$parameter==newRows$var[v]],na.rm=T),1)
      newRows$nNA[v]=sum(is.na(mfl$Value[mfl$parameter==newRows$var[v]]))
    }
    nms <- rbind.data.frame(nms,newRows)
  }
}
write.csv(nms,paste0("h:/ericg/16666LAWA/2018/WaterQuality/QA/WQAudit.csv"),row.names = F)



#Per site/measurement start, stop, n and range audit
for(agency in c("ac","boprc","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  mfl=loadLatestCSVRiver(agency)
  nvar=length(uvars <- unique(mfl$parameter))
  nsite=length(usites <- unique(mfl$SiteName))
  agencyDeets=as.data.frame(matrix(nrow=nvar*nsite,ncol=7))
  names(agencyDeets)=c("Site","Var","StartDate","EndDate","nMeas","MinVal","MaxVal")
  r=1
  for(ns in 1:nsite){
    for(nv in 1:nvar){
      these=which(mfl$SiteName==usites[ns]&mfl$parameter==uvars[nv])
      agencyDeets[r,]=c(usites[ns],uvars[nv],
                         as.character(min(dmy(mfl$Date[these]))),
                         as.character(max(dmy(mfl$Date[these]))),
                         length(these),
                         min(mfl$Value[these],na.rm=T),max(mfl$Value[these],na.rm=T))
      r=r+1
    }
  }
  write.csv(agencyDeets,row.names = F,paste0("h:/ericg/16666LAWA/2018/WaterQuality/QA/",agency,"/",agency,"audit.csv"))
}

#Uniquety check
for(agency in c("ac","boprc","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  mfl=loadLatestCSVRiver(agency)
  dmfl=dim(mfl)
  umfl=unique(mfl)
  dumfl=dim(umfl)
  if(any(dmfl!=dumfl)){
    cat(agency,'\t',dmfl,'\t',dumfl,'\n')
    write.csv(umfl,
              file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/u',agency,'.csv'),row.names=F)
    
    }
}



