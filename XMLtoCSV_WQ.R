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


##############################################################################
#                                 *****
##############################################################################
for(agency in c("ac","boprc","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  xml2csvRiver(agency = agency)
}
##############################################################################
#                                 *****
##############################################################################


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


#Build the combo
stbl=tail(dir(path="h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",pattern="LAWA_Site_Table1.csv",recursive=T,full.names=T),1)
catSiteTable <- read.csv(stbl,stringsAsFactors = F)
rm(stbl)

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
  mfl=loadLatestCSVRiver(council)
  while(grepl(pattern = '^X',x = names(mfl)[1])){
    mfl=mfl[,-1]
  }
  names(mfl)[names(mfl)=='SWQFrequencyAll'] <- 'Frequency'
  
  
  if(sum(is.na(mfl$Agency))>0){
    cat(sum(is.na(mfl$Agency)),'non agency')
    cat('\t',paste(collapse=', ',unique(mfl$SiteName[mfl$Agency==''|is.na(mfl$Agency)])))
  }
  if(sum(!tolower(mfl$CouncilSiteID)%in%tolower(catSiteTable$CouncilSiteID))>0){
    cat('\t',sum(!unique(tolower(mfl$CouncilSiteID))%in%catSiteTable$CouncilSiteID),'not in site table\n')
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


boprc=boprc[-which(is.na(boprc$CouncilSiteID)),]

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

wqdata$CenType[wqdata$CenType%in%c("L","Left")] <- "Left"
wqdata$CenType[wqdata$CenType%in%c("R","Right")] <- "Right"

try(dir.create(paste0("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"))))
write.csv(wqdata,paste0("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/AllCouncils.csv"),row.names = F)
rm(ac,boprc,ecan,es,gdc,gwrc,hbrc,hrc,mdc,ncc,nrc,orc,tdc,trc,wcrc,wrc,council,niwa)



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





