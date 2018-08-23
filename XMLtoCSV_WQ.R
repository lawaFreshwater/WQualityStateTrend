rm(list=ls())
library(XML)



checkCSVage <- function(agency,maxHistory=100){
  stepBack=0
  while(stepBack<maxHistory){
    if(dir.exists(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                         format(Sys.Date()-stepBack,"%Y-%m-%d"),'/'))){
      if(file.exists(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                            format(Sys.Date()-stepBack,"%Y-%m-%d"),'/',agency,'.csv'))){
        cat(agency,'from',stepBack,'days ago,',format(Sys.Date()-stepBack,"%Y-%m-%d"),'\n')
        return(stepBack)
      }
    }
    stepBack=stepBack+1
  }
  if(stepBack==maxHistory){
    cat(agency,'not found within',maxHistory,'days\n')
    return(maxHistory)
  }
}
checkXMLage <- function(agency,maxHistory=100){
  stepBack=0
  while(stepBack<maxHistory){
    if(dir.exists(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                         format(Sys.Date()-stepBack,"%Y-%m-%d"),'/'))){
      if(file.exists(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                            format(Sys.Date()-stepBack,"%Y-%m-%d"),'/',agency,'swq.xml'))){
        cat(agency,'from',stepBack,'days ago,',format(Sys.Date()-stepBack,"%Y-%m-%d"),'\n')
        return(stepBack)
      }
    }
    stepBack=stepBack+1
  }
  if(stepBack==maxHistory){
    cat(agency,'not found within',maxHistory,'days\n')
    return(maxHistory)
  }
}
xml2csv <- function(maxHistory=10,quiet=F,reportCensor=F,agency){
  
  rm(forcsv)
  stepBack=0
  while(stepBack<maxHistory){
    if(dir.exists(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                         format(Sys.Date()-stepBack,"%Y-%m-%d"),'/'))){
      if(file.exists(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                            format(Sys.Date()-stepBack,"%Y-%m-%d"),'/',agency,'swq.xml'))){
        cat('loading',agency,'from',stepBack,'days ago,',format(Sys.Date()-stepBack,"%Y-%m-%d"),'\n')
        xmlIn <- xmlParse(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                                 format(Sys.Date()-stepBack,"%Y-%m-%d"),'/',agency,'swq.xml'))
        break
      }
    }
    stepBack=stepBack+1
  }
  if(!exists("xmlIn")){
    cat(agency,'not found, check abbrev. or increase search history\n')
    return(NULL)
    }else{
  varNames = unique(sapply(getNodeSet(doc=xmlIn,path="//Measurement/DataSource"),xmlGetAttr,name='Name'))
  if("WQ Sample"%in%varNames){
    cat("\t\tExcluding WQ Sample\n")
    varNames=varNames[-which(varNames=="WQ Sample")]
  }
  siteNames = unique(sapply(getNodeSet(doc=xmlIn,path=paste0("//Measurement")),xmlGetAttr,name='SiteName'))
  siteNameslc=tolower(siteNames)
  for(sn in 1:length(siteNames)){
    siteDeets = grep(pattern = siteNameslc[sn],x = siteTable$CouncilSiteID)
    if(length(siteDeets)==0){
      siteDeets = which(tolower(siteTable$CouncilSiteID)==tolower(siteNames[sn]))
    }
    if(length(siteDeets)>1){
      siteDeets=siteDeets[1]
    }
    if(length(siteDeets)!=1){
      siteDeets=which(siteNameslc[sn]==tolower(siteTable$LawaSiteID))
    }
    if(length(siteDeets)!=1){
      siteDeets=which(siteNameslc[sn]==tolower(siteTable$SiteID))
    }
    if(length(siteDeets)!=1){
      #This site may have been retired or removed, it didnt come up in the WFS 
      cat(siteNames[sn],'missing from the WFS feed\n')
    }
    
    for(vn in 1:length(varNames)){
      dt=sapply(getNodeSet(doc=xmlIn, paste0("//Measurement[@SiteName = '",siteNames[sn],
                                             "']/DataSource[@Name='",varNames[vn],"']/..//T")), xmlValue)
      vv=as.character(sapply(getNodeSet(doc=xmlIn, paste0("//Measurement[@SiteName = '",siteNames[sn],
                                                          "']/DataSource[@Name='",varNames[vn],"']/..//I1")), xmlValue))
      cens=sapply(getNodeSet(doc=xmlIn, paste0("//Measurement[@SiteName = '",siteNames[sn],
                                               "']/DataSource[@Name='",varNames[vn],"']/..//I2")),xmlValue)
      cenL=grepl(x = cens,pattern = 'ND.<')
      cenR=grepl(x = cens,'ND.>')
      if(reportCensor){
        if(any(cenL)){cat(varNames[vn],'left censored\n')}
        if(any(cenR)){cat(varNames[vn],'right censored\n')}
      }
      if(length(dt)!=length(vv)){
        browser()
      }
      if(length(dt)>0){
        formattedDate=format.Date(strptime(dt,format="%Y-%m-%dT%H:%M:%S"),"%d-%b-%Y")
        if(all(is.na(formattedDate))){
          formattedDate=format.Date(strptime(dt,format="%d/%m/%Y %H:%M"),"%d-%b-%Y")
        }
        dtvv=data.frame(SiteName=rep(siteNameslc[sn],length(dt)),
                        Date=formattedDate,
                        Value = vv,
                        Method="",
                        parameter=varNames[vn],
                        Censored=F,
                        CenType=F,
                        stringsAsFactors = F)
        rm(formattedDate)
        if(any(cenL)|any(cenR)){
          dtvv$Censored[cenL|cenR]=TRUE
          dtvv$CenType[which(cenL)]="Left"
          dtvv$CenType[which(cenR)]="Right"
        }
        if(length(siteDeets)==1){
          dtvv=cbind(dtvv,siteTable[siteDeets,])  
        }
        if(!exists('forcsv')){
          forcsv=dtvv
        }else{
          forcsv=merge(forcsv,dtvv,all=T)
        }
      }
    }
    if(!quiet){
      cat(paste0(siteNames[sn],'\t',sn,' of ',length(siteNames),'\n'))
    }
  }
  # forcsv=merge(x=forcsv,y=siteTable,by.x="SiteName",by.y="CouncilSiteID",all.x=T,suffixes = NULL)
  forcsv$parameter[tolower(forcsv$parameter) %in% tolower(transfers$nameFrom[tolower(transfers$council) %in% tolower(agency)])] =
    transfers$nameTo[tolower(transfers$council) %in% tolower(agency)][match(x = tolower(forcsv$parameter[tolower(forcsv$parameter) %in%
                                                                                                  tolower(transfers$nameFrom[tolower(transfers$council) %in% tolower(agency)])]),
               table = tolower(transfers$nameFrom[tolower(transfers$council) %in% tolower(agency)]))]
  
  return(forcsv)
    }
}

loadLatestCSV <- function(council=NULL,maxHistory=10){
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


siteTable=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors=FALSE)
while(grepl(pattern = '^X',x = names(siteTable)[1])){
  siteTable=siteTable[,-1]
}
siteTable$SiteID=trimws(siteTable$SiteID)
rownames(siteTable)=NULL

transfers=read.table("h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/transfers_plain_english_view.txt",
                     sep=',',header = F,stringsAsFactors = F)
names(transfers)=c("council","nameFrom","nameTo","code")
transfers$nameTo=gsub(pattern = "\\(HRC\\)",replacement = "(LAWA)",x = transfers$nameTo)
apply(transfers,MARGIN = 2,FUN=function(x)length(unique(x)))
transfers$council[transfers$council=='Horizons'] <- "HRC"

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
accsv=xml2csv(agency="AC")
names(accsv)[which(names(accsv)=="SWQFrequencyAll")]="Frequency"
excess=unique(accsv$parameter)[!unique(accsv$parameter)%in%lawaset]
if(length(excess)>0){
  accsv=accsv[-which(accsv$parameter%in%excess),]
}
rm(excess)
write.csv(accsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/ac.csv'),
          row.names=F)

#Bay of Plenty
boprccsv=xml2csv(agency="boprc",
                 maxHistory = 15,quiet = T,reportCensor = F)
names(boprccsv)[which(names(boprccsv)=="SWQFrequencyAll")]="Frequency"
excess=unique(boprccsv$parameter)[!unique(boprccsv$parameter)%in%lawaset]
if(length(excess)>0){
  boprccsv=boprccsv[-which(boprccsv$parameter%in%excess),]
}
rm(excess)
write.csv(boprccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/boprc.csv'),
          row.names=F)

#Canterbury
ecancsv=xml2csv(agency="ECan")
names(ecancsv)[which(names(ecancsv)=="SWQFrequencyAll")]="Frequency"
excess=unique(ecancsv$parameter)[!unique(ecancsv$parameter)%in%lawaset]
if(length(excess)>0){
  ecancsv=ecancsv[-which(ecancsv$parameter%in%excess),]
}
rm(excess)
write.csv(ecancsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/ecan.csv'),
          row.names=F)

#Horizons
hrccsv=xml2csv(agency="hrc")
names(hrccsv)[which(names(hrccsv)=="SWQFrequencyAll")]="Frequency"
excess=unique(hrccsv$parameter)[!unique(hrccsv$parameter)%in%lawaset]
if(length(excess)>0){
  hrccsv=hrccsv[-which(hrccsv$parameter%in%excess),]
}
rm(excess)
write.csv(hrccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/hrc.csv'),
          row.names=F)

#Northland 
nrccsv=xml2csv(agency="nrc")
names(nrccsv)[which(names(nrccsv)=="SWQFrequencyAll")]="Frequency"
write.csv(nrccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/nrc.csv'),
          row.names=F)

#Gisborne 
gdcsv=xml2csv(agency='gdc',maxHistory = 20)
names(gdcsv)[which(names(gdcsv)=="SWQFrequencyAll")]="Frequency"
excess=unique(gdcsv$parameter)[!unique(gdcsv$parameter)%in%lawaset]
if(length(excess)>0){
  gdcsv=gdcsv[-which(gdcsv$parameter%in%excess),]
}
rm(excess)
write.csv(gdcsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/gdc.csv'),
          row.names=F)

#Taranaki 
trccsv=xml2csv(agency='TRC')
names(trccsv)[which(names(trccsv)=="SWQFrequencyAll")]="Frequency"
excess=unique(trccsv$parameter)[!unique(trccsv$parameter)%in%lawaset]
if(length(excess)>0){
  trccsv=trccsv[-which(trccsv$parameter%in%excess),]
}
rm(excess)
write.csv(trccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/trc.csv'),
          row.names=F)

#Hawkes Bay 
hbrccsv=xml2csv(agency='HBRC')
names(hbrccsv)[which(names(hbrccsv)=="SWQFrequencyAll")]="Frequency"
excess=unique(hbrccsv$parameter)[!unique(hbrccsv$parameter)%in%lawaset]
if(length(excess)>0){
  hbrccsv=hbrccsv[-which(hbrccsv$parameter%in%excess),]
}
rm(excess)
write.csv(hbrccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/hbrc.csv'),
          row.names=F)

#Greater Wellington 
gwrccsv=xml2csv(agency="GWRC")
names(gwrccsv)[which(names(gwrccsv)=="SWQFrequencyAll")]="Frequency"
write.csv(gwrccsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/gwrc.csv'),
          row.names=F)

#Nelson
ncccsv=xml2csv(agency="NCC")
names(ncccsv)[which(names(ncccsv)=="SWQFrequencyAll")]="Frequency"
excess=unique(ncccsv$parameter)[!unique(ncccsv$parameter)%in%lawaset]
if(length(excess)>0){
  ncccsv=ncccsv[-which(ncccsv$parameter%in%excess),]
}
write.csv(ncccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/ncc.csv'),
          row.names=F)

#Tasman 
tdcsv=xml2csv(agency='TDC')
names(tdcsv)[which(names(tdcsv)=="SWQFrequencyAll")]="Frequency"
excess=unique(tdcsv$parameter)[!unique(tdcsv$parameter)%in%lawaset]
if(length(excess)>0){
  tdcsv=tdcsv[-which(tdcsv$parameter%in%excess),]
}
rm(excess)
write.csv(tdcsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/tdc.csv'),
          row.names = F)

#Marlborough 
mdccsv=xml2csv(agency="mdc")
names(mdccsv)[which(names(mdccsv)=="SWQFrequencyAll")]="Frequency"
excess=unique(mdccsv$parameter)[!unique(mdccsv$parameter)%in%lawaset]
if(length(excess)>0){
  mdccsv=mdccsv[-which(mdccsv$parameter%in%excess),]
}
write.csv(mdccsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/mdc.csv'),
          row.names=F)


#Otago 
orccsv=xml2csv(agency = 'orc')
names(orccsv)[which(names(orccsv)=="SWQFrequencyAll")]="Frequency"
excess=unique(orccsv$parameter)[!unique(orccsv$parameter)%in%lawaset]
if(length(excess)>0){
  orccsv=orccsv[-which(orccsv$parameter%in%excess),]
}
rm(excess)
write.csv(orccsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/orc.csv'),
          row.names=F)

#Environment Southland 
escsv=xml2csv(agency="ES")
names(escsv)[which(names(escsv)=="SWQFrequencyAll")]="Frequency"
write.csv(escsv,file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/es.csv'),row.names=F)

#West Coast 
wcrcsv=xml2csv(agency="WCRC")
names(wcrcsv)[which(names(wcrcsv)=="SWQFrequencyAll")]="Frequency"
excess=unique(wcrcsv$parameter)[!unique(wcrcsv$parameter)%in%lawaset]
if(length(excess)>0){
  wcrcsv=wcrcsv[-which(wcrcsv$parameter%in%excess),]
}
write.csv(wcrcsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/wcrc.csv'),
          row.names=F)

#Waikato
wrcsv=xml2csv(agency='WRC')
names(wrcsv)[which(names(wrcsv)=="SWQFrequencyAll")]="Frequency"
write.csv(wrcsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/wrc.csv'),
          row.names=F)







#Per council/measure xmlAge, start, stop, n, nSite, mean, max, min audit
library(lubridate)
nms=data.frame(council=NULL,xmlAge=NULL,var=NULL,earliest=NULL,latest=NULL,nMeas=NULL,nSite=NULL,meanMeas=NULL,maxMeas=NULL,minMeas=NULL,nNA=NULL)
for(council in c("boprc","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  xmlAge = checkXMLage(council)
  mfl=loadLatestCSV(council)
  if(!is.null(mfl)){
    newRows=data.frame(council=rep(council,length(unique(mfl$parameter))),
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
write.csv(nms,paste0("h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/WQAudit.csv"),row.names = F)



#Per site/measurement start, stop, n and range audit
nvar=length(uvars <- unique(mfl$parameter))
nsite=length(usites <- unique(mfl$SiteName))
councilDeets=as.data.frame(matrix(nrow=nvar*nsite,ncol=7))
names(councilDeets)=c("Site","Var","StartDate","EndDate","nMeas","MinVal","MaxVal")
r=1
for(ns in 1:nsite){
  for(nv in 1:nvar){
    these=which(mfl$SiteName==usites[ns]&mfl$parameter==uvars[nv])
    councilDeets[r,]=c(usites[ns],uvars[nv],
                       as.character(min(dmy(mfl$Date[these]))),
                       as.character(max(dmy(mfl$Date[these]))),
                       length(these),
                       min(mfl$Value[these],na.rm=T),max(mfl$Value[these],na.rm=T))
    r=r+1
  }
}
write.csv(councilDeets,row.names = F,paste0("h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/",council,"audit.csv"))




#Switch to using month words instead of month numbers
for(council in c("boprc","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  mfl=loadLatestCSV(council)
  mfl$Date=format.Date(strptime(mfl$Date,format="%d-%m-%Y"),"%d-%b-%Y")
  write.csv(mfl,paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/',council,'.csv'))
}
  




acSites=c(44603,45313,7830,6811,6804,6604,43829,8110,7502,7904,8219,8214,8205,7811,8217,8215,43856,43807,7506,8516,7104,7206,43601,438100,8568,45373,45505,8249,45415,8019,1043837,7171,74401,74701,"Hoteo","Kaukapakapa","Oteha","Puhinui","Rangitopuni at Walkers","Wairoa","West Hoe")


