rm(list=ls())
library(XML)

lawaset=c("NH4", "TURB", "BDISC",  "DRP",  "ECOLI",  "TN",  "TP",  "TON",  "PH")


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
    #WFS returns a list of agencySiteID, SiteID and LawaSiteID
    #We call the SOS server with agencySiteID and it returns SiteName
    #So, the SiteNames should match the agencySiteID, right?!
    cat(length(siteNames),'\n')
    newRN=1
    for(sn in 1:length(siteNames)){
      siteDeets = grep(pattern = siteNameslc[sn],x = siteTable$agencySiteID,ignore.case=T)
      if(length(siteDeets)==0){
        cat('A')
        siteDeets = which(tolower(siteTable$agencySiteID)==tolower(siteNames[sn]))
      }
      if(length(siteDeets)>1){
        cat('B')
        siteDeets=siteDeets[1]
      }
      if(length(siteDeets)!=1){
        cat('C')
        siteDeets=which(siteNameslc[sn]==tolower(siteTable$LawaSiteID))
      }
      if(length(siteDeets)!=1){
        cat('D')
        siteDeets=which(siteNameslc[sn]==tolower(siteTable$SiteID))
      }
      if(length(siteDeets)!=1){
        #This site may have been retired or removed, it didnt come up in the WFS 
        cat('\t',siteNames[sn],'missing from the WFS feed\n')
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
          eval(parse(text=paste0('dtvv',newRN,'=dtvv')))
          newRN=newRN+1
          if(!exists('forcsv')){
            forcsv=dtvv
          }else{
            forcsv=merge(forcsv,dtvv,all=T)
          }
        }
      }
      if(!quiet){
        cat('\t',siteNames[sn],'\t',sn,' of ',length(siteNames),'\n')
      }else{cat('.')}
    }
    # forcsv=merge(x=forcsv,y=siteTable,by.x="SiteName",by.y="agencySiteID",all.x=T,suffixes = NULL)
    forcsv$parameter[tolower(forcsv$parameter) %in% tolower(transfers$nameFrom[tolower(transfers$agency) %in% tolower(agency)])] =
      transfers$nameTo[tolower(transfers$agency) %in% tolower(agency)][match(x = tolower(forcsv$parameter[tolower(forcsv$parameter) %in%
                                                                                                             tolower(transfers$nameFrom[tolower(transfers$agency) %in% tolower(agency)])]),
                                                                              table = tolower(transfers$nameFrom[tolower(transfers$agency) %in% tolower(agency)]))]
    
    names(forcsv)[which(names(forcsv)=="SWQFrequencyAll")]="Frequency"
    excess=unique(forcsv$parameter)[!unique(forcsv$parameter)%in%lawaset]
    if(length(excess)>0){
      forcsv=forcsv[-which(forcsv$parameter%in%excess),]
    }
    rm(excess)
    
    return(forcsv)
  }
}

loadLatestCSV <- function(agency=NULL,maxHistory=10){
  stepBack=0
  while(stepBack<maxHistory){
    if(dir.exists(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                         format(Sys.Date()-stepBack,"%Y-%m-%d"),'/'))){
      if(file.exists(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                            format(Sys.Date()-stepBack,"%Y-%m-%d"),'/',agency,'.csv'))){
        cat('loading',agency,'from',stepBack,'days ago,',format(Sys.Date()-stepBack,"%Y-%m-%d"),'\n')
        return(read.csv(paste0('h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',
                               format(Sys.Date()-stepBack,"%Y-%m-%d"),'/',agency,'.csv'),
                        stringsAsFactors = F))
      }
    }
    stepBack=stepBack+1
  }
  return(NULL)
}

checkReturnNames <- function(maxHistory=10,quiet=F,reportCensor=F,agency){
  #WFS returns a list of agencySiteID, SiteID and LawaSiteID
  #We call the SOS server with agencySiteID and it returns SiteName
  #So, the SiteNames should match the agencySiteID, right?!
  #What proportion do?
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
    siteNames = unique(tolower(sapply(getNodeSet(doc=xmlIn,path=paste0("//Measurement")),xmlGetAttr,name='SiteName')))
    cat(sum(siteNames%in%tolower(siteTable$agencySiteID))/length(siteNames),'\t')
    cat(sum(siteNames%in%tolower(siteTable$LAWASiteID))/length(siteNames),'\t')
    cat(sum(siteNames%in%tolower(siteTable$SiteID))/length(siteNames),'\n')
  }
}
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
accsv=xml2csv(agency="AC")
write.csv(accsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/ac.csv'),row.names=F)
# boprccsv$SiteID=ucSIDs[match(boprccsv$agencySiteID,uSIDs)]

#Bay of Plenty
boprccsv=xml2csv(agency="boprc")
write.csv(boprccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/boprc.csv'),row.names=F)

#Canterbury
ecancsv=xml2csv(agency="ECan")
write.csv(ecancsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/ecan.csv'),row.names=F)

#Gisborne 
gdcsv=xml2csv(agency='gdc',maxHistory = 20)
write.csv(gdcsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/gdc.csv'),row.names=F)

#Horizons
hrccsv=xml2csv(agency="hrc")
write.csv(hrccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/hrc.csv'),row.names=F)

#Northland 
nrccsv=xml2csv(agency="nrc")
write.csv(nrccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/nrc.csv'),row.names=F)

#Taranaki 
trccsv=xml2csv(agency='TRC')
write.csv(trccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/trc.csv'),row.names=F)

#Hawkes Bay 
hbrccsv=xml2csv(agency='HBRC')
write.csv(hbrccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/hbrc.csv'),row.names=F)

#Greater Wellington 
gwrccsv=xml2csv(agency="GWRC")
write.csv(gwrccsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/gwrc.csv'),row.names=F)

#Nelson
ncccsv=xml2csv(agency="NCC")
write.csv(ncccsv,
          file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/ncc.csv'),row.names=F)

#Tasman 
tdcsv=xml2csv(agency='TDC')
write.csv(tdcsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/tdc.csv'),row.names = F)

#Marlborough 
mdccsv=xml2csv(agency="mdc")
write.csv(mdccsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/mdc.csv'),row.names=F)

#Otago 
orccsv=xml2csv(agency = 'orc')
write.csv(orccsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/orc.csv'),row.names=F)

#Environment Southland 
escsv=xml2csv(agency="ES")
write.csv(escsv,file =paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/es.csv'),row.names=F)

#West Coast 
wcrcsv=xml2csv(agency="WCRC")
write.csv(wcrcsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/wcrc.csv'),row.names=F)

#Waikato
wrcsv=xml2csv(agency='WRC')
write.csv(wrcsv,
          file=paste0( 'h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/wrc.csv'),row.names=F)







#Per agency/measure xmlAge, start, stop, n, nSite, mean, max, min audit
library(lubridate)
nms=data.frame(agency=NULL,xmlAge=NULL,var=NULL,earliest=NULL,latest=NULL,nMeas=NULL,nSite=NULL,meanMeas=NULL,maxMeas=NULL,minMeas=NULL,nNA=NULL)
for(agency in c("ac","boprc","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  xmlAge = checkXMLage(agency)
  mfl=loadLatestCSV(agency)
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
  mfl=loadLatestCSV(agency)
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





