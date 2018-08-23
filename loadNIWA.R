## Import data from Council Hilltop Server

## Export to a Hilltop XML file with all supplied data intact.

## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data

## SET LOCAL WORKING DIRECTORY
setwd("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state")


## Load libraries ------------------------------------------------
require(XML)     ### XML library to write hilltop XML
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(RCurl)


#function to create xml file from url. 
ld <- function(url){
  str<- tempfile(pattern = "file", tmpdir = tempdir())
  (download.file(url,destfile=str,method="wininet",quiet=T))
  xmlfile <- xmlParse(file = str)
  unlink(str)
  return(xmlfile)
}

#function to determine which created xmls have an error message.
#I/e/ the measurement value does not exist for that site. 
htsServiceError <- function(url){
  xmldata <- ld(url)
  error<-as.character(sapply(getNodeSet(doc=xmldata, path="//Error"), xmlValue))
  if(length(error)==0){
    return(xmldata)   # if no error, return xml data
  } else {
    return(NULL)
  }
}

#function to either create full xml file or return xml file as NULL depending
#on the result from the above funciton
requestData <- function(url){
  ret <- htsServiceError(url)
  if(!is.null(ret)){
    return(ret)
  } else {
    return(NULL)
  }
}

## ===============================================================================
## Getting Site Data 

## Build XML Document --------------------------------------------
tm<-Sys.time()
tab="\t"
cat("Building XML\n")
cat("Creating:",Sys.time()-tm,"\n")

con <- xmlOutputDOM("Hilltop")
con$addTag("Agency", "NIWA")

# temp=readLines("file:///H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/NIWAwqData.xml",n = -1)
try( xmlfile <- xmlParse(file = "file:///H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/NIWAwqData.xml"))

if(is.null(xmlfile)){
  url <- "http://gs.niwa.co.nz/nemo/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=nemo:fchem_chemistry"
  xmlfile <- requestData(url)
}

if(!is.null(xmlfile)){
  xmltop<-xmlRoot(xmlfile)
  
  m<-xmltop[['featureMember']]
  NIWAdataIn=data.frame(location_uri = rep("",26543), site_ident = rep("",26543), site_desc = rep("",26543), 
                       region = rep("",26543), catchment_area_km2 = rep("",26543), catchment_height_m = rep("",26543), 
                       altitude = rep("",26543), start_date = rep("",26543), start_time = rep("",26543),
                       wq_temperature = rep("",26543), 
                       wq_dissolved_oxygen_percent = rep("",26543), wq_dissolved_oxygen = rep("",26543), 
                       wq_instantaneous_discharge = rep("",26543), wq_visual_clarity_distance = rep("",26543), 
                       wq_turbidity_ntu = rep("",26543), wq_ph = rep("",26543), wq_conductivity = rep("",26543), 
                       wq_ammoniacal_nitrogen = rep("",26543), wq_nitrate_nitrite = rep("",26543), wq_total_nitrogen = rep("",26543), 
                       wq_dissolved_phosphorus = rep("",26543), wq_total_phosphorus = rep("",26543),
                       wq_absorption_coeff_340 = rep("",26543), 
                       wq_absorption_coeff_440 = rep("",26543), wq_absorbance_340 = rep("",26543), wq_absorbance_440 = rep("",26543), 
                       wq_absorbance_740 = rep("",26543), wq_oxygen_demand = rep("",26543), wq_total_coliforms = rep("",26543), 
                       wq_e_coli = rep("",26543), wq_calcium = rep("",26543), wq_magnesium = rep("",26543), wq_sodium = rep("",26543), 
                       wq_potassium = rep("",26543), wq_total_alkalinity = rep("",26543), wq_chloride = rep("",26543), 
                       wq_sulphate = rep("",26543), longitude = rep("",26543), latitude = rep("",26543), datasource = rep("",26543), 
                       date_uploaded = rep("",26543), metadata_uuid = rep("",26543), geom = "",stringsAsFactors = F)
  lineIn=1
  while(!is.null(m)){
    cat('.')
    newNode=xmlToDataFrame(m,stringsAsFactors = F)
    NIWAdataIn[lineIn,]=newNode
    lineIn=lineIn+1
    m <- getSibling(m)
  }
}
rm(newNode,m,lineIn,xmltop)
mtRows=which(apply(NIWAdataIn,1,FUN=function(x){sum(x=="")==length(x)}))
if(length(mtRows)>0){
  NIWAdataIn=NIWAdataIn[-mtRows,]
}
rm(mtRows)
  
NIWAdataIn$date=lubridate::ymd(NIWAdataIn$start_date)
NIWAdataIn=NIWAdataIn[NIWAdataIn$date>lubridate::ymd("2003-01-01"),]

 # save(NIWAdataIn,file="h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/NIWAwqData.rData")
 # write.csv(NIWAdataIn,file="h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/NIWAwqData.csv",row.names = F)
  																																			
lawaset=c("NH4", "TURB", "BDISC",  "DRP",  "ECOLI",  "TN",  "TP",  "TON",  "PH")

lawaIDs=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/LAWAMasterSiteListasatMarch2018.csv",stringsAsFactors = F)
lawaIDs$Lat=as.numeric(lawaIDs$Latitude)
lawaIDs$Long=as.numeric(lawaIDs$Longitude)
sum(is.na(lawaIDs$Lat))
sum(is.na(lawaIDs$Long))
lawaIDs=lawaIDs[lawaIDs$Module=="Freshwater Quality",]


niwaSites=unique(NIWAdataIn[,c(2,3,38,39)])
niwaSites$latitude=as.numeric(niwaSites$latitude)
niwaSites$longitude=as.numeric(niwaSites$longitude)

md=rep(0,dim(niwaSites)[1])
nameMatch=rep("",dim(niwaSites)[1])
bestMatch=rep(NA,dim(niwaSites)[1])
for(nwst in 1:dim(niwaSites)[1]){
    dists=sqrt((niwaSites$latitude[nwst]-lawaIDs$Lat)^2+(niwaSites$longitude[nwst]-lawaIDs$Long)^2)
    cat(min(dists,na.rm=T)*111000,'\t')
    bestMatch[nwst]=which.min(dists)
    md[nwst]=min(dists,na.rm=T)
    nameMatch[nwst]=lawaIDs$SiteName[which.min(dists)]
}
mean(md[md>0])*111000 #107
cbind(niwaSites[,c(1,2)],nameMatch,md*111000)[md>0,]
niwaSites$LawaSiteID=lawaIDs$LawaID[bestMatch]
niwaSites$LawaSiteName=lawaIDs$SiteName[bestMatch]
niwaSites$DistToLawaSite=round(md*111000,1)

niwaSites[sort.int(niwaSites$DistToLawaSite,decreasing = F,index.return = T)$ix,]

NIWAdataIn$LawaSiteID = niwaSites$LawaSiteID[match(NIWAdataIn$site_desc,niwaSites$site_desc)]
NIWAdataIn$SiteName = niwaSites$LawaSiteName[match(NIWAdataIn$site_desc,niwaSites$site_desc)]
rm(niwaSites,nwst,md,bestMatch,nameMatch,dists)

NIWAdataIn$SWQuality="yes"
NIWAdataIn$LandUse=NA
NIWAdataIn$Agency="NIWA"
NIWAdataInB <- NIWAdataIn%>%dplyr::rename(CouncilSiteID=site_desc,
                                          LawaSiteID=LawaSiteID,
                                          SiteID=site_ident,
                                          Date=start_date,
                                          SWQuality=SWQuality,
                                          SWQAltitude=altitude,
                                          SWQLanduse=LandUse,
                                          Region=region,
                                          Lat=latitude,
                                          Long=longitude,
                                          NH4=wq_ammoniacal_nitrogen,
                                          TURB=wq_turbidity_ntu,
                                            BDISC=wq_visual_clarity_distance,
                                            DRP=wq_dissolved_phosphorus,
                                            ECOLI=wq_e_coli,
                                            TN=wq_total_nitrogen,
                                            TP=wq_total_phosphorus,
                                            TON=wq_nitrate_nitrite,
                                            PH=wq_ph)%>%
  dplyr::select(CouncilSiteID,LawaSiteID,SiteID,SiteName,Date,SWQuality,SWQAltitude,SWQLanduse,Region,Agency,Lat,Long, #Identifiers
                NH4, TURB, BDISC,  DRP,  ECOLI,  TN,  TP,  TON,  PH)%>% #Data
tidyr::gather(parameter,Value,NH4:PH)

rm(NIWAdataIn)

NIWAdataInB$Censored=F
NIWAdataInB$CenType=F

cenL=grepl(pattern = '<',NIWAdataInB$Value)
cenR=grepl(pattern = '>',NIWAdataInB$Value)
NIWAdataInB$Censored[cenL|cenR]=T
NIWAdataInB$CenType[cenL]="L"
NIWAdataInB$CenType[cenR]="R"

NIWAdataInB$Value[cenL|cenR]=substr(NIWAdataInB$Value[cenL|cenR],2,nchar(NIWAdataInB$Value[cenL|cenR]))
NIWAdataInB$Value=as.numeric(NIWAdataInB$Value) #NAs are due to "missing"
NIWAdataInB$SWQAltitude=as.numeric(NIWAdataInB$SWQAltitude) 
NIWAdataInB$Lat=as.numeric(NIWAdataInB$Lat) 
NIWAdataInB$Long=as.numeric(NIWAdataInB$Long) 


NIWAdataInB$Date=format.Date(strptime(NIWAdataInB$Date,format="%Y-%m-%d"),"%d-%b-%Y")
  
NIWAdataInB$Method=NA

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
niwaSites=unique(NIWAdataInB$CouncilSiteID)
for(ns in 1:length(niwaSites)){
  siteDates = unique(sort(strptime(NIWAdataInB$Date[NIWAdataInB$CouncilSiteID==niwaSites[ns]],'%d-%b-%Y')))
  cat(niwaSites[ns],'\t\t',Mode(diff(siteDates)),'\n')
}

NIWAdataInB$Frequency="Monthly"
NIWAdataInB$SWQFrequencyLast5="Monthly"


 save(NIWAdataInB,file="h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/NIWAwqDataB.rData")
 write.csv(NIWAdataInB,file="h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/NIWAwqDataB.csv",row.names = F)

 
 
siteTableWithNIWA=merge(x=siteTable,y=unique(NIWAdataInB[,which(names(NIWAdataInB)%in%names(siteTable))]),all.x=T,all.y=T)
 
write.csv(siteTableWithNIWA,file="h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/SiteTableWithNIWA.csv",row.names = F)

