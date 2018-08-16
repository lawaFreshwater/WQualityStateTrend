#===================================================================================================
#  LAWA DATA PREPARATION - WFS
#  Horizons Regional Council
#
#  28 August 2016
#
#  Jorn Sijbertsma
#  Sean Hodges
#  Horizons Regional Council
#===================================================================================================

# Clearing workspace
rm(list = ls())
closeAllConnections()
ANALYSIS<-"LOAD WFS"
# Set working directory

od <- getwd()
wd <- "H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state"
setwd(wd)

logfolder <- "H:/ericg/16666LAWA/2018/WaterQuality/ROutput/"

#/* -===Include required function libraries===- */ 


source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")

## Supplementary functions



ld <- function(urlIn,dataLocation,case.fix=TRUE){
  if(dataLocation=="web"){
    (download.file(urlIn,destfile="tmp1",method="wininet",quiet=T))
    if(case.fix)  cc("tmp1")
    xmlfile <- try(xmlParse(file = "tmp1"))
    unlink("tmp1")
  } else if(dataLocation=="file"){
    cc(urlIn)
    message("trying file",urlIn,"\nContent type  'text/xml'\n")
    if(grepl("xml$",urlIn)){
      xmlfile <- xmlParse(urlIn)
    } else {
      xmlfile=FALSE
    }
  }
  return(xmlfile)
}

cc <- function(file){
  x <- readLines(file)
  y <- gsub( "SITEID",            "SiteID",            x, ignore.case = TRUE  )
  y <- gsub( "ELEVATION",         "Elevation",         y, ignore.case = TRUE  )
  y <- gsub( "COUNCILSITEID",     "CouncilSiteID",     y, ignore.case = TRUE  )
  y <- gsub( "LAWASITEID",        "LawaSiteID",        y, ignore.case = TRUE  )
  y <- gsub( "SWMANAGEMENTZONE",  "SWManagementZone",  y, ignore.case = TRUE  )
  y <- gsub( "GWMANAGEMENTZONE",  "GWManagementZone",  y, ignore.case = TRUE  )
  y <- gsub( "CATCHMENT",         "Catchment",         y, ignore.case = TRUE  )
  y <- gsub( "NZREACH",           "NZReach",           y, ignore.case = TRUE  )
  y <- gsub( "DESCRIPTION",       "Description",       y, ignore.case = TRUE  )
  y <- gsub( "PHOTOGRAPH",        "Photograph",        y, ignore.case = TRUE  )
  y <- gsub( "SWQUALITY",         "SWQuality",         y, ignore.case = TRUE  )
  y <- gsub( "SWQUALITYSTART",    "SWQualityStart",    y, ignore.case = TRUE  )
  y <- gsub( "SWQFREQUENCYALL",   "SWQFrequencyAll",   y, ignore.case = TRUE  )
  y <- gsub( "SWQFREQUENCYLAST5", "SWQFrequencyLast5", y, ignore.case = TRUE  )
  y <- gsub( "SWQALTITUDE",       "SWQAltitude",       y, ignore.case = TRUE  )
  y <- gsub( "SWQLANDUSE",        "SWQLanduse",        y, ignore.case = TRUE  )
  y <- gsub( "RWQUALITY",         "RWQuality",         y, ignore.case = TRUE  )
  y <- gsub( "RWQUALITYSTART",    "RWQualityStart",    y, ignore.case = TRUE  )
  y <- gsub( "LWQUALITY",         "LWQuality",         y, ignore.case = TRUE  )
  y <- gsub( "LWQUALITYSTART",    "LWQualityStart",    y, ignore.case = TRUE  )
  y <- gsub( "LTYPE",             "LType",             y, ignore.case = TRUE  )
  y <- gsub( "LFENZID",           "LFENZID",           y, ignore.case = TRUE  )
  y <- gsub( "MACRO",             "Macro",             y, ignore.case = TRUE  )
  y <- gsub( "MACROSTART",        "MacroStart",        y, ignore.case = TRUE  )
  y <- gsub( "SWQUANTITY",        "SWQuantity",        y, ignore.case = TRUE  )
  y <- gsub( "SWQUANTITYSTART",   "SWQuantityStart",   y, ignore.case = TRUE  )
  y <- gsub( "REGION",            "Region",            y, ignore.case = TRUE  )
  y <- gsub( "AGENCY",            "Agency",            y, ignore.case = TRUE  ) 
  y <- gsub( "ns2.",              "",                  y, ignore.case = TRUE  ) 
  y <- gsub( "ns3.",              "",                  y, ignore.case = TRUE  ) 
  
  writeLines(y,file)
  
}


# ======================================
# Load WFS locations from CSV

## Load csv with WFS addresses
urls2018      <- "H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/CouncilWFS.csv"  #H:\ericg\16666LAWA\2018\WaterQuality\R\lawa_state
urls          <- read.csv(urls2018,stringsAsFactors=FALSE)

#url <- "https://hbrcwebmap.hbrc.govt.nz/arcgis/services/emar/MonitoringSiteReferenceData/MapServer/WFSServer?request=GetFeature&service=WFS&typename=MonitoringSiteReferenceData&srsName=urn:ogc:def:crs:EPSG:6.9:4326"
#url = "http://gis.horizons.govt.nz/arcgis/services/emar/MonitoringSiteReferenceData/MapServer/WFSServer?request=GetFeature&service=WFS&typename=MonitoringSiteReferenceData"
#http://hilltop.nrc.govt.nz/PublicTelemetry.hts?Service=WFS&request=GetFeature&TypeName=MonitoringSiteReferenceData&version=1.1.0
# Config for data extract from WFS
WQvars <- c("CouncilSiteID","LawaSiteID","SiteID","SWQuality","SWQAltitude","SWQLanduse",
            "SWQFrequencyAll","SWQFrequencyLast5",
            "Region","Agency")
#GetFeatures from amazon
# http://ec2-52-6-196-14.compute-1.amazonaws.com/sos-bop/service?service=SOS&request=GetFeatureOfInterest&version=2.0.0


### Even though the field names have been defined in the documentation, there are still differences in Field Names specified by each Council
### Either 
###  1. Define a method that determines the name of the elements in each WFS feed; OR
###  2. Note discrepencies as ERRORS and feedback to supplying Council.

### We'll go with option 2 for the moment.

### LOG START: output to ROutput folder
# logfolder <- "H:/ericg/16666LAWA/2018/WaterQuality/ROutput/"
logfile <- paste(logfolder,"lawa_dataPrep_WFS.log",sep="")
sink(logfile)
###

rm(siteTable)
h=1
for(h in h:length(urls$URL)){
  if(grepl("^x", urls$Agency[h])){
    next
  } 
  # Fixing case issue with attribute names with WRC
  if(urls$Agency[h]=="WRC"){
    xmldata<-try(ld(urlIn = urls$URL[h],dataLocation = urls$Source[h],case.fix = TRUE))
  } else{
    xmldata<-try(ld(urlIn = urls$URL[h],dataLocation = urls$Source[h],case.fix = FALSE))
  }
  
  if('try-error'%in%attr(xmldata,'class')||
     grepl(pattern = 'error',xmlValue(getNodeSet(xmldata,'/')[[1]]))){
    cat('Failed for ',urls$Agency[h],'\n')
    next
  }
  
  if(urls$Source[h]=="file" & grepl("csv$",urls$URL[h])){
    # northland RC data currently in XML as at 16-Aug-2018
    # Load CSV and append it to siteTable dataframe
    cc(urls$URL[h])
    tmp <- read.csv(urls$URL[h],stringsAsFactors=FALSE,strip.white = TRUE,sep=",")
    
    tmp <- tmp[,c(5,7,8,9,30,10,28,29,20,21,22)]
    tmp$Lat <- sapply(strsplit(as.character(tmp$pos),' '), "[",1)
    tmp$Long <- sapply(strsplit(as.character(tmp$pos),' '), "[",2)
    tmp <- tmp[-11]
    if(!exists("siteTable")){
      siteTable<-as.data.frame(tmp,stringsAsFactors=FALSE)
    } else{
      siteTable<-rbind.data.frame(siteTable,tmp,stringsAsFactors=FALSE)
    }
    rm(tmp)
  }else {
    ### Determine the values used in the [emar:SWQuality] element
    emarSTR="emar:"
    swq<-unique(sapply(getNodeSet(doc=xmldata, path=paste0("//",emarSTR,"MonitoringSiteReferenceData/emar:SWQuality")), xmlValue))
    if(any(swq=="")){
      swq=swq[-which(swq=='')]
    }
    if(length(swq)==0){
      swq<-unique(sapply(getNodeSet(doc=xmldata, path="//MonitoringSiteReferenceData/emar:SWQuality"), xmlValue))
      if(any(swq=="")){
        swq=swq[-which(swq=='')]
      }
      if(length(swq)>0){
        emarSTR=""
      }
    }
    
    # since it appears that the possible values for Yes,No, True, False, Y, N, T,F, true, false, yes, no all have the
    # sample alphabetic order, Y, Yes, y, yes, True, true, T, t are always going to be item 2 in this character vector.
    # Handy.
    # Enforcing order in swq
    swq<-swq[order(swq,na.last = TRUE)]
    
    if(length(swq)==2){
      module <- paste("[emar:SWQuality='",swq[2],"']",sep="")
    } else {
      if(all(swq%in%c("NO","Yes","YES"))){   #This copes specifically with ecan, which had these three present
        module <- paste("[emar:SWQuality=",c("'Yes'","'YES'")[which(c("Yes","YES")%in%swq)],"]",sep='')
      }else{
        module <- paste("[emar:SWQuality='",swq,"']",sep="")
      }
    }
    
    #xmltop<-xmlRoot(xmldata)
    #c <- length(xmlSApply(xmltop, xmlSize)) # number of children for i'th E Element inside <Data></Data> tags
    cat("\n",urls$Agency[h],"\n---------------------------\n",urls$URL[h],"\n",module,"\n",sep="")
    
    # Determine number of records in a wfs with module before attempting to extract all the necessary columns needed
    emarSWnodes=sapply(getNodeSet(doc=xmldata, 
                                  path=paste0("//",emarSTR,"MonitoringSiteReferenceData",
                                              module,"/emar:",WQvars[1])),xmlValue)
    if(length(emarSWnodes)==0){
      cat(urls$Agency[h],"has no records for <emar:SWQuality>\n")
    } else {
      # We declared WQvars earlier. Next section of code goes and gets these values from the WFS
      # in sequence
      
      for(i in 1:length(WQvars)){
        if(i==1){
          # for the first WQVar, the LAWASiteID
          a<- unique(sapply(getNodeSet(doc=xmldata, 
                                       path=paste0("//emar:LawaSiteID/../../",
                                                   emarSTR,"MonitoringSiteReferenceData",
                                                   module,"/emar:",WQvars[i])),xmlValue))
          cat(WQvars[i],":\t",length(a),"\n")
          #Cleaning var[i] to remove any leading and trailing spaces
          a=trimws(a)
          nn <- length(a)
          theseSites=a
        } else {
          # for all subsequent URL's
          # b<- sapply(getNodeSet(doc=xmldata, 
          #                       path=paste("//emar:LawaSiteID/../../",emarSTR,"MonitoringSiteReferenceData",
          #                                  module,"/emar:",WQvars[i],sep="")),
          #            xmlValue)
          
          #Get the new parameter for each site already obtained.  
          #If the new parameter is not there for a certain site, it will give it an NA 
          for(thisSite in 1:length(theseSites)){
            newb<- sapply(getNodeSet(doc=xmldata, 
                                     path=paste0("//emar:LawaSiteID/../../",
                                                 emarSTR,"MonitoringSiteReferenceData[emar:CouncilSiteID='",
                                                 theseSites[thisSite],"'] ",module,"/emar:",WQvars[i])),
                          xmlValue)
            newb=unique(newb)
            if(any(newb=="")){
              newb=newb[-which(newb=="")]
            }
            if(length(newb)==0){
              newb=NA
            }
            if(length(newb)>1){
              if(length(unique(gsub(x = newb,pattern=" at| @| Road| Rd",replacement='')))==1){
                newb=newb[1]
              }else{
                browser()
                newb=paste(newb,collapse=' OR ')
              }
            }
            if(thisSite==1){
              b=newb
            }else{
              b=c(b,newb)
            }
          }
          
          
          cat(WQvars[i],":\t",length(b),"\n")
          if(any(is.na(b))){
            if(WQvars[i]=="Region"){
              b[is.na(b)] <-urls$Agency[h]#stopGapNames[stopGapNames$Agency==urls$Agency[h],2]
            } else if(WQvars[i]=="Agency"){
              b[is.na(b)]<-urls$Agency[h]#stopGapNames[stopGapNames$Agency==urls$Agency[h],1]
            } else {
              b[is.na(b)]<-""
            }
          }
          
          
          if(!i%in%c(1,2,3)){
            #Cleaning b to remove any leading and trailing spaces
            b=trimws(tolower(b))
          }
          
          if(is.matrix(a)){
            nra=nrow(a)
          }else{
            nra=length(a)
          }
          nrb=length(b)
          
          if(nrb!=nra & length(unique(b))==1){
            browser()
            b=rep(unique(b),nra)
          }
          
          wn=options('warn')$warn
          options(warn=2)
          a <- cbind(unlist(a),unlist(b))
          options(warn=wn)
          rm(wn)
        }
      }
      
      a <- as.data.frame(a,stringsAsFactors=FALSE)
      ### grab the latitude and longitude values (WFS version must be 1.1.0)
      latlong <- sapply(getNodeSet(doc=xmldata, 
                                   path=paste0("//gml:Point[../../../",
                                               emarSTR,"MonitoringSiteReferenceData",
                                               module,"]")),
                        xmlValue)
      latlong <- sapply(getNodeSet(doc=xmldata, 
                                   path=paste0("//gml:Point[../../emar:LawaSiteID/../../",
                                               emarSTR,"MonitoringSiteReferenceData",
                                               module,"]")),
                        xmlValue)
      if(length(latlong)>0){
        latlong <- simplify2array(strsplit(latlong," "))
      }else{
        latlong=matrix(data = NA,nrow = 1,ncol=2)
      }
      
      llSiteName <- sapply(getNodeSet(doc=xmldata, 
                                      path=paste0("//gml:Point[../../emar:LawaSiteID/../../",
                                                  emarSTR,"MonitoringSiteReferenceData",
                                                  module,"]","/../../../",
                                                  emarSTR,"MonitoringSiteReferenceData/emar:CouncilSiteID")),
                           xmlValue)
      rm(b,xmldata)
      if(nrow(a)==length(latlong[1,])){
        a <- cbind.data.frame(a,as.numeric(latlong[1,]),as.numeric(latlong[2,]))
      } else {
        # browser()
        b <- as.data.frame(matrix(latlong,ncol=2,nrow=length(latlong[1,]),byrow=TRUE))
        stopifnot(length(llSiteName)==dim(b)[1])
        b <- cbind.data.frame(b,llSiteName,stringsAsFactors=FALSE)
        names(b) <- c("Lat","Long","CouncilSiteID")
        #Cleaning CouncilSiteID to remove any leading and trailing spaces
        b$CouncilSiteID <- trimws(b$CouncilSiteID)
        #b$SiteID <- trimws(b$SiteID)
        
        cat("Only",length(latlong[1,]),"out of",nrow(a),"sites with lat-longs.\nSome site locations missing\n")
        
        #if(h==11){  # Change back to 11 once BOPRC included again
        if(h==12){  # Northland - might be case for all other councils too. Verify
          a <- merge(x = a,y = b,by.x="V3",by.y="CouncilSiteID",all.x=TRUE)
        } else {        
          a <- merge(a,b,by.x="V1",by.y="CouncilSiteID",all.x=TRUE)
        }
        
      }
      rm(latlong) 
      
      #a<-as.data.frame(a,stringsAsFactors=FALSE)
      names(a)<-c(WQvars,"Lat","Long")
      if(!exists("siteTable")){
        siteTable<-as.data.frame(a,stringsAsFactors=FALSE)
      } else{
        siteTable<-rbind.data.frame(siteTable,a,stringsAsFactors=FALSE)
      }
      rm(a)
    }
    cat("\n---------------------------\n\n",sep="")
  }
  
}

### LOG FINISH: output to ROutput folder
sink()
###

table(siteTable$Agency)

# For some reason, the lat/longs for Wairua at Purua are being loaded into columns as a 2 item vector - the first item being NA, the second being the value
# The next two lines sort the problem, but if the problem should be resolved, these two lines should error.
#siteTable$Lat[siteTable$LawaSiteID=="NRWQN-00022"] <- siteTable$Lat[siteTable$LawaSiteID=="NRWQN-00022"][2]
#siteTable$Long[siteTable$LawaSiteID=="NRWQN-00022"] <-siteTable$Long[siteTable$LawaSiteID=="NRWQN-00022"][2]

#dropping Northland for the timebeing 7-Sep-2016
#siteTable <- siteTable[siteTable$Region!="Northland",]

#Converting values in the frequency columns to Title case
#From http://www.johnmyleswhite.com/notebook/2009/02/25/text-processing-in-r/
pseudo.titlecase = function(str)
{
  substr(str, 1, 1) = toupper(substr(str, 1, 1))
  return(str)
}


siteTable$SWQFrequencyAll   <- pseudo.titlecase(siteTable$SWQFrequencyAll )
siteTable$SWQFrequencyLast5 <- pseudo.titlecase(siteTable$SWQFrequencyLast5 )


## Changing BOP Site names that use extended characters
## Waiōtahe at Toone Rd             LAWA-100395   Waiotahe at Toone Rd 
## Waitahanui at Ōtamarākau Marae   EBOP-00038    Waitahanui at Otamarakau Marae
siteTable$SiteID[siteTable$LawaSiteID=="LAWA-100395"] <- "Waiotahe at Toone Rd"
siteTable$SiteID[siteTable$LawaSiteID=="EBOP-00038"] <- "Waitahanui at Otamarakau Marae"
## A better solution would be to deal directly with the characters and bulk convert to plain ascii text, rather than simply
## discovering sites with issues and renaming them manually

siteTable=unique(siteTable)
toPull = which(duplicated(siteTable[,-c(11,12)]))
if(length(toPull)>0){
  siteTable=siteTable[-toPull,]
}
rm(toPull)

## Swapping coordinate values for Agency=Environment Canterbury Regional Council, Christchurch

toSwitch=which(siteTable$Long<0 & siteTable$Lat>0)
unique(siteTable$Agency[toSwitch])
newLon=siteTable$Lat[toSwitch]
siteTable$Lat[toSwitch] <- siteTable$Long[toSwitch]
siteTable$Long[toSwitch]=newLon
rm(newLon,toSwitch)
plot(siteTable$Long,siteTable$Lat,col=as.numeric(factor(siteTable$Agency)))
points(siteTable$Long,siteTable$Lat,pch=16,cex=0.2)
table(siteTable$Agency)


# Hey Eric,
# I think SQ30305 is for macroinvertebrates where as STYX05 is for the water quality data – there may be a few with the same issues 😊
# I hope that makes sense?
# Emily 

#siteTable$Long[siteTable$LawaSiteID=="NRWQN-00022"] <-siteTable$Long[siteTable$LawaSiteID=="NRWQN-00022"][2]

# For WCRC-00031 - location is wrong in WFS
# NZTM coordinates from WCRC website: 1466541,5295450
# WGS84, now:   Latitude	Longitude  	-42.48179737	171.37623113

# siteTable$Lat[siteTable$LawaSiteID=="WCRC-00031"]  <- -42.48179737
# siteTable$Long[siteTable$LawaSiteID=="WCRC-00031"] <- 171.37623113
by(INDICES = siteTable$Agency,data = siteTable,FUN = function(x)head(x))
## Output for next script
write.csv(x = siteTable,file = "H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",row.names = F)
write.csv(x = siteTable,file = "H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_WFS_PULL_River.csv",row.names = F)



