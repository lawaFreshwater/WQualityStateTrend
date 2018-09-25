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

setwd("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state")
logfolder <- "H:/ericg/16666LAWA/2018/WaterQuality/ROutput/"

#/* -===Include required function libraries===- */ 


source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")

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
  x <- readLines(file,encoding='UTF-8-BOM')
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
urls2018      <- "H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/CouncilWFS.csv"  
urls          <- read.csv(urls2018,stringsAsFactors=FALSE)

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
# logfile <- paste(logfolder,"lawa_dataPrep_WFS.log",sep="")
# sink(logfile)
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
            if(urls$Agency[h]=="ES"&WQvars[i]=="SWQLanduse"){
              newb<- sapply(getNodeSet(doc=xmldata, 
                                       path=paste0("//emar:LawaSiteID/../../",
                                                   emarSTR,"MonitoringSiteReferenceData[emar:CouncilSiteID='",
                                                 theseSites[thisSite],"'] ",module,"/emar:SWQLandUse")),
                            xmlValue)
            }
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
              b[is.na(b)] <-urls$Agency[h]
            } else if(WQvars[i]=="Agency"){
              b[is.na(b)]<-urls$Agency[h]
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
        b <- as.data.frame(matrix(latlong,ncol=2,nrow=length(latlong[1,]),byrow=TRUE))
        stopifnot(length(llSiteName)==dim(b)[1])
        b <- cbind.data.frame(b,llSiteName,stringsAsFactors=FALSE)
        names(b) <- c("Lat","Long","CouncilSiteID")
        #Cleaning CouncilSiteID to remove any leading and trailing spaces
        b$CouncilSiteID <- trimws(b$CouncilSiteID)

        cat("Only",length(latlong[1,]),"out of",nrow(a),"sites with lat-longs.\nSome site locations missing\n")
        
        if(h==12){  # Northland - might be case for all other councils too. Verify
          a <- merge(x = a,y = b,by.x="V3",by.y="CouncilSiteID",all.x=TRUE)
        } else {        
          a <- merge(a,b,by.x="V1",by.y="CouncilSiteID",all.x=TRUE)
        }
        
      }
      rm(latlong) 
      
      #a<-as.data.frame(a,stringsAsFactors=FALSE)
      names(a)<-c(WQvars,"Lat","Long")
      a$accessDate=format(Sys.Date(),"%d-%b-%Y")
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

rm(urls2018, urls,h,i,newb,nn,nra,nrb,swq,theseSites,thisSite,emarSTR,emarSWnodes,llSiteName,module,ANALYSIS,logfolder)

#Load Auckland metadata separately.  Special little snowflakes.
acMetaData=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/ACRiverSitesMetaData.csv",stringsAsFactors = F)
names(acMetaData)=c("CouncilSiteID","SiteID","Lat","Long","SWQAltitude","SWQLanduse","SWQFrequencyLast5","SWQFrequencyAll")
acMetaData$Region='auckland'
acMetaData$Agency='ac'
acMetaData$SWQuality='yes'
acMetaData$accessDate=format(file.info("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/ACRiverSitesMetaData.csv")$ctime,"%d-%b-%Y")


lawaIDs=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/LAWAMasterSiteListasatMarch2018.csv",stringsAsFactors = F)
lawaIDs$Lat=as.numeric(lawaIDs$Latitude)
lawaIDs$Long=as.numeric(lawaIDs$Longitude)
sum(is.na(lawaIDs$Lat))
sum(is.na(lawaIDs$Long))

md=rep(0,dim(acMetaData)[1])
nameMatch=rep("",dim(acMetaData)[1])
bestMatch=rep(NA,dim(acMetaData)[1])
for(ast in 1:dim(acMetaData)[1]){
  dists=sqrt((acMetaData$Lat[ast]-lawaIDs$Lat)^2+(acMetaData$Long[ast]-lawaIDs$Long)^2)
  cat(min(dists,na.rm=T),'\t')
  bestMatch[ast]=which.min(dists)
  md[ast]=min(dists,na.rm=T)
  nameMatch[ast]=lawaIDs$SiteName[which.min(dists)]
}
bestMatch[which(acMetaData$CouncilSiteID=="Cascades @ Whakanewha")]=which(lawaIDs$SiteName=="Cascades @ Whakanewha")#636 #Manual override because wrong lat/longs for this site in LawaSiteID table
nameMatch[which(acMetaData$CouncilSiteID=="Cascades @ Whakanewha")]=lawaIDs$SiteName[which(lawaIDs$SiteName=="Cascades @ Whakanewha")]
md[which(acMetaData$CouncilSiteID=="Cascades @ Whakanewha")]=0
mean(md)*111000 #54m
cbind(acMetaData[,1:2],nameMatch,md*111000)
sqrt((acMetaData$Lat[which(acMetaData$CouncilSiteID=="Cascades @ Whakanewha")]-
        lawaIDs$Lat[which(lawaIDs$SiteName=="Cascades @ Whakanewha")])^2+
       (acMetaData$Long[which(acMetaData$CouncilSiteID=="Cascades @ Whakanewha")]-
          lawaIDs$Long[which(lawaIDs$SiteName=="Cascades @ Whakanewha")])^2)

acMetaData$LawaSiteID=lawaIDs$LawaID[bestMatch]

siteTable <- merge(siteTable,acMetaData,all=T)%>%select(c("CouncilSiteID", "LawaSiteID", "SiteID", "SWQuality", "SWQAltitude", 
                                                          "SWQLanduse", "SWQFrequencyAll", "SWQFrequencyLast5", "Region", 
                                                          "Agency", "Lat", "Long","accessDate"))
rm(acMetaData,nameMatch,dists,md,ast,bestMatch)
### LOG FINISH: output to ROutput folder
# sink()
###
save(siteTable,file='tempSiteTabBeforeNIWA.rData')


#Add NIWA sites separately ####

#This depends on script "LoadNIWA.r" having been run recently

NIWAdataIn=read.csv("h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/NIWAwqData.csv",stringsAsFactors=FALSE)
niwaSub=unique(NIWAdataIn[which(!NIWAdataIn$LawaSiteID%in%siteTable$LawaSiteID),which(names(NIWAdataIn)%in%names(siteTable))])
rm(NIWAdataIn)

#Drop duplicates from NIWA data
dups=names(table(niwaSub$LawaSiteID)[table(niwaSub$LawaSiteID)>1])
# siteTable[siteTable$LawaSiteID%in%dups,][order(siteTable$LawaSiteID[siteTable$LawaSiteID%in%dups]),]

for(dp in seq_along(dups)){
  these=which(niwaSub$LawaSiteID==dups[dp])
  if(!all(identical(x=niwaSub[these[1],],y=niwaSub[these[2],]))){
    for(cc in 1:dim(niwaSub)[2]){
      if(!is.numeric(niwaSub[these[1],cc])){
        if(!identical(niwaSub[these[1],cc],niwaSub[these[2],cc])){
          if(niwaSub[these[1],cc]!="" & niwaSub[these[2],cc]!=""){
            replacement=paste(niwaSub[these[1],cc],'*or*',niwaSub[these[2],cc])
            cat(niwaSub[these[1],cc],'\t',niwaSub[these[2],cc],'\t',replacement,'\n')
            niwaSub[these[1],cc] <- replacement
            niwaSub[these[2],cc] <- replacement
          }
          else{
            replacement=paste(unique(niwaSub[these,cc]),collapse='')
            niwaSub[these[2],cc]=replacement
            niwaSub[these[1],cc]=replacement
          }
        }
      }else{
        if(abs(diff(niwaSub[these,cc]))>1e-4){browser()}
        replacement=mean(niwaSub[these,cc],na.rm=T)
        niwaSub[these[1],cc] <- replacement
        niwaSub[these[2],cc] <- replacement
      }
    }
  }
}
niwaSub=unique(niwaSub)
rm(dups,replacement,cc,dp,these)

siteTable=merge(x=siteTable,y=niwaSub,all.x=T,all.y=T)
rm(niwaSub)


siteTable$Region=tolower(siteTable$Region)
siteTable$Region[siteTable$Region=='alexandra'] <- 'otago'
siteTable$Region[siteTable$Region=='dunedin'] <- 'otago'
siteTable$Region[siteTable$Region=='tekapo'] <- 'otago'
siteTable$Region[siteTable$Region=='orc'] <- 'otago'
siteTable$Region[siteTable$Region=='christchurch'] <- 'canterbury'
siteTable$Region[siteTable$Region=='gisborne'] <- 'gisborne'
siteTable$Region[siteTable$Region=='gdc'] <- 'gisborne'
siteTable$Region[siteTable$Region=='greymouth'] <- 'west coast'
siteTable$Region[siteTable$Region=='wcrc'] <- 'west coast'
siteTable$Region[siteTable$Region=='hamilton'] <- 'waikato'
siteTable$Region[siteTable$Region=='turangi'] <- 'waikato'
siteTable$Region[siteTable$Region=='wrc'] <- 'waikato'
siteTable$Region[siteTable$Region=='havelock nth'] <- 'hawkes bay'
siteTable$Region[siteTable$Region=='hbrc'] <- 'hawkes bay'
siteTable$Region[siteTable$Region=='ncc'] <- 'nelson'
siteTable$Region[siteTable$Region=='rotorua'] <- 'bay of plenty'
siteTable$Region[siteTable$Region=='wanganui'] <- 'horizons'
siteTable$Region[siteTable$Region=='gwrc'] <- 'wellington'
siteTable$Region[siteTable$Region=='whangarei'] <- 'northland'
siteTable$Region[siteTable$Region=='nrc'] <- 'northland'
siteTable$Region[siteTable$Region=='mdc'] <- 'marlborough'
siteTable$Region[siteTable$Region=='tdc'] <- 'tasman'
siteTable$Region[siteTable$Region=='trc'] <- 'taranaki'
table(siteTable$Region)


siteTable$Agency=tolower(siteTable$Agency)
siteTable$Agency[siteTable$Agency=='auckland council'] <- 'ac'
siteTable$Agency[siteTable$Agency=='christchurch'] <- 'ecan'
siteTable$Agency[siteTable$Agency=='environment canterbury'] <- 'ecan'
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


siteTable$SiteID[grepl(pattern = "[^a-z,A-Z, ,/,@,0-9,.,-,[:punct:]]",x = siteTable$SiteID)]
## Changing BOP Site names that use extended characters
## Waiōtahe at Toone Rd             LAWA-100395   Waiotahe at Toone Rd 
## Waitahanui at Ōtamarākau Marae   EBOP-00038    Waitahanui at Otamarakau Marae
siteTable$SiteID[siteTable$LawaSiteID=="LAWA-100395"] <- "Waiotahe at Toone Rd"
siteTable$SiteID[siteTable$LawaSiteID=="EBOP-00038"] <- "Waitahanui at Otamarakau Marae"
siteTable$SiteID[grepl(pattern = "[^a-z,A-Z, ,/,@,0-9,.,-,[:punct:]]",x = siteTable$SiteID)]

# siteTable$Region[siteTable$LawaSiteID=="NRWQN-00034"] <- 'southland'
# siteTable$Region[siteTable$LawaSiteID=="ES-00010"] <- 'southland'



## Swapping coordinate values for Agency=Environment Canterbury Regional Council, Christchurch

toSwitch=which(siteTable$Long<0 & siteTable$Lat>0)
if(length(toSwitch)>0){
  unique(siteTable$Agency[toSwitch])
  newLon=siteTable$Lat[toSwitch]
  siteTable$Lat[toSwitch] <- siteTable$Long[toSwitch]
  siteTable$Long[toSwitch]=newLon
  rm(newLon)
}
rm(toSwitch)
plot(siteTable$Long,siteTable$Lat,col=as.numeric(factor(siteTable$Region)))
points(siteTable$Long[siteTable$Agency=='niwa'],
       siteTable$Lat[siteTable$Agency=='niwa'],pch=16,cex=0.5,
       col=as.numeric(factor(siteTable$Region)[siteTable$Agency=='niwa']))
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

trcSites=which(siteTable$Agency=='trc')
for(siten in seq_along(trcSites)){
  if(siteTable$SWQAltitude[trcSites[siten]]=="" & length(which(siteTable$LawaSiteID==siteTable$LawaSiteID[trcSites[siten]]))>1){
    siteTable$SWQAltitude[trcSites[siten]] = unique(
      siteTable$SWQAltitude[which(siteTable$LawaSiteID == siteTable$LawaSiteID[trcSites[siten]] &
                                    siteTable$SWQAltitude!="")])
  }
  if(siteTable$SWQLanduse[trcSites[siten]]=="" & length(which(siteTable$LawaSiteID==siteTable$LawaSiteID[trcSites[siten]]))>1){
    siteTable$SWQLanduse[trcSites[siten]] = unique(
      siteTable$SWQLanduse[which(siteTable$LawaSiteID == siteTable$LawaSiteID[trcSites[siten]] &
                                   siteTable$SWQLanduse!="")])
  }
  if(siteTable$SWQFrequencyAll[trcSites[siten]]=="" & length(which(siteTable$LawaSiteID==siteTable$LawaSiteID[trcSites[siten]]))>1){
    siteTable$SWQFrequencyAll[trcSites[siten]] = unique(
      siteTable$SWQFrequencyAll[which(siteTable$LawaSiteID == siteTable$LawaSiteID[trcSites[siten]] &
                                        siteTable$SWQFrequencyAll!="")])
  }
  if(siteTable$SWQFrequencyLast5[trcSites[siten]]=="" & length(which(siteTable$LawaSiteID==siteTable$LawaSiteID[trcSites[siten]]))>1){
    siteTable$SWQFrequencyLast5[trcSites[siten]] = unique(
      siteTable$SWQFrequencyLast5[which(siteTable$LawaSiteID == siteTable$LawaSiteID[trcSites[siten]] &
                                          siteTable$SWQFrequencyLast5!="")])
  }
}

dups=names(table(siteTable$LawaSiteID)[table(siteTable$LawaSiteID)>1])
# siteTable[siteTable$LawaSiteID%in%dups,][order(siteTable$LawaSiteID[siteTable$LawaSiteID%in%dups]),]

for(dp in seq_along(dups)){
  these=which(siteTable$LawaSiteID==dups[dp])
  if(!all(identical(x=siteTable[these[1],],y=siteTable[these[2],]))){
    for(cc in 1:dim(siteTable)[2]){
      if(!is.numeric(siteTable[these[1],cc])){
        if(!identical(siteTable[these[1],cc],siteTable[these[2],cc])){
          if(!is.na(siteTable[these[1],cc]) &siteTable[these[1],cc]!="" & !is.na(siteTable[these[2],cc]) & siteTable[these[2],cc]!=""){
            replacement=paste(siteTable[these[1],cc],'*or*',siteTable[these[2],cc])
            cat(siteTable[these[1],cc],'\t',siteTable[these[2],cc],'\t',replacement,'\n')
            siteTable[these[1],cc] <- replacement
            siteTable[these[2],cc] <- replacement
          }
          else{
            replacement=paste(unique(siteTable[these[!is.na(siteTable[these,cc])],cc]),collapse='')
            siteTable[these[2],cc]=replacement
            siteTable[these[1],cc]=replacement
          }
        }
      }else{
        if(abs(diff(siteTable[these,cc]))>1e-4){browser()}
        replacement=mean(siteTable[these,cc],na.rm=T)
        siteTable[these[1],cc] <- replacement
        siteTable[these[2],cc] <- replacement
      }
    }
  }
}
siteTable=unique(siteTable)

by(INDICES = siteTable$Agency,data = siteTable,FUN = function(x)head(x))
## Output for next script

siteTable$SWQLanduse[siteTable$LawaSiteID=="TDC-00001"]="rural"
siteTable$SWQAltitude[siteTable$LawaSiteID=="TDC-00001"]="lowland"
siteTable$SWQFrequencyAll[siteTable$LawaSiteID=="TDC-00001"]="quarterly"
siteTable$SWQFrequencyLast5[siteTable$LawaSiteID=="TDC-00001"]="quarterly"

siteTable$SWQLanduse[siteTable$LawaSiteID=="TDC-00008"]="rural"
siteTable$SWQAltitude[siteTable$LawaSiteID=="TDC-00008"]="upland"
siteTable$SWQFrequencyAll[siteTable$LawaSiteID=="TDC-00008"]="quarterly"
siteTable$SWQFrequencyLast5[siteTable$LawaSiteID=="TDC-00008"]="quarterly"

siteTable$SWQLanduse[siteTable$LawaSiteID=="TDC-00013"]="rural"
siteTable$SWQAltitude[siteTable$LawaSiteID=="TDC-00013"]="lowland"
siteTable$SWQFrequencyAll[siteTable$LawaSiteID=="TDC-00013"]="quarterly"
siteTable$SWQFrequencyLast5[siteTable$LawaSiteID=="TDC-00013"]="quarterly"

siteTable$SWQLanduse[siteTable$LawaSiteID=="TDC-00014"]="rural"
siteTable$SWQAltitude[siteTable$LawaSiteID=="TDC-00014"]="lowland"
siteTable$SWQFrequencyAll[siteTable$LawaSiteID=="TDC-00014"]="quarterly"
siteTable$SWQFrequencyLast5[siteTable$LawaSiteID=="TDC-00014"]="quarterly"

siteTable$SWQLanduse[siteTable$LawaSiteID=="TDC-00016"]="rural"
siteTable$SWQAltitude[siteTable$LawaSiteID=="TDC-00016"]="lowland"
siteTable$SWQFrequencyAll[siteTable$LawaSiteID=="TDC-00016"]="quarterly"
siteTable$SWQFrequencyLast5[siteTable$LawaSiteID=="TDC-00016"]="quarterly"

siteTable$SWQLanduse[siteTable$LawaSiteID=="TDC-00017"]="rural"
siteTable$SWQAltitude[siteTable$LawaSiteID=="TDC-00017"]="lowland"
siteTable$SWQFrequencyAll[siteTable$LawaSiteID=="TDC-00017"]="quarterly"
siteTable$SWQFrequencyLast5[siteTable$LawaSiteID=="TDC-00017"]="quarterly"

siteTable$SWQAltitude[siteTable$LawaSiteID=="GDC-00005"]="lowland"

siteTable$SWQLanduse[siteTable$LawaSiteID=="ORC-00031"]="rural"
siteTable$SWQAltitude[siteTable$LawaSiteID=="ORC-00031"]="upland"

siteTable$SWQLanduse=tolower(siteTable$SWQLanduse)
siteTable$SWQAltitude=tolower(siteTable$SWQAltitude)

#Land use and altitude infos pulled from lawa backend

siteTable$SWQLanduse[tolower(siteTable$SWQLanduse)%in%c("unstated","")] <- NA
siteTable$SWQLanduse[tolower(siteTable$SWQLanduse)%in%c("forest","native","exotic","natural")] <- "forest"

siteTable$SWQAltitude[siteTable$LawaSiteID=="LAWA-00669"]='lowland'
siteTable$SWQLanduse[siteTable$LawaSiteID=="LAWA-00669"]='rural'

siteTable$Region[siteTable$LawaSiteID=="NRWQN-00035"]='taranaki'
siteTable$SWQLanduse[siteTable$LawaSiteID=="NRWQN-00035"]='rural'
siteTable$SWQAltitude[siteTable$LawaSiteID=="NRWQN-00035"]='upland'

siteTable$SWQAltitude[siteTable$LawaSiteID=="LAWA-100557"]="upland"
siteTable$SWQLanduse[siteTable$LawaSiteID=="LAWA-100557"]="forest"

siteTable$Region[siteTable$LawaSiteID=="ES-00010"]='southland'  #Note, may be the same as ES-00012
siteTable$SWQLanduse[siteTable$LawaSiteID=="ES-00010"]='rural'
siteTable$SWQAltitude[siteTable$LawaSiteID=="ES-00010"]='lowland' 

siteTable$SWQLanduse[siteTable$LawaSiteID=="EBOP-00223"]='forest'
siteTable$SWQAltitude[siteTable$LawaSiteID=="EBOP-00223"]='upland'

siteTable$SWQLanduse[siteTable$LawaSiteID=="ECAN-10028"]='rural'
siteTable$SWQAltitude[siteTable$LawaSiteID=="ECAN-10028"]='lowland'

siteTable$SWQLanduse[siteTable$LawaSiteID=="NRWQN-00036"]='rural' #Confrmed, email Fiza Hafiz 14/9/18
siteTable$SWQAltitude[siteTable$LawaSiteID=="NRWQN-00036"]='lowland'


write.csv(x = siteTable,file = "H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",row.names = F)
write.csv(x = siteTable,file = "H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_WFS_PULL_River.csv",row.names = F)



