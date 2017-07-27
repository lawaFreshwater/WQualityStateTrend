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

ANALYSIS<-"LOAD WFS"
# Set working directory

od <- getwd()
wd <- "\\\\file\\herman\\R\\OA\\08\\02\\2017\\Water Quality\\R\\lawa_state"
setwd(wd)

logfolder <- "\\\\file\\herman\\R\\OA\\08\\02\\2017\\Water Quality\\ROutput\\"

#/* -===Include required function libraries===- */ 


source("scripts/WQualityStateTrend/lawa_state_functions.R")

## Supplementary functions



ld <- function(url,dataLocation,case.fix=TRUE){
  if(dataLocation=="web"){
    (download.file(url,destfile="tmp1",method="wininet"))
    if(case.fix)  cc("tmp1")
    xmlfile <- xmlParse(file = "tmp1")
    unlink("tmp1")
  } else if(dataLocation=="file"){
    cc(url)
    message("trying file",url,"\nContent type  'text/xml'\n")
    if(grepl("xml$",url)){
      xmlfile <- xmlParse(url)
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
urls2017      <- "//file/herman/R/OA/08/02/2017/Water Quality/R/lawa_state/CouncilWFS.csv"
urls          <- read.csv(urls2017,stringsAsFactors=FALSE)
#urls2016      <- "//file/herman/R/OA/08/02/2016/Water Quality/R/lawa_state/CouncilWFS.csv"
#urls          <- read.csv(urls2016,stringsAsFactors=FALSE)
stopGapNames  <- read.csv("agencyRegion.csv",stringsAsFactors=FALSE)

# Drop BOPRC - GIS Server erroring
#urls <- urls[-2,]
# Drop GDC - Error on SErver
#urls <- urls[-5,]

#url <- "https://hbrcwebmap.hbrc.govt.nz/arcgis/services/emar/MonitoringSiteReferenceData/MapServer/WFSServer?request=GetFeature&service=WFS&typename=MonitoringSiteReferenceData&srsName=urn:ogc:def:crs:EPSG:6.9:4326"
#url = "http://gis.horizons.govt.nz/arcgis/services/emar/MonitoringSiteReferenceData/MapServer/WFSServer?request=GetFeature&service=WFS&typename=MonitoringSiteReferenceData"

# Config for data extract from WFS
vars <- c("SiteID","CouncilSiteID","LawaSiteID","SWQuality","SWQAltitude","SWQLanduse",
          "SWQFrequencyAll","SWQFrequencyLast5","Region","Agency")



### Even though the field names have been defined in the documentation, there are still differences in Field Names specified by each Council
### Either 
###  1. Define a method that determines the name of the elements in each WFS feed; OR
###  2. Note discrepencies as ERRORS and feedback to supplying Council.

### We'll go with option 2 for the moment.

### LOG START: output to ROutput folder
logfile <- paste(logfolder,"lawa_dataPrep_WFS.log",sep="")
sink(logfile)
###


for(h in 1:length(urls$URL)){

  if(grepl("^x", urls$Agency[h])){
    next
  } 
  if(!nzchar(urls$URL[h])){  ## returns false if the URL string is missing
    next
  }
  #if(h==12){
  #  next()
  #}

  xmldata<-ld(urls$URL[h],urls$Source[h])
  
  if(urls$Source[h]=="file" & grepl("csv$",urls$URL[h])){
    # Waikato RC data currently in CSV as at 7-Sep-20116
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
    
    
  } else {
    ### Determine the values used in the [emar:SWQuality] element
    swq<-unique(sapply(getNodeSet(doc=xmldata, path="//emar:MonitoringSiteReferenceData/emar:SWQuality"), xmlValue))
    # since it appears that the possible values for Yes,No, True, False, Y, N, T,F, true, false, yes, no all have the
    # sample alphabetic order, Y, Yes, y, yes, True, true, T, t are always going to be item 2 in this character vector.
    # Handy.
    # Enforcing order in swq
    swq<-swq[order(swq,na.last = TRUE)]
    
    if(length(swq)==2){
      module <- paste("[emar:SWQuality='",swq[2],"']",sep="")
    } else {
      module <- paste("[emar:SWQuality='",swq,"']",sep="")
    }
      
    #xmltop<-xmlRoot(xmldata)
    #c <- length(xmlSApply(xmltop, xmlSize)) # number of children for i'th E Element inside <Data></Data> tags
    cat("\n",urls$Agency[h],"\n---------------------------\n",urls$URL[h],"\n",module,"\n",sep="")
  
    # Determine number of records in a wfs with module before attempting to extract all the necessary columns needed
    if(length(sapply(getNodeSet(doc=xmldata, 
                          path=paste("//emar:MonitoringSiteReferenceData",module,"/emar:",vars[1],sep="")), xmlValue))==0){
      cat(urls$Agency[h],"has no records for <emar:SWQuality>\n")
  
    } else {
    

      # We declared vars earlier. Next section of code goes and gets these values from the WFS
      # in sequence
      #vars <- c("SiteID","CouncilSiteID","LawaSiteID","SWQuality","SWQAltitude","SWQLanduse",
      #          "SWQFrequencyAll","SWQFrequencyLast5","Region","Agency")

      for(i in 1:length(vars)){
        
        if(i==1){
          # for the first URL
          a<- sapply(getNodeSet(doc=xmldata, 
                                path=paste("//emar:LawaSiteID/../../emar:MonitoringSiteReferenceData",module,"/emar:",vars[i],sep="")), xmlValue)
          cat(vars[i],":\t",length(a),"\n")
          #Cleaning var[i] to remove any leading and trailing spaces
          trimws(a)
          nn <- length(a)
        } else {
          # for all subsequent URL's
         
          b<- sapply(getNodeSet(doc=xmldata, 
                                path=paste("//emar:LawaSiteID/../../emar:MonitoringSiteReferenceData",module,"/emar:",vars[i],sep="")), xmlValue)
          cat(vars[i],":\t",length(b),"\n")
          if(length(b)==0){
            if(vars[i]=="Region"){
              b[1:nn] <-stopGapNames[stopGapNames$Agency==urls$Agency[h],2]
            } else if(vars[i]=="Agency"){
              b[1:nn]<-stopGapNames[stopGapNames$Agency==urls$Agency[h],1]
            } else {
              b[1:nn]<-""
            }
          }

          #Cleaning b to remove any leading and trailing spaces
          trimws(b)
          
          a <- cbind(unlist(a),unlist(b))
        }
    
      }
      a <- as.data.frame(a,stringsAsFactors=FALSE)
      ### grab the latitude and longitude values (WFS version must be 1.1.0)
      latlong    <- sapply(getNodeSet(doc=xmldata, 
                            path=paste("//gml:Point[../../../emar:MonitoringSiteReferenceData",module,"]",sep="")), xmlValue)
      
      latlong    <- sapply(getNodeSet(doc=xmldata, 
                           path=paste("//gml:Point[../../emar:LawaSiteID/../../emar:MonitoringSiteReferenceData",module,"]",sep="")), xmlValue)
      
      
      llSiteName <- sapply(getNodeSet(doc=xmldata, 
                            path=paste("//gml:Point[../../emar:LawaSiteID/../../emar:MonitoringSiteReferenceData",module,"]",
                                       "/../../../emar:MonitoringSiteReferenceData/emar:CouncilSiteID",sep="")), xmlValue)
      latlong <- simplify2array(strsplit(latlong," "))
  
      rm(b,xmldata)
      if(nrow(a)==length(latlong[1,])){
        
        a <- cbind.data.frame(a,as.numeric(latlong[1,]),as.numeric(latlong[2,]))
        
      } else {
        b <- as.data.frame(matrix(latlong,ncol=2,nrow=length(latlong[1,]),byrow=TRUE))
        b <- cbind.data.frame(b,llSiteName,stringsAsFactors=FALSE)
        names(b) <- c("Lat","Long","CouncilSiteID")
        #Cleaning CouncilSiteID to remove any leading and trailing spaces
        b$CouncilSiteID <- trimws(b$CouncilSiteID)
        #b$SiteID <- trimws(b$SiteID)
        
        cat("Only",length(latlong[1,]),"out of",nrow(a),"sites with lat-longs.\nSome site locations missing\n")
        
        #if(h==11){  # Change back to 11 once BOPRC included again
        if(h==12){  # Northland - might be case for all other councils too. Verify
          a <- merge(a,b,by.x="V2",by.y="CouncilSiteID",all.x=TRUE)
        } else {        
          a <- merge(a,b,by.x="V1",by.y="CouncilSiteID",all.x=TRUE)
        }
        
      }
      rm(latlong)      
      #a<-as.data.frame(a,stringsAsFactors=FALSE)
      names(a)<-c(vars,"Lat","Long")
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


# 
# #Editing the BOPRC values as landuse and altitude values are missing
# boprc <- siteTable[siteTable$Agency=="BOPRC",]
# siteAltLand <- read.csv("//file/herman/r/oa/08/02/2017/Water Quality/1.AsSupplied/BOP/siteAltLand.csv")
# boprc <- merge(boprc,siteAltLand,by.x="LawaSiteID",by.y="LAWAID",all.x=TRUE)
# boprc$SWQLanduse  <- boprc$LanduseGroup
# boprc$SWQAltitude <- boprc$AltitudeGroup
# 
# boprc <- boprc[,c(1:12)]
# 
# n <- names(boprc)
# n[9] <- "Region"
# names(boprc) <- n
# 
# siteTable <- siteTable[siteTable$Region!="Bay of Plenty",]
# siteTable <- rbind(siteTable,boprc)
# 
# rm(boprc,n,siteAltLand)


#Editing the NIWA values as landuse and altitude values are missing
NIWA <- siteTable[siteTable$Agency=="NIWA",]
siteAltLand <- read.csv("//file/herman/r/oa/08/02/2017/Water Quality/0.AsSupplied/NIWA/siteAltLand.txt")
NIWA <- merge(NIWA,siteAltLand,by.x="LawaSiteID",by.y="LAWAID",all.x=TRUE)
NIWA$SWQLanduse  <- NIWA$LanduseGroup
NIWA$SWQAltitude <- NIWA$AltitudeGroup

NIWA <- NIWA[,c(1:12)]

n <- names(NIWA)
n[9] <- "Region"
names(NIWA) <- n

siteTable <- siteTable[siteTable$Agency!="NIWA",]
siteTable <- rbind(siteTable,NIWA)

rm(NIWA,n,siteAltLand)

## Changing BOP Site names that use extended characters
## Waiōtahe at Toone Rd             LAWA-100395   Waiotahe at Toone Rd 
## Waitahanui at Ōtamarākau Marae   EBOP-00038    Waitahanui at Otamarakau Marae
siteTable$SiteID[siteTable$LawaSiteID=="LAWA-100395"] <- "Waiotahe at Toone Rd"
siteTable$SiteID[siteTable$LawaSiteID=="EBOP-00038"] <- "Waitahanui at Otamarakau Marae"
## A better solution would be to deal directly with the characters and bulk convert to plain ascii text, rather than simply
## discovering sites with issues and renaming them manually



#siteTable <- read.csv(file = "LAWA_Site_Table.csv",stringsAsFactors=FALSE)
#siteTable <- siteTable[,c(2:13)]

# 
# #Editing Southland data - error in LawaSiteID for one record
# # ES-00165\nES-00165
# sum(grepl("^ES-00165",x = siteTable$LawaSiteID))
# siteTable$LawaSiteID[grepl("^ES-00165",x = siteTable$LawaSiteID)] <- "ES-00165"


## Swapping coordinate values for Agency=Environment Canterbury Regional Council, Christchurch

agencies <- c("Environment Canterbury Regional Council","Christchurch")

for(a in 1:length(agencies)){
  lon <- siteTable$Lat[siteTable$Agency==agencies[a]]
  siteTable$Lat[siteTable$Agency==agencies[a]] <- siteTable$Long[siteTable$Agency==agencies[a]]
  siteTable$Long[siteTable$Agency==agencies[a]]=lon
}

#siteTable$Long[siteTable$LawaSiteID=="NRWQN-00022"] <-siteTable$Long[siteTable$LawaSiteID=="NRWQN-00022"][2]


## Output for next script
write.csv(x = siteTable,file = "LAWA_Site_Table.csv")
#write.csv(x = siteTable,file = "LAWA_Site_Table1.csv")
write.csv(x = siteTable,file = "LAWA_Site_Table_WFS_PULL.csv")


