## Import data from Council Hilltop Server

## Export to a Hilltop XML file with all supplied data intact.

## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data

message(paste("HRC: Loading data from HRC Hilltop Server"))



## SET LOCAL WORKING DIRECTORY
setwd("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state")

## Load libraries ------------------------------------------------
require(XML)     ### XML library to write hilltop XML
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(RCurl)


## To pull the data from Taranaki hilltop server, I have a config csv that contains the 
## site and measurement names

fname <- "H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/hrcSWQ_config.csv"
df <- read.csv(fname,sep=",",stringsAsFactors=FALSE)
siteTable=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors=FALSE)

configsites <- subset(df,df$Type=="Site")[,2]
configsites <- as.vector(configsites)
sites = unique(siteTable$CouncilSiteID[siteTable$Agency=='hrc'])
Measurements <- subset(df,df$Type=="Measurement")[,2]

requestData <- function(url){
  cat(url,"\n")
  xmldata <- ld(url)
  return(xmldata)
}

#function to create xml file from url. 
ld <- function(url){
  (download.file(url,destfile="tmpr",method="wininet",quiet=T))
  # pause(1)
  xmlfile <- xmlParse(file = "tmpr")
  unlink("tmpr")
  return(xmlfile)
}

pause <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}


## ===============================================================================
## Getting Site Data 

# For each council server specified...
# Assumption is that gml:pos has coordinates recorded in lat,lon order
## Build XML Document --------------------------------------------
tm<-Sys.time()
cat("Building XML\n")
cat("Creating:",Sys.time()-tm,"\n")


rm(Data)
i=1
for(i in i:length(sites)){
  cat(sites[i],i,'out of',length(sites),'\n')
  j=1
  for(j in j:length(Measurements)){
    url <- paste("http://tsdata.horizons.govt.nz/boo.hts?service=SOS&",
                 "&request=GetObservation",
                 "&FeatureOfInterest=",sites[i],
                 "&ObservedProperty=",Measurements[j],
                 "&TemporalFilter=om:phenomenonTime,2004-01-01,2018-01-01",sep="")
    url <- gsub(" ", "%20", url)
    # cat(url,"\n")
    
    
    #------------------------------------------
    
    xmlfile <- ld(url)
    
    
    if(!is.null(xmlfile)&&!grepl(pattern = 'No data',x = xmlValue(xmlRoot(xmlfile)),ignore.case = T)){
      #Create vector of times
      time <- sapply(getNodeSet(doc=xmlfile, "//wml2:time"), xmlValue)          #Create vector of  values
      value <- sapply(getNodeSet(doc=xmlfile, "//wml2:value",namespaces=c(wml2="http://www.opengis.net/waterml/2.0")), xmlValue)
      
      
      if(length(time)!=0){
        #Get QC metadata
        xPath <-"//wml2:qualifier"
        c<-getNodeSet(xmlfile,path=xPath)
        QC<-sapply(c,function(el) xmlGetAttr(el, "xlink:title")) 
        
        #Create dataframe holding both
        df <- as.data.frame(time, stringsAsFactors = FALSE)
        df$value <- value
        
        
        #Create vector of units
        myPath<-"//wml2:uom"
        c<-getNodeSet(xmlfile, path=myPath)
        u<-sapply(c,function(el) xmlGetAttr(el, "code"))
        u <-unique(u)
        
        df$Site <- sites[i]
        df$Measurement <- Measurements[j]
        if(length(u)>0){
          df$Units <- u
        }else{
          df$Units <- rep("",length(df$Site))
        }
        
        df <- df[,c(3,4,1,2,5)]
        
        if(!exists("Data")){
          Data <- df
        } else{
          Data <- rbind.data.frame(Data, df)
        }
        
      }  
      
    }
  } #j
} #i


save(Data, file="hrcout.rData")


con <- xmlOutputDOM("Hilltop")
con$addTag("Agency", "HRC")


#-------

max<-nrow(Data)
#max<-nrows(datatbl)

i<-1
#for each site
while(i<=max){
  s<-Data$Site[i]
  # store first counter going into while loop to use later in writing out sample values
  start<-i
  
  cat(i,Data$Site[i],"\n")   ### Monitoring progress as code runs
  
  while(Data$Site[i]==s){
    #for each measurement
    #cat(datatbl$SiteName[i],"\n")
    con$addTag("Measurement",  attrs=c(SiteName=Data$Site[i]), close=FALSE)
    con$addTag("DataSource",  attrs=c(Name=Data$Measurement[i],NumItems="2"), close=FALSE)
    con$addTag("TSType", "StdSeries")
    con$addTag("DataType", "WQData")
    con$addTag("Interpolation", "Discrete")
    con$addTag("ItemInfo", attrs=c(ItemNumber="1"),close=FALSE)
    con$addTag("ItemName", Data$Measurement[i])
    con$addTag("ItemFormat", "F")
    con$addTag("Divisor", "1")
    con$addTag("Units", Data$Units[i])
    con$addTag("Format", "#.###")
    con$closeTag() # ItemInfo
    con$closeTag() # DataSource
    #saveXML(con$value(), file="out.xml")
    
    # for the TVP and associated measurement water quality parameters
    con$addTag("Data", attrs=c(DateFormat="Calendar", NumItems="2"),close=FALSE)
    d<- Data$Measurement[i]
    
    cat("       - ",Data$Measurement[i],"\n")   ### Monitoring progress as code runs
    
    while(Data$Measurement[i]==d & Data$Site[i]==s){
      # for each tvp
      # Handle Greater than symbols
      
      # Handle Less than symbols  
      # if(!is.na(Data$qualifier[i])){    # this will need to be expanded to deal with range of qualifiers
      if(grepl(pattern = "^\\>",Data$value[i],perl=T)){                   ## GREATER THAN VALUES
        con$addTag("E",close=FALSE)
        con$addTag("T",Data$time[i])
        con$addTag("I1", substr(Data$value[i],2,nchar(Data$value[i])))
        con$addTag("I2", "$ND\t>\t")
        con$closeTag() # E
      } else if(grepl(pattern = "^\\<",Data$value[i],perl=T)){           ## LESS THAN VALUES
        con$addTag("E",close=FALSE)
        con$addTag("T",Data$time[i])
        con$addTag("I1", substr(Data$value[i],2,nchar(Data$value[i])))
        con$addTag("I2", "$ND\t<\t")
        con$closeTag() # E 
      } else {                                               ## UNCENSORED VALUES
        con$addTag("E",close=FALSE)
        con$addTag("T",Data$time[i])
        con$addTag("I1", Data$value[i])
        con$addTag("I2", "\t")
        con$closeTag() # E
      }
      # Write all other result values  
      # } else {                                                 ## UNCENSORED VALUES
      #   con$addTag("E",close=FALSE)
      #   con$addTag("T",Data$time[i])
      #   con$addTag("I1", Data$value[i])
      #   con$addTag("I2", "\t")
      #   con$closeTag() # E
      #   
      # }
      
      i<-i+1 # incrementing overall for loop counter
      if(i>max){break}
    }
    # next
    con$closeTag() # Data
    con$closeTag() # Measurement
    
    if(i>max){break}
    # Next 
  }
  # store last counter going out of while loop to use later in writing out sample values
  end<-i-1
  
  # Adding WQ Sample Datasource to finish off this Site
  # along with Sample parameters
  con$addTag("Measurement",  attrs=c(SiteName=Data$Site[start]), close=FALSE)
  con$addTag("DataSource",  attrs=c(Name="WQ Sample", NumItems="1"), close=FALSE)
  con$addTag("TSType", "StdSeries")
  con$addTag("DataType", "WQSample")
  con$addTag("Interpolation", "Discrete")
  con$addTag("ItemInfo", attrs=c(ItemNumber="1"),close=FALSE)
  con$addTag("ItemName", "WQ Sample")
  con$addTag("ItemFormat", "S")
  con$addTag("Divisor", "1")
  con$addTag("Units")
  con$addTag("Format", "$$$")
  con$closeTag() # ItemInfo
  con$closeTag() # DataSource
  
  # for the TVP and associated measurement water quality parameters
  con$addTag("Data", attrs=c(DateFormat="Calendar", NumItems="1"),close=FALSE)
  # for each tvp
  ## LOAD SAMPLE PARAMETERS
  ## SampleID, ProjectName, SourceType, SamplingMethod and mowsecs
  sample<-Data[start:end,3]
  sample<-unique(sample)
  sample<-sample[order(sample)]
  ## THIS NEEDS SOME WORK.....
  for(a in 1:length(sample)){ 
    con$addTag("E",close=FALSE)
    con$addTag("T",sample[a])
    #put metadata in here when it arrives
    con$addTag("I1", "")
    con$closeTag() # E
  }
  
  con$closeTag() # Data
  con$closeTag() # Measurement    
  
}






cat("Saving: ",Sys.time()-tm,"\n")
saveXML(con$value(), paste0("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),file="/hrcSWQ.xml"))
cat("Finished",Sys.time()-tm,"\n")
