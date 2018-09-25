## Import data from Council Hilltop Server

## Export to a Hilltop XML file with all supplied data intact.

## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data


message(paste("WCRC: Loading data from WCRC Hilltop Server",Process))



## Import data from Council Spreadsheets

## Export to a Hilltop XML file with all supplied data intact.

## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data


## --- Functions ---
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Convert datestring to mow seconds (number of seconds since 1-Jan-1940 00:00)
#mowSecs <- function(x){
#  s<-strptime("1940-01-01","%Y-%m-%d")
#  t<-strptime(x,"%Y-%m-%d %H:%M:%S")
#   t<-strptime(x,"%Y-%m-%d %H:%M:%S")
#  x<-(t-s)*86400
#}

require(XML)     ### XML library to write hilltop XML
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(RCurl)


od<-getwd()
setwd("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state")


fname <- "H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/wcrcSWQ_config.csv"
df <- read.csv(fname,sep=",",stringsAsFactors=FALSE)
siteTable=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors=FALSE)

configsites <- subset(df,df$Type=="Site")[,2]
configsites <- as.vector(configsites)
sites = unique(siteTable$CouncilSiteID[siteTable$Agency=='wcrc'])
Measurements <- subset(df,df$Type=="Measurement")[,2]
Measurements <- as.vector(Measurements)

#function to create xml file from url. 
ld <- function(url){
  str<- tempfile(pattern = "tempwcrc", tmpdir = tempdir())
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
    return(xmldata)   # if no error, return TRUE
  } else {
    return(NULL)
  }
}

#function to either create full xml file or return xml file as NULL depending
#on the result from the above funciton
requestData <- function(url){
  # cat(url,"\n")
  ret <- htsServiceError(url)
  if(!is.null(ret)){
    return(ret)
  }else {
    return(NULL)
  }
}


tab<-"\t"

## Load libraries ------------------------------------------------
require(RODBC)   ### ODBC library for SQL connection
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(XML)     ### XML library to write hilltop XML


for(i in 1:length(sites)){
  cat(sites[i],i,'out of',length(sites),'\n')
  for(j in 1:length(Measurements)){
    
    # cat(sites[i],Measurements[j],"\n")
    
    if(Measurements[j]=="xE.coli (Mem Filtration)"){
      
      url <- paste("http://hilltop.wcrc.govt.nz/wq.hts?service=Hilltop",
                   "&request=GetData",
                   "&Site=",sites[i],
                   "&Measurement=E.coli (Mem Filtration)",
                   "&From=2004-01-01",
                   "&To=2018-01-01",sep="")
      url <- gsub(" ", "%20", url)
      cat(url,"\n")
      xmlfile <- requestData(url) 
      
      if(is.null(xmlfile)){
        next
      } else{
        
        time <- sapply(getNodeSet(doc=xmlfile, "//T"), xmlValue)
        
        time <- time[! time %in% rectime]
        
        df <- data.frame(time)
        
        if(nrow(df)!=0){
          time <- sapply(getNodeSet(doc=xmlfile, "//T"), xmlValue)
          #Create vector of  values
          value <- sapply(getNodeSet(doc=xmlfile, "//I1"), xmlValue)
          #Add in bit here to get the measurements I2 Info
          df4 <- data.frame(time, value, stringsAsFactors=FALSE)
          df4 <- merge(df,df4, by= "time", all = TRUE)
          df4 <- na.omit(df4)
          u <- sapply(getNodeSet(doc=xmlfile, "//Units"), xmlValue)
          df4$Site <- sites[i]
          df4$Measurement <- Measurements[j]
          df4$Units <- u
          df4 <- df4[,c(3,4,1,2,5)]  
          if(!exists("Data")){
            Data <- df4
          } else{
            Data <- rbind.data.frame(Data, df4)
          }
        }
      }
      
      #add an else if for WQ sample to pull in data to add as a column in data frame, 
    }else{
      url <- paste("http://hilltop.wcrc.govt.nz/wq.hts?service=Hilltop",
                   "&request=GetData",
                   "&Site=",sites[i],
                   "&Measurement=",Measurements[j],
                   "&From=2004-01-01",
                   "&To=2018-01-01",sep="")
      url <- gsub(" ", "%20", url)
      
      
      xmlfile <- requestData(url)
      
      if(is.null(xmlfile)){
        next
      } else{
        #Create vector of times
        time <- sapply(getNodeSet(doc=xmlfile, "//T"), xmlValue)
        #Create vector of  values
        value <- sapply(getNodeSet(doc=xmlfile, "//Value"), xmlValue)
        if(length(value)==0){
          value <- sapply(getNodeSet(doc=xmlfile, "//I1"), xmlValue)
        }
        #Add in bit here to get the I2 info
        df <- as.data.frame(time, stringsAsFactors = FALSE)
        df$value <- value
        u <- sapply(getNodeSet(doc=xmlfile, "//Units"), xmlValue)
        if(length(u)>1&any(u=="")){u=u[u!=""]}
        df$Site <- sites[i]
        df$Measurement <- Measurements[j]
        df$Units <- u
        df <- df[,c(3,4,1,2,5)]
        
        if(!exists("Data")){
          Data <- df
        } else{
          Data <- rbind.data.frame(Data, df)
        }
        
      } 
      
    }
  }
}

#By this point, we have all the data downloaded from the council, in a data frame called Data.
# write.csv(Data,file = paste0("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/wcrcSWQ.csv",sep=""),row.names = F)
#The remainder here formats and saves XML
#----------------
tm<-Sys.time()
cat("Building XML\n")
cat("Creating:",Sys.time()-tm,"\n")

con <- xmlOutputDOM("Hilltop")
con$addTag("Agency", "WCRC")
#saveXML(con$value(), file="out.xml")

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
    e<-Data$Site[i]
    
    cat("       - ",Data$Measurement[i],"\n")   ### Monitoring progress as code runs
    
    while(Data$Measurement[i]==d & Data$Site[i]==s){
      # for each tvp
      con$addTag("E",close=FALSE)
      con$addTag("T",Data$time[i])
      
      #Check for < or > or *
      ## Hand Greater than symbol
      if(grepl(pattern = "^\\>",x =  Data$value[i],perl = TRUE)){
        elemValue <- substr(Data$value[i],2,nchar(Data$value[i]))
        item2 <- paste("$ND",tab,">",tab,sep="")
        
        # Handle Less than symbols  
      } else if(grepl(pattern = "^\\<",x =  Data$value[i],perl = TRUE)){
        elemValue <- substr(Data$value[i],2,nchar(Data$value[i]))
        item2 <- paste("$ND",tab,"<",tab,sep="")
        
        # Handle Asterixes  
      } else if(grepl(pattern = "^\\*",x =  Data$value[i],perl = TRUE)){
        elemValue <- gsub(pattern = "^\\*", replacement = "", x = Data$value[i])
        item2 <- paste("$ND",tab,"*",tab,sep="")
      } else{
        elemValue <- Data$value[i]
        item2 <- ""
      }
      
      con$addTag("I1", elemValue)
      if(exists("item2")){
        item2 <- paste(item2,"Units", tab, Data$Units[i], tab, sep="")
      } else {
        item2 <- paste("Units", tab, Data$Units[i], tab, sep="")        
      }
      con$addTag("I2", item2)
      rm(item2)
      
      con$closeTag() # E
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

saveXML(con$value(), paste0("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/wcrcSWQ.xml"))
# Setting timeseries to be WQData
x <- readLines(paste0("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/wcrcSWQ.xml"))
y <- gsub( "NumItems=\"1\"", "NumItems=\"2\"", x, ignore.case = TRUE  )
y <- gsub( "SimpleTimeSeries", "WQData", y, ignore.case = TRUE  )  
writeLines(y,paste0("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/wcrcSWQ.xml"))


cat("Finished",Sys.time()-tm,"\n")

