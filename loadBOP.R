## Import data from Council Spreadsheets

## Export to a Hilltop XML file with all supplied data intact.

## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data

Process<-TRUE
message(paste("BOPRC: Loading data from 52 North End point",Process))

if(Process){
  if(exists("importDestination")&!file.exists(paste(importDestination,file="bopSWQ.csv",sep=""))){
    write.csv(c(0),file=paste(importDestination,file="bopSWQ.csv",sep=""))
    
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
    
    
    STOP
    
    THIS ONE GETS ITS DATA FROM A FILE!  SEE LINE 180
    
    od<-getwd()
    setwd("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state")
    
    #function to either create full xml file or return xml file as NULL depending
    #on the result from the above funciton
    requestData <- function(url){
      #url<-"http://hilltopdev.horizons.govt.nz/data.hts?service=Hilltop"
      #RCurl::getURL(paste(url,"&request=Reset",sep=""))
      #url <- paste(url,request,sep="")
      #cat(url,"\n")
      # ret <- htsServiceError(url)
      #if(ret==TRUE){
      xmldata <- ld(url)
      return(xmldata)
      # }else {
      # xmldata <- NULL
      # return(xmldata)
      
      # }
    }
    
    
    #function to create xml file from url. 
    ld <- function(url){
      str<- tempfile(pattern = "file", tmpdir = tempdir())
      (download.file(url,destfile=str,method="wininet",quiet=T))
      xmlfile <- xmlParse(file = str)
      unlink(str)
      return(xmlfile)
    }
    
    fname <- "H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/boprcSWQ_config.csv"
    df <- read.csv(fname,sep=",",stringsAsFactors=FALSE)
    siteTable=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors=FALSE)
    
    configsites <- subset(df,df$Type=="Site")[,2]
    configsites <- as.vector(configsites)
    sites = unique(siteTable$CouncilSiteID[siteTable$Agency=='boprc'])
    
    Measurements <- subset(df,df$Type=="Measurement")[,2]
    Measurements <- as.vector(Measurements)
    Qualifier    <- subset(df,df$Type=="Qualifier")[,2]
    Qualifier    <- as.data.frame(t(as.data.frame(strsplit(Qualifier,split="|",fixed=TRUE))),row.names=FALSE)
    colnames(Qualifier) <- c("Value","Description")
    
    ## Load libraries ------------------------------------------------
    require(RODBC)   ### ODBC library for SQL connection
    require(dplyr)   ### dply library to manipulate table joins on dataframes
    require(XML)     ### XML library to write hilltop XML
    require(scales)  ### Graphical scales map data to aesthetics, and provide methods for automatically determining breaks and labels for axes and legends.
    ### For this script, it holds the percent function to deal with formatting numbers
    
    sink("boprc_endpoint_scan.txt")
    
    rm(Data)
    for(i in 1:length(sites)){
      cat(sites[i],'\n')
      for(j in 1:length(Measurements)){
        url <- paste0("http://ec2-52-6-196-14.compute-1.amazonaws.com/sos-bop/service?service=SOS&version=2.0.0&request=GetObservation&observedProperty=",Measurements[j],"&featureOfInterest=%",sites[i],"%&temporalfilter=om:phenomenonTime,2004-01-01/2018-01-01")
        url <- gsub(url,pattern = ' ',replacement = '%20')
        xmlfile <- ld(url)
        
        #Default metadata for data qualifiers (such as "<" and ">") are set at the beginning of the time series, and appears to
        #be based on the first value in the timeseries time-value pairs. We need to be aware of what the default qualifier
        #value is, in order to correctly tag values inthe rest of the timeseries.
        
        #The first step is to parse the DefaultTVPMetadata element and store those values
        #The second step is to determine if a <wml2:metadata> element exists with a <wml2:qualifier> child. If it does, 
        #then need to parse that child element and extract the qualifier in order to present the timeseries value
        #properly.
        #Thirdly, retrieve all the data and apply the qualifiers
        
        #STEP 1: Load wml2:DefaultTVPMetadata to a vector
        xattrs_qualifier         <- xpathSApply(xmlfile, "//wml2:DefaultTVPMeasurementMetadata/wml2:qualifier/@xlink:title")
        if(is.null(xattrs_qualifier)){
          xattrs_qualifier <- ""
        }
        xattrs_uom               <- xpathSApply(xmlfile, "//wml2:DefaultTVPMeasurementMetadata/wml2:uom/@code")
        xattrs_interpolationType <- xpathSApply(xmlfile, "//wml2:DefaultTVPMeasurementMetadata/wml2:interpolationType/@xlink:title")
        xattrs_default           <- c(xattrs_qualifier,xattrs_uom,xattrs_interpolationType)
        names(xattrs_default)    <- c("qualifier","uom","interpolationType")
        rm(xattrs_qualifier,xattrs_uom,xattrs_interpolationType)
        
        #STEP 2: Get wml2:MeasurementTVP metadata values
        xattrs_qualifier         <- xpathSApply(xmlfile, "//wml2:TVPMeasurementMetadata/wml2:qualifier/@xlink:title")
        #If xattrs_qualifier is empty, it means there are no additional qualifiers in this timeseries.
        #Test for Null, and create an empty dataframe as a consequence 
        if(is.null(xattrs_qualifier)){
          df_xattrs <- data.frame(time=character(),qualifier=character())
        } else{
          xattrs_time              <- sapply(getNodeSet(doc=xmlfile, "//wml2:TVPMeasurementMetadata/wml2:qualifier/@xlink:title/../../../../wml2:time"), xmlValue)
          #Store measurementTVPs in dataframe to join back into data after it is all retrieved
          df_xattrs <- as.data.frame(xattrs_time,stringsAsFactors = FALSE)
          names(df_xattrs) <- c("time")
          df_xattrs$qualifier <- xattrs_qualifier
          rm(xattrs_time,xattrs_qualifier)
        }    
        #Create vector of times
        time <- sapply(getNodeSet(doc=xmlfile, "//wml2:time"), xmlValue)
        #Create vector of  values
        value <- sapply(getNodeSet(doc=xmlfile, "//wml2:value"), xmlValue)
        
        df <- as.data.frame(time, stringsAsFactors = FALSE)
        df$value <- value
        rm(time, value)
        
        df$Site <- sites[i]
        df$Measurement <- Measurements[j]
        df$Units <- xattrs_default[2]  ## xattrs_default vector contains (qualifier_default, unit, interpolationtype)
        df <- df[,c(3,4,1,2,5)]
        
        # merge in additional qualifiers, if present, from df_xattrs
        if(nrow(df_xattrs)!=0) {
          df <- merge(df,df_xattrs,by="time",all=TRUE)
          df$qualifier[is.na(df$qualifier)] <- xattrs_default[1]
        } else {
          df$qualifier<-xattrs_default[1]
        }
        
        # Remove default metadata attributes for current timeseries
        rm(xattrs_default, df_xattrs)
        
        
        if(!exists("Data")){
          Data <- df
        } else{
          Data <- rbind.data.frame(Data, df)
        }
        
        # Remove current timeseries data frame
        rm(df, xmlfile)
      }
    }
    
    sink()
    
    save(Data,file="boprcSWQ.Rda")
    
    qualifiers_added <- unique(Data$qualifier)
    
    #p <- sapply(getNodeSet(doc=xmlfilec ,path="//sos:ObservationOffering/swes:name"), xmlValue)
    
    #procedure <- c("RERIMP.Sample.Results.P", "WARIMP.Sample.Results.P")
    
    Data=read.csv("h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/BoPLawaRivers2004_2017.csv",stringsAsFactors = F)
    #----------------
    tm<-Sys.time()
    cat("Building XML\n")
    cat("Creating:",Sys.time()-tm,"\n")
    
    con <- xmlOutputDOM("Hilltop")
    con$addTag("Agency", "BOPRC")
    #saveXML(con$value(), file="out.xml")
    
    #-------
    
    max<-nrow(Data)
    #max<-nrows(datatbl)
    
    i<-1
    #for each site
    while(i<=max){
      s<-Data$Site.Name[i]
      # store first counter going into while loop to use later in writing out sample values
      start<-i
      
      cat(round(i/max,3),"\t",Data$Site.Name[i],"\n")   ### Monitoring progress as code runs
      
      while(Data$Site[i]==s){
        #for each measurement
        #cat(datatbl$SiteName[i],"\n")
        con$addTag("Measurement",  attrs=c(SiteName=Data$Aquarius.ID[i]), close=FALSE)
        con$addTag("LawaID",Data$ï..LAWAID[i])
        con$addTag("SiteID",Data$Aquarius.ID[i])
        con$addTag("CouncilSiteID",  attrs=c(SiteName=Data$Site.Name[i]))
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
        
        # for the TVP and associated measurement water quality parameters
        con$addTag("Data", attrs=c(DateFormat="Calendar", NumItems="2"),close=FALSE)
        d<- Data$Measurement[i]
        
        while(Data$Measurement[i]==d & Data$Site[i]==s){
          # for each tvp
          # Handle Greater than symbols
          
          # Handle Less than symbols  
          if(!is.na(Data$ReportedLabValue[i])&Data$ReportedLabValue[i]!=""){    # this will need to be expanded to deal with range of qualifiers
            if(grepl("^\\>",Data$ReportedLabValue[i],perl=T)){                   ## GREATER THAN VALUES
              con$addTag("E",close=FALSE)
              con$addTag("T",Data$Date.Time[i])
              con$addTag("I1", substr(x = Data$ReportedLabValue[i],start = 2,stop = nchar(Data$ReportedLabValue[i])))
              con$addTag("I2", "$ND\t>\t")
              con$closeTag() # E
            } else if(grepl("^\\<",Data$ReportedLabValue[i],perl=T)){           ## LESS THAN VALUES
              con$addTag("E",close=FALSE)
              con$addTag("T",Data$Date.Time[i])
              con$addTag("I1", substr(x = Data$ReportedLabValue[i],start = 2,stop = nchar(Data$ReportedLabValue[i])))
              con$addTag("I2", "$ND\t<\t")
              con$closeTag() # E 
            } else {                                               ## UNCENSORED VALUES
              con$addTag("E",close=FALSE)
              con$addTag("T",Data$Date.Time[i])
              con$addTag("I1", Data$ReportedLabValue[i])
              con$addTag("I2", "\t")
              con$closeTag() # E
            }
            # Write all other result values  
          } else {                                                 ## UNCENSORED VALUES
            if(Data$RawValue[i]!=""){
              con$addTag("E",close=FALSE)
              con$addTag("T",Data$Date.Time[i])
              con$addTag("I1", Data$RawValue[i])
              con$addTag("I2", "\t")
              con$closeTag() # E
            }else{
              con$addTag("E",close=FALSE)
              con$addTag("T",Data$Date.Time[i])
              con$addTag("I1", "")
              con$addTag("I2", "\t")
              con$closeTag() # E
            }
          }
          
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
      con$addTag("Measurement",  attrs=c(SiteName=Data$Aquarius.ID[start]), close=FALSE)
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
      saveXML(con$value(), paste0("h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/boprcSWQ.xml"))
    cat("Finished",Sys.time()-tm,"\n")
  }
}
rm(Process)
