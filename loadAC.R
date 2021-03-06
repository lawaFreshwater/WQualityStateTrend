## Import data from Council Data Source

## Export to a Hilltop XML file with all supplied data intact.

## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data

# The process flag assumes this script will be called from a batch file. IF the flag is true, the code in this file will run.


#Auckland provided metadata which suggested a site list to retrieve from
#incorporate this site list into the siteTable that would have otherwise been built from their WFS feed




message(paste("AC: Loading data from Kisters/Hydrotel"))


     ## Load libraries ------------------------------------------------
    require(RODBC)   ### ODBC library for SQL connection
    require(dplyr)   ### dply library to manipulate table joins on dataframes
    require(XML)     ### XML library to write hilltop XML
    require(XML)     ### XML library to write hilltop XML
    require(dplyr)   ### dply library to manipulate table joins on dataframes
    require(RCurl)
    
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
    
    
    fname <- "H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/acSWQ_config.csv"
    df <- read.csv(fname,sep=",",stringsAsFactors=FALSE)
    siteTable=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors=FALSE)
    
    
    configsites <- subset(df,df$Type=="Site")[,2]
    configsites <- as.vector(configsites)
    sites = unique(siteTable$CouncilSiteID[siteTable$Agency=='auckland council'])
    sites=configsites
    Measurements <- subset(df,df$Type=="Measurement")[,2]
    Measurements <- as.vector(Measurements)
    
    setwd("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state")
    
    #function to either create full xml file or return xml file as NULL depending
    #on the result from the above funciton
    requestData <- function(url){
      cat(url,"\n")
      xmldata <- ld(url)
      return(xmldata)
    }

    #function to create xml file from url. 
    ld <- function(url){
      (download.file(url,destfile="tmpac",method="wininet",quiet=T))
      pause(1)
      xmlfile <- xmlParse(file = "tmpac")
      unlink("tmpr")
      return(xmlfile)
    }
    
    pause <- function(x){
      p1 <- proc.time()
      Sys.sleep(x)
      proc.time() - p1 # The cpu usage should be negligible
    }
    
    
    for(i in 1:length(sites)){
      cat(sites[i],i,'out of',length(sites),'\n')
      for(j in 1:length(Measurements)){
        
        url <- paste("http://aklc.hydrotel.co.nz:8080/KiWIS/KiWIS?datasource=2&service=SOS&version=2.0&&request=GetObservation&featureOfInterest="
                     ,sites[i], "&observedProperty=", Measurements[j], "&Procedure=raw&temporalfilter=om:phenomenonTime,P15Y", sep="")
        url <- gsub(" ", "%20", url)
        xmlfile <- ld(url)
        
        #Create vector of times
        time <- sapply(getNodeSet(doc=xmlfile, "//wml2:time"), xmlValue)
        #Create vector of  values
        value <- sapply(getNodeSet(doc=xmlfile, "//wml2:value"), xmlValue)
        
        if(length(time!=0)){
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
    
    #By this point, we have all the data downloaded from the council, in a data frame called Data.
    write.csv(Data,file = paste0("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/acSWQ.csv"),row.names = F)
    #The remainder here formats and saves XML
    
  
    #----------------
    tm<-Sys.time()
    cat("Building XML\n")
    cat("Creating:",Sys.time()-tm,"\n")
    
    con <- xmlOutputDOM("Hilltop")
    con$addTag("Agency", "AC")
    #saveXML(con$value(), file="out.xml")
    
    #-------
    if(length(t)==0){
      next
    } else{
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

          # for the TVP and associated measurement water quality parameters
          con$addTag("Data", attrs=c(DateFormat="Calendar", NumItems="2"),close=FALSE)
          d<- Data$Measurement[i]
          
          cat("       - ",Data$Measurement[i],"\n")   ### Monitoring progress as code runs
          
          while(Data$Measurement[i]==d){
            # for each tvp
            con$addTag("E",close=FALSE)
            con$addTag("T",Data$time[i])
            con$addTag("I1", Data$value[i])
            con$addTag("I2", paste("Units", Data$Units[i], sep="\t"))
            
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
          con$addTag("I2", paste("QC", QC, sep="\t"))
          con$closeTag() # E
        }
        
        con$closeTag() # Data
        con$closeTag() # Measurement    
        
      }
    }
    cat("Saving: ",Sys.time()-tm,"\n")
      saveXML(con$value(),paste0("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/acSWQ.xml"))
    cat("Finished",Sys.time()-tm,"\n")
