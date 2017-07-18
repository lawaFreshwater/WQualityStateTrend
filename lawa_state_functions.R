#===================================================================================================
#  LAWA STATE ANALYSIS
#  FUNCTION LIBRARY
#  Horizons Regional Council
#
#  2-December-2013
#
#  Purpose: Water Quality State Analysis Service Definition
#
#  Processing of council water quality monitoring data for the LAWNZ website
#  has been completed by Horizons council staff between 2011 and 2013. To
#  reduce the dependancy on council staff and to increase transparency to
#  all participants, this script file has been prepared to automate the STATE
#  assessment portion of LAWA's State and Trend Analysis.
#
#  To make the data collation component of this script as flexible as possible,
#  proprietary file formats or RDBMS systems are not used. Instead, data is
#  accessed using standards-based requests to Council time series servers that
#  deliver WaterML 2.0 XML files. WaterML 2.0 is an Open Geospatial Consortium
#  standard that encodes water data time series into an XML file. These data
#  can be accessed using standard XML libraries provided by many programming
#  languages.
#
#  Maree Clark
#  Sean Hodges
#  Horizons Regional Council
#===================================================================================================

#/*  -===Load required libraries=== */ 
# 
# These libraries will need to be installed within R first, otherwise
# the script will error and stop. The first couple of lines do the install
# if the libraries are not detected.
# */


pkgs <- c('XML', 'RCurl','ggplot2','gridExtra','plyr','reshape2','RODBC','doBy','NADA','gdata','survival')
if(!all(pkgs %in% installed.packages()[, 'Package']))
  install.packages(pkgs, dep = T)

require(XML)        # XML library
require(RCurl)
require(reshape2)   # melt, cast, ...
require(ggplot2)    # pretty plots ...
require(gridExtra)
require(plyr)
require(RODBC)      # Database connectivity
require(doBy)
require(NADA)
require(gdata)
require(survival)

#===================================================================================================
#/* -===Pseudo-Function prototypes===- 
#  A list of the required functions for this routine
#  Rather than taking a linear approach to scripting, a number of
#  functions will be defined to do key tasks with the STATE analysis
#  script
#*/

#// SiteTable <- function(){}
#// requestData <- function(vendor,tss_url,request){}
#// SiteList <- function(xmlsdata){}
#// MeasurementList <- function(xmlmdata,requestType){}
#// StateAnalysis <- function(df,type,level){}
#// StateScore <- function(df,scope,altitude,landuse,wqparam,comparison)
#// calcScore <- function(df1,df2,wqparam)
#===================================================================================================

#/* -===Function definitions===-  */

# dataCleanse <- function(df,args){}

# Create yearMon variable to order data yyyy-mm
# Count results by Site and parameter for each date
# Count less thans for each Site and parameter
# Count each less than value for each site and parameter combo
# Remove greater thans for each site and parameter
# Review results

SiteTable <- function(databasePathFileName,sqlID){

  # Creating connection to hilltop database containing lawa site table
  # Creates a lockfile on the access mdb   
  # conn <- odbcConnectAccess("g:/projects/LAWNZ/RC_DATA/LAWNZ_WQ.mdb")

  #databasePathFileName<-"//ares/waterquality/LAWA/2013/hilltop.mdb"

  
  # If you run a 64bit Windows env, you may need to install the
  # Access Database engine in order to get this to work
  if(Sys.getenv("R_ARCH")=="/i386"){        #32bit Environment R_ARCH=="/i386"
    conn <- odbcConnectAccess(databasePathFileName)
  } else if(Sys.getenv("R_ARCH")=="/x64"){  #64bit Environment R_ARCH=="/x64"
    dbtxt <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",databasePathFileName,sep="")
    conn <- odbcDriverConnect(dbtxt,readOnlyOptimize = FALSE)
  }
  
  sql <- c("SELECT Lawnz-Sites].* FROM [Lawnz-Sites]",
           "SELECT [NationalSiteTable].* FROM [NationalSiteTable]",
           "SELECT [NationalSiteView].* FROM [NationalSiteView] WHERE [NationalSiteView].[UsedInLAWA] = TRUE AND [NationalSiteView].[Site_Type] like '%SoE%'")
  

  #sqlID=3
  # ' // Creating query and extracting dataset to LAWNZ data.frame
  LAWASites <- sqlQuery(conn, sql[sqlID])
  names(LAWASites) <- make.names(names(LAWASites))
  
  # ' // Closing connnection to hilltop database containing lawa site table
  # ' // Removes lock file on Access mdb
  close(conn)
  
  # removing variable "conn"
  rm(conn)
  
  return(LAWASites)
}

readUrl <- function(vendor,tss_url,request){
  
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      message("Attempting data retrieval")
      
      if(vendor=="HILLTOP"){
        url <- paste(tss_url,request,sep="")
        #cat(url,'\n')
      }
      xmlInternalTreeParse(url)
      
      #readLines(con=url, warn=FALSE) 
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      message(paste("URL returning empty document:", url))
      message("Trying again ...")
      xmldata <- xmlInternalTreeParse(url)
      #message("Here's the original error message:")
      #message(cond)
      # Choose a return value in case of error
      return(xmldata)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      message(paste("Processed URL:", url))
      #message("Some other message at the end")
    }
  )    
  return(out)
}

requestData <- function(vendor,tss_url,request){
  if(vendor=="HILLTOP"){
    url <- paste(tss_url,request,sep="")
    cat(url,'\n')
    xmldata <- xmlInternalTreeParse(url)
    
  } else if(vendor=="KISTERS"){
    
  } else if(vendor=="52NORTH"){
    
  }
  return(xmldata)
}

SiteList <- function(xmlsdata){
   
  # GETTING SITES WITH LOCATION DATA ONLY
  #site.attr<-getNodeSet(xmlsdata,"//Latitude/../@Name")
  #site.list<-sapply(site.attr, as.character)
  #data.lat <- sapply(getNodeSet(getSites.xml, "//HilltopServer/Site/Latitude"), xmlValue)
  #data.lon <- sapply(getNodeSet(getSites.xml, "//HilltopServer/Site/Longitude"), xmlValue)
  
  #ds<-data.frame(site.list,data.lat,data.lon, stringsAsFactors=FALSE)
  
  # GETTING ALL SITES WITHOUT LOCATION DATA
  sites<-sapply(getNodeSet(xmlsdata,"//Site/@Name"),as.character)
  sites <- gsub("''","'",sites)   # replacing the double apostrophe's generated by parsing the XML with single '.
  return(sites)
}

MeasurementList <- function(xmlmdata,requestType){
  if(requestType=="SOS"){
    
    DateTime <- sapply(getNodeSet(doc=xmlmdata, path="//wml2:point/wml2:MeasurementTVP/wml2:time"), xmlValue)
    value <- as.numeric(sapply(getNodeSet(doc=xmlmdata, path="//wml2:point/wml2:MeasurementTVP/wml2:value"), xmlValue))
  

    } else if(requestType=="Hilltop"){
    
    #DateTime <- sapply(getNodeSet(doc=xmlmdata, path="//Data/E/T"), xmlValue)
    #value <- as.numeric(sapply(getNodeSet(doc=xmlmdata, path="//Data/E/Value"), xmlValue))
    #site.attr<-getNodeSet(doc=xmlmdata,path="//T/ancestor::Measurement/@SiteName")
    #site.list<-sapply(site.attr, as.character)
      
    a<-getNodeSet(xmlmdata, path="//Measurement")
    
    for(i in 1:length(a)){

      myPath<-paste("//Measurement[",i,"]",sep="")
      c<-getNodeSet(xmlmdata, path=myPath)
      s<-sapply(c,function(el) xmlGetAttr(el, "SiteName"))
      
      c<-getNodeSet(xmlmdata, path=paste(myPath,"/Data/E/T"))
      d<-xmlSApply(c, xmlValue)
      
      c<-getNodeSet(xmlmdata, path=paste(myPath,"/Data/E/Value"))
      # need to represent this data as a character vector before converting to a numeric vector
      # in order to account for '<' and other value qualifiers. Will deal with these in another
      # function call
      v<-xmlSApply(c, xmlValue)
      #v<-as.numeric(xmlSApply(c, xmlValue))
      
      s <- rep(s,length(v))
      
      # Get method description
      myPath<-paste("//Measurement[",i,"]/Data/E/Parameter[@Name='Method']",sep="")
      f<-getNodeSet(xmlmdata, path=myPath)
      g<-sapply(f,function(el) xmlGetAttr(el, "Value"))
      
      if(length(f)==0){
        g <- rep("",length(v))
        tmp <- data.frame(as.POSIXct(strptime(d,format="%Y-%m-%dT%H:%M:%S")), g,stringsAsFactors=FALSE)
        names(tmp)<-c("Date","Method")
      } else {
        # Get dates related to method description
        myPath<-paste("//Measurement[",i,"]/Data/E/Parameter[@Name='Method']/../T",sep="")
        h<-getNodeSet(xmlmdata, path=myPath)
        k<-xmlSApply(h, xmlValue)

        tmp <- data.frame(as.POSIXct(strptime(k,format="%Y-%m-%dT%H:%M:%S")), g,stringsAsFactors=FALSE)
        names(tmp)<-c("Date","Method")
      }
          
      if(i==1){
        df<-data.frame(s,as.POSIXct(strptime(d,format="%Y-%m-%dT%H:%M:%S")),v, stringsAsFactors=TRUE)
        names(df) <- c("Site","Date","Value")
        df <- merge(df, tmp,by.x="Date",by.y="Date", all.x=TRUE)
      } else {  
        df1<-data.frame(s,as.POSIXct(strptime(d,format="%Y-%m-%dT%H:%M:%S")),v, stringsAsFactors=TRUE)
        names(df1) <- c("Site","Date","Value")
        df1 <- merge(df1, tmp,by.x="Date",by.y="Date", all.x=TRUE)
        df<-rbind(df,df1)
      }
    }
    
  }
  #df <- data.frame(as.POSIXct(strptime(DateTime,format="%Y-%m-%dT%H:%M:%S")),value,stringsAsFactors=FALSE)
  names(df) <- c("Date","SiteName","Value","Method")
  df$SiteName <- gsub("''","'",df$SiteName)   # replacing the double apostrophe's generated by parsing the XML with single '.
  df<-df[,c(2,1,3,4)]
  
  return(df)
} 

qualifiedValues <- function(df){
  
  df$qualifier <- substr(df$Value,start=1,stop=1)
  df$v <-ifelse(df$qualifier=="<",as.numeric(as.character(substr(df$Value,2,length(df$Value)-1)))/2,as.numeric(as.character(df$Value)))
  df$value <- df$Value
  df$Value <- df$v
  df$v <- NULL
  df$qualifier <- NULL
  df$value <- NULL
  return(df)
  

  
}

qualifiedValues2 <- function(df){
  df$ROS     <- df$Value  # If no reason to apply ROS, just return values
  df$i1Values <- df$Value  # If no reason to apply ROS, just return values
  df$i1Values[df$CenType=="Left"] <- (df$Value[df$CenType=="Left"])/2
  return(df)

}

StateAnalysis <- function(df,type,level){
  
  # df = dataframe
  # type = Site, Catchment, Region, NZ
  # level = LandUseAltitude,Landuse,Altitude,None
  
  # ' // =================================================================
  
  
  # ' // =================================================================
  # ' // summaryBy .... Calculating medians for all sites.
  # ' // =================================================================
  # ' // The summaryBy command allows selected summary statistics to be generated
  # ' // for selected variables across various factors
  
  # ' // The output of summaryBy is a data.frame with site medians, 
  # ' // with the grouping variables of landuse, altitude, catchment and local
  # ' // local authority name. This data.frame forms the basis for calculating
  # ' // State for each site, based on the median of the last sampled values
  
  # ' // s = sitemedians
  dfs <- summaryBy(Value~SiteName+LAWAID+parameter,
                 id=~LanduseGroup+AltitudeGroup+Catchment+Region+Frequency,
                 data=df, 
                 FUN=c(quantile), probs=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
  
  # ==============================================================
  # Test number of samples by Site/parameter
  # ==============================================================
  # Added 8-Oct-2015
  #
  # Exclusion criteria
  #   - less than 30 samples for monthly
  #   - less than 80 percent of samples for bimonthly/quarterly
  
  # ========================
  # Add Sample Counts to dfs
  # ========================
  # This summaryBy call is identical to the one above, except that it asks for the number of 
  # samples by group, instead of the median.
  # Both function calls return data.frames of the same length with records in the same order.
  # As a consequence, the Count field can be joined directly to the data.frame with medians
  # with bothering with a merge().
  dfs_count <- summaryBy(Value~SiteName+LAWAID+parameter,
                         id=~LanduseGroup+AltitudeGroup+Catchment+Region+Frequency,
                         data=wqdata, 
                         FUN=c(length))
  
  # Renaming Value.Length field
  c<-names(dfs_count)
  c[4]<-"Count"
  names(dfs_count) <- c
  
  dfs$Count <- dfs_count$Count
  rm(dfs_count)
  
  
  # ========================
  # Identifying rows meeting exclusion criteria
  dfs$Exclude<-FALSE
  dfs$Exclude[dfs$Frequency=="Monthly" & dfs$Count<30] <- TRUE
  dfs$Exclude[dfs$Frequency=="Bimonthly" & dfs$Count<(0.8*6*5)] <- TRUE
  dfs$Exclude[dfs$Frequency=="Quarterly" & dfs$Count<(0.8*4*5)] <- TRUE
  
  # ========================
  # Filtering data.frame to remove rows meeting exclusion criteria
  dfs<-dfs[dfs$Exclude==FALSE,1:9]

  # ==============================================================
  
  
  
  if(type=="Site"){
    if(level=="LandUseAltitude"){
    s <- summaryBy(Value~AltitudeGroup+LanduseGroup+Region+Catchment+SiteName+LAWAID+parameter,  
                    data=dfs, 
                    FUN=c(quantile), type=5, na.rm=TRUE, keep.name=TRUE)
    t <- summaryBy(Value~AltitudeGroup+LanduseGroup+Region+Catchment+SiteName+LAWAID+parameter,  
                   data=dfs, 
                   FUN=c(length), keep.name=TRUE)
    s$Value<-t$Value
    }
    
    s$scope <- type
    
  } else if(type=="Catchment"){
    if(level=="LandUseAltitude"){
      s <- summaryBy(Value~AltitudeGroup+LanduseGroup+Region+Catchment+parameter,
                    id=~SiteName+LAWAID,
                    data=dfs, 
                    FUN=c(quantile),na.rm=TRUE, keep.name=TRUE)
      t <- summaryBy(Value~AltitudeGroup+LanduseGroup+Region+Catchment+parameter,
                     id=~SiteName+LAWAID,
                     data=dfs, 
                     FUN=c(length), keep.name=TRUE)
      s$Value<-t$Value
      s$SiteName <- "All"
      s$LAWAID <- "All"
    } else if(level=="LandUse"){
      s <- summaryBy(Value~LanduseGroup+Region+Catchment+parameter,
                     id=~SiteName+LAWAID+AltitudeGroup,
                     data=dfs, 
                     FUN=c(quantile),na.rm=TRUE, keep.name=TRUE)
      t <- summaryBy(Value~LanduseGroup+Region+Catchment+parameter,
                     id=~SiteName+LAWAID+AltitudeGroup,
                     data=dfs, 
                     FUN=c(length), keep.name=TRUE)
      s$Value<-t$Value
      s$AltitudeGroup <- "All"
      s$SiteName <- "All"
      s$LAWAID <- "All"
      
    } else if(level=="Altitude"){
      s <- summaryBy(Value~AltitudeGroup+Region+Catchment+parameter,
                     id=~SiteName+LAWAID+LanduseGroup,
                     data=dfs, 
                     FUN=c(quantile),na.rm=TRUE, keep.name=TRUE)
      t <- summaryBy(Value~AltitudeGroup+Region+Catchment+parameter,
                     id=~SiteName+LAWAID+LanduseGroup,
                     data=dfs, 
                     FUN=c(length), keep.name=TRUE)
      s$Value<-t$Value
      s$LanduseGroup <- "All"
      s$SiteName <- "All"
      s$LAWAID <- "All"
      
    } else if(level=="None"){
      s <- summaryBy(Value~Region+Catchment+parameter,
                     id=~SiteName+LAWAID+AltitudeGroup+LanduseGroup,
                     data=dfs, 
                     FUN=c(quantile),na.rm=TRUE, keep.name=TRUE)
      t <- summaryBy(Value~Region+Catchment+parameter,
                     id=~SiteName+LAWAID+AltitudeGroup+LanduseGroup,
                     data=dfs, 
                     FUN=c(length), keep.name=TRUE)
      s$Value<-t$Value
      s$LanduseGroup <- "All"
      s$AltitudeGroup <- "All"
      s$SiteName <- "All"
      s$LAWAID <- "All"      
    }
  s$scope <- type
    
  } else if(type=="Region"){
    if(level=="LandUseAltitude"){
    s <- summaryBy(Value~AltitudeGroup+LanduseGroup+Region+parameter,  
                   id=~Catchment+SiteName+LAWAID,
                   data=dfs, 
                   FUN=c(quantile),na.rm=TRUE, keep.name=TRUE)
    t <- summaryBy(Value~AltitudeGroup+LanduseGroup+Region+parameter,  
                   id=~Catchment+SiteName+LAWAID,
                   data=dfs, 
                   FUN=c(length), keep.name=TRUE)
    s$Value<-t$Value
    s$SiteName <- "All"
    s$LAWAID <- "All"
    s$Catchment <- "All"
    } else if(level=="LandUse"){
      s <- summaryBy(Value~LanduseGroup+Region+parameter,  
                     id=~Catchment+SiteName+LAWAID+AltitudeGroup,
                     data=dfs, 
                     FUN=c(quantile), type=5, na.rm=TRUE, keep.name=TRUE) 
      t <- summaryBy(Value~LanduseGroup+Region+parameter,  
                     id=~Catchment+SiteName+LAWAID+AltitudeGroup,
                     data=dfs, 
                     FUN=c(length), keep.name=TRUE)     
      s$Value<-t$Value
      s$AltitudeGroup <- "All"
      s$SiteName <- "All"
      s$LAWAID <- "All"
      s$Catchment <- "All"
      
    } else if(level=="Altitude"){
      s <- summaryBy(Value~AltitudeGroup+Region+parameter,  
                     id=~Catchment+SiteName+LAWAID+LanduseGroup,
                     data=dfs, 
                     FUN=c(quantile),na.rm=TRUE, keep.name=TRUE)
      t <- summaryBy(Value~AltitudeGroup+Region+parameter,  
                     id=~Catchment+SiteName+LAWAID+LanduseGroup,
                     data=dfs, 
                     FUN=c(length), keep.name=TRUE)  
      s$Value<-t$Value
      s$LanduseGroup <- "All"
      s$SiteName <- "All"
      s$LAWAID <- "All"
      s$Catchment <- "All"
    } else if(level=="None"){
      s <- summaryBy(Value~Region+parameter,  
                     id=~Catchment+SiteName+LAWAID+LanduseGroup,
                     data=dfs, 
                     FUN=c(quantile), type=5, na.rm=TRUE, keep.name=TRUE)
      t <- summaryBy(Value~Region+parameter,  
                     id=~Catchment+SiteName+LAWAID+LanduseGroup,
                     data=dfs, 
                     FUN=c(length), keep.name=TRUE)
      s$Value<-t$Value
      s$AltitudeGroup <- "All"
      s$LanduseGroup <- "All"
      s$SiteName <- "All"
      s$LAWAID <- "All"
      s$Catchment <- "All"
    }
    s$scope <- type
    
  } else if(type=="NZ"){
      if(level=="LandUseAltitude"){
        s <- summaryBy(Value~AltitudeGroup+LanduseGroup+parameter,  
                       id=~Region+Catchment+SiteName+LAWAID,
                       data=dfs, 
                       FUN=c(quantile),na.rm=TRUE, keep.name=TRUE)
        t <- summaryBy(Value~AltitudeGroup+LanduseGroup+parameter,  
                       id=~Region+Catchment+SiteName+LAWAID,
                       data=dfs, 
                       FUN=c(length), keep.name=TRUE)
        s$Value<-t$Value
        s$SiteName <- "All"
        s$LAWAID <- "All"
        s$Catchment <- "All"
        s$Region <-"All"
  
        } else if(level=="LandUse"){
        s <- summaryBy(Value~LanduseGroup+parameter,  
                       id=~Region+Catchment+SiteName+LAWAID+AltitudeGroup,
                       data=dfs, 
                       FUN=c(quantile),na.rm=TRUE, keep.name=TRUE)
        t <- summaryBy(Value~LanduseGroup+parameter,  
                       id=~Region+Catchment+SiteName+LAWAID+AltitudeGroup,
                       data=dfs, 
                       FUN=c(length), keep.name=TRUE)
        s$Value<-t$Value
        s$AltitudeGroup <- "All"
        s$SiteName <- "All"
        s$LAWAID <- "All"
        s$Catchment <- "All"
        s$Region <-"All"
        
      } else if(level=="Altitude"){
          s <- summaryBy(Value~AltitudeGroup+parameter,  
                         id=~Region+Catchment+SiteName+LAWAID+LanduseGroup,
                         data=dfs, 
                         FUN=c(quantile),na.rm=TRUE, keep.name=TRUE)
          t <- summaryBy(Value~AltitudeGroup+parameter,  
                         id=~Region+Catchment+SiteName+LAWAID+LanduseGroup,
                         data=dfs, 
                         FUN=c(length), keep.name=TRUE)
          s$Value<-t$Value
          s$LanduseGroup <- "All"
          s$SiteName <- "All"
          s$LAWAID <- "All"
          s$Catchment <- "All"
          s$Region <-"All"
          
      } else if(level=="None"){
          s <- summaryBy(Value~parameter,  
                         id=~Region+Catchment+SiteName+LAWAID+LanduseGroup+AltitudeGroup,
                         data=dfs, 
                         FUN=c(quantile),na.rm=TRUE, keep.name=TRUE)
          t <- summaryBy(Value~parameter,  
                         id=~Region+Catchment+SiteName+LAWAID+LanduseGroup+AltitudeGroup,
                         data=dfs, 
                         FUN=c(length), keep.name=TRUE)
          s$Value<-t$Value
          s$LanduseGroup <- "All"
          s$AltitudeGroup <- "All"
          s$SiteName <- "All"
          s$LAWAID <- "All"
          s$Catchment <- "All"
          s$Region <-"All"
      }
    s$scope <- type
    
  }
  
  return(s)
}

StateScore <- function(df,scope,altitude,landuse,wqparam,comparison){
  # df <- sa
  # scope <- scope[i]
  # altitude <- ""
  # landuse <- ""
  # wqparam
  # comparison <- 1
  
  # ' // In assigning state scores, the routine needs to process each combination of altitude
  # ' // and landuse and compare to the National levels for the same combinations.
  # ' //   These combinations are:
  
  # ' //   NZ scale data set - no factors
  # ' //       Each site (all altitude and landuses) compared to overall National medians
  
  # ' //   Single factor comparisons
  # ' //       Each upland site (all landuses) compared to upland National medians
  # ' //       Each lowland site (all landuses) compared to lowland National medians
  # ' //       Each rural site (all altitudes) compared to rural National medians
  # ' //       Each forest site (all altitudes) compared to forest National medians
  # ' //       Each urban site (all altitudes) compared to urban National medians
  
  # ' //   Multiple factor comparisons 
  # ' //      For each Landuse
  # ' //        Each upland site compared to upland National medians
  # ' //        Each lowland site compared to lowland National medians
  # ' //      For each Altitude
  # ' //        Each rural site compared to rural National medians
  # ' //        Each forest site compared to forest National medians
  # ' //        Each urban site compared to urban National medians

  # ' // Then repeat for catchment and region.

  #  scope = "NZ", "Region", "Catchment", "Site"
  
  #  scope = "NZ" is used for comparison, so only Region, Catchment and Site feed through.
  
  #  comparision = 1,2,3,4
  #              1 = All -                 AltitudeGroup=="All"    & LanduseGroup=="All"   & Scope=="NZ"
  #              2 = Altitude -            AltitudeGroup==altitude & LanduseGroup=="All"   & Scope=="NZ"
  #              3 = Land use -            AltitudeGroup=="All"    & LanduseGroup==landuse & Scope=="NZ"
  #              4 = Altitude & Land use - AltitudeGroup==altitude & LanduseGroup==landuse & Scope=="NZ"
  
  
  # -=== WQ PARAMETERS ===-
    for(i in 1:length(wqparam)){
  
      # set a comparison enum_type 1,2,3,4 - make this a function argument to specify which
      # comparison to undertake.
      
      # Region|All, Catchment|All, Site|All 
      if(comparison==1){
        # df_scope represents the results for a specific scope : Site, Catchment, Region
        df_scope           <- subset(df, Scope==scope & Parameter==wqparam[i])
        df_scope$StateGroup<- paste(scope,"All",sep="|")
        # df_baseline represents the dataset against which site results will be compared and
        # assigned a state score  
        df_baseline <- subset(df, AltitudeGroup=="All" & LanduseGroup=="All" & Scope=="NZ" & Parameter==wqparam[i])  

      # Region|Upland, Catchment|Upland, Site|Upland     Region|Lowland, Catchment|Lowland, Site|Lowland
      } else if(comparison==2){          
        df_scope           <- subset(df, Scope==scope & AltitudeGroup==altitude & Parameter==wqparam[i])
        df_scope$StateGroup<- paste(scope,altitude,sep="|")
        # df_baseline represents the dataset against which site results will be compared and
        # assigned a state score  
        df_baseline <- subset(df, AltitudeGroup==altitude & LanduseGroup=="All" & Scope=="NZ" & Parameter==wqparam[i])  
      
      # Region|Forest, Catchment|Forest, Site|Forest     Region|Rural, Catchment|Rural, Site|Rural     Region|Urban, Catchment|Urban, Site|Urban
      } else if(comparison==3){          
        df_scope           <- subset(df, Scope==scope & LanduseGroup==landuse & Parameter==wqparam[i])
        df_scope$StateGroup<- paste(scope,landuse,sep="|")
        # df_baseline represents the dataset against which site results will be compared and
        # assigned a state score  
        df_baseline <- subset(df, AltitudeGroup=="All" & LanduseGroup==landuse & Scope=="NZ" & Parameter==wqparam[i])  
      }
        # Region|Upland|Forest, Catchment|Upland|Forest, Site|Upland|Forest     Region|Upland|Rural, Catchment|Upland|Rural, Site|Upland|Rural     Region|Upland|Urban, Catchment|Upland|Urban, Site|Upland|Urban
        # Region|Lowland|Forest, Catchment|Lowland|Forest, Site|Lowland|Forest     Region|Lowland|Rural, Catchment|Lowland|Rural, Site|Lowland|Rural     Region|Lowland|Urban, Catchment|Lowland|Urban, Site|Lowland|Urban 
        else if(comparison==4){          
        df_scope           <- subset(df, Scope==scope & AltitudeGroup==altitude & LanduseGroup==landuse & Parameter==wqparam[i])
        if(length(df_scope[,1])!=0){ # Testing for zero records returned - allows for zero Upland - Urban combination during testing.
          df_scope$StateGroup<- paste(scope,altitude,landuse,sep="|")
          # df_baseline represents the dataset against which site results will be compared and
          # assigned a state score  
          df_baseline <- subset(df, AltitudeGroup==altitude & LanduseGroup==landuse & Scope=="NZ" & Parameter==wqparam[i])
        } 
      }          
          
          
      if(i==1){
        state<-calcScore(df_scope,df_baseline,wqparam[i])
      } else {
        state<-rbind(state, calcScore(df_scope,df_baseline,wqparam[i]))
      }

    }
    
    
  return(state)
}

calcScore <- function(df1,df2,wqparam){
  
  df1 <- na.omit(df1)
  df2 <- na.omit(df2)
  for(i in 1:length(df1[,1]))
  if(wqparam=="BDISC"){
    if(df1$Q50[i]<=df2$Q25){
      df1$LAWAState[i] <- 4
    } else if(df1$Q50[i]>df2$Q25 & df1$Q50[i]<df2$Q50){
      df1$LAWAState[i] <- 3
    } else if(df1$Q50[i]>=df2$Q50 & df1$Q50[i]<df2$Q75){
      df1$LAWAState[i] <- 2
    } else if(df1$Q50[i]>=df2$Q75){
      df1$LAWAState[i] <- 1
    }
  } else if(wqparam=="PH"){
    if(df1$Q50[i]<=df2$Q25){
      df1$LAWAState[i] <- 2
    } else if(df1$Q50[i]>df2$Q25 & df1$Q50[i]<df2$Q50){
      df1$LAWAState[i] <- 1
    } else if(df1$Q50[i]>=df2$Q50 & df1$Q50[i]<df2$Q75){
      df1$LAWAState[i] <- 1
    } else if(df1$Q50[i]>=df2$Q75){
      df1$LAWAState[i] <- 2
    }
  } else {
    if(df1$Q50[i]<=df2$Q25){
      df1$LAWAState[i] <- 1
    } else if(df1$Q50[i]>df2$Q25 & df1$Q50[i]<df2$Q50){
      df1$LAWAState[i] <- 2
    } else if(df1$Q50[i]>=df2$Q50 & df1$Q50[i]<df2$Q75){
      df1$LAWAState[i] <- 3
    } else if(df1$Q50[i]>=df2$Q75){
      df1$LAWAState[i] <- 4
    }
  }
  return(df1)
}

calcTrendScore <- function(df1){
  #names(seasonalkendall) <- c("LAWAID","Parameter","Sen.Pct","Sen.Slope","p.value")
  #trendscores <- calcTrendScore(seasonalkendall)
  
  df1 <- na.omit(df1)
  
  for(i in 1:length(df1[,1])){
    if(df1$Parameter[i]=="BDISC"){
      if(df1$p.value[i]<0.05){
        if(df1$Sen.Pct[i]<=-1){
          df1$TrendScore[i] <- -2
        } else if(df1$Sen.Pct[i]>=1){
          df1$TrendScore[i] <- 2
        } else {
          df1$TrendScore[i] <- sign(df1$Sen.Pct[i])*1
        }
      } else{
        df1$TrendScore[i] <- 0
      }
    } else {
      if(df1$p.value[i]<0.05){
        if(df1$Sen.Pct[i]<=-1){
          df1$TrendScore[i] <- 2
        } else if(df1$Sen.Pct[i]>=1){
          df1$TrendScore[i] <- -2
        } else {
          df1$TrendScore[i] <- sign(df1$Sen.Pct[i])*-1
        }
      } else{
        df1$TrendScore[i] <- 0
      }
    }
  }
  return(df1)
}

calcTrendScoreAggregate <- function(df1){
  #names(seasonalkendall) <- c("LAWAID","Parameter","Sen.Pct","Sen.Slope","p.value")
  #trendscores <- calcTrendScore(seasonalkendall)
  
  for(i in 1:length(df1[,1])){
    if(df1$Parameter[i]=="BDISC"){
      if(df1$p.flag[i]>=0.5){
        if(df1$Sen.Pct[i]<=-1){
          df1$TrendScore[i] <- -2
        } else if(df1$Sen.Pct[i]>=1){
          df1$TrendScore[i] <- 2
        } else {
          df1$TrendScore[i] <- sign(df1$Sen.Pct[i])*1
        }
      } else{
        df1$TrendScore[i] <- 0
      }
    } else {
      if(df1$p.flag[i]>=0.5){
        if(df1$Sen.Pct[i]<=-1){
          df1$TrendScore[i] <- 2
        } else if(df1$Sen.Pct[i]>=1){
          df1$TrendScore[i] <- -2
        } else {
          df1$TrendScore[i] <- sign(df1$Sen.Pct[i])*-1
        }
      } else{
        df1$TrendScore[i] <- 0
      }
    }
  }
  return(df1)
}

PlotSites1 <- function(df,label){
  
  pdf("test.pdf", width=21, height=27.8)
  n <- 1
  plot = list()
  for(i in unique(df$LAWAID)){
#    
    dfSite<-subset(df, df$LAWAID==i)
    print(i)
    
   
#    par()              	# view current settings
#    opar <- par()      	# make a copy of current settings
    
#    par(mfrow=c(1,1))	 	# Specifying 1 graphs to plot on one page
#    par(oma=c(2,2,2,2))	# outer margin area = 2 lines all the way around
#    par(mar=c(4,4,0.5,0.5))	# reducing whitespace between multiple graphs
    
    # ' // Plotting a graph
#    plot(df$Date, df$Value,
#         xlab="",
#         ylab=label,
#         pch=20, col="black"
#    )
#    lines(df$Date, df$Value,
#          type="l",	
#          col="black"
#    )
    
    
#    dev.off()
#    par(opar)          # restore original settings
    
    plot[[n]] <- ggplot(dfSite, aes(Date,Value)) + geom_bar(stat="identity")
    if(n %% 8 == 0) {
      print (do.call(grid.arrange, plot))
      plot<-list()
      n<-0
    }
    
    # Create a simple timeseries plot for given time period
    h <- ggplot(dfSite, aes(Date,Value))
    p <- h + geom_bar(stat="identity")
    n <- n + 1
  }
  if(length(plot) != 0){
    print(do.call(grid.arrange, plot))
  }
  dev.off()

  return()
}

PlotSites <- function(df,label){
      
  # Create a simple timeseries plot for given time period
  p <- ggplot(df, aes(Date,Value)) + geom_bar(stat="identity")
  
  plots <- dlply(df, "LAWAID", '%+%', el=p)
  ml <- do.call(marrangeGrob, c(plots, list(nrow=8, ncol=1)))
  ggsave("multipage.pdf",ml)
  
  return()
}

sk <- function(lawa_ids,freqNum,freqText,df,rate,years,months){
  k <- 1 # counter for sites/parameters meeting minimum N
  for(i in 1:length(lawa_ids)){
    months<-freqNum
    l <- lawa_ids[i]
    df1 <- subset(df, LAWAID==l)
    parameters <- as.character(unique(df1$parameter))
    # this step is to double check output with TimeTrends
    #Uncomment if needed
    #write.csv(df_value_monthly1,file=paste("c:/data/MWR_2013/2013/ES-00022.csv",sep=""))
    
    lawa <- wqData(data=df1,locus=c(3,5,15),c(2,4),site.order=TRUE,time.format="%Y-%m-%d",type="long")
    x <- tsMake(object=lawa,focus=gsub("-",".",l))
    cat("length(parameters)",length(parameters),"\n")
    cat(parameters,"\n")
    
    # calculating seasonal Kendall statistics for individual parameters for an individual site
    for(j in 1:length(parameters)){
      # exclude sites/parameters where period of record is insufficient
      
      if(length(subset(df1,parameter==parameters[j])[,1])>=rate*(years*months)){
        
        s<-seaKen(x[,j])
        
        #s$sen.slope.pct  #  <---- required for LAWA
        #s$sen.slope      #  <----
        #s$p.value        #  <---- required for LAWA
        
        
        m <-matrix(data=c(l,parameters[j],s$sen.slope.pct,s$sen.slope,s$p.value),nrow=1,ncol=5,byrow=TRUE)
        if(k==1){   # removed i==i condition - causing errors where first site doesn't meet criteria for trend analysis
          seasonalkendall <-as.data.frame(m,stringsAsFactors=FALSE)
          cat("seasonalkendal dataframe created\n")
        } else {
          seasonalkendall <- rbind(seasonalkendall, as.data.frame(m,stringsAsFactors=FALSE))
          cat("Appending to seasonalkendall dataframe\n")
        }
        k <- k + 1
      }
    }
    
  }
  
  names(seasonalkendall) <- c("LAWAID","Parameter","Sen.Pct","Sen.Slope","p.value")
  
  seasonalkendall$Sen.Pct <-as.numeric(as.character(seasonalkendall$Sen.Pct))
  seasonalkendall$Sen.Slope <-as.numeric(as.character(seasonalkendall$Sen.Slope))
  seasonalkendall$p.value <-as.numeric(as.character(seasonalkendall$p.value))
  
  seasonalkendall$freq<- freqText

  return(seasonalkendall)

}

seaKenLAWA <- function (x,stat) {
  if (!is(x, "ts")) 
    stop("x must be a 'ts'")
  fr <- frequency(x)
  S <- 0
  varS <- 0
  miss <- NULL
  slopes <- NULL
  for (m in 1:fr) {
    xm <- x[cycle(x) == m]
    tm <- time(x)[cycle(x) == m]
    ken <- mannKen(ts(xm, start = start(x)[1], frequency = 1))
    S <- S + ken$S
    varS <- varS + ken$varS
    miss <- c(miss, ken$miss)
    outr <- outer(xm, xm, "-")/outer(tm, tm, "-")
    slopes.m <- outr[lower.tri(outr)]
    slopes <- c(slopes, slopes.m)
  }
  sen.slope <- median(slopes, na.rm = TRUE)
  if(stat=="mean"){
    sen.slope.pct <- 100 * sen.slope/abs(mean(x, na.rm = TRUE))
  } else if (stat=="median"){
    sen.slope.pct <- 100 * sen.slope/abs(median(x, na.rm = TRUE))
  }
  
  Z <- (S - sign(S))/sqrt(varS)
  p.value <- 2 * pnorm(-abs(Z))
  names(miss) <- as.character(1:m)
  list(sen.slope = sen.slope, sen.slope.pct = sen.slope.pct, 
       p.value = p.value, miss = round(miss, 3))

}

prepareData <- function(value){
    
  # 1. Asterixes
  #Finding strings starting with "*"
  n<-grepl(pattern = "^\\*",x =  value,perl = TRUE)
  cat("Fix asterixes symbols\n")
  cat("Items: ",length(n))
  cat("Found: ",sum(n))
  
  # Remove  hash symbol and set quality code to 400
  value[n==TRUE]<-gsub(pattern = "^\\*", replacement = NA, x = value[n==TRUE])
  
  return(value)
}

na_asterixes_val <- function(value){
    
  # 1. Asterixes
  #Finding strings starting with "*"
  n<-grepl(pattern = "^\\*",x =  value,perl = TRUE)
  cat("Fix asterixes symbols\n")
  cat("Items: ",length(n))
  cat("Found: ",sum(n))
  
  # Remove asterix symbol
  value[n==TRUE]<-gsub(pattern = "^\\*", replacement = NA, x = value[n==TRUE])
  
  return(value)
}

flagCensoredDataDF <- function(df){
   df$Censored<-FALSE
   df$CenType<-"FALSE"
  # 1. Less thans
  #Finding strings starting with "<"
  n<-grepl(pattern = "^<",x =  df$Value,perl = TRUE)
  cat("Find less than symbols\n")
  cat("Items: ",length(n),"\n")
  cat("Found: ",sum(n),"\n")
  
  df$Censored[n==TRUE] <- TRUE  
  df$CenType[n==TRUE] <- "Left"

  # Remove < symbol
  df$Value[n==TRUE]<-gsub(pattern = "^<", replacement = "", x = df$Value[n==TRUE])
  
  # 2. Greater thans
  #Finding strings starting with ">"
  n<-grepl(pattern = "^>",x =  df$Value,perl = TRUE)
  cat("Find greater than symbols\n")
  cat("Items: ",length(n),"\n")
  cat("Found: ",sum(n),"\n")
  
  df$Censored[n==TRUE] <- TRUE  
  df$CenType[n==TRUE] <- "Right"
  # Remove < symbol
  df$Value[n==TRUE]<-gsub(pattern = "^>", replacement = "", x = df$Value[n==TRUE])
  
  return(df)
}

# ## Function Deprecated - replace with impute.lower()
# leftCensored <- function(df){
#   
#   ####################################
#   ##             LAWA               ##
#   ###  LEFT CENSORED VALUE METHOD  ###
#   
#   # ROS Regression on Ordered statistics
#   #  cenros(<value>,<left censored T/F>)
#   
#   # Based on code kindly supplied by Ton Snelder
#   
#   if(sum(df$CenType=="Left")==0){ # if there are no dl values 
#     #cat(" -",sum(df$CenType=="Left"),"< values found","\n")
#     df$ROS     <- df$Value  # If no reason to apply ROS, just return values
#     df$iValues <- df$Value  # If no reason to apply ROS, just return values
#     
# #  } else if((sum(df$CenType=="Left")/nrow(df))>0.15){
#     #cat(" - Site excluded due to >15% left censored values\n")
#   } else if((sum(df$CenType=="Left")/nrow(df))>0.30){
#     #cat(" - Site excluded due to >30% left censored values\n")
#     df <- FALSE # FALSE is returned and this Site and Measurement are omitted from the dataset for later analysis.
#   } else {
#     #cat(" -",sum(df$CenType=="Left"),"< values found","\n")
#     row.names(df) <- 1:nrow(df) # to keep order of values in time
#     
#     # catch some bad myData if censored values  exceed max of uncensored values
#     MaxUncen <- max(df[df$CenType != "Left", "Value"])              # myData[ , "values"]
#     if(sum(df[df$CenType  == "Left", "Value"] >=  MaxUncen)>0) { #
#       BadDates <- df$CenType == "Left" & df$Value >=  MaxUncen
#       data2 <- df[!BadDates, ]  # drop these bad dates  which(data2[data2$dflag == "dl", "values"] >=  MaxUncen)
#     } else {
#       data2 <- df # data2 <- data2[-c(1,3,4,5,6,8,10, 111), ]
#     }
#     
#     # Pull out values to a vector and assign row names
#     usedValues <- data2$Value
#     names(usedValues) <- row.names(data2)    
#     
#     if(sum(data2$CenType == "Left") == 0 | nrow(data2) < 12) { # if if there are no dl values there is less than a year of df
#       df$ROS <- df$Value  # just return the values
#       df$iValues <- df$Value
# 
#     }  else {
#     
#       usedValues[usedValues == 0] <- min(data2[data2$CenType == "Left", "Value"], na.rm=T) # replace any zero values with the minimum DL
#       
#       myros = ros(obs = usedValues, censored = data2$CenType == "Left")
# 
#       SortValues <- sort(usedValues)  # sort order of values
#       NewROS <- cbind.data.frame(SortValues,as.data.frame(myros))
#       
#       DLvalues <- unique(NewROS$obs[NewROS$censored]) # the individual detection limit values
#       NewROS$iROS <- NewROS$modeled
#       if(sum(df$CenType=="Left")>1){
#         for(i in 1:length(DLvalues)) { # randomise the ROS imputed values within each DL strata
#           theseRows <- NewROS$censored & NewROS$obs == DLvalues[i]
#           NewROS[theseRows, "iROS"] <- resample(x=NewROS[theseRows, "modeled"])
#         }
#       }      
#       NewROS$OrigOrder <- as.numeric(names(SortValues)) # this is the original time order
#       SortROS <- NewROS[order(NewROS$OrigOrder), ]  # reorder ROS to
#       
#        #Now that SortROS has been ordered, we need to update the ordering to respect
#       #any bad dates that have been removed. This will allow the modelled values to 
#       #go back into the remaining rows properly, with trying to add to rows that no longer
#       #exist.
#       SortROS$OrigOrder<-1:nrow(SortROS)
#       row.names(SortROS) <- 1:nrow(SortROS)
#       
#       # Adding final vars to data2
#       data2$ROS <- NA
#       data2$ROS[SortROS$OrigOrder] <- SortROS[,"modeled"] # retain the order.
#       data2$iValues[SortROS$OrigOrder] <- SortROS[,"iROS"] # retain the order.
#       df<-data2
#     }
#   }
#   return(df)
# }
# 
# ## Function Deprecated - replace with impute.upper()
# rightCensored <- function(x) {   # this function uses survreg (package::survival) to impute values for CLARITY observations that are ABOVE multiple detection limits
#   
#   myData <-  x
#   myVar <-  as.character(myData$parameter[1])
#   
#   if(myVar != "BDISC") {     # if this is not Clarity
#     myData$status <- TRUE
#     myData$i2Values <- myData$iValues
#     myData$i2Values[myData$CenType == "Right"] <- 1.1*myData$iValues[myData$CenType == "Right"] # add 10% (catchs the E.coli greater thans).
#   } else { # only 24 samples or non detects for clarity < 5% [this guardes against crashing survreg below.
#     if(nrow(myData) < 24 | (sum(myData$CenType == "Right")/nrow(myData) < 0.05)) {
#       myData$status <- TRUE
#       myData$i2Values <- myData$iValues
#       myData$i2Values[myData$CenType == "Right"] <- 1.1*myData$iValues[myData$CenType == "Right"] # add 10%
#           
#     } else {
#       #print(as.character(myData$sIDnpID[1]))
#       # these lines can be used to see what is happening.
#       #plot(myData$myDate, myData$values)
#       #points(myData$myDate[myData$dflag == "al"],  myData$values[myData$dflag == "al"], col="red", pch=3)
#       # fit distribution
#       myData$status <- myData$CenType != "Right"   # note well if myData flag is "al" (meaning censored) this
#       # is equivalent to ?not dead? status in a survival model and therefore is
#       #cbind.myData.frame(myData$status , myData$values)
#       myMod <- survreg(Surv(Value, status) ~ 1, data = myData, dist="weibull") # using original observed non-censored
#       RScale <- as.numeric(exp(myMod$coefficients)) # this is the rweibull scale (see "survreg" function notes)
#       RShape <- 1/myMod$scale   # this is the weibull shape
#       #hist(myData$values[myData$dflag != "al"], main= "Example Distribution for site clarity myData")
#       #plot(density(myData$values[myData$dflag != "al"]), type="p", pch=1, main="Modelled Distribution of survival times (or observed values)")
#       #points(density(rweibull(10000, shape=RShape, scale=RScale)), col="blue")
#       SynthDist <- sort(rweibull(10000, shape=RShape, scale=RScale))   # synthetic distribution
#       # NB large sample taken to ensure that there are several values in SynthDist > the non detects
#       # otherwise the function below falls over.
#       myData$i2Values <-  unlist(lapply(1:length(myData$Value), function(x) {
#         if(myData$CenType[x] != "Right") {
#           Impute <- myData$iValues[x]
#         } else {
#           myVal <- myData$Value[x]
#           Impute <- resample(SynthDist[SynthDist > myVal], size=1)
#         }
#         return(Impute)
#       }))
#     }
#   }
#   return(myData)
# }
# 
# ## Function Deprecated - replace with impute.tied()
# addJitter <- function(x, myValues="i2Values") {     # this function adds jitter to break ties.
#   #browser()
#   myData <-  x
#   myData$i3Values <- myData[, myValues]   # add jitter is impute step 3
#   DL <- myData$CenType == "Left" | myData$CenType == "Right" # these need not be jittered as randomly imputed.
#   Jit <- !DL & duplicated(myData[,myValues]) & !is.na(myData[,myValues])  # these values are Not at Detect Limit 
#   # and are duplicated (need jittering)
#   # head(cbind.data.frame(myData$i2Values, DL, !DL, duplicated(myData$i2Values), Jit), 25)
#   myData$i3Values[Jit] <-  jitter(myData[,myValues][Jit])
#   # these lines can be used to see what is happening.
#   #  plot(myData$myDate, myData$values, pch=4, col="black", xlab="Date", ylab = "Water Quality variable")
#   #                              points(myData$myDate[DL],  myData$values[DL], col="blue", pch=1)
#   #                              points(myData$myDate[Jit],  myData$values[Jit], col="red", pch=3) # shows the tied values
#   #                              points(myData$myDate[Jit],  myData$impute3[Jit], col="green", pch=16, cex=1)
#   #                              points(myData$myDate[!Jit],  myData$impute3[!Jit], col="grey", pch=16, cex=1)
#   #                              legend("topright",  legend=c("Obs","DL", "Ties", "Jittered", "Imputed"), col = c("black", "blue","red", "green", "grey"),
#   #                              text.col = "black",  pch = c(4, 1, 3, 16, 16), bg = 'white', inset = .05)   # REMOVE THESE GUYS  ->  lty = c(1, 1),   merge = TRUE,
#   return(myData)
# }

TrendCriteria <- function (first_20pct, last__20pct, num_samples, rate, years, months, monthMultiplier) {
  # Check Trend Criteria - Assess pass-fail for trends analysis
  
  pass<-TRUE
#   if(first_20pct!= (months*monthMultiplier)){
#     pass <- FALSE
#   }
#   
#   if(last__20pct!=(months*monthMultiplier)){
#     pass <- FALSE
#   }

    # if the data has passed the first two criteria for sufficient data at beginning and end of period
  # then check for sufficient number of samples
  if(pass){
    if(num_samples<rate*(years*months)){
      pass <- FALSE
    }
  }
  
  return(pass)
  
}

samplesByInterval <- function (StartYear, EndYear, StartMonth, EndMonth, lawadata) {
  # Creating YYYY-MM values for period of trend analysis
  for(i in StartYear:EndYear){
    for(j in StartMonth:EndMonth){
      if(i==StartYear && j==StartMonth){
        #Using formatC to pad months 1-9 with leading zero
        arr<-paste(i,"-",formatC(j, width = 2, format = "d", flag = "0") ,sep="")
      } else {
        arr<-c(arr,paste(i,"-",formatC(j, width = 2, format = "d", flag = "0") ,sep=""))
      }
    }
  }
  
  
  #Setting up data frames with one value per month, bimonth and quarter
  aa<- as.data.frame(arr)
  names(aa) <- c("yearMon")
  
  lawadata$depth <- 0   # indicating surface samples
  
  lawadata$year <- as.character(lawadata$Date,format="%Y")
  lawadata$mon <- as.character(lawadata$Date,format="%m")
  lawadata$yearMon <- paste(lawadata$year,"-",lawadata$mon,"-01",sep="")
  
  lawadata$bimon <- floor((as.numeric(lawadata$mon)-1)/2)*2+1 # returns 1,2,3,4,5,6 depending on bimonth of year month falls in
  lawadata$yearBimon <- paste(lawadata$year,"-",formatC(lawadata$bimon, width = 2, format = "d", flag = "0"),"-01",sep="")
  
  
  lawadata$Qtr <- floor((as.numeric(lawadata$mon)-1)/3)*3+1 # returns 1,2,3,4 depending on quarter of year month falls in
  lawadata$yearQtr <- paste(lawadata$year,"-",formatC(lawadata$Qtr, width = 2, format = "d", flag = "0"),"-01",sep="")
  return(lawadata)
}

#Ton Snelder 
#LWP Ltd
#March 2016

# replaced original impute() functions and  with functions "Impute.lower" "Impute.upper", "Impute.tied"
# these 3 function supplied by Niall Broekhuizen <Niall.Broekhuizen@niwa.co.nz>
# Nials functions were further modified 

################################################################################
# imputation functions for censored values
################################################################################

resample <- function(x, ...) x[sample.int(length(x), ...)]

# leftCensored <- function(df)
# Impute.lower <-  function(x, forwardT="log", reverseT="exp") {     
leftCensored <-  function(x, forwardT="log", reverseT="exp") {     
    # this uses ROS to impute values for obs below multiple detection limits.  
  # It is a modified version of code the function "Impute1()" supplied to Niall Broekhuizen <Niall.Broekhuizen@niwa.co.nz> by Bruce Dudley and originally written by 
  # Ton Snelder, with modifications by Olivia Burge
  # further modifications were made by Ton on 8/2/2017
  # x is a data-frame containing (at a minimum)
  # x$converted_values : the raw data 
  # x$dflag            :a flag indicating whether the raw data are left censored ("dl"), right censored ("al"), 
  #                     or within the lower and upper detection limits ("ok")
  # x$npID: string specifying what quantity these data represent (CHl, NH4-N etc) - not used in this function
  #           but for compatibility with Impute.upper
  # forwardT: forward transformation to be used in the NADA:ros() function
  # reverseT: reverse transformation to be used in the NADA:ros() function
  # 
  # x<-tmp
  # forwardT="log"
  # reverseT="exp"
  # ------------------------------------------------------------------------------------------------
  # Sean Hodges 2017-07-09
  # This routine differs from the originally supplied censoring algorithm
  # The following code block adds in the variables described above. This should be worked into the
  # main state and trend script files, but for the moment, it will be ok to leave it here.
  # x$converted_values : the raw data 
  # x$dflag            :a flag indicating whether the raw data are left censored ("dl"), right censored ("al"), 
  #                     or within the lower and upper detection limits ("ok")
  x$converted_values <- x$Value
  
  x$dflag <- "ok"

  x$dflag[x$Censored==TRUE & x$CenType=="Left"] <- "dl"
  x$dflag[x$Censored==TRUE & x$CenType=="Right"] <- "al"
  # ------------------------------------------------------------------------------------------------
  
  myData <- x
  myData$dflag <- as.character(myData$dflag)
  #cat("Site = ", as.character(x$SiteName)[1], " & Variable = ", as.character(x$parameter)[1], "\n")
  
  if(0 == sum("dl"==myData$dflag)) {   
    # if no left censored obs, return the values.
    print("no below limits")
    #myData$ROS <- myData$converted_values
    myData$i1Values <- myData$converted_values
    #  } else if((sum(df$CenType=="Left")/nrow(df))>0.15){
       #cat(" - Site excluded due to >15% left censored values\n")
  } else if((sum(myData$CenType=="Left")/nrow(myData))>0.30){
    #cat(" - Site excluded due to >30% left censored values\n")
    myData <- FALSE # FALSE is returned and this Site and Measurement are omitted from the dataset for later analysis.
  
  } else {
    
    #-------------------------
    # Sean Hodges = 2017-07-09
    # Case of all values being censored is not dealt with here
    # refer back to original left censored function to determine how to handle this case.
    #-------------------------
    
    r.na2 <- is.na(myData$converted_values)  # added by Niall.  I will treat NAs as censored values, then sub NA back right at end
    # Since they are marked as censored, they will not influence the regression.  
    # They will receive imputed values, but those will later be overwritten with NAs
    dflag.original <- myData$dflag
    myData$dflag[r.na2] <- "dl"
    #    myData$converted_values[r.na2] <- min(myData$converted_values["dl"==myData$dflag],na.rm=TRUE) # to avoid possible problems with NAs problems in ros() later
    
    rownames(myData) <- 1:nrow(myData) # to keep order of values in time
    print("some below limits")
    # catch some bad myData if censored values exceed max of uncensored values
    MaxUncen <- max(myData[myData$dflag != "dl", "converted_values"]) 
    
    
    # MODIFIED BY TON BELOW
    ######################
    if( (sum(myData[myData$dflag == "dl", "converted_values"] >=  MaxUncen, na.rm=T)>0)){ # if any censored values exceed the obsevered uncensored values
      # |(0 < sum(is.na(myData$converted_values))) ){                                  # or if there are NA values (Why did Nial do this???)
      print("some bad data")
      BadDates <- (myData$dflag == "dl" & myData$converted_values >=  MaxUncen) 
      #      browser()
      # ensure that these bad data get treated as censored data by ROS (ie don't form part of the regression) # c("dflag", "Censored", "converted_values")
      myData_converted_values_orig <- myData$converted_values
      myData$converted_values[BadDates] <- 0.5 * MaxUncen
      myData$dflag[BadDates] <- "dl" # this is already the case  if condition above is True
      
      data2 <- myData  # modified by Ton 
      #################
      
      #      data2 <- myData[!BadDates, ]  # drop these bad dates  which(data2[data2$dflag == "dl", "values"] >=  MaxUncen)
    } else {
      print("no bad dates")
      data2 <- myData
    }
    rownames(data2) <- 1:nrow(data2) # to keep order of values in time
    
    #data2 <- data2[data2$dflag != "al", ]  # remove the right censored myData ??
    usedValues <- data2$converted_values
    
    names(usedValues) <- rownames(data2)    #  cbind.myData.frame(usedValues, data2$dflag == "dl")
    
    if(sum(data2$dflag == "dl") == 0 | nrow(data2) < 12) { # if there are no dl values or less than a year of myData
      print("no dl values or less than a year")
      # myData$ROS <- myData$converted_values  # just return the values
      myData$i1Values <- myData$converted_values
    }  else {
      ##      print(as.character(myData$RCIDsiteIDnpID[1]))
      #      print(as.character(myData$SiteName))
      #browser()
      ## changed from ==0 to < or = to 0 to catch any negatives
      usedValues[usedValues <= 0] <- min(data2[data2$dflag == "dl", "converted_values"], na.rm=T) # replace any zero values with the minimum DL
      
      # added a catch as ros can fail occasionally if the data are not conducive to a regression
      myros <- try(ros(obs = usedValues, censored = data2$dflag == "dl",forwardT=forwardT,reverseT=reverseT))
      
      if (any(class(myros) == c("ros", "lm"))) {  # if a model is fitted then contiune 
        
        SortValues <- sort(usedValues, na.last = T)  # sort order of values # TON added na.last to preserve the NA values on sorting
        
        NewROS <- cbind.data.frame(SortValues,as.data.frame(myros))
        DLvalues <- as.numeric(na.omit(unique(NewROS$obs[NewROS$censored]))) # the individual detection limit values TON MODIFIED TO DROP NA
        
        NewROS$iROS <- NewROS$modeled
        for(i in 1:length(DLvalues)) { # randomise the ROS imputed values within each DL strata
          theseRows <- NewROS$censored & NewROS$obs == DLvalues[i]
          theseRows[is.na(theseRows)] <- FALSE  # TON MODIFIED TO Deal with NA
          NewROS[theseRows, "iROS"] <- resample(x=NewROS[theseRows, "modeled"])
        }
        
        NewROS$OrigOrder <- as.numeric(names(SortValues)) # this is the original time order
        # print((SortValues))
        # print(sort(NewROS$OrigOrder))
        SortROS <- NewROS[order(NewROS$OrigOrder), ]  # reorder ROS to original time order
        
        # data2$ROS <- NA
        # data2$ROS[SortROS$OrigOrder] <- SortROS[,"modeled"] # retain the order.
        #      browser()
        data2$i1Values[SortROS$OrigOrder] <- SortROS[,"iROS"] # retain the order.
        data2$i1Values[r.na2] <- NA
        data2$dflag[r.na2] <- dflag.original[r.na2]
        #data2$converted_values[r.na2] <- myData_converted_values_orig[r.na2]
        # note that any data where dflag=="dl" but (original) converted_value > MaxUncen now has an invented values stemming from ROS
      } else {
        myData$i1Values <- myData$converted_values
        print("Warning: ROS model was not fitted. Imputed values are original values")
      }
      myData <- data2
    }
  }
  return(myData)
}



# ## Function Deprecated - replace with impute.upper()
# rightCensored <- function(x) {   # this function uses survreg (package::survival) to impute values for CLARITY observations that are ABOVE multiple detection limits
#   
#   myData <-  x
#   myVar <-  as.character(myData$parameter[1])
#   
#   if(myVar != "BDISC") {     # if this is not Clarity
#     myData$status <- TRUE
#     myData$i2Values <- myData$iValues
#     myData$i2Values[myData$CenType == "Right"] <- 1.1*myData$iValues[myData$CenType == "Right"] # add 10% (catchs the E.coli greater thans).
#   } else { # only 24 samples or non detects for clarity < 5% [this guardes against crashing survreg below.
#     if(nrow(myData) < 24 | (sum(myData$CenType == "Right")/nrow(myData) < 0.05)) {
#       myData$status <- TRUE
#       myData$i2Values <- myData$iValues
#       myData$i2Values[myData$CenType == "Right"] <- 1.1*myData$iValues[myData$CenType == "Right"] # add 10%
#           
#     } else {
#       #print(as.character(myData$sIDnpID[1]))
#       # these lines can be used to see what is happening.
#       #plot(myData$myDate, myData$values)
#       #points(myData$myDate[myData$dflag == "al"],  myData$values[myData$dflag == "al"], col="red", pch=3)
#       # fit distribution
#       myData$status <- myData$CenType != "Right"   # note well if myData flag is "al" (meaning censored) this
#       # is equivalent to ?not dead? status in a survival model and therefore is
#       #cbind.myData.frame(myData$status , myData$values)
#       myMod <- survreg(Surv(Value, status) ~ 1, data = myData, dist="weibull") # using original observed non-censored
#       RScale <- as.numeric(exp(myMod$coefficients)) # this is the rweibull scale (see "survreg" function notes)
#       RShape <- 1/myMod$scale   # this is the weibull shape
#       #hist(myData$values[myData$dflag != "al"], main= "Example Distribution for site clarity myData")
#       #plot(density(myData$values[myData$dflag != "al"]), type="p", pch=1, main="Modelled Distribution of survival times (or observed values)")
#       #points(density(rweibull(10000, shape=RShape, scale=RScale)), col="blue")
#       SynthDist <- sort(rweibull(10000, shape=RShape, scale=RScale))   # synthetic distribution
#       # NB large sample taken to ensure that there are several values in SynthDist > the non detects
#       # otherwise the function below falls over.
#       myData$i2Values <-  unlist(lapply(1:length(myData$Value), function(x) {
#         if(myData$CenType[x] != "Right") {
#           Impute <- myData$iValues[x]
#         } else {
#           myVal <- myData$Value[x]
#           Impute <- resample(SynthDist[SynthDist > myVal], size=1)
#         }
#         return(Impute)
#       }))
#     }
#   }
#   return(myData)
# }
# 
# rightCensored <- function(x)
#Impute.upper <- function(x) {   
rightCensored <- function(x) {   
    # this function uses survreg (package::survival) to impute values for CLARITY observations that are ABOVE multiple detection limits
  # this is the original function written by Ton Snelder
  #browser()
  #x<-tmp
  # ------------------------------------------------------------------------------------------------
  # Sean Hodges 2017-07-09
  # This routine differs from the originally supplied censoring algorithm
  # The following code block adds in the variables described above. This should be worked into the
  # main state and trend script files, but for the moment, it will be ok to leave it here.
  # x$converted_values : the raw data 
  # x$dflag            :a flag indicating whether the raw data are left censored ("dl"), right censored ("al"), 
  #                     or within the lower and upper detection limits ("ok")
  x$converted_values <- x$Value
  
  x$dflag <- "ok"
  
  x$dflag[x$Censored==TRUE & x$CenType=="Left"] <- "dl"
  x$dflag[x$Censored==TRUE & x$CenType=="Right"] <- "al"
  # ------------------------------------------------------------------------------------------------
  
  myData <-  x
  myVar <-  as.character(myData$parameter[1])
     
  if(myVar != "BDISC") {     # if this is not Clarity ( NB  E. coli have "al" values but very low numbers - not enough to fit model.
    myData$i2Values <- myData$i1Values
    myData$i2Values[myData$dflag == "al"] <- 1.1*myData$i1Values[myData$dflag == "al"] # add 10% (catchs the E.coli greater thans).
    
  } else { # only 24 samples or non detects for clarity < 5% [this guards against crashing survreg below.
    if(nrow(myData) < 24 | (sum(myData$dflag == "al")/nrow(myData) < 0.05)) {
      myData$i2Values <- myData$i1Values
      myData$i2Values[myData$dflag == "al"] <- 1.1*myData$i1Values[myData$dflag == "al"] # add 10%
     
    } else {
      #print(as.character(myData$sIDnpID[1]))
      # these lines can be used to see what is happening.
      #plot(myData$myDate, myData$values)
      #points(myData$myDate[myData$dflag == "al"],  myData$values[myData$dflag == "al"], col="red", pch=3)
      # fit distribution
      myData$status <- myData$dflag != "al"   # note well if myData flag is "al" (meaning censored) this is equivalent to ?not dead? status in a survival model and therefore is
      #cbind.myData.frame(myData$status , myData$values)
      #stop("check that using 'values' in the code below is correct!!")
      #myMod <- survreg(Surv(converted_values, status) ~ 1, data = myData, dist="weibull") # using original observed non-censored
      SurvObj <- Surv(myData$converted_values, myData$status)
      myMod <- lawaSurvReg(SurvObj ~ 1, myData, weights, subset, na.action, dist = "weibull", 
                                      init = NULL, scale = 0, control, parms = NULL, model = FALSE, 
                                      x = FALSE, y = TRUE, robust = FALSE, score = FALSE)
      RScale <- as.numeric(exp(myMod$coefficients)) # this is the rweibull scale (see "survreg" function notes)
      RShape <- 1/myMod$scale   # this is the weibull shape
      #hist(myData$values[myData$dflag != "al"], main= "Example Distribution for site clarity myData")
      #plot(density(myData$values[myData$dflag != "al"]), type="p", pch=1, main="Modelled Distribution of survival times (or observed values)")
      #points(density(rweibull(10000, shape=RShape, scale=RScale)), col="blue")
      SynthDist <- sort(rweibull(10000, shape=RShape, scale=RScale))   # synthetic distribution
      # NB large sample taken to ensure that there are several values in SynthDist > the non detects
      # otherwise the function below falls over.
      
      if(length(SynthDist) == 0){
        myData$i2Values <- myData$i1Values
      } else myData$i2Values <-  unlist(lapply(1:length(myData$Value), function(x) {
        if(myData$dflag[x] != "al") {
          Impute <- myData$i1Values[x]
        } else {
          myVal <- myData$converted_values[x]
          Impute <- resample(SynthDist[SynthDist > myVal], size=1)
        }
        return(Impute)
      }))
      #drop status column
      lngth <- length(myData[1,])
      myData <- myData[,c(1:(lngth-2),lngth)]
    }
  }
  
  return(myData)
}

# addJitter <- function(x, myValues="i2Values")
#Impute.tied <-  function(x) {     
addJitter <-  function(x) {     
    # this uses jittering to break ties between measured values.  
  # It is loosely based upon code the function "Impute2()" supplied to me by Bruce Dudley and origianlly written by 
  # Ton Snelder, with modifications by Olivia Burge
  # function expects as imput a column of data named i2Values - which is the output from succesively applying "Impute.lower" & "Impute.upper"
  
  # ------------------------------------------------------------------------------------------------
  # Sean Hodges 2017-07-09
  # This routine differs from the originally supplied censoring algorithm
  # The following code block adds in the variables described above. This should be worked into the
  # main state and trend script files, but for the moment, it will be ok to leave it here.
  # x$converted_values : the raw data 
  # x$dflag            :a flag indicating whether the raw data are left censored ("dl"), right censored ("al"), 
  #                     or within the lower and upper detection limits ("ok")
  x$converted_values <- x$Value
  
  x$dflag <- "ok"
  
  x$dflag[x$Censored==TRUE & x$CenType=="Left"] <- "dl"
  x$dflag[x$Censored==TRUE & x$CenType=="Right"] <- "al"
  # ------------------------------------------------------------------------------------------------
  
  myData <-  x
  myData$i3Values <- myData[, "i2Values"]   # add jitter is impute step 3
  DL <- myData$dflag == "dl" | myData$dflag == "al" # these need not be jittered as randomly imputed.
  Jit <- !DL & duplicated(myData[, "i2Values"]) & !is.na(myData[, "i2Values"])  # these values are Not at Detect Limit and are duplicated (need jittering)
  # head(cbind.data.frame(myData$i2Values, DL, !DL, duplicated(myData$i2Values), Jit), 25)
  myData$i3Values[Jit] <-  jitter(myData[,"i2Values"][Jit])
  # these lines can be used to see what is happening.
  # plot(myData$myDate, 
  #      myData$values, pch=4, col="grey50", xlab="Date", ylab = "Water Quality variable")
  #   points(myData$myDate[DL],  myData$values[DL], col="blue", pch=1)
  #   points(myData$myDate[Jit],  myData$values[Jit], col="red", pch=3) # shows the tied values
  #   points(myData$myDate[Jit],  myData$i3Values[Jit], col="green", pch=16, cex =1)
  #   points(myData$myDate[!Jit],  myData$i3Values[!Jit], col="grey", pch=16, cex=1)
  #   legend("topright",  legend=c("Obs","DL", "Ties", "Jittered", "Imputed"), col = c("black", "blue","red", "green", "grey"),
  #   text.col = "black",  pch = c(4, 1, 3, 16, 16), bg = 'white', inset = .05)   # REMOVE THESE GUYS  ->  lty = c(1, 1),   merge = TRUE,
  return(myData)
  
}

## survreg {survival}
## edited survreg function - customising to take data for censoring - dropping a few of the parameter checks as
## only need a couple of arguments in the function.

## Description

## Fit a parametric survival regression model. These are location-scale models for an arbitrary transform of the time variable; 
## the most common cases use a log transformation, leading to accelerated failure time models.

lawaSurvReg <- function (formula, data, weights, subset, na.action, dist = "weibull", 
                         init = NULL, scale = 0, control, parms = NULL, model = FALSE, 
                         x = FALSE, y = TRUE, robust = FALSE, score = FALSE, ...) 
{
  
  Call <- match.call()
  # indx <- match(c("formula", "data", "weights", "subset", "na.action"), 
  #               names(Call), nomatch = 0)
  indx <- c(1,1,0,0,0)
  if (indx[1] == 0) 
    stop("A formula argument is required")
  #temp <- Call[c(1, indx)]
  temp <- formula
  temp[[1L]] <- quote(stats::model.frame)
  special <- c("strata", "cluster")
  temp$formula <- terms(formula, special)
  # temp$formula <- if (missing(data)) 
  #   terms(formula, special)
  # else terms(formula, special, data = data)
  m <- eval(temp, parent.frame())
  Terms <- attr(m, "terms")
  weights <- model.extract(m, "weights")
  Y <- model.extract(m, "response")
  if (!inherits(Y, "Surv")) 
    stop("Response must be a survival object")
  type <- attr(Y, "type")
  if (type == "counting") 
    stop("start-stop type Surv objects are not supported")
  if (type == "mright" || type == "mcounting") 
    stop("multi-state survival is not supported")
  strats <- attr(Terms, "specials")$strata
  cluster <- attr(Terms, "specials")$cluster
  dropx <- NULL
  if (length(cluster)) {
    if (missing(robust)) 
      robust <- TRUE
    tempc <- untangle.specials(Terms, "cluster", 1:10)
    ord <- attr(Terms, "order")[tempc$terms]
    if (any(ord > 1)) 
      stop("Cluster can not be used in an interaction")
    cluster <- strata(m[, tempc$vars], shortlabel = TRUE)
    dropx <- tempc$terms
  }
  if (length(strats)) {
    temp <- untangle.specials(Terms, "strata", 1)
    dropx <- c(dropx, temp$terms)
    if (length(temp$vars) == 1) {
      strata.keep <- m[[temp$vars]]}
    else {strata.keep <- strata(m[, temp$vars], shortlabel = TRUE)}
    strata <- as.numeric(strata.keep)
    nstrata <- max(strata)
  } else {
    nstrata <- 1
    strata <- 0
  }
  if (length(dropx)) {
    newTerms <- Terms[-dropx]
    attr(newTerms, "intercept") <- attr(Terms, "intercept")
  } else {newTerms <- Terms}
  X <- model.matrix(newTerms, m)
  assign <- lapply(attrassign(X, newTerms)[-1], function(x) x - 1)
  xlevels <- .getXlevels(newTerms, m)
  contr.save <- attr(X, "contrasts")
  n <- nrow(X)
  nvar <- ncol(X)
  offset <- model.offset(m)
  if (length(offset) == 0 || all(offset == 0)) {
    offset <- rep(0, n)}
  if (is.character(dist)) {
    dist <- match.arg(dist, names(survreg.distributions))
    dlist <- survreg.distributions[[dist]]
    if (is.null(dlist)) {
      stop(paste(dist, ": distribution not found"))}
  } else if (is.list(dist)) {
    #dlist <- dist
  } else {
    stop("Invalid distribution object")
  }
  if (!survregDtest(dlist)) {
    stop("Invalid distribution object")}
  logcorrect <- 0
  if (!is.null(dlist$trans)) {
    tranfun <- dlist$trans
    exactsurv <- Y[, ncol(Y)] == 1
    if (any(exactsurv)) {
      if (is.null(weights)) 
        logcorrect <- sum(log(dlist$dtrans(Y[exactsurv, 
                                             1])))
      else logcorrect <- sum(weights[exactsurv] * log(dlist$dtrans(Y[exactsurv, 
                                                                     1])))
    }
    if (type == "interval") {
      if (any(Y[, 3] == 3)) 
        Y <- cbind(tranfun(Y[, 1:2]), Y[, 3])
      else Y <- cbind(tranfun(Y[, 1]), Y[, 3])
    }
    else if (type == "left") 
      Y <- cbind(tranfun(Y[, 1]), 2 - Y[, 2])
    else Y <- cbind(tranfun(Y[, 1]), Y[, 2])
    if (!all(is.finite(Y))) 
      stop("Invalid survival times for this distribution")
  } else {
    if (type == "left") 
      Y[, 2] <- 2 - Y[, 2]
    else if (type == "interval" && all(Y[, 3] < 3)) 
      Y <- Y[, c(1, 3)]
  }
  if (!is.null(dlist$scale)) {
    if (!missing(scale)) 
      warning(paste(dlist$name, "has a fixed scale, user specified value ignored"))
    scale <- dlist$scale
  }
  if (!is.null(dlist$dist)) {
    if (is.atomic(dlist$dist)){ 
      dlist <- survreg.distributions[[dlist$dist]]
    }
  } else {dlist <- dlist$dist}
  ptemp <- dlist$parms
  if (is.null(ptemp)) {
    if (!is.null(parms)) 
      stop(paste(dlist$name, "distribution has no optional parameters"))
  } else {
    if (!is.numeric(ptemp)) 
      stop("Default parameters must be a numeric vector")
    if (!missing(parms)) {
      temp <- unlist(parms)
      indx <- match(names(temp), names(ptemp))
      if (any(is.na(indx))) 
        stop("Invalid parameter names")
      ptemp[names(ptemp)] <- temp
    }
    parms <- ptemp
  }
  #if (missing(control)) {
  control <- survreg.control()
  #} else {control <- do.call("survreg.control", control)}
  if (any(scale < 0)) 
    stop("Invalid scale value")
  if (any(scale > 0) && nstrata > 1) 
    stop("The scale argument is not valid with multiple strata")
  pterms <- sapply(m, inherits, "coxph.penalty")
  if (any(pterms)) {
    pattr <- lapply(m[pterms], attributes)
    temp <- c(attr(Terms, "response"), attr(Terms, "offset"))
    if (length(dropx)) 
      temp <- c(temp, dropx + 1)
    pterms <- pterms[-temp]
    temp <- match((names(pterms))[pterms], attr(Terms, "term.labels"))
    ord <- attr(Terms, "order")[temp]
    if (any(ord > 1)) 
      stop("Penalty terms cannot be in an interaction")
    assign <- attrassign(X, newTerms)
    pcols <- assign[match(names(pterms[pterms]), names(assign))]
    fit <- survpenal.fit(X, Y, weights, offset, init = init, 
                         controlvals = control, dist = dlist, scale = scale, 
                         strata = strata, nstrat = nstrata, pcols, pattr, 
                         parms = parms, assign)
  }  else{ fit <- survreg.fit(X, Y, weights, offset, init = init, 
                              controlvals = control, dist = dlist, scale = scale, nstrat = nstrata, 
                              strata, parms = parms)}
  if (is.character(fit)) {
    fit <- list(fail = fit)
  } else {
    if (scale == 0) {
      nvar <- length(fit$coefficients) - nstrata
      fit$scale <- exp(fit$coefficients[-(1:nvar)])
      if (nstrata == 1){ 
        names(fit$scale) <- NULL
      } else {
        names(fit$scale) <- levels(strata.keep)
      }
      fit$coefficients <- fit$coefficients[1:nvar]
      fit$idf <- 1 + nstrata
    } else {
      fit$scale <- scale
      fit$idf <- 1
    }
    fit$loglik <- fit$loglik + logcorrect
  }
  if (!score) 
    fit$score <- NULL
  fit$df.residual <- n - sum(fit$df)
  fit$terms <- Terms
  fit$contrasts <- contr.save
  if (length(xlevels)) 
    fit$xlevels <- xlevels
  fit$means <- apply(X, 2, mean)
  if (!is.null(weights)) 
    fit$weights <- weights
  fit$call <- Call
  fit$dist <- dist
  if (model) 
    fit$model <- m
  if (x) 
    fit$x <- X
  if (y) 
    fit$y <- Y
  if (length(parms)) 
    fit$parms <- parms
  if (robust) {
    fit$naive.var <- fit$var
    if (!model){ 
      fit$model <- m}
    if (length(cluster)) {
      fit$var <- crossprod(rowsum(residuals.survreg(fit, 
                                                    "dfbeta"), cluster))
    } else {fit$var <- crossprod(residuals.survreg(fit, "dfbeta"))}
    if (!model) {
      fit$model <- NULL}
  }
  na.action <- attr(m, "na.action")
  if (length(na.action)){ 
    fit$na.action <- na.action}
  if (any(pterms)){ 
    class(fit) <- c("survreg.penal", "survreg")
  } else {class(fit) <- "survreg"}
  fit
}