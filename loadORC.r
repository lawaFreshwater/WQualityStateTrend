## Import data from Council Hilltop Server

## Export to a Hilltop XML file with all supplied data intact.

## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data

Process<-TRUE
message(paste("ORC: Loading data from ORC Hilltop Server",Process))

if(Process){

  ## SET LOCAL WORKING DIRECTORY
  od<-getwd()
  setwd("//file/herman/R/OA/08/02/2018/Water Quality/R/lawa_state")
  
  
  ## Load libraries ------------------------------------------------
  require(XML)     ### XML library to write hilltop XML
  require(dplyr)   ### dply library to manipulate table joins on dataframes
  require(RCurl)
  
  curdir<-getwd()
  
  ### Canterbury
  
  ## To pull the data from Canterbury hilltop server, I have a config csv that contains the 
  ## site and measurement names
  
  fname <- "//file/herman/R/OA/08/02/2018/Water Quality/R/lawa_state/2018_csv_config_files/orcSWQ_config.csv"
  df <- read.csv(fname,sep=",",stringsAsFactors=FALSE)
  
  sites <- subset(df,df$Type=="Site")[,2]
  Measurements <- subset(df,df$Type=="Measurement")[,2]
  
  #function to create xml file from url. 
  ld <- function(url){
    str<- tempfile(pattern = "file", tmpdir = tempdir())
    (download.file(url,destfile=str,method="wininet"))
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
      return(TRUE)   # if no error, return TRUE
    } else {
      return(FALSE)
    }
  }
  
  #function to either create full xml file or return xml file as NULL depending
  #on the result from the above funciton
  requestData <- function(url){
    #url<-"http://hilltopdev.horizons.govt.nz/data.hts?service=Hilltop"
    #RCurl::getURL(paste(url,"&request=Reset",sep=""))
    #url <- paste(url,request,sep="")
    cat(url,"\n")
    ret <- htsServiceError(url)
    if(ret==TRUE){
      xmldata <- ld(url)
      return(xmldata)
    }else {
      xmldata <- NULL
      return(xmldata)
      
    }
  }
  
  
  
  ## ===============================================================================
  ## Getting Site Data 
  
  # For each council server specified...
  # Assumption is that gml:pos has coordinates recorded in lat,lon order
  ## Build XML Document --------------------------------------------
  tm<-Sys.time()
  cat("Building XML\n")
  cat("Creating:",Sys.time()-tm,"\n")
  
  con <- xmlOutputDOM("Hilltop")
  con$addTag("Agency", "ORC")
  
  
  for(i in 1:length(sites)){
    
    for(j in 1:length(Measurements)){
      
      url <- paste("http://gisdata.orc.govt.nz/hilltop/WQGlobal.hts?service=Hilltop",
                   "&request=GetData",
                   "&Site=",sites[i],
                   "&Measurement=",Measurements[j],
                   "&From=2004-01-01",
                   "&To=2018-01-01",sep="")
      url <- gsub(" ", "%20", url)
      cat(url,"\n")
      
      
      #------------------------------------------
      
      
      
      xmlfile <- requestData(url)
      
      
      if(!is.null(xmlfile)){
        xmltop<-xmlRoot(xmlfile)
        
        m<-xmltop[['Measurement']]
        
        
        # Create new node to replace existing <Data /> node in m
        DataNode <- newXMLNode("Data",attrs=c(DateFormat="Calendar",NumItems="2"))
        
        #addChildren(DataNode, newXMLNode(name = "E",parent = DataNode))
        
        #addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "T","time"))
        #addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "I1","item1"))
        #addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "I2","item2"))
        
        
        #cat(saveXML(DataNode),"\n")
        tab="\t"
        
        #work on this to make more efficient
        if(Measurements[j]=="WQ Sample"){
          ## Make new E node
          # Get Time values
          #ans <- lapply(c("T"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
          ans <- xpathApply(m,"//T",xmlValue)
          ans <- unlist(ans)
          
          
          
          #new bit here
          for(k in 1:length(ans)){
            
            # Adding the new "E" node
            addChildren(DataNode, newXMLNode(name = "E",parent = DataNode))
            # Adding the new "T" node
            addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "T",ans[k]))
            
            # Identifying and iterating through Parameter Elements of "m"
            p <- m[["Data"]][[k]] 
            
            # i'th E Element inside <Data></Data> tags
            c <- length(xmlSApply(p, xmlSize)) # number of children for i'th E Element inside <Data></Data> tags
            for(n in 2:c){   # Starting at '2' and '1' is the T element for time
              #added in if statement to pass over if any date&times with no metadata attached
              if(!is.null(p[[n]])){
                metaName  <- as.character(xmlToList(p[[n]])[1])   ## Getting the name attribute
                metaValue <- as.character(xmlToList(p[[n]])[2])   ## Getting the value attribute
                if(n==2){      # Starting at '2' and '1' is the T element for time
                  item1 <- paste(metaName,metaValue,sep=tab)
                } else {
                  item1 <- paste(item1,metaName,metaValue,sep=tab)
                }
              }
            }
            # Adding the Item1 node
            addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "I1",item1))
            
          }
          
          
          
        } else {
          ## Make new E node
          # Get Time values
          ansTime <- lapply(c("T"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
          ansTime <- unlist(ansTime)
          ansValue <- lapply(c("Value"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
          ansValue <- unlist(ansValue)
          
          # loop through TVP nodes
          for(N in 1:xmlSize(m[['Data']])){  ## Number of Time series values
            # loop through all Children - T, Value, Parameters ..    
            addChildren(DataNode, newXMLNode(name = "E",parent = DataNode))
            addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "T",ansTime[N]))
            
            #Check for < or > or *
            ## Hand Greater than symbol
            if(grepl(pattern = "^\\>",x =  ansValue[N],perl = TRUE)){
              ansValue[N] <- substr(ansValue[N],2,nchar(ansValue[N]))
              item2 <- paste("$ND",tab,">",tab,sep="")
              
              # Handle Less than symbols  
            } else if(grepl(pattern = "^\\<",x =  ansValue[N],perl = TRUE)){
              ansValue[N] <- substr(ansValue[N],2,nchar(ansValue[N]))
              item2 <- paste("$ND",tab,"<",tab,sep="")
              
              # Handle Asterixes  
            } else if(grepl(pattern = "^\\*",x =  ansValue[N],perl = TRUE)){
              ansValue[N] <- gsub(pattern = "^\\*", replacement = "", x = ansValue[N])
              item2 <- paste("$ND",tab,"*",tab,sep="")
            } else{
              item2 <- ""
            }
            addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "I1",ansValue[N]))
            
            if(xmlSize(m[['Data']][[N]])>2){    ### Has attributes to add to Item 2
              for(n in 3:xmlSize(m[['Data']][[N]])){      
                #Getting attributes and building string to put in Item 2
                attrs <- xmlAttrs(m[['Data']][[N]][[n]])  
                
                item2 <- paste(item2,attrs[1],tab,attrs[2],tab,sep="")
                
              }
            }
            
            addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "I2",item2))
            
          } 
        }
        #saveXML(DataNode)
        
        oldNode <- m[['Data']]
        newNode <- DataNode
        replaceNodes(oldNode, newNode)
        
        
        
        con$addNode(m) 
        
      }
    }
  }
  cat("Saving: ",Sys.time()-tm,"\n")
  if(exists("importDestination")){
  saveXML(con$value(), paste(importDestination,file="orcSWQ.xml",sep=""))
  } else {
  saveXML(con$value(), file="orcSWQ.xml")
  }
  cat("Finished",Sys.time()-tm,"\n")
  
  setwd(od)
}

rm(Process)