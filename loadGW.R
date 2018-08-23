## Import data from Council Hilltop Server

## Export to a Hilltop XML file with all supplied data intact.

## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data
Process<-TRUE
message(paste("GWRC: Loading data from GWRC Hilltop Server",Process))

if(Process){
            if(exists("importDestination")&!file.exists(paste(importDestination,file="gwrcSWQ.csv",sep=""))){
  write.csv(c(0),file=paste(importDestination,file="gwrcSWQ.csv",sep=""))

  ## SET LOCAL WORKING DIRECTORY
  od<-getwd()
  setwd("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state")
  
  
  ## Load libraries ------------------------------------------------
  require(XML)     ### XML library to write hilltop XML
  require(dplyr)   ### dply library to manipulate table joins on dataframes
  require(RCurl)
  
  curdir<-getwd()
  
  ### Wellington
  
  ## To pull the data from Northland hilltop server, I have a config csv that contains the 
  ## site and measurement names
  
  fname <- "H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/gwrc_config.csv"
  df <- read.csv(fname,sep=",",stringsAsFactors=FALSE)
  siteTable=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors=FALSE)
  
  configsites <- subset(df,df$Type=="Site")[,2]
  configsites <- as.vector(configsites)
  sites = unique(siteTable$CouncilSiteID[siteTable$Agency=='gwrc'])
  Measurements <- subset(df,df$Type=="Measurement")[,2]
  
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
      df <- data.frame(Date=as.Date(character()),
                       File=character(), 
                       User=character(), 
                       stringsAsFactors=FALSE) 
      return(df)
    }
  }

  #function to either create full xml file or return xml file as NULL depending
  #on the result from the above funciton
  requestData <- function(url){
    ret <- htsServiceError(url)
    if(attr(ret,"class")[1]=="XMLInternalDocument"){
      return(ret)
    } else {
      return(NULL)
    }
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
  
  con <- xmlOutputDOM("Hilltop")
  con$addTag("Agency", "GWRC")
  
  
  for(i in 1:length(sites)){
    cat(sites[i],'out of ',length(sites),'\n')
    for(j in 1:length(Measurements)){
      
      url <- paste("http://hilltop.gw.govt.nz/Data.hts?service=Hilltop",
                   "&request=GetData",
                   "&Site=",sites[i],
                   "&Measurement=",Measurements[j],
                   "&From=2004-01-01",
                   "&To=2018-01-01",sep="")
      url <- gsub(" ", "%20", url)
      #cat(url,"\n")
      
      
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
          
          
        # Special Case  
        } else if(Measurements[j]=="Turbidity (Lab) [Turbidity (Lab)(X)]" & sites[i]=="Horokiri Stream at Snodgrass"){
          ## Make new E node
          # Get Time values
          # ansTime <- lapply(c("T"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
          # ansTime <- unlist(ansTime)
          # ansValue <- lapply(c("Value"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
          # ansValue <- unlist(ansValue)
          # If a xml element has a parameter with Name="Lab", it is an SOE sample. Autosamples don't have Lab attributes
          # ansParamLab <- lapply(c("Parameter[@Name='Lab']/@Value"),function(var) unlist(xpathApply(m,paste("//",var,sep=""))))
          # ansParamLab <- unlist(ansParamLab)
          ansTime <- lapply(c("Parameter[@Name='Lab']/../T"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
          ansTime <- unlist(ansTime)
          ansValue <- lapply(c("Parameter[@Name='Lab']/../Value"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
          ansValue <- unlist(ansValue)
          
          # build temporary dataframe to hold result
          m_df <- data.frame(ansTime,ansValue, stringsAsFactors=FALSE)
          
          # Get Time and values for nodes with Quality Codes
          ansTimeQ <- lapply(c("T"),function(var) unlist(xpathApply(m,paste("//QualityCode/../",var,sep=""),xmlValue)))
          ansTimeQ <- unlist(ansTimeQ)
          qualValue <- lapply(c("QualityCode"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
          qualValue <- as.numeric(unlist(qualValue))
          # build temporary dataframe to hold result
          q_df <- data.frame(ansTimeQ,qualValue, stringsAsFactors=FALSE)
          
          # merge the two data frames by Time
          if(nrow(q_df)==0){
            x_df <- m_df
            x_df$qualValue <- 0 
          } else {
            x_df <- merge(m_df,q_df,by.x="ansTime", by.y = "ansTimeQ",all = TRUE)
          }
          # where there is no quality code, set value to 0
          x_df$qualValue[is.na(x_df$qualValue)] <- 0
          rm(ansTime, ansValue, ansTimeQ, qualValue)
          
          
          # loop through TVP nodes
          #for(N in 1:xmlSize(m[['Data']])){  ## Number of Time series values
          for(N in 1:nrow(x_df)){  ## Number of Time series values
            # Quality codes for GWRC data are not applied against all values. Therefore, only use values
            # that match the specification provided by Mark Heath at GWRC, i.e. Exclude values that are QC400
            if(x_df$qualValue[N]!=400){
              # loop through all Children - T, Value, Parameters ..    
              addChildren(DataNode, newXMLNode(name = "E",parent = DataNode))
              addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "T",x_df$ansTime[N]))
              
              #Check for < or > or *
              ## Hand Greater than symbol
              if(grepl(pattern = "^\\>",x =  x_df$ansValue[N],perl = TRUE)){
                x_df$ansValue[N] <- substr(x_df$ansValue[N],2,nchar(x_df$ansValue[N]))
                item2 <- paste("$ND",tab,">",tab,sep="")
                
                # Handle Less than symbols  
              } else if(grepl(pattern = "^\\<",x =  x_df$ansValue[N],perl = TRUE)){
                x_df$ansValue[N] <- substr(x_df$ansValue[N],2,nchar(x_df$ansValue[N]))
                item2 <- paste("$ND",tab,"<",tab,sep="")
                
                # Handle Asterixes  
              } else if(grepl(pattern = "^\\*",x =  x_df$ansValue[N],perl = TRUE)){
                x_df$ansValue[N] <- gsub(pattern = "^\\*", replacement = "", x = x_df$ansValue[N])
                item2 <- paste("$ND",tab,"*",tab,sep="")
              } else{
                item2 <- ""
              }
              addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "I1",x_df$ansValue[N]))
              
              # if(xmlSize(m[['Data']][[N]])>2){    ### Has attributes to add to Item 2
              #   for(n in 3:xmlSize(m[['Data']][[N]])){      
              #     #Getting attributes and building string to put in Item 2
              #     attrs <- xmlAttrs(m[['Data']][[N]][[n]])  
              #     
              #     item2 <- paste(item2,attrs[1],tab,attrs[2],tab,sep="")
              #     
              #   }
              # }
              
              item2 <- paste(item2,"$QC",tab,x_df$qualValue[N],tab,sep="")
              
              addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "I2",item2))
            }
          } 
        } else {
          ## Make new E node
          # Get Time values
          ansTime <- lapply(c("T"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
          ansTime <- unlist(ansTime)
          ansValue <- lapply(c("Value"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
          ansValue <- unlist(ansValue)
          # build temporary dataframe to hold result
          m_df <- data.frame(ansTime,ansValue, stringsAsFactors=FALSE)
          
          # Get Time and values for nodes with Quality Codes
          ansTimeQ <- lapply(c("T"),function(var) unlist(xpathApply(m,paste("//QualityCode/../",var,sep=""),xmlValue)))
          ansTimeQ <- unlist(ansTimeQ)
          qualValue <- lapply(c("QualityCode"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
          qualValue <- as.numeric(unlist(qualValue))
          # build temporary dataframe to hold result
          q_df <- data.frame(ansTimeQ,qualValue, stringsAsFactors=FALSE)
          
          # merge the two data frames by Time
          if(nrow(q_df)==0){
            x_df <- m_df
            x_df$qualValue <- 0 
          } else {
            x_df <- merge(m_df,q_df,by.x="ansTime", by.y = "ansTimeQ",all = TRUE)
          }
          # where there is no quality code, set value to 0
          x_df$qualValue[is.na(x_df$qualValue)] <- 0
          rm(ansTime, ansValue, ansTimeQ, qualValue)
          
          
          # loop through TVP nodes
          for(N in 1:xmlSize(m[['Data']])){  ## Number of Time series values
            # Quality codes for GWRC data are not applied against all values. Therefore, only use values
            # that match the specification provided by Mark Heath at GWRC, i.e. Exclude values that are QC400
            if(x_df$qualValue[N]!=400){
              # loop through all Children - T, Value, Parameters ..    
              addChildren(DataNode, newXMLNode(name = "E",parent = DataNode))
              addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "T",x_df$ansTime[N]))
              
              #Check for < or > or *
              ## Hand Greater than symbol
              if(grepl(pattern = "^\\>",x =  x_df$ansValue[N],perl = TRUE)){
                x_df$ansValue[N] <- substr(x_df$ansValue[N],2,nchar(x_df$ansValue[N]))
                item2 <- paste("$ND",tab,">",tab,sep="")
                
                # Handle Less than symbols  
              } else if(grepl(pattern = "^\\<",x =  x_df$ansValue[N],perl = TRUE)){
                x_df$ansValue[N] <- substr(x_df$ansValue[N],2,nchar(x_df$ansValue[N]))
                item2 <- paste("$ND",tab,"<",tab,sep="")
                
                # Handle Asterixes  
              } else if(grepl(pattern = "^\\*",x =  x_df$ansValue[N],perl = TRUE)){
                x_df$ansValue[N] <- gsub(pattern = "^\\*", replacement = "", x = x_df$ansValue[N])
                item2 <- paste("$ND",tab,"*",tab,sep="")
              } else{
                item2 <- ""
              }
              addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "I1",x_df$ansValue[N]))
              
              if(xmlSize(m[['Data']][[N]])>2){    ### Has attributes to add to Item 2
                for(n in 3:xmlSize(m[['Data']][[N]])){      
                  #Getting attributes and building string to put in Item 2
                  attrs <- xmlAttrs(m[['Data']][[N]][[n]])  
                  
                  item2 <- paste(item2,attrs[1],tab,attrs[2],tab,sep="")
                  
                }
              }
              
              item2 <- paste(item2,"$QC",tab,x_df$qualValue[N],tab,sep="")
              
              addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "I2",item2))
            }
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
    saveXML(con$value(), paste(importDestination,file="gwrcSWQ.xml",sep=""))
  } else {
    saveXML(con$value(), file="gwrcSWQ.xml")
  }
  cat("Finished",Sys.time()-tm,"\n")
  
  setwd(od)
  
}
}
rm(Process)