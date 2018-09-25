#===================================================================================================
#  LAWA DATA PREPARATION - Intersect WFS Sites with Catchments
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

ANALYSIS<-"Intersect WFS"
# Set working directory

# od     <- getwd()
# wd     <- "\\\\file\\herman\\R\\OA\\08\\02\\2017\\Water Quality\\R\\lawa_state"
# setwd(wd)
#/* -===Include required function libraries===- */ 

# source("h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")
# setwd('../../')

# require(rgeos)
# require(spatialEco)
# require(maptools)
require(rgdal)

## Supplementary functions
# ld <- function(url){
#   (download.file(url,destfile="tmp",method="wininet"))
#   xmlfile <- xmlParse(file = "tmp")
#   unlink("tmp")
#   return(xmlfile)
# }

points.in.polys <- function (pts, polys) {
  if (!inherits(polys, "SpatialPolygonsDataFrame")) 
    stop("MUST BE SP SpatialPolygonsDataFrame OBJECT")
  if ((inherits(pts, "SpatialPointsDataFrame") | inherits(pts, 
                                                          "SpatialPoints")) == FALSE) 
    stop("Must be sp SpatialPointsDataFrame object")
  z <- pts[!is.na(sp::over(pts, sp::geometry(polys))), ]
  if(length(z)<length(pts)){
    stop(paste0(length(pts)-length(z)," points not inside polys, these will be dropped!"))
  }
  y <- sp::over(pts,polys)
  y <- y[!is.na(y$LAWA_CATCH),]
  z@data <- data.frame(z@data, y)
  
  
  z@proj4string <- pts@proj4string
  z
}

# ======================================
# Load WFS locations from CSV

 siteTable <- read.csv("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors=FALSE)  #From lawa_dataPrep_WFS.r


pts <- siteTable[complete.cases(siteTable$Lat),]  #All are complete


#Several sites identified as falling outside polygons, this pulls them in 

thisN=which.min(sqrt((pts$Lat+42.40396)^2 + (pts$Long-173.6842)^2))
pts$Long[thisN]=173.6826
pts$Lat[thisN]=-42.40353
rm(thisN)

# plot(polys,xlim=c(172.7,173),ylim=c(-43.56,-43.52))
# points(pts$Long,pts$Lat,pch=16,col='red')

# locator(1)
thisN=which.min(sqrt((pts$Lat+43.52365)^2 + (pts$Long-172.7235)^2))
# points(pts$Long[thisN],pts$Lat[thisN],pch=16,col='blue')
pts$Long[thisN]=172.7235
pts$Lat[thisN]=-43.52365
rm(thisN)

# locator(1)
thisN=which.min(sqrt((pts$Lat+43.55053)^2 + (pts$Long-172.7002)^2))
# points(pts$Long[thisN],pts$Lat[thisN],pch=16,col='blue')
pts$Long[thisN]=172.7002
pts$Lat[thisN]=-43.55053
rm(thisN)


# Cast dataframe as SpatialPointDataFrame
coordinates(pts) <- ~ Long + Lat


# Load catchment polys as SpatialPolygonsDataFrame
# LAWA_CATCHMENTS_WGS84 - original - replaced by the eIDI FW catchment file

polys <- readOGR(dsn="H:/ericg/16666LAWA/2018/eIDI-FW-Catchments.shp",
                 layer="eIDI-FW-Catchments",p4s = NULL,
                 stringsAsFactors = FALSE)

# Just use the Parent Catchments
#polys <- subset(polys,CatchType=="Parent")

# Set the projection of the wq sites to match the catchments, since everything in WGS84 
pts@proj4string <- polys@proj4string


which(is.na(sp::over(pts, sp::geometry(polys))))
# Intersect points and polygons using user-defined function
pip <- points.in.polys(pts,polys)  #Any warnings here means points need pulled in to inside catchments, as above

# Just keeping required fields
pip.data <- pip@data[,c(2:3,1,4:11,16:22)] ## field list(LawaSiteID,SiteID,CouncilSiteID,SWQuality:Agency,)

dd<- pip.data[!grepl("NRWQN",pip.data$LawaSiteID,ignore.case = TRUE),]
df<- pip.data[grepl("NRWQN",pip.data$LawaSiteID,ignore.case = TRUE),]

pip.data <- rbind.data.frame(dd,df)

pip.data$SiteID        <- tolower(trimws(pip.data$SiteID))
pip.data$CouncilSiteID <- tolower(trimws(pip.data$CouncilSiteID))


#pip.data <- read.csv("LAWA_Site_Table1.csv", stringsAsFactors=FALSE)
chk <- grepl("\\&",pip.data$CouncilSiteID)
if(sum(chk)>0){
  cat(sum(chk),": site names includuing '&' character")
  pip.data$CouncilSiteID[chk]
  pip.data$CouncilSiteID <- gsub("\\&","%26",pip.data$CouncilSiteID)    # replace "&" symbols (as a reserved character) with ascii representation %26
}
rm(chk)
#pip.data$LawaCatchm[pip.data$LawaCatchm==0]<-1    # Make sure all catchments set to 1 in case filters set later to exclude zeros.
pip.data <- unique(pip.data)

write.csv(pip.data,paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/LAWA_Site_Table1.csv"),row.names=F)
#write.csv(pip.data,"LAWA_Site_Table.csv")

siteTableMerge <- merge(siteTable,pip.data,by="LawaSiteID",all.x=TRUE)
write.csv(siteTableMerge,
          paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/reviewWFS-SiteList.csv"),row.names=F)


#Load latest siteTable1, which is intersected with Catchment
#NOTE  THIS IS NOT JUST THE SITE  TABLE!
stbl=tail(dir(path="h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",pattern="LAWA_Site_Table1.csv",recursive=T,full.names=T),1)
catSiteTable <- read.csv(stbl,stringsAsFactors = F)
rm(stbl)
catSiteTable$SWQLanduse[tolower(catSiteTable$SWQLanduse)%in%c("forest","native","exotic","natural")] <- "forest"
catSiteTable$SWQLanduse[tolower(catSiteTable$SWQLanduse)%in%c("unstated","")] <- NA
catSiteTable$SiteID[catSiteTable$SiteID=="karapiro stm at hickey rd bridge - cambridge"] <- "karapiro stm at hickey rd bridge"
catSiteTable$SiteID=trimws(catSiteTable$SiteID)
catSiteTable$CouncilSiteID=trimws(catSiteTable$CouncilSiteID)
catSiteTable$LawaSiteID=trimws(catSiteTable$LawaSiteID)
catSiteTable$SWQAltitude=tolower(catSiteTable$SWQAltitude)
catSiteTable$SWQLanduse=tolower(catSiteTable$SWQLanduse)
catSiteTable$SWQFrequencyAll=tolower(catSiteTable$SWQFrequencyAll)
catSiteTable$SWQFrequencyLast5=tolower(catSiteTable$SWQFrequencyLast5)
catSiteTable$Region=tolower(catSiteTable$Region)
catSiteTable$Agency=tolower(catSiteTable$Agency)
save(catSiteTable,file="h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawa_sitetable.RData")
