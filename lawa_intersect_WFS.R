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

od <- getwd()
wd <- "\\\\file\\herman\\R\\OA\\08\\02\\2017\\Water Quality\\R\\lawa_state"
setwd(wd)

#/* -===Include required function libraries===- */ 

source("lawa_state_functions.R")

require(rgeos)
require(spatialEco)
require(maptools)
require(rgdal)

## Supplementary functions
ld <- function(url){
  (download.file(url,destfile="tmp",method="wininet"))
  xmlfile <- xmlParse(file = "tmp")
  unlink("tmp")
  return(xmlfile)
}

points.in.polys <- function (pts, polys) {
  if (!inherits(polys, "SpatialPolygonsDataFrame")) 
    stop("MUST BE SP SpatialPolygonsDataFrame OBJECT")
  if ((inherits(pts, "SpatialPointsDataFrame") | inherits(pts, 
                                                          "SpatialPoints")) == FALSE) 
    stop("Must be sp SpatialPointsDataFrame object")
  z <- pts[!is.na(sp::over(pts, sp::geometry(polys))), ]
  y <- sp::over(pts,polys)
  y <- y[!is.na(y$LAWA_CATCH),]
  z@data <- data.frame(z@data, y)
  
  
  z@proj4string <- pts@proj4string
  z
}

# ======================================
# Load WFS locations from CSV

## Load csv with WFS locations
siteTable <- read.csv("LAWA_Site_Table.csv",stringsAsFactors=FALSE)

siteTable$Lat <- as.numeric(siteTable$Lat)
siteTable$Long <- as.numeric(siteTable$Long)

pts <- siteTable[complete.cases(siteTable$Lat),]
# Cast dataframe as SpatialPointDataFrame
coordinates(pts) <- ~ Long + Lat


# Load catchment polys as SpatialPolygonsDataFrame
# LAWA_CATCHMENTS_WGS84 - original - replaced by the eIDI FW catchment file

polys <- readOGR(dsn="\\\\file\\herman\\R\\OA\\08\\02\\Mapping\\data\\2017\\eIDI-FW-Catchments.shp",
                 layer="eIDI-FW-Catchments",p4s = NULL,
                 stringsAsFactors = FALSE)

# Just use the Parent Catchments
#polys <- subset(polys,CatchType=="Parent")

# Set the projection of the wq sites to match the catchments, since everything in WGS84 
pts@proj4string <- polys@proj4string

# Intersect points and polygons using user-defined function
pip <- points.in.polys(pts,polys)

# Just keeping required fields
pip.data <- pip@data[,c(4,2:3,5:11,15:22)] ## field list(LawaSiteID,SiteID,CouncilSiteID,SWQuality:Agency,)

#pip.data$LAWA_CATCH[pip.data$LAWA_CATCH==0] <- pip.data$CatchID[pip.data$LAWA_CATCH==0]

dd<- pip.data[!grepl("NRWQN",pip.data$LawaSiteID,ignore.case = TRUE),]
df<- pip.data[grepl("NRWQN",pip.data$LawaSiteID,ignore.case = TRUE),]

# Some columns need to be moved around so that correct id's are in correct order
# This should be dealt with during the initial pull, but for the time-being, we'll
# deal with this through post-processing feeds

# EBOP and NRC need to have SiteId and CouncilSiteID's swapped
region <-c ("Auckland","Bay of Plenty","Northland","Gisborne")
for(p in 1:length(region)){
  siteID <- dd$CouncilSiteID[grepl(region[p],dd$Region,ignore.case = TRUE)]
  dd$CouncilSiteID[grepl(region[p],dd$Region,ignore.case = TRUE)] <- dd$SiteID[grepl(region[p],dd$Region,ignore.case = TRUE)]
  dd$SiteID[grepl(region[p],dd$Region,ignore.case = TRUE)] <- siteID
}


site<-c("HRC-00036","HRC-00042")
for(p in 1:length(site)){
  siteID <- dd$CouncilSiteID[dd$LawaSiteID==site[p]]
  dd$CouncilSiteID[dd$LawaSiteID==site[p]] <- dd$SiteID[dd$LawaSiteID==site[p]]
  dd$SiteID[dd$LawaSiteID==site[p]] <- siteID
}

pip.data <- rbind.data.frame(dd,df)

pip.data$SiteID        <- trimws(pip.data$SiteID)
pip.data$CouncilSiteID <- trimws(pip.data$CouncilSiteID) 


#pip.data <- read.csv("LAWA_Site_Table1.csv", stringsAsFactors=FALSE)
chk <- grepl("\\&",pip.data$CouncilSiteID)
cat(sum(chk),": site names includuing '&' character")
pip.data$CouncilSiteID <- gsub("\\&","%26",pip.data$CouncilSiteID)    # replace "&" symbols (as a reserved character) with ascii representation %26
#pip.data$LawaCatchm[pip.data$LawaCatchm==0]<-1    # Make sure all catchments set to 1 in case filters set later to exclude zeros.
pip.data <- unique(pip.data)
cat(sum(chk),": site names includuing '&' character")
write.csv(pip.data,"LAWA_Site_Table1.csv")
#write.csv(pip.data,"LAWA_Site_Table.csv")


