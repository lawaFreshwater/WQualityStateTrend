# ======================
ANALYSIS<-"Main routine"
# Set working directory
od<-getwd()
rm(list = ls())
setwd("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state")
source("scripts/WQualityStateTrend/lawa_state_functions.R")


message("LAWA State and Trend analysis started")
print(Sys.time())
timeAtStart<- Sys.time()

## DATA Import
## Site Tables / Location data
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/lawa_dataPrep_WFS.R")

## Water Quality data
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/lawa_load_swq.R")

## GEOLOCATE Sites in Catchments
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/lawa_intersect_WFS.R")

## STATE And TREND Analysis
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/lawa_state_5yr.R")
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/lawa_trend_5yr.R")
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/lawa_trend_10yr.R")

## NOF Analysis
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/NOF_SWQ.R")

## Preparing data for Export
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/FinaliseTrend.R")
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/lawa_10yrsDataPull.R")
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/lawa_dataForGraphingOnLaWA.R")
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/PackageDataForDelivery.R")


message("Process finished")
print(Sys.time())
print(Sys.time()-timeAtStart)