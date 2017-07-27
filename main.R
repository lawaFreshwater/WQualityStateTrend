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
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/scripts/WQualityStateTrend/lawa_dataPrep_WFS.R")

## Water Quality data
#source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/scripts/WQualityStateTrend/lawa_load_swq.R")

## GEOLOCATE Sites in Catchments
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/scripts/WQualityStateTrend/lawa_intersect_WFS.R")

## STATE And TREND Analysis
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_5yr.R")
#source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/scripts/WQualityStateTrend/lawa_trend_5yr.R")
#source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/scripts/WQualityStateTrend/lawa_trend_10yr.R")

## NOF Analysis
#source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/scripts/WQualityStateTrend/NOF_SWQ.R")

## Preparing data for Export
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/scripts/WQualityStateTrend/FinaliseTrend.R")
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/scripts/WQualityStateTrend/lawa_10yrsDataPull.R")
#source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/scripts/WQualityStateTrend/lawa_dataForGraphingOnLaWA.R")
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/scripts/WQualityStateTrend/PackageDataForDelivery.R")


message("Process finished")
print(Sys.time())
print(Sys.time()-timeAtStart)