# ======================
ANALYSIS<-"Main routine"
# Set working directory
od<-getwd()
rm(list = ls())

# //file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/scripts/WQualityStateTrend/

setwd("./scripts/WQualityStateTrend/")
source("./lawa_state_functions.R")


message("LAWA State and Trend analysis started")
print(Sys.time())
timeAtStart<- Sys.time()

## DATA Import
## Site Tables / Location data
source("./lawa_dataPrep_WFS.R")

## Water Quality data
source("./lawa_load_swq.R")

## GEOLOCATE Sites in Catchments
source("./lawa_intersect_WFS.R")

## STATE And TREND Analysis
source("./lawa_state_5yr.R")
source("./lawa_trend_x_yrs.R")

## NOF Analysis
source("./NOF_SWQ.R")

## Preparing data for Export
source("./FinaliseTrend.R")
source("./lawa_10yrsDataPull.R")
source("./lawa_dataForGraphingOnLaWA.R")
source("./PackageDataForDelivery.R")


message("Process finished")
print(Sys.time())
print(Sys.time()-timeAtStart)


EColi
Black Disc
Turbidity
Total nitrogen
Total oxidised nitrogen
Ammoniacal nitrogen
Dissolved reactive phosphorus
Reactive phosphorus
pH

