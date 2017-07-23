# ------------------------------
# BATCH LOADER FOR COUNCIL DATA
# ------------------------------

message("Load Surface Water Quality data from Councils")
message("-- A folder for todays date will be created and the imported files will be stashed there.")

# Encapsulating mkdir commands in the try() in order to suppress error messages on failure
# Failure's can mean
#               1. Directories already exist
#               1. R:/ drive not mapped to \\file\herman\R\OA\08\02

try(shell(paste('mkdir "R:/2017/Water Quality/1.Imported/"',format(Sys.Date(),"%Y-%m-%d"),sep=""), translate=TRUE),silent = TRUE)
try(shell(paste('mkdir "R:/2017/Water Quality/4.Analysis/"',format(Sys.Date(),"%Y-%m-%d"),sep=""), translate=TRUE),silent = TRUE)


## ----------------------------------------------------------------------------,
## Write Hilltop XML for Water Quality Data

## import destination will be in folder with todays date (created above)
importDestination <- paste("//file/herman/R/OA/08/02/2017/Water Quality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/",sep="")


#Northland
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadNRC.R")
rm("Data","df","df2","df2","sample","udf")

#Auckland
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadAC.R")
rm("Data","df","df2","df2","sample","udf")

#Waikato
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadWRC.R")
rm("Data","df","df2","df2","sample","udf")

#Bay of Plenty
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadBOP.R")
rm("Data","df","df2","df2","sample","udf")

#Gisborne
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadGDC.R")
rm("Data","df","df2","df2","sample","udf")

#Taranaki
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadTRC.R")
rm("Data","df","df2","df2","sample","udf")

#Hawkes Bay
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadHBRC_v2.R")
rm("Data","df","df2","df2","sample","udf")

#Horizons
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadHRC.R")
rm("Data","df","df2","df2","sample","udf")

#Greater Wellington
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadGW.R")
rm("Data","df","df2","df2","sample","udf")

#Nelson
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadNCC.R")
rm("Data","df","df2","df2","sample","udf")

#Tasman
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadTDC.R")
rm("Data","df","df2","df2","sample","udf")

#Marlborough
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadMDC.R")
rm("Data","df","df2","df2","sample","udf")

#Canterbury
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadECAN.R")
rm("Data","df","df2","df2","sample","udf")

#Otago
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadORC2.R")
rm("Data","df","df2","df2","sample","udf")

#Southland
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadES.R")
rm("Data","df","df2","df2","sample","udf")

#NIWA
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadNIWA.R")
rm("Data","df","df2","df2","sample","udf")

#West Coast
source("//file/herman/r/oa/08/02/2017/Water Quality/R/lawa_state/loadWCRC.R")
