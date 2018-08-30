# ------------------------------
# BATCH LOADER FOR COUNCIL DATA
# ------------------------------

message("Load Surface Water Quality data from Councils")
message("-- A folder for todays date will be created and the imported files will be stashed there.")

# Encapsulating mkdir commands in the try() in order to suppress error messages on failure
# Failure's can mean
#               1. Directories already exist
#               1. R:/ drive not mapped to \\file\herman\R\OA\08\02

try(shell(paste('mkdir "H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/"',format(Sys.Date(),"%Y-%m-%d"),sep=""), translate=TRUE),silent = F)
# try(shell(paste('mkdir "H:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/"',format(Sys.Date(),"%Y-%m-%d"),sep=""), translate=TRUE),silent = F)


## ----------------------------------------------------------------------------,
## Write Hilltop XML for Water Quality Data

## import destination will be in folder with todays date (created above)
importDestination <- paste("H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/",sep="")

# 
# #1 Auckland
#  # http://aklc.hydrotel.co.nz:8080/KiWIS/KiWIS
  try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadAC.R"))
#  rm("Data","df","df2","df2","sample","udf")
# 
# #2 Bay of Plenty
#  # http://envdata.waikatoregion.govt.nz:8080/KiWIS/KiWIS
#try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadBOP.R"))
#  rm("Data","df","df2","df2","sample","udf")
# 
# #3 Canterburyhttp://wateruse.ecan.govt.nz/wqlawa.hts?service=Hilltop
 try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadECAN.R"))
# rm("Data","df","df2","df2","sample","udf")
# 
# #4 Southland http://odp.es.govt.nz/WQ.hts?service=Hilltop
# try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadES.R"))
# rm("Data","df","df2","df2","sample","udf")
# 
# #5 Gisborne http://hilltop.gdc.govt.nz/data.hts?service=Hilltop"
#  try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadGDC.R"))
# rm("Data","df","df2","df2","sample","udf")
# 
# #6 Greater Wellington http://hilltop.gw.govt.nz/Data.hts?service=Hilltop
# try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadGW.R"))
# rm("Data","df","df2","df2","sample","udf")
# 
# #7 Hawkes Bay http://data.hbrc.govt.nz/Envirodata/WQForTrend.hts?service=Hilltop
# try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadHBRC.R"))
# rm("Data","df","df2","df2","sample","udf")
# 
#8 Horizons http://tsdata.horizons.govt.nz/boo.hts?service=SOS
try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadHRC.R"))
rm("Data","df","df2","df2","sample","udf")
# 
# #9 Marlborough http://hydro.marlborough.govt.nz/LAWA_WQ.hts?service=Hilltop
# try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadMDC.R"))
# rm("Data","df","df2","df2","sample","udf")
# 
# #10 Nelson #http://envdata.nelson.govt.nz/data.hts?service=Hilltop
# try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadNCC.R")) 
# rm("Data","df","df2","df2","sample","udf")
# 
# #11 Northland http://hilltop.nrc.govt.nz/SOERiverWQ.hts?service=Hilltop
# try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadNRC.R"))
#  rm("Data","df","df2","df2","sample","udf")
# 
#12 Otago http://gisdata.orc.govt.nz/hilltop/WQGlobal.hts?service=Hilltop
# try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadORC.R"))
# rm("Data","df","df2","df2","sample","udf")
# 
# #13 Tasman http://envdata.tasman.govt.nz/WaterQuality.hts?service=Hilltop
# try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadTDC.R"))
# rm("Data","df","df2","df2","sample","udf")
# 
# #14 Taranaki https://extranet.trc.govt.nz/getdata/LAWA_river_WQ.hts?service=Hilltop
# try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadTRC.R"))
# rm("Data","df","df2","df2","sample","udf")
# 
# #15 West Coast http://hilltop.wcrc.govt.nz/wq.hts?service=Hilltop
# try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadWCRC.R"))
# 
# #6 Waikato http://envdata.waikatoregion.govt.nz:8080/KiWIS/KiWIS
#  try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadWRC.R"))
#  rm("Data","df","df2","df2","sample","udf")





#NIWA
# try(source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/loadNIWA.R"))
rm("Data","df","df2","df2","sample","udf")