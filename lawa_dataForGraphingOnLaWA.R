#Import rdata set of 10 years of data
load("//file/herman/R/OA/08/02/2017/Water Quality/ROutput/RC-Supplied-Data2007-2016.RData")

# columns: LAWAID, SiteName, Measurement, Date, Date, Censored, Censored Type, Value, Region
raw_data <- lawadata[,c(9,1,5,2,2,6,7,3,16)]

# Reorganising table
raw_data$Symbol[raw_data$CenType=="Left"] <- "<"
raw_data$Symbol[raw_data$CenType=="Right"] <- ">"
raw_data$Raw_Value <- raw_data$Value
raw_data$Raw_Value[!is.na(raw_data$Symbol)] <- paste(raw_data$Symbol[!is.na(raw_data$Symbol)],raw_data$Value[!is.na(raw_data$Symbol)],sep="")
raw_data <- raw_data[,c(1:5,11,10,8,9)]
names(raw_data) <- c("LAWAID","Site","Measurement","DateTime","Date","Raw Value","Symbol","Value","Region")

#The export below will include all sites imported from Councils, irrespective of whether they 
#have lawa id's or not.
#Export table
write.csv(raw_data,"//file/herman/R/OA/08/02/2017/Water Quality/ROutput/RiverWQ_GraphData.csv")

#To write this table out, excluding sites without lawa ids, uncomment the following line.
write.csv(raw_data[!is.na(raw_data$LAWAID),],"//file/herman/R/OA/08/02/2017/Water Quality/ROutput/RiverWQ_GraphData_LAWASitesOnly.csv")


# #----------------------------------------------------------------------------------------------------------
# # Filter data to remove NIWA
# raw_data <- lawadata[!grepl("NRWQN",lawadata$LAWAID,ignore.case = TRUE),c(1,5,2,2,6,7,13)]
# sitesToDrop <- c("Manawatu at Teachers Coll.","Manwatu at Weber Rd","Manawatu at Opiki Br.") # NIWA Sites not identified by NRWQN ids
# for(i in length(sitesToDrop)){
#   raw_data <- raw_data[raw_data$Site!=sitesToDrop[i],]
# }
# raw_data$Symbol[raw_data$CenType=="Left"] <- "<"
# raw_data$Symbol[raw_data$CenType=="Right"] <- ">"
# raw_data$Raw_Value <- raw_data$OriginalValue
# raw_data$Raw_Value[!is.na(raw_data$Symbol)] <- paste(raw_data$Symbol[!is.na(raw_data$Symbol)],raw_data$OriginalValue[!is.na(raw_data$Symbol)],sep="")
# raw_data <- raw_data[,c(1:4,9,8,7)]
# names(raw_data) <- c("Site","Measurement","DataTime","Date","Raw Value","Symbol","Value")
# 
# #rm(lawadata,raw_data)
