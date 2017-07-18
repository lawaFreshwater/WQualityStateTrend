#requestData(vendor[tss],tss_url,"service=Hilltop&request=Reset")

# build template file of all possible results for Trend - Sites x Parameter x Five/Ten Yr

# specify a matrix of appropriate size

# building blank data file
if(!exists("templateSeason")){
  library(readr)
  seasons <- read_csv("//file/herman/R/OA/08/02/2016/Water Quality/R/lawa_state/seasons.csv")
  
  
  for(j in 1:nrow(l)){
    for(i in 1:length(wqparam)){
    
    if(!exists("templateSeason")){
      templateSeason <- seasons
      templateSeason$Parameter <- wqparam[i]
      templateSeason$LawaSiteID <- l$LawaSiteID[j]
    } else {
      tmpSeason <- seasons
      tmpSeason$Parameter <- wqparam[i]
      tmpSeason$LawaSiteID <- l$LawaSiteID[j]
      
      templateSeason      <- rbind(templateSeason,tmpSeason)
    }
    
    
    }
  }
  rm(tmpSeason)
  
  save(templateSeason,file = "templateSeason.Rda")
}

#build blank trend results file

trend_fordelivery <- read.csv("//file/herman/R/OA/08/02/2016/Water Quality/ROutput/trend_fordelivery.csv")
trend_fordelivery <- trend_fordelivery[,c(2:length(trend_fordelivery))]

#/* -===Local variable/constant definitions===- */
l <- read.csv("LAWA_Site_Table1.csv",stringsAsFactors=FALSE)
l$SWQLanduse[l$SWQLanduse=="Native"|l$SWQLanduse=="Exotic"|l$SWQLanduse=="Natural"] <- "Forest"

wqparam  <- c("BDISC","TURB","NH4","TON","TN","DRP","TP","ECOLI") 


blank.trend.rows <- nrow(l)*length(wqparam)*2
data.blank <- matrix(data=NA, nrow=blank.trend.rows, ncol=length(trend_fordelivery), byrow=TRUE)

counter<-1
for(i in 1:nrow(l)){
  for(j in 1:length(wqparam)){
    for(k in 1:length(landuse)){
      for(m in 1:length(altitude)){
      data.blank[counter,] <- c(l$LawaSiteID[i],wqparam[j],l$SWQLanduse[i],l$SWQAltitude[i],NA,l$SWQFrequencyLast5[i],l$Region[i],5)
      counter<-counter+1
      data.blank[counter,] <- c(l$LawaSiteID[i],wqparam[j],l$SWQLanduse[i],l$SWQAltitude[i],NA,l$SWQFrequencyAll[i],l$Region[i],10)
      counter<-counter+1
      }
    }
  }
}

data.blank <- as.data.frame(data.blank)

names(data.blank) <- c("Location","Parameter","Altitude","Landuse","TrendScore","Frequency","Region","period")

tmp <- merge(data.blank,trend_fordelivery,by=c("Location","Parameter","Altitude","Landuse","period"),all=TRUE)
tmp$Frequency.x[complete.cases(tmp$Frequency.y)] <- tmp$Frequency.y[complete.cases(tmp$Frequency.y)]
tmp$Region.x[complete.cases(tmp$Region.y)] <- tmp$Region.y[complete.cases(tmp$Region.y)]

tmp <- tmp[,c(1:4,9,7,8,5)]
names(tmp) <- c("Location","Parameter","Altitude","Landuse","TrendScore","Frequency","Region","period")
tmp$TrendScore <- as.character(tmp$TrendScore)
tmp$TrendScore[is.na(tmp$TrendScore)] <- "NA"

trend_fordelivery <- write.csv(tmp, "//file/herman/R/OA/08/02/2016/Water Quality/ROutput/trend_fordelivery_with_NA.csv")
