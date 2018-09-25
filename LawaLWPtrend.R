rm(list=ls())
library(tidyverse)
source("h:/ericg/16666LAWA/LWPTrends_v1811_beta.R")
source("h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")
try(dir.create(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"))))

riverSiteTable=read.csv(file="h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors = F)
# load("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawa_sitetable.RData")

#Load the latest made (might need to check it works nicely across month folders) 
if(!exists('wqdata')){
  acwqdata=tail(dir(path = "H:/ericg/16666LAWA/2018/WaterQuality/1.Imported",pattern = "AllCouncils.csv",recursive = T,full.names = T),1)
  cat(acwqdata)
  wqdata=read.csv(acwqdata,stringsAsFactors = F)
  rm(acwqdata)
  wqdata$SWQLanduse[is.na(wqdata$SWQLanduse)]=riverSiteTable$SWQLanduse[match(wqdata$LawaSiteID[is.na(wqdata$SWQLanduse)],riverSiteTable$LawaSiteID)]
  wqdata$SWQAltitude[is.na(wqdata$SWQAltitude)]=riverSiteTable$SWQAltitude[match(wqdata$LawaSiteID[is.na(wqdata$SWQAltitude)],riverSiteTable$LawaSiteID)]
  wqdata$SWQLanduse[tolower(wqdata$SWQLanduse)%in%c("unstated","")] <- NA
  wqdata$SWQLanduse[tolower(wqdata$SWQLanduse)%in%c("forest","native","exotic","natural")] <- "forest"
  wqdata$myDate <- as.Date(as.character(wqdata$Date),"%d-%b-%Y")
  wqdata <- GetMoreDateInfo(wqdata)
  wqdata$monYear = format(wqdata$myDate,"%b-%Y")
  
  wqdata$Season <- wqdata$Month
  SeasonString <- sort(unique(wqdata$Season))
  
  wqdata$CenType[wqdata$CenType%in%c("Left","L")]='lt'
  wqdata$CenType[wqdata$CenType%in%c("Right","R")]='gt'
  wqdata$CenType[!wqdata$CenType%in%c("lt","gt")]='not'
  
  wqdata$NewValues=wqdata$Value
}
#Show that censored values are excluded from Trend Analysis
if(0){
  par(mfrow=c(1,1))
  SenSlope(x=data.frame(RawValue=c(seq(1,1.1,length.out = 300)+rnorm(n = 300,mean = 0,sd=0.2),rep(0.5,300)),
                      Censored=c(rep(FALSE,300),rep(TRUE,300)),myDate=seq(from = 1,to = 600),
                      Season=rep(1:12,each=50),Year=rep(1:50,each=12),SeasonYear=paste0(rep(1:12,each=50),rep(1:50,each=12)),
                      CenType=c(rep("not",300),rep('lt',300))),doPlot = T)
}

EndYear <- 2017
startYear5 <- EndYear - 5+1
startYear10 <- EndYear - 10+1

# https://www.lawa.org.nz/learn/factsheets/calculating-water-quality-trends/
# But LWPTrends drops all censored values for SenSlope calcn





#10 year trend ####
wqdatafor10=wqdata%>%filter(Year>=startYear10 & Year <= EndYear & parameter!="PH")

usites=unique(wqdatafor10$LawaSiteID)
uMeasures=unique(wqdatafor10$parameter)
trendTable10=structure(list(LawaSiteID=NA,parameter=NA,
                            Observations = NA_integer_, KWstat = NA_real_,pvalue = NA_real_,
                            nObs = NA_integer_, S = NA_real_, VarS = NA_real_,D = NA_real_, tau = NA_real_, Z = NA_real_, p = NA_real_,Probability=NA_real_,
                             Median = NA_real_, VarS = NA_real_, AnnualSenSlope = NA_real_, Intercept = NA_real_, Lci = NA_real_, Uci = NA_real_, TrendCategory = NA, 
                             TrendDirection = NA, Sen_Probability = NA_real_, Probabilitymax = NA_real_, Probabilitymin = NA_real_, Percent.annual.change = NA_real_), class = "data.frame")
nMax=length(table(wqdatafor10$LawaSiteID,wqdatafor10$parameter)[table(wqdatafor10$LawaSiteID,wqdatafor10$parameter)>0])
passCriteria10=data.frame(LawaSiteID=rep('',nMax),param=rep('',nMax),repFreq=rep('',nMax),
                          nFirstYear=rep(0,nMax),nLastYear=rep(0,nMax),
                          numSamples=rep(0,nMax),numYears=rep(0,nMax),
                          stringsAsFactors = F)
pcpos=1
cat(length(usites),'\n')
usite=1
latestAgency=''
for(usite in usite:length(usites)){
  cat('.')
  if(as.integer(usite/10)==(usite/10)){cat(usite,'\n')}
  subDat=wqdatafor10%>%filter(LawaSiteID==usites[usite])
  if(subDat$Agency[1]!=latestAgency){
    latestAgency=subDat$Agency[1]
    cat(latestAgency,'\n')
  }
  for(uparam in 1:length(uMeasures)){
    subSubDat=subDat%>%filter(subDat$parameter==uMeasures[uparam])
    if(dim(subSubDat)[1]>0){
      SSD_med <- summaryBy(formula=Value~LawaSiteID+monYear,
                           id=~Censored+CenType+myDate+Year+Month+Qtr+Season,
                           data=subSubDat, 
                           FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
      
      #Censoring fails with tiny machine rounding errors
      SSD_med$Value=round(SSD_med$Value*10^6)/10^6
      
      firstYear=length(which(SSD_med$Year==startYear10))
      lastYear=length(which(SSD_med$Year==EndYear))
      numSamples=dim(SSD_med)[1]
      numYears=length(unique(SSD_med$Year[!is.na(SSD_med$Value)]))
      passCriteria10[pcpos,]=c(usites[usite],uMeasures[uparam],paste0(unique(subSubDat$SWQFrequencyAll),collapse=','),
                               firstYear,lastYear,numSamples,numYears)
      pcpos=pcpos+1
      rm(subSubDat)
      #For 10 year monthly we want 90% of measures and 90% of years
      if(numSamples >= 0.9*120 & numYears>=9){
        cat('+')
        suppressWarnings(rm(st,mk,ss,sk,sss))
        SeasonString <- sort(unique(SSD_med$Season))
        (st <- SeasonalityTest(x = SSD_med,main=uMeasures[uparam],ValuesToUse = "Value",do.plot =F))
        if(!is.na(st$pvalue)&&st$pvalue<0.05){
          (sk <- SeasonalKendall(x = SSD_med,ValuesToUse = "Value",doPlot = F))
          (sss <- SeasonalSenSlope(HiCensor=T,x = SSD_med,ValuesToUse = "Value",ValuesToUseforMedian="Value",doPlot = F))
          newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,sk,sss)
        }else{
          (mk <- MannKendall(x = SSD_med,ValuesToUse = "Value",doPlot=F))
          (ss <- SenSlope(HiCensor=T,x = SSD_med,ValuesToUse = "Value",ValuesToUseforMedian="Value",doPlot = F))
          newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,mk,ss)
        }
        trendTable10=rbind(trendTable10,newRow)
        rm(newRow)
      }else{
        cat('.')
      }
      rm(SSD_med)
    }
  }
  rm(subDat)
}
rm(usites,uMeasures,usite,uparam,wqdatafor10)
rownames(trendTable10) <- NULL
trendTable10$Probability[trendTable10$parameter!="BDISC"]=1-(trendTable10$Probability[trendTable10$parameter!="BDISC"])
trendTable10$Probabilitymin[trendTable10$parameter!="BDISC"]=1-(trendTable10$Probabilitymin[trendTable10$parameter!="BDISC"])
trendTable10$Probabilitymax[trendTable10$parameter!="BDISC"]=1-(trendTable10$Probabilitymax[trendTable10$parameter!="BDISC"])
save(trendTable10,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10Year.rData"))
save(passCriteria10,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/PassCriteria10.rData"))

#Note that MDC DRP and ECOLI are NOT removed in this saved dataset. They're available in there, so will need removing before use.
# #Remove MDC DRP and ECOLI
# #See email Steffi Henkel 14/9/2018 to Kati Doehring, Eric Goodwin, Abi Loughnan and Peter Hamill
# if(any(tt10agency=="mdc" & trendTable10$parameter=="DRP")){
#   trendTable10 <- trendTable10[-which(tt10agency=='mdc'& trendTable10$parameter=="DRP"),]
#   # 3257 to 3231
#   tt10agency=riverSiteTable$Agency[match(trendTable10$LawaSiteID,riverSiteTable$LawaSiteID)]
# }
# if(any(tt10agency=="mdc" & trendTable10$parameter=="ECOLI")){
#   trendTable10 <- trendTable10[-which(tt10agency=='mdc'& trendTable10$parameter=="ECOLI"),]
#   #3231 to 3205
#   tt10agency=riverSiteTable$Agency[match(trendTable10$LawaSiteID,riverSiteTable$LawaSiteID)]
# }


rm(ss,st,mk)


#5 year trend ####
wqdatafor5=wqdata%>%filter(Year>=startYear5 & Year <= EndYear & parameter!="PH")

usites=unique(wqdatafor5$LawaSiteID)
uMeasures=unique(wqdatafor5$parameter)
trendTable5=structure(list(LawaSiteID=NA,parameter=NA,
                           Observations = NA_integer_, KWstat = NA_real_,pvalue = NA_real_,Probability = NA_real_,
                            nObs = NA_integer_, S = NA_real_, VarS = NA_real_,D = NA_real_, tau = NA_real_, Z = NA_real_, p = NA_real_,
                            Median = NA_real_, VarS = NA_real_, AnnualSenSlope = NA_real_, Intercept = NA_real_, Lci = NA_real_, Uci = NA_real_, TrendCategory = NA,
                            TrendDirection = NA, Sen_Probability = NA_real_, Probabilitymax = NA_real_, Probabilitymin = NA_real_, Percent.annual.change = NA_real_), class = "data.frame")
nMax=length(table(wqdatafor5$LawaSiteID,wqdatafor5$parameter)[table(wqdatafor5$LawaSiteID,wqdatafor5$parameter)>0])
passCriteria5=data.frame(LawaSiteID=rep('',nMax),param=rep('',nMax),repFreq=rep('',nMax),
                          nFirstYear=rep(0,nMax),nLastYear=rep(0,nMax),
                          numSamples=rep(0,nMax),numYears=rep(0,nMax),
                          stringsAsFactors = F)
pcpos=1
cat(length(usites),'\n')
latestAgency=''
for(usite in 1:length(usites)){
  if(as.integer(usite/10)==(usite/10)){cat(usite,'\n')}
  subDat=wqdatafor5%>%filter(LawaSiteID==usites[usite])
  if(subDat$Agency[1]!=latestAgency){
    latestAgency=subDat$Agency[1]
    cat(latestAgency,'\n')
  }
  for(uparam in 1:length(uMeasures)){
    subSubDat=subDat%>%filter(subDat$parameter==uMeasures[uparam])
    if(dim(subSubDat)[1]>0){
      SSD_med <- summaryBy(formula=Value~LawaSiteID+monYear,
                           id=~Censored+CenType+myDate+Year+Month+Qtr+Season,
                           data=subSubDat, 
                           FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
      #Censoring fails with tiny machine rounding errors
      SSD_med$Value=round(SSD_med$Value*10^6)/10^6
      firstYear=length(which(SSD_med$Year==startYear5))
      lastYear=length(which(SSD_med$Year==EndYear))
      numSamples=dim(SSD_med)[1]
      numYears=length(unique(SSD_med$Year[!is.na(SSD_med$Value)]))
      passCriteria5[pcpos,]=c(usites[usite],uMeasures[uparam],paste0(unique(subSubDat$SWQFrequencyAll),collapse=','),
                               firstYear,lastYear,numSamples,numYears)
      pcpos=pcpos+1
      rm(subSubDat)
      #For 5 year monthly we want 90% of measures 
      if(numSamples >= 0.9*60){
        cat('+')
        suppressWarnings(rm(st,mk,ss,sk,sss))
        SeasonString <- sort(unique(SSD_med$Season))
        (st <- SeasonalityTest(x = SSD_med,main=uMeasures[uparam],ValuesToUse = "Value",ValuesToUseforMedian="Value",do.plot =F))
        if(!is.na(st$pvalue)&&st$pvalue<0.05){
          (sk <- SeasonalKendall(x = SSD_med,ValuesToUse = "Value",doPlot = F))
          (sss <- SeasonalSenSlope(HiCensor=T,x = SSD_med,ValuesToUse = "Value",ValuesToUseforMedian="Value",doPlot = F))
          newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,sk,sss)
        }else{
          (mk <- MannKendall(x = SSD_med,ValuesToUse = "Value",doPlot=F))
          (ss <- SenSlope(HiCensor=T,x = SSD_med,ValuesToUse = "Value",ValuesToUseforMedian="Value",doPlot = F))
          newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,mk,ss)
        }
        trendTable5=rbind(trendTable5,newRow)
        rm(newRow)
      }else{
        cat('.')
      }
      rm(SSD_med)
    }
  }
  rm(subDat)
}
rm(usites,uMeasures,usite,uparam,wqdatafor5)
rownames(trendTable5) <- NULL
trendTable5$Probability[trendTable5$parameter!="BDISC"]=1-(trendTable5$Probability[trendTable5$parameter!="BDISC"])
save(trendTable5,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend5Year.rData"))
save(passCriteria5,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/PassCriteria5.rData"))

rm(ss,st,mk)



#Quarterly, to fill gaps where needed ####
wqdataforQ10=wqdata%>%filter(Year>startYear10 & Year <= EndYear & parameter!="PH")
wqdataforQ10$quYear=paste0(wqdataforQ10$Qtr,wqdataforQ10$Year)
usites=unique(wqdataforQ10$LawaSiteID)
uMeasures=unique(wqdataforQ10$parameter)
trendTableQ10=structure(list(LawaSiteID=NA,parameter=NA,
                            Observations = NA_integer_, KWstat = NA_real_,pvalue = NA_real_,Probability = NA_real_,
                             nObs = NA_integer_, S = NA_real_, VarS = NA_real_,D = NA_real_, tau = NA_real_, Z = NA_real_, p = NA_real_, 
                             Median = NA_real_, VarS = NA_real_, AnnualSenSlope = NA_real_, Intercept = NA_real_, Lci = NA_real_, Uci = NA_real_, TrendCategory = NA, 
                             TrendDirection = NA, Sen_Probability = NA_real_, Probabilitymax = NA_real_, Probabilitymin = NA_real_, Percent.annual.change = NA_real_), class = "data.frame")
nMax=length(table(wqdataforQ10$LawaSiteID,wqdataforQ10$parameter)[table(wqdataforQ10$LawaSiteID,wqdataforQ10$parameter)>0])
passCriteriaQ10=data.frame(LawaSiteID=rep('',nMax),param=rep('',nMax),repFreq=rep('',nMax),
                          nFirstYear=rep(0,nMax),nLastYear=rep(0,nMax),
                          numSamples=rep(0,nMax),numYears=rep(0,nMax),
                          stringsAsFactors = F)
pcpos=1
latestAgency=''
cat(length(usites),'\n')
for(usite in 1:length(usites)){
  cat('.')
  if(as.integer(usite/10)==(usite/10)){cat(usite,'\n')}
  subDat=wqdataforQ10%>%filter(LawaSiteID==usites[usite])
  if(subDat$Agency[1]!=latestAgency){
    latestAgency=subDat$Agency[1]
    cat(latestAgency,'\n')
  }
  uparam=1
  for(uparam in uparam:length(uMeasures)){
    subSubDat=subDat%>%filter(subDat$parameter==uMeasures[uparam])
    if(dim(subSubDat)[1]>0){
      SSD_med <- summaryBy(formula=Value~LawaSiteID+quYear,
                           id=~Censored+CenType+myDate+Year+Month+Qtr+Season,
                           data=subSubDat, 
                           FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
      #Censoring fails with tiny machine rounding errors
       SSD_med$Value=round(SSD_med$Value*10^6)/10^6
      rm(subSubDat)
      firstYear=length(which(SSD_med$Year==startYear10))
      lastYear=length(which(SSD_med$Year==EndYear))
      numSamples=dim(SSD_med)[1]
      numYears=length(unique(SSD_med$Year[!is.na(SSD_med$Value)]))
      passCriteriaQ10[pcpos,]=c(usites[usite],uMeasures[uparam],paste0(unique(subDat$SWQFrequencyAll),collapse=','),
                               firstYear,lastYear,numSamples,numYears)
      pcpos=pcpos+1
      #For 10 year quarterly we want 90% of measures 
      if(numSamples >= 0.9*40){
        cat('+')
        suppressWarnings(rm(st,mk,ss,sk,sss))
        SeasonString <- sort(unique(SSD_med$Season))
        (st <- SeasonalityTest(x = SSD_med,main=uMeasures[uparam],ValuesToUse = "Value",ValuesToUseforMedian="Value",do.plot =F))
        if(!is.na(st$pvalue)&&st$pvalue<0.05){
          sk <- SeasonalKendall(x = SSD_med,ValuesToUse = "Value",doPlot = F)
           sss <- SeasonalSenSlope(HiCensor=T,x = SSD_med,ValuesToUse = "Value",ValuesToUseforMedian="Value",doPlot = F)
          newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,sk,sss)
        }else{
          (mk <- MannKendall(x = SSD_med,ValuesToUse = "Value",doPlot=F))
          (ss <- SenSlope(HiCensor=T,x = SSD_med,ValuesToUse = "Value",ValuesToUseforMedian="Value",doPlot = F))
          newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,mk,ss)
        }
        trendTableQ10=rbind(trendTableQ10,newRow)
        rm(newRow)
      }else{
        cat('.')
      }
      rm(SSD_med)
    }
  }
  rm(subDat)
}
rm(usites,uMeasures,usite,uparam)
rownames(trendTableQ10) <- NULL
trendTableQ10$Probability[trendTableQ10$parameter!="BDISC"]=1-(trendTableQ10$Probability[trendTableQ10$parameter!="BDISC"])
save(trendTableQ10,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10YearQ.rData"))
save(passCriteriaQ10,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/PassCriteriaQ10.rData"))

rm(ss,st,mk)



#Reload if necess and continue ####
# rm(list=ls())
source("h:/ericg/16666LAWA/LWPTrends/LWPTrends_v1808.R")
if(!exists('riverSiteTable')){
  riverSiteTable=read.csv("file:///H:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors = F)
}
if(!exists('wqdata')){
  acwqdata=tail(dir(path = "H:/ericg/16666LAWA/2018/WaterQuality/1.Imported",pattern = "AllCouncils.csv",recursive = T,full.names = T),1)
  cat(acwqdata)
  wqdata=read.csv(acwqdata,stringsAsFactors = F)
  rm(acwqdata)
  wqdata$SWQLanduse[is.na(wqdata$SWQLanduse)]=riverSiteTable$SWQLanduse[match(wqdata$LawaSiteID[is.na(wqdata$SWQLanduse)],riverSiteTable$LawaSiteID)]
  wqdata$SWQAltitude[is.na(wqdata$SWQAltitude)]=riverSiteTable$SWQAltitude[match(wqdata$LawaSiteID[is.na(wqdata$SWQAltitude)],riverSiteTable$LawaSiteID)]
  wqdata$SWQLanduse[tolower(wqdata$SWQLanduse)%in%c("unstated","")] <- NA
  wqdata$SWQLanduse[tolower(wqdata$SWQLanduse)%in%c("forest","native","exotic","natural")] <- "forest"
  wqdata$myDate <- as.Date(as.character(wqdata$Date),"%d-%b-%Y")
  wqdata <- GetMoreDateInfo(wqdata)
  
  wqdata$Season <- wqdata$Month
  SeasonString <- sort(unique(wqdata$Season))
  
  wqdata$CenType[wqdata$CenType%in%c("Left","L")]='lt'
  wqdata$CenType[wqdata$CenType%in%c("Right","R")]='gt'
  wqdata$CenType[!wqdata$CenType%in%c("lt","gt")]='not'
  
  wqdata$NewValues=wqdata$Value
}
load(tail(dir(ignore.case=T,
              path = "h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",
              pattern = "Trend10YearQ.rData",full.names = T,recursive = T),1),verbose =T)
load(tail(dir(ignore.case=T,
              path = "h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",
              pattern = "Trend5Year.rData",full.names = T,recursive = T),1),verbose = T)
load(tail(dir(ignore.case=T,
              path = "h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",
              pattern = "Trend10Year.rData",full.names = T,recursive = T),1),verbose = T)
load(tail(dir(ignore.case=T,
              path = "h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",
              pattern = "PassCriteria10.rData",full.names = T,recursive = T),1),verbose = T)
load(tail(dir(ignore.case=T,
              path = "h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",
              pattern = "PassCriteria5.rData",full.names = T,recursive = T),1),verbose = T)
load(tail(dir(ignore.case=T,
              path = "h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",
              pattern = "PassCriteriaQ10.rData",full.names = T,recursive = T),1),verbose = T)

tt10agency=riverSiteTable$Agency[match(trendTable10$LawaSiteID,riverSiteTable$LawaSiteID)]
tt10qagency=riverSiteTable$Agency[match(trendTableQ10$LawaSiteID,riverSiteTable$LawaSiteID)]
tt5agency=riverSiteTable$Agency[match(trendTable5$LawaSiteID,riverSiteTable$LawaSiteID)]

table(tt10agency)/table(riverSiteTable$Agency)[match(names(table(tt10agency)),names(table(riverSiteTable$Agency)))]
table(tt10qagency)/table(riverSiteTable$Agency)[match(names(table(tt10qagency)),names(table(riverSiteTable$Agency)))]
table(tt5agency)/table(riverSiteTable$Agency)[match(names(table(tt5agency)),names(table(riverSiteTable$Agency)))]


#Remove MDC DRP and ECOLI
#See email Steffi Henkel 14/9/2018 to Kati Doehring, Eric Goodwin, Abi Loughnan and Peter Hamill
if(any(tt10agency=="mdc" & trendTable10$parameter=="DRP")){
  trendTable10 <- trendTable10[-which(tt10agency=='mdc'& trendTable10$parameter=="DRP"),]
  # 3257 to 3231
  tt10agency=riverSiteTable$Agency[match(trendTable10$LawaSiteID,riverSiteTable$LawaSiteID)]
}
if(any(tt10agency=="mdc" & trendTable10$parameter=="ECOLI")){
  trendTable10 <- trendTable10[-which(tt10agency=='mdc'& trendTable10$parameter=="ECOLI"),]
  #3231 to 3205
  tt10agency=riverSiteTable$Agency[match(trendTable10$LawaSiteID,riverSiteTable$LawaSiteID)]
}




EndYear <- 2017
startYear5 <- EndYear - 5+1
startYear10 <- EndYear - 10+1

#10yr monthly is gold standard trend. 5yr monthly is silver. 10yr quarterly is bronze standard
trendTable10$standard='gold'
trendTable5$standard='silver'
trendTableQ10$standard='bronze'

combTrend=trendTable10
# 3205
combTrend=rbind(combTrend,trendTable5) #[which(!paste0(trendTable5$LawaSiteID,trendTable5$parameter)%in%paste0(combTrend$LawaSiteID,combTrend$parameter)),]
# 3205 + 5125 = 8330
combTrend=rbind(combTrend,trendTableQ10[which(!paste0(trendTableQ10$LawaSiteID,trendTableQ10$parameter)%in%paste0(combTrend$LawaSiteID,combTrend$parameter)),])
# + 311 = 8641
table(combTrend$standard)
# bronze gold silver 
# 311   3205  5125 








trendTable5$ConfCat <- cut(trendTable5$Probability, breaks=  c(0, 0.1,0.33,0.66,0.90, 1),
                           labels = c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading"))
trendTable10$ConfCat <- cut(trendTable10$Probability, breaks=  c(0, 0.1,0.33,0.66,0.90, 1),
                            labels = c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading"))
trendTableQ10$ConfCat <- cut(trendTableQ10$Probability, breaks=  c(0, 0.1,0.33,0.66,0.90, 1),
                            labels = c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading"))
combTrend$ConfCat <- cut(combTrend$Probability, breaks=  c(0, 0.1,0.33,0.66,0.90, 1),
                             labels = c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading"))

trendTable5$ConfCat=factor(trendTable5$ConfCat,levels=rev(c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading")))
trendTable10$ConfCat=factor(trendTable10$ConfCat,levels=rev(c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading")))
trendTableQ10$ConfCat=factor(trendTableQ10$ConfCat,levels=rev(c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading")))
combTrend$ConfCat=factor(combTrend$ConfCat,levels=rev(c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading")))


trendTable5$period=5
trendTable10$period=10
trendTableQ10$period=10
# trendTable5=trendTable5[,-which(names(trendTable5)=="VarS")[2]]
# trendTable10=trendTable10[,-which(names(trendTable10)=="VarS")[2]]
# trendTableQ10=trendTableQ10[,-which(names(trendTableQ10)=="VarS")[2]]
combTrend=combTrend[,-which(names(combTrend)=="VarS")[2]]
combTrend$period=10
combTrend$period[combTrend$standard=='silver']=5
combTrend$Frequency='monthly'
combTrend$Frequency[combTrend$standard=='bronze'] <- 'quarterly'



combTrend$Altitude =  riverSiteTable$SWQAltitude[match(combTrend$LawaSiteID,riverSiteTable$LawaSiteID)]
combTrend$Landuse =   riverSiteTable$SWQLanduse[match(combTrend$LawaSiteID,riverSiteTable$LawaSiteID)]
combTrend$Region =    riverSiteTable$Region[match(combTrend$LawaSiteID,riverSiteTable$LawaSiteID)]
# combTrend$Frequency = riverSiteTable$SWQFrequencyAll[match(combTrend$LawaSiteID,riverSiteTable$LawaSiteID)]
combTrend$TrendScore=as.numeric(combTrend$ConfCat)-3
combTrend$TrendScore[is.na(combTrend$TrendScore)]<-(-99)
combTrendExport <- combTrend%>%select(LawaSiteID,parameter,Altitude,Landuse,TrendScore,Region,Frequency,period)

write.csv(combTrendExport,paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/RiverWQ_Trend_ForITE",
                         format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),row.names = F)
rm(combTrendExport)

savePlott=F
usites=unique(combTrend$LawaSiteID)
uMeasures=unique(combTrend$parameter)%>%as.character
for(uparam in seq_along(uMeasures)){
  subwq=wqdata[wqdata$parameter==uMeasures[uparam],]
  eval(parse(text=paste0('names(subwq)[3]="',uMeasures[uparam],'"')))
  subTrend=trendTable10[which(trendTable10$parameter==uMeasures[uparam]),]
  worstDeg <- which.max(subTrend$Probability) 
  bestImp <- which.min(subTrend$Probability)
  leastKnown <- which.min(abs(subTrend$Probability-0.5))
  if(savePlott){
    tiff(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/BestWorst",uMeasures[uparam],".tif"),
         width = 8,height=12,units='in',res=300,compression='lzw',type='cairo')
  }else{
    windows()
  }
    par(mfrow=c(3,1),mar=c(2,4,1,2))
    theseDeg <- which(subwq$LawaSiteID==subTrend$LawaSiteID[worstDeg] & dmy(subwq$Date)>dmy("1-1-2008"))
    theseInd <- which(subwq$LawaSiteID==subTrend$LawaSiteID[leastKnown] & dmy(subwq$Date)>dmy("1-1-2008"))
    theseImp <- which(subwq$LawaSiteID==subTrend$LawaSiteID[bestImp] & dmy(subwq$Date)>dmy("1-1-2008"))
    Deg_med <- eval(parse(text=paste0("summaryBy(formula=",uMeasures[uparam],"~LawaSiteID+monYear,
                         id=~Censored+CenType+myDate+Year+Month+Qtr+Season,
                         data=subwq[theseDeg,], 
                         FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)")))
    Ind_med <- eval(parse(text=paste0("summaryBy(formula=",uMeasures[uparam],"~LawaSiteID+monYear,
                         id=~Censored+CenType+myDate+Year+Month+Qtr+Season,
                                      data=subwq[theseInd,], 
                                      FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)")))
    Imp_med <- eval(parse(text=paste0("summaryBy(formula=",uMeasures[uparam],"~LawaSiteID+monYear,
                         id=~Censored+CenType+myDate+Year+Month+Qtr+Season,
                                      data=subwq[theseImp,], 
                                      FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)")))
    st <- SeasonalityTest(x = Deg_med,main=uMeasures[uparam],ValuesToUse = uMeasures[uparam],do.plot =F)
    if(!is.na(st$pvalue)&&st$pvalue<0.05){
      SeasonalKendall(x = Deg_med,ValuesToUse = uMeasures[uparam],doPlot = F)
      SeasonalSenSlope(HiCensor=T,x = Deg_med,ValuesToUse = uMeasures[uparam],ValuesToUseforMedian = uMeasures[uparam],doPlot = T,mymain = subTrend$LawaSiteID[worstDeg])
    }else{
      MannKendall(x = Deg_med,ValuesToUse = uMeasures[uparam],doPlot=F)
      SenSlope(HiCensor=T,x = Deg_med,ValuesToUse = uMeasures[uparam],ValuesToUseforMedian = uMeasures[uparam],doPlot = T,mymain = subTrend$LawaSiteID[worstDeg])
    }
    
    st <- SeasonalityTest(x = Ind_med,main=uMeasures[uparam],ValuesToUse = uMeasures[uparam],do.plot =F)
    if(!is.na(st$pvalue)&&st$pvalue<0.05){
      SeasonalKendall(x = Ind_med,ValuesToUse = uMeasures[uparam],doPlot = F)
      SeasonalSenSlope(HiCensor=T,x = Ind_med,ValuesToUse = uMeasures[uparam],ValuesToUseforMedian = uMeasures[uparam],doPlot = T,mymain = subTrend$LawaSiteID[worstDeg])
    }else{
      MannKendall(x = Ind_med,ValuesToUse = uMeasures[uparam],doPlot=F)
      SenSlope(HiCensor=T,x = Ind_med,ValuesToUse = uMeasures[uparam],ValuesToUseforMedian = uMeasures[uparam],doPlot = T,mymain = subTrend$LawaSiteID[worstDeg])
    }
    
    st <- SeasonalityTest(x = Imp_med,main=uMeasures[uparam],ValuesToUse = uMeasures[uparam],do.plot =F)
    if(!is.na(st$pvalue)&&st$pvalue<0.05){
      SeasonalKendall(x = Imp_med,ValuesToUse = uMeasures[uparam],doPlot = F)
      if(is.na(SeasonalSenSlope(HiCensor=T,x = Imp_med,ValuesToUse = uMeasures[uparam],ValuesToUseforMedian = uMeasures[uparam],doPlot = T,mymain = subTrend$LawaSiteID[bestImp])$Probability)){
        SenSlope(HiCensor=T,x = Imp_med,ValuesToUse = uMeasures[uparam],ValuesToUseforMedian = uMeasures[uparam],doPlot = T,mymain = subTrend$LawaSiteID[bestImp])
      }
    }else{
      MannKendall(x = Imp_med,ValuesToUse = uMeasures[uparam],doPlot=F)
      SenSlope(HiCensor=T,x = Imp_med,ValuesToUse = uMeasures[uparam],ValuesToUseforMedian = uMeasures[uparam],doPlot = T,mymain = subTrend$LawaSiteID[bestImp])
    }
    if(names(dev.cur())=='tiff'){dev.off()}
    rm(theseDeg,theseImp,theseInd)
  rm(Deg_med,Ind_med,Imp_med)
  rm(worstDeg,bestImp,leastKnown)
}

 
# #Grab MCI along with
# if(dim(trendTable10)[1]>1000){
#   wqTrend10=trendTable10
#   load("H:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/2018-09-12/Trend10Year.rData")
#   MCItt10=trendTable10
#   trendTable10=wqTrend10
# }

MCItrend=read.csv(tail(dir(path="h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis",
                            pattern='MacroMCI_Trend',full.names = T,recursive = T,ignore.case = T),1),stringsAsFactors = F)
TrendsForPlotting = rbind(combTrend%>%dplyr::filter(standard%in%c('gold','bronze'))%>%dplyr::select(LawaSiteID,parameter,Region,TrendScore),
                          MCItrend%>%dplyr::select(LawaSiteID,parameter,Region,TrendScore))
rm(MCItrend)

table(TrendsForPlotting$parameter[which(TrendsForPlotting$TrendScore==-99)]) ->naTab

#Drop the -99 NAs
TrendsForPlotting = TrendsForPlotting%>%dplyr::filter(TrendScore!=-99)
#4189 to 3932
#Starts as DRP   NH4   TP    TON   TURB  ECOLI TN    BDISC  MCI
#labelled  DRP   NH4   TP    TON   TURB  ECOLI TN    CLAR   MCI
#Reorder 2 CLAR TURB   TN    TON   NH4   TP    DRP   ECOLI  MCI
TrendsForPlotting$parameter <- factor(TrendsForPlotting$parameter,levels=c("BDISC","TURB","TN","TON","NH4","TP","DRP","ECOLI","MCI"))

#Make the coloured plot
par(mfrow=c(1,1))
colMPs=-0.5+(1:9)*1.2
tb <- plot(factor(TrendsForPlotting$parameter),
           factor(TrendsForPlotting$TrendScore),
           col=c("#dd1111FF","#ee4411FF","#aaaaaaFF","#11cc11FF","#008800FF"), #"#dddddd",
           main="Ten year trends")
tbp <- apply(X = tb,MARGIN = 1,FUN = function(x)x/sum(x))
mbp <- apply(tbp,MARGIN = 2,FUN=cumsum)
mbp <- rbind(rep(0,9),mbp)
mbp = (mbp[-1,]+mbp[-6,])/2
tiff(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/TenYearMonthlyAndQuarterlyTrendsNumbres.tif"),
     width = 8,height=8,units='in',res=300,compression='lzw',type='cairo')
par(mfrow=c(1,1),mar=c(5,10,6,2))
barplot(tbp,main="Ten year trends, monthly and quarterly",las=2,
        col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),yaxt='n',xaxt='n') #"#dddddd",
axis(side = 1,at=colMPs,labels = c('Clarity','Turbidity','TN','TON',expression(NH[4]),'TP',
                                   'DRP',expression(italic(E.~coli)),'MCI'),las=2)
axis(side = 2,at = seq(0,1,le=11),labels = paste0(100*seq(0,1,le=11),'%'),las=2,lty = 1,lwd = 0,lwd.ticks = 0)
axis(side = 2,at = seq(0,1,le=11),labels = NA,las=2,lty = 1,lwd = 0,lwd.ticks = 1,line=-1)
par(xpd=NA)
for(cc in 1:9){
  text(rep(colMPs[cc],6),mbp[,cc],tb[cc,])
  text(colMPs[cc],1.025,sum(tb[cc,]))
}
if(names(dev.cur())=='tiff'){dev.off()}

tiff(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/TenYearMonthlyAndQuarterlyTrendsPercentage.tif"),
     width = 8,height=8,units='in',res=300,compression='lzw',type='cairo')
par(mfrow=c(1,1),mar=c(5,10,6,2))
barplot(tbp,main="Ten year trends, monthly and quarterly",las=2,
        col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),yaxt='n',xaxt='n') #"#dddddd",
axis(side = 1,at=colMPs,labels = c('Clarity','Turbidity','TN','TON',expression(NH[4]),'TP',
                                   'DRP',expression(italic(E.~coli)),'MCI'),las=2)
axis(side = 2,at = seq(0,1,le=11),labels = paste0(100*seq(0,1,le=11),'%'),las=2,lty = 0)
axis(side = 2,at = seq(0,1,le=11),labels = NA,las=2,lty = 1,lwd = 1,lwd.ticks = 1,line=0,tcl=1,tck=0.02)
par(xpd=NA)
for(cc in 1:9){
  text(rep(colMPs[cc],6),mbp[,cc],paste0(round(tb[cc,]/sum(tb[cc,])*100,0),'%'),cex=0.75)
  text(colMPs[cc],1.025,sum(tb[cc,]))
}
if(names(dev.cur())=='tiff'){dev.off()}

# 
# par(mfrow=c(3,1))
# t10 <- plot(factor(trendTable10$parameter),trendTable10$ConfCat,col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),main="10 Year monthly")
# t5 <- plot(factor(trendTable5$parameter),trendTable5$ConfCat,col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),main="5 Year monthly")
# t10q <- plot(factor(trendTableQ10$parameter),trendTableQ10$ConfCat,col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),main="10 Year quarterly")
# write.csv(t5,paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend5Year.csv"))
# write.csv(t10,paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10Year.csv"))
# write.csv(t10q,paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10YearQuarterly.csv"))
# t5p <- apply(X = t5,MARGIN = 1,FUN = function(x)x/sum(x))
# t10p <- apply(X = t10,MARGIN = 1,FUN = function(x)x/sum(x))
# t10pq <- apply(X = t10q,MARGIN = 1,FUN = function(x)x/sum(x))
# 
# m5p <- apply(t5p,MARGIN = 2,FUN=cumsum)
# m5p <- rbind(rep(0,8),m5p)
# m5p = (m5p[-1,]+m5p[-6,])/2
# 
# m10p <- apply(t10p,MARGIN = 2,FUN=cumsum)
# m10p <- rbind(rep(0,8),m10p)
# m10p = (m10p[-1,]+m10p[-6,])/2
# 
# m10pq <- apply(t10pq,MARGIN = 2,FUN=cumsum)
# m10pq <- rbind(rep(0,8),m10pq)
# m10pq = (m10pq[-1,]+m10pq[-6,])/2
# 
# colMPs=-0.5+(1:8)*1.2
# 
# tiff(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/TrendByPeriod.tif"),
#      width = 10,height=15,units='in',res=350,compression='lzw',type='cairo')
# par(mfrow=c(3,1),mar=c(5,10,4,2))
# barplot(t10p,main="10 Year",las=2,
#         col=c("#dd1111FF","#ee7711FF","#bbbbbbFF","#11cc11FF","#008800FF"),yaxt='n')
# axis(side = 2,at = m10p[,1],labels = colnames(t10),las=2,lty = 0)
# for(cc in 1:8){
#   text(rep(colMPs[cc],5),m10p[,cc],paste0(t10[cc,],'\n(',round(t10p[,cc]*100,0),'%)'))
# }
# barplot(t5p,main="5 Year",las=2,
#         col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),yaxt='n')
# axis(side = 2,at = m5p[,1],labels = colnames(t5),las=2,lty = 0)
# for(cc in 1:8){
#   text(rep(colMPs[cc],5),m5p[,cc],paste0(t5[cc,],'\n(',round(t5p[,cc]*100,0),'%)'))
# }
# barplot(t10pq,main="10 Year quarterly",las=2,
#         col=c("#dd1111FF","#ee7711FF","#bbbbbbFF","#11cc11FF","#008800FF"),yaxt='n')
# axis(side = 2,at = m10pq[,1],labels = colnames(t10q),las=2,lty = 0)
# for(cc in 1:8){
#   text(rep(colMPs[cc],5),m10pq[,cc],paste0(t10q[cc,],'\n(',round(t10pq[,cc]*100,0),'%)'))
# }
# if(names(dev.cur())=='tiff'){dev.off()}
# par(mfrow=c(1,1))


