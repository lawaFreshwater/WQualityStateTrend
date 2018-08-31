rm(list=ls())

source("h:/ericg/16666LAWA/LWPTrends/LWPTrends_v1808.R")
load('h:/ericg/16666LAWA/LWPTrends/LWPTrends_SampleData.rdata')

#Load the latest made (might need to check it works nicely across month folders) 
if(!exists('wqdata')){
  acwqdata=tail(dir(path = "H:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",pattern = "AllCouncils.csv",recursive = T,full.names = T),1)
  wqdata=read.csv(acwqdata,stringsAsFactors = F)
  rm(acwqdata)
}

wqdata$myDate <- as.Date(as.character(wqdata$Date),"%d-%b-%Y")
wqdata <- GetMoreDateInfo(wqdata)

wqdata$Season <- wqdata$Month
SeasonString <- sort(unique(wqdata$Season))

wqdata$CenType[wqdata$CenType%in%c("Left","L")]='lt'
wqdata$CenType[wqdata$CenType%in%c("Right","R")]='gt'
wqdata$CenType[!wqdata$CenType%in%c("lt","gt")]='not'

wqdata$NewValues=wqdata$Value



usites=unique(wqdata$LawaSiteID)
uMeasures=unique(wqdata$parameter)
trendTable=structure(list(LawaSiteID=NA,parameter=NA,
  Observations = NA_integer_, KWstat = NA_real_,pvalue = NA_real_,
                          nObs = NA_integer_, S = NA_real_, VarS = NA_real_,D = NA_real_, tau = NA_real_, Z = NA_real_, p = NA_real_, 
                          Median = NA_real_, VarS = NA_real_, AnnualSenSlope = NA_real_, Intercept = NA_real_, Lci = NA_real_, Uci = NA_real_, TrendCategory = NA, 
                          TrendDirection = NA, Probability = NA_real_, Probabilitymax = NA_real_, Probabilitymin = NA_real_, Percent.annual.change = NA_real_), class = "data.frame")
for(usite in seq_along(usites)){
  cat('.')
  for(uparam in seq_along(uMeasures)){
    subDat=wqdata%>%filter(LawaSiteID==usites[usite] & wqdata$parameter==uMeasures[uparam])
    if(dim(subDat)[1]>0){
    suppressWarnings(rm(st,mk,ss,sk,sss))
    SeasonString <- sort(unique(subDat$Season))
    (st <- SeasonalityTest(x = subDat,main=uMeasures[uparam],ValuesToUse = "Value",do.plot =F))
    if(!is.na(st$pvalue)&&st$pvalue<0.05){
      sk <- SeasonalKendall(x = subDat,ValuesToUse = "Value",doPlot = F)
      sss <- SeasonalSenSlope(x = subDat,ValuesToUse = "Value",doPlot = F)
      newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,sk,sss)
    }else{
      mk <- MannKendall(x = subDat,ValuesToUse = "Value",doPlot=F)
      ss <- SenSlope(x = subDat,ValuesToUse = "Value",doPlot = F)
      newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,mk,ss)
    }
    trendTable=rbind(trendTable,newRow)
    rm(newRow)
    }
    }
}


