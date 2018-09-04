rm(list=ls())
library(tidyverse)
source("h:/ericg/16666LAWA/LWPTrends/LWPTrends_v1808.R")

#Load the latest made (might need to check it works nicely across month folders) 
if(!exists('wqdata')){
  acwqdata=tail(dir(path = "H:/ericg/16666LAWA/2018/WaterQuality/1.Imported",pattern = "AllCouncils.csv",recursive = T,full.names = T),1)
  cat(acwqdata)
  wqdata=read.csv(acwqdata,stringsAsFactors = F)
  rm(acwqdata)
}


EndYear <- 2017
startYear5 <- EndYear - 5+1
startYear10 <- EndYear - 10+1



suppressWarnings(rm(upara,ucounc,up,pvals,p1,p5,p75,p95,p999))

wqdata$myDate <- as.Date(as.character(wqdata$Date),"%d-%b-%Y")
wqdata <- GetMoreDateInfo(wqdata)

wqdata$Season <- wqdata$Month
SeasonString <- sort(unique(wqdata$Season))

wqdata$CenType[wqdata$CenType%in%c("Left","L")]='lt'
wqdata$CenType[wqdata$CenType%in%c("Right","R")]='gt'
wqdata$CenType[!wqdata$CenType%in%c("lt","gt")]='not'

wqdata$NewValues=wqdata$Value





#5 year trend ####
wqdatafor5=wqdata%>%filter(Year>startYear5 & parameter!="PH")

usites=unique(wqdatafor5$LawaSiteID)
uMeasures=unique(wqdatafor5$parameter)
trendTable5=structure(list(LawaSiteID=NA,parameter=NA,
  Observations = NA_integer_, KWstat = NA_real_,pvalue = NA_real_,
                          nObs = NA_integer_, S = NA_real_, VarS = NA_real_,D = NA_real_, tau = NA_real_, Z = NA_real_, p = NA_real_, 
                          Median = NA_real_, VarS = NA_real_, AnnualSenSlope = NA_real_, Intercept = NA_real_, Lci = NA_real_, Uci = NA_real_, TrendCategory = NA, 
  TrendDirection = NA, Probability = NA_real_, Probabilitymax = NA_real_, Probabilitymin = NA_real_, Percent.annual.change = NA_real_), class = "data.frame")
usite=1
for(usite in usite:length(usites)){
  cat('.')
  subDat=wqdatafor5%>%filter(LawaSiteID==usites[usite])
  uparam=1
  for(uparam in uparam:length(uMeasures)){
  # if(usite==739 & uparam==1){next}
    subSubDat=subDat%>%filter(subDat$parameter==uMeasures[uparam])
    if(dim(subSubDat)[1]>0){
      suppressWarnings(rm(st,mk,ss,sk,sss))
      SeasonString <- sort(unique(subSubDat$Season))
      (st <- SeasonalityTest(x = subSubDat,main=uMeasures[uparam],ValuesToUse = "Value",do.plot =F))
      if(!is.na(st$pvalue)&&st$pvalue<0.05){
        sk <- SeasonalKendall(x = subSubDat,ValuesToUse = "Value",doPlot = F)
        sss <- SeasonalSenSlope(x = subSubDat,ValuesToUse = "Value",doPlot = F)
        newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,sk,sss)
      }else{
        mk <- MannKendall(x = subSubDat,ValuesToUse = "Value",doPlot=F)
        ss <- SenSlope(x = subSubDat,ValuesToUse = "Value",doPlot = F)
        newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,mk,ss)
      }
      trendTable5=rbind(trendTable5,newRow)
      rm(newRow)
    }
    rm(subSubDat)
  }
  rm(subDat)
}
rm(usites,uMeasures,usite,uparam)
rownames(trendTable5) <- NULL
save(trendTable5,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend5Year.rData"))




#10 year trend ####
wqdatafor10=wqdata%>%filter(Year>startYear10 & parameter!="PH")

usites=unique(wqdatafor10$LawaSiteID)
uMeasures=unique(wqdatafor10$parameter)
trendTable10=structure(list(LawaSiteID=NA,parameter=NA,
                          Observations = NA_integer_, KWstat = NA_real_,pvalue = NA_real_,
                          nObs = NA_integer_, S = NA_real_, VarS = NA_real_,D = NA_real_, tau = NA_real_, Z = NA_real_, p = NA_real_, 
                          Median = NA_real_, VarS = NA_real_, AnnualSenSlope = NA_real_, Intercept = NA_real_, Lci = NA_real_, Uci = NA_real_, TrendCategory = NA, 
                          TrendDirection = NA, Probability = NA_real_, Probabilitymax = NA_real_, Probabilitymin = NA_real_, Percent.annual.change = NA_real_), class = "data.frame")
usite=1
for(usite in usite:length(usites)){
  cat('.')
  subDat=wqdatafor10%>%filter(LawaSiteID==usites[usite])
  uparam=1
  for(uparam in uparam:length(uMeasures)){
    # if(usite==739 & uparam==1){next}
    subSubDat=subDat%>%filter(subDat$parameter==uMeasures[uparam])
    if(dim(subSubDat)[1]>0){
      suppressWarnings(rm(st,mk,ss,sk,sss))
      SeasonString <- sort(unique(subSubDat$Season))
      (st <- SeasonalityTest(x = subSubDat,main=uMeasures[uparam],ValuesToUse = "Value",do.plot =F))
      if(!is.na(st$pvalue)&&st$pvalue<0.05){
        sk <- SeasonalKendall(x = subSubDat,ValuesToUse = "Value",doPlot = F)
        sss <- SeasonalSenSlope(x = subSubDat,ValuesToUse = "Value",doPlot = F)
        newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,sk,sss)
      }else{
        mk <- MannKendall(x = subSubDat,ValuesToUse = "Value",doPlot=F)
        ss <- SenSlope(x = subSubDat,ValuesToUse = "Value",doPlot = F)
        newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,mk,ss)
      }
      trendTable10=rbind(trendTable10,newRow)
      rm(newRow)
    }
    rm(subSubDat)
  }
  rm(subDat)
}
rm(usites,uMeasures,usite,uparam)
rownames(trendTable10) <- NULL
save(trendTable10,file=paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10Year.rData"))

trendTable5$Probability[trendTable5$parameter!="BDISC"]=1-(trendTable5$Probability[trendTable5$parameter!="BDISC"])
trendTable10$Probability[trendTable10$parameter!="BDISC"]=1-(trendTable10$Probability[trendTable10$parameter!="BDISC"])

trendTable5$ConfCat <- cut(trendTable5$Probability, breaks=  c(0, 0.1,0.33,0.66,0.90, 1), labels = c("Very likely improving",
                                                                                                     "Likely improving",
                                                                                                     "Indeterminate",
                                                                                                     "Likely degrading",
                                                                                                     "Very likely degrading"))
trendTable10$ConfCat <- cut(trendTable10$Probability, breaks=  c(0, 0.1,0.33,0.66,0.90, 1), labels = c("Very likely improving",
                                                                                                       "Likely improving",
                                                                                                       "Indeterminate",
                                                                                                       "Likely degrading",
                                                                                                       "Very likely degrading"))

trendTable5$ConfCat=factor(trendTable5$ConfCat,levels=rev(c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading")))
trendTable10$ConfCat=factor(trendTable10$ConfCat,levels=rev(c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading")))



load(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10Year.rData"))
load(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend5Year.rData"))

trendTable5$period=5
trendTable10$period=10

trendEx <- rbind(trendTable5%>%select(LawaSiteID,parameter,ConfCat,period),
                 trendTable10%>%select(LawaSiteID,parameter,ConfCat,period))

trendEx$Altitude = siteTable$SWQAltitude[match(trendEx$LawaSiteID,siteTable$LawaSiteID)]
trendEx$Landuse = siteTable$SWQLanduse[match(trendEx$LawaSiteID,siteTable$LawaSiteID)]
trendEx$Region = siteTable$Region[match(trendEx$LawaSiteID,siteTable$LawaSiteID)]
trendEx$Frequency = siteTable$SWQFrequencyAll[match(trendEx$LawaSiteID,siteTable$LawaSiteID)]

trendEx$TrendScore=as.numeric(trendEx$ConfCat)-3

trendEx <- trendEx%>%select(LawaSiteID,parameter,Altitude,Landuse,TrendScore,Frequency,Region,period)
write.csv(trendEx,paste0("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/RiverWQ_Trend_ForITE",
                         format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),row.names = F)
plott=F
for(uparam in seq_along(uMeasures)){
  subTrend=trendTable10[which(trendTable10$parameter==uMeasures[uparam]),]
  which.max(subTrend$Probability)->worstDeg 
  which.min(subTrend$Probability)->bestImp
  cat(subTrend$Probability[worstDeg],'\t')
  cat(subTrend$Probability[bestImp],'\n')
  if(plott){
  tiff(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/BestWorst",uMeasures[uparam],".tif"),
       width = 8,height=8,units='in',res=300,compression='lzw',type='cairo')
  
    par(mfrow=c(2,1),mar=c(2,4,1,2))
  theseDeg <- which(wqdata$LawaSiteID==subTrend$LawaSiteID[worstDeg] &
                      wqdata$parameter==uMeasures[uparam] & dmy(wqdata$Date)>dmy("1-1-2008"))
  theseImp <- which(wqdata$LawaSiteID==subTrend$LawaSiteID[bestImp] &
                      wqdata$parameter==uMeasures[uparam] & dmy(wqdata$Date)>dmy("1-1-2008"))

  st <- SeasonalityTest(x = wqdata[theseDeg,],main=uMeasures[uparam],ValuesToUse = "Value",do.plot =F)
  if(!is.na(st$pvalue)&&st$pvalue<0.05){
    SeasonalKendall(x = wqdata[theseDeg,],ValuesToUse = "Value",doPlot = F)
    SeasonalSenSlope(x = wqdata[theseDeg,],ValuesToUse = "Value",doPlot = T)
  }else{
    MannKendall(x = wqdata[theseDeg,],ValuesToUse = "Value",doPlot=F)
    SenSlope(x = wqdata[theseDeg,],ValuesToUse = "Value",doPlot = T)
  }

  st <- SeasonalityTest(x = wqdata[theseImp,],main=uMeasures[uparam],ValuesToUse = "Value",do.plot =F)
  if(!is.na(st$pvalue)&&st$pvalue<0.05){
    SeasonalKendall(x = wqdata[theseImp,],ValuesToUse = "Value",doPlot = F)
    if(is.na(SeasonalSenSlope(x = wqdata[theseImp,],ValuesToUse = "Value",doPlot = T)$Probability)){
      SenSlope(x = wqdata[theseImp,],ValuesToUse = "Value",doPlot = T)
    }
  }else{
    MannKendall(x = wqdata[theseImp,],ValuesToUse = "Value",doPlot=F)
    SenSlope(x = wqdata[theseImp,],ValuesToUse = "Value",doPlot = T)
  }
  if(names(dev.cur())=='tiff'){dev.off()}
  }
}

t5 <- plot(factor(trendTable5$parameter),trendTable5$ConfCat,col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),main="5 Year")
t10 <- plot(factor(trendTable10$parameter),trendTable10$ConfCat,col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),main="10 Year")

t5p <- apply(X = t5,MARGIN = 1,FUN = function(x)x/sum(x))
t10p <- apply(X = t10,MARGIN = 1,FUN = function(x)x/sum(x))

m5p <- apply(t5p,MARGIN = 2,FUN=cumsum)
m5p <- rbind(rep(0,8),m5p)
m5p = (m5p[-1,]+m5p[-6,])/2

m10p <- apply(t10p,MARGIN = 2,FUN=cumsum)
m10p <- rbind(rep(0,8),m10p)
m10p = (m10p[-1,]+m10p[-6,])/2

colMPs=-0.5+(1:8)*1.2

par(mfrow=c(2,1),mar=c(5,10,4,2))
barplot(t5p,main="5 Year",las=2,
        col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),yaxt='n')
axis(side = 2,at = m5p[,1],labels = colnames(t5),las=2,lty = 0)
for(cc in 1:8){
  text(rep(colMPs[cc],5),m5p[,cc],t5[cc,])
  }

tiff(paste0("h:/ericg/16666LAWA/2018/WaterQuality/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10Year.tif"),
     width = 8,height=8,units='in',res=300,compression='lzw',type='cairo')
par(mar=c(5,10,4,2))
barplot(t10p,main="10 Year",las=2,
        col=c("#dd1111FF","#ee7711FF","#bbbbbbFF","#11cc11FF","#008800FF"),yaxt='n')
axis(side = 2,at = m10p[,1],labels = colnames(t10),las=2,lty = 0)
for(cc in 1:8){
  text(rep(colMPs[cc],5),m10p[,cc],paste0(t10[cc,],'\n(',round(t10p[,cc]*100,0),'%)'))
}
if(names(dev.cur())=='tiff'){dev.off()}






 