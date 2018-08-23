#===================================================================================================
#  LAWA NATIONAL OBJECTIVES FRAMEWORK
#  Horizons Regional Council
#
#  4 September 2016
#
#  Creator: Kelvin Phan  2014
#
#  Updated by: Maree Patterson 2016
#              Sean Hodges
#
#  Horizons Regional Council
#===================================================================================================


# Clearing workspace
rm(list = ls())

ANALYSIS<-"NOF"
# Set working directory
od <- getwd()
wd <- "h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state"
setwd(wd)

source("scripts/WQualityStateTrend/NOF_Functions.R")
NOF_PERIOD <- 5 # years

## Load NOF Bands
#NOF         <- read.csv("NOFSummary.csv", header = TRUE, stringsAsFactors=FALSE)
NOF        <- read.csv("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/NOFSummary2.csv", header = TRUE, stringsAsFactors=FALSE)

#Band,Median Nitrate,95th Percentile Nitrate,Median Ammoniacal N,Max Ammoniacal N,Chl a max,E. coli,Ecoli95
#A,<1.0,<1.5,<0.03,<0.05,<50,<260,540
#B,2.4,3.5,0.24,0.4,120,540,1000
#C,6.9,9.8,1.3,2.2,200,1000,1200
#D,>6.9,>9.8,>1.30,>2.2,>200,>1000,>1200


# This is for setting the years you would like to undertake the assessment on for the National objectives Framework.
# Add more years using comma seperation as needed 
yr  <-c("2012", "2013", "2014","2015","2016","2017","Overall")
reps  <-length(yr)


#===================================================================================================
## Adjust the NOF Summary by dealing with "<" or ">"
grt <- ">" # give the greater symbol a name
lst <- "<" # give the lester symbol a name

for (c in 2:ncol(NOF))
{
  for (r in 1:nrow(NOF))
  {
    if (grepl(grt,NOF[r,c]) == TRUE){     
       NOF[r,c] = gsub(grt,"0",NOF[r,c])
    }
    else if (grepl(lst,NOF[r,c]) == TRUE){
      NOF[r,c] = gsub(lst,"0",NOF[r,c])
    }
  }
  NOF[,c] <- as.numeric(NOF[,c])
}

#===================================================================================================
## Load LAWA Data
#Reference Dates
EndYear <- 2017
StartYear <- EndYear - NOF_PERIOD + 1


# loads lawadata dataframe
load(file=paste("h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawadata",StartYear,"-",EndYear,".RData",sep=""))

# Subset to just have NH4
# Date column in lawadata already POSIXct data type
sub_swq <- lawadata%>%select(c("LawaSiteID","SiteName","Date","parameter","Value"))%>%filter(parameter%in%c("NH4","TON","ECOLI","PH"))
# dfswq <- lawadata[lawadata$parameter=="NH4" | lawadata$parameter=="TON" | lawadata$parameter=="ECOLI" | lawadata$parameter=="PH",]

#+++++++++++++++++++++++++++++ Dealing with data in dfswq table++++++++++++++++++++++++++++++++++++
# sub_swq <- dfswq[,c(5,1,3,2,4)]
# names(sub_swq) <- c("LAWAID","Name","Date","Parameter","Value")


#remove all the NA column
#sub_swq <- sub_swq[complete.cases(sub_swq$), ]
Name_swq<- unique(sub_swq$SiteName)

#+++++++++++++++++++++++++++++ Ammonia adjustment for pH++++++++++++++++++++++++++++++++++++
csv="scripts/WQualityStateTrend/NOFAmmoniaAdjustment.csv"
sub_swq<-rbind(sub_swq,NH4adj(sub_swq,c("NH4","PH"),csv))

# NH4adj<-function(sub_swq,vars=c("NH4","PH"),csv){
#   A<-read.csv(csv,stringsAsFactors = FALSE)
#   A <- rbind(A,c(NA,1))
#   
#   D <- sub_swq[sub_swq$parameter%in%vars,] 
#   D <- D%>%spread(parameter,Value)          
#   
#   D$Ratio=approx(x = A$PH,y = A$Ratio,xout = D$PH)$y
#   D$NH4adj<-D$NH4/D$Ratio
#   
#   D <- D%>%select("LawaSiteID","SiteName","Date","NH4adj")
#   Dg <- D%>%gather(key="parameter",value="Value",4)%>%drop_na()
#   return(Dg)
# }
names(sub_swq)[names(sub_swq)=='parameter'] <- "Parameter"
sub_swq$Date=strptime(sub_swq$Date,"%d-%b-%Y")

if(exists("NOFSummaryTable")) { rm("NOFSummaryTable") }
i=1
for(i in i:length(Name_swq)){
    # create table of compliance with proposed National Objectives Frameqork
    Com_NOF <- data.frame (Year = yr,
                           Median_Nitrate        = as.numeric(rep(NA,reps)),
                           M_Nitrate_Band        = factor(rep(NA,reps),levels = c("A","B","C","D")),
                           Per_Nitrate           = as.numeric(rep(NA,reps)),
                           Per_Nitrate_Band      = factor(rep(NA,reps),levels = c("A","B","C","D")),
                           Nitrate_Toxicity_Band = factor(rep(NA,reps),levels = c("A","B","C","D")),
                           Median_Ammoniacal     = as.numeric(rep(NA,reps)),
                           M_Ammoniacal_Band     = factor(rep(NA,reps),levels = c("A","B","C","D")),
                           Max_Ammoniacal        = as.numeric(rep(NA,reps)),
                           Max_Ammoniacal_Band   = factor(rep(NA,reps),levels = c("A","B","C","D")),
                           Ammonia_Toxicity_Band = factor(rep(NA,reps),levels = c("A","B","C","D")),
                           Chl_a_max             = as.numeric(rep(NA,reps)),
                           Chl_a_band            = factor(rep(NA,reps),levels = c("A","B","C","D")),
                           E_coli                = as.numeric(rep(NA,reps)),
                           E_coli_band           = factor(rep(NA,reps),levels = c("A","B","C","D")),
                           E_coli95              = as.numeric(rep(NA,reps)),
                           E_coli95_band         = factor(rep(NA,reps),levels = c("A","B","C","D")),
                           E_coliRecHealth540    = as.numeric(rep(NA,reps)),
                           E_coliRecHealth540band= factor(rep(NA,reps),levels = c("A","B","C","D","E")),
                           E_coliRecHealth260    = as.numeric(rep(NA,reps)),
                           E_coliRecHealth260band= factor(rep(NA,reps),levels = c("A","B","C","D","E")),
                           stringsAsFactors = FALSE)
    
      
    ###################### Nitrate Toxicity  ########################################
    #--------Median Nitrate--------------------------------------
    Value <- tapply(sub_swq$Value[sub_swq$Parameter=="TON" & sub_swq$SiteName==Name_swq[i]], 
                    format(sub_swq$Date[sub_swq$Parameter=="TON" & sub_swq$SiteName==Name_swq[i]], '%Y'), 
                    na.rm=TRUE, quantile,prob=c(0.5),type=5)
    
    if(length(Value)!=0){
      #adding values into Com_NOF table
      Com_NOF$Median_Nitrate <- NOF_AddValue(Value, Com_NOF$Median_Nitrate, Com_NOF$Year)
      
      #calculate the overall median
    #  Com_NOF$Median_Nitrate[nrow(Com_NOF)] <- median(Com_NOF$Median_Nitrate[1:(nrow(Com_NOF)-1)], na.rm = TRUE)
      #removed following condition
      # & sum(complete.cases((Com_NOF$Median_Nitrate[1:(nrow(Com_NOF)-1)])))==5)
      
      if(length(sub_swq$Value[sub_swq$Parameter=="TON"& sub_swq$SiteName==Name_swq[i]])>=30){
      Com_NOF$Median_Nitrate[nrow(Com_NOF)] <- quantile(sub_swq$Value[sub_swq$Parameter=="TON"& sub_swq$SiteName==Name_swq[i]],prob=c(0.5),type=5)
      }
      
      #find the band which each value belong to
      Com_NOF$M_Nitrate_Band <- NOF_FindBand(Com_NOF$Median_Nitrate, NOF$Median.Nitrate, Com_NOF$M_Nitrate_Band)
      
      #   #make a graph  
      #   jpeg('Median Nitrate.jpg')  
      #   pp <- ggplot (Com_NOF, aes(x = Year[1:(nrow(Com_NOF))], y = Median_Nitrate[(nrow(Com_NOF))]), size= 5) +    
      #     geom_rect(xmin=0,xmax=20,ymin=0, ymax=NOF$Median.Nitrate[1], aes(fill = "A"), alpha = 0.3) +
      #     geom_rect(xmin=0,xmax=20,ymin=NOF$Median.Nitrate[1], ymax=NOF$Median.Nitrate[2], aes(fill = "B"), alpha = 0.3) +
      #     geom_rect(xmin=0,xmax=20,ymin=NOF$Median.Nitrate[2], ymax=NOF$Median.Nitrate[3], aes(fill = "C"), alpha = 0.3) +
      #     geom_rect(xmin=0,xmax=20,ymin=NOF$Median.Nitrate[3], ymax=2000, aes(fill = "D"), alpha = 0.3) +
      #     scale_fill_manual("Band :", breaks = c("A", "B", "C", "D"),values = c("green", "yellow", "orange", "red")) +
      #     geom_point() +
      #     theme_bw() +    
      #     labs(x = "Year", y = "Median Nitrate", face="bold") +    
      #     scale_y_continuous(expand = c(0,0)) + expand_limits(y = NOF$Median.Nitrate[3] + 2)  
      #   print(pp); 
      #   dev.off()
      
      #-------95th percentage Nitrate--------------------------------------
      Value <- tapply(sub_swq$Value[sub_swq$Parameter=="TON" & sub_swq$SiteName==Name_swq[i]], 
                      format(sub_swq$Date[sub_swq$Parameter=="TON" & sub_swq$SiteName==Name_swq[i]], '%Y'), 
                      na.rm=TRUE, quantile, probs = 0.95, type =5)
      
      #adding values into Com_NOF table
      Com_NOF$Per_Nitrate <- NOF_AddValue(Value, Com_NOF$Per_Nitrate, Com_NOF$Year)
      
      #calculate the overall 95th percentage
      #Com_NOF$Per_Nitrate[nrow(Com_NOF)] <- quantile(Com_NOF$Per_Nitrate[1:(nrow(Com_NOF)-1)], na.rm=TRUE, probs = 0.95, type = 5)
      if(length(sub_swq$Value[sub_swq$Parameter=="TON"& sub_swq$SiteName==Name_swq[i]])>=30){
        Com_NOF$Per_Nitrate[nrow(Com_NOF)] <- quantile(sub_swq$Value[sub_swq$Parameter=="TON"& sub_swq$SiteName==Name_swq[i]],prob=c(0.95),type=5)
      }
      
      #find the band which each value belong to
      Com_NOF$Per_Nitrate_Band <- NOF_FindBand(Com_NOF$Per_Nitrate, NOF$X95th.Percentile.Nitrate, Com_NOF$Per_Nitrate_Band)
      
      #make a graph
      #   jpeg('95th percentage Nitrate.jpg')  
      #   pp <- ggplot (Com_NOF, aes(x = Year[1:(nrow(Com_NOF))], y = Per_Nitrate[1:(nrow(Com_NOF))]), size= 5) +    
      #     geom_rect(xmin=0,xmax=20,ymin=0, ymax=NOF$X95th.Percentile.Nitrate[1], aes(fill = "A"), alpha = 0.3) +
      #     geom_rect(xmin=0,xmax=20,ymin=NOF$X95th.Percentile.Nitrate[1], ymax=NOF$X95th.Percentile.Nitrate[2], aes(fill = "B"), alpha = 0.3) +
      #     geom_rect(xmin=0,xmax=20,ymin=NOF$X95th.Percentile.Nitrate[2], ymax=NOF$X95th.Percentile.Nitrate[3], aes(fill = "C"), alpha = 0.3) +
      #     geom_rect(xmin=0,xmax=20,ymin=NOF$X95th.Percentile.Nitrate[3], ymax=2000, aes(fill = "D"), alpha = 0.3) +
      #     scale_fill_manual("Band :", breaks = c("A", "B", "C", "D"),values = c("green", "yellow", "orange", "red")) +
      #     geom_point() +
      #     theme_bw() +    
      #     labs(x = "Year", y = "95th percentage Nitrate", face="bold") +    
      #     scale_y_continuous(expand = c(0,0)) + expand_limits(y = NOF$X95th.Percentile.Nitrate[3] + 2)  
      #   print(pp); 
      #   dev.off()
      
      #---------------------Finding the band for Nitrate Toxicity --------------------------
      
      NT <- subset(Com_NOF,select = c(Year, M_Nitrate_Band, Per_Nitrate_Band))
      NT["result"] <- as.numeric()
      
      #Changing the band to number
      NT$M_Nitrate_Band <- NOF_BtoN(NT$M_Nitrate_Band)
      NT$Per_Nitrate_Band <- NOF_BtoN(NT$Per_Nitrate_Band)
      
      # comparing the value to find the Toxicity value 
      NT$result <- NOF_Compare(NT$M_Nitrate_Band, NT$Per_Nitrate_Band, NT$result)
      
      # Adding the result to the summary table 
      Com_NOF$Nitrate_Toxicity_Band <- NOF_NtoB(NT$result,Com_NOF$Nitrate_Toxicity_Band)
    }
    
    ###################### Ammonia Toxicity  ############################
    #--------Median Ammoniacal Nitrogen--------------------------------------
    Value <- tapply(sub_swq$Value[sub_swq$Parameter=="NH4adj" & sub_swq$SiteName==Name_swq[i]], 
                    format(sub_swq$Date[sub_swq$Parameter=="NH4adj" & sub_swq$SiteName==Name_swq[i]], '%Y'), 
                    na.rm=TRUE, quantile,prob=c(0.5),type=5)
    
    if(length(Value)!=0){
      #adding values into Com_NOF table
      Com_NOF$Median_Ammoniacal <- NOF_AddValue(Value, Com_NOF$Median_Ammoniacal, Com_NOF$Year)
      
      #calculate the overall median
     # Com_NOF$Median_Ammoniacal[nrow(Com_NOF)] <- median(Com_NOF$Median_Ammoniacal[1:(nrow(Com_NOF)-1)], na.rm=TRUE)
      if(length(sub_swq$Value[sub_swq$Parameter=="NH4adj"& sub_swq$SiteName==Name_swq[i]])>=30){
        Com_NOF$Median_Ammoniacal[nrow(Com_NOF)] <- quantile(sub_swq$Value[sub_swq$Parameter=="NH4adj"& sub_swq$SiteName==Name_swq[i]],prob=c(0.5),type=5)
      }
      
      #find the band which each value belong to
      Com_NOF$M_Ammoniacal_Band <- NOF_FindBand(Com_NOF$Median_Ammoniacal, NOF$Median.Ammoniacal.N, Com_NOF$M_Ammoniacal_Band)
      
      #make a graph
      # jpeg('Median Ammoniacal Nitrogen.jpg')  
      # pp <- ggplot (Com_NOF, aes(x = Year[1:(nrow(Com_NOF))], y = Median_Ammoniacal[1:(nrow(Com_NOF))]), size= 5) +    
      #   geom_rect(xmin=0,xmax=20,ymin=0, ymax=NOF$Median.Ammoniacal.N[1], aes(fill = "A"), alpha = 0.3) +
      #   geom_rect(xmin=0,xmax=20,ymin=NOF$Median.Ammoniacal.N[1], ymax=NOF$Median.Ammoniacal.N[2], aes(fill = "B"), alpha = 0.3) +
      #   geom_rect(xmin=0,xmax=20,ymin=NOF$Median.Ammoniacal.N[2], ymax=NOF$Median.Ammoniacal.N[3], aes(fill = "C"), alpha = 0.3) +
      #   geom_rect(xmin=0,xmax=20,ymin=NOF$Median.Ammoniacal.N[3], ymax=2000, aes(fill = "D"), alpha = 0.3) +
      #   scale_fill_manual("Band :", breaks = c("A", "B", "C", "D"),values = c("green", "yellow", "orange", "red")) +
      #   geom_point() +
      #   theme_bw() +    
      #   labs(x = "Year", y = "Median Ammoniacal ", face="bold") +    
      #   scale_y_continuous(expand = c(0,0)) + expand_limits(y = NOF$Median.Ammoniacal.N[3] + 0.5)  
      # print(pp); 
      # dev.off()
      
      #-------95th percentage Ammoniacal Nitrogen--------------------------------------
      Value <- tapply(sub_swq$Value[sub_swq$Parameter=="NH4adj" & sub_swq$SiteName==Name_swq[i]], 
                      format(sub_swq$Date[sub_swq$Parameter=="NH4adj" & sub_swq$SiteName==Name_swq[i]], '%Y'), 
                      na.rm=TRUE, max)
      
      #adding values into Com_NOF table
      Com_NOF$Max_Ammoniacal <- NOF_AddValue(Value, Com_NOF$Max_Ammoniacal, Com_NOF$Year)
      
      #calculate the overall median
     # Com_NOF$Max_Ammoniacal[nrow(Com_NOF)] <- quantile(Com_NOF$Max_Ammoniacal[1:(nrow(Com_NOF)-1)], na.rm=TRUE, probs = 0.95)
      if(length(sub_swq$Value[sub_swq$Parameter=="NH4adj"& sub_swq$SiteName==Name_swq[i]])>=30){
        Com_NOF$Max_Ammoniacal[nrow(Com_NOF)] <- quantile(sub_swq$Value[sub_swq$Parameter=="NH4adj"& sub_swq$SiteName==Name_swq[i]],prob=c(0.95),type=5)
      }
      
      
      #find the band which each value belong to
      Com_NOF$Max_Ammoniacal_Band <- NOF_FindBand(Com_NOF$Max_Ammoniacal, NOF$Max.Ammoniacal.N, Com_NOF$Max_Ammoniacal_Band)   
      
      #make a graph
      #  jpeg('95th percentage Ammoniacal Nitrogen.jpg')  
      # pp <- ggplot (Com_NOF, aes(x = Year[1:(nrow(Com_NOF))], y = Max_Ammoniacal[1:(nrow(Com_NOF))]), size= 5) +    
      #   geom_rect(xmin=0,xmax=20,ymin=0, ymax=NOF$Max.Ammoniacal.N[1], aes(fill = "A"), alpha = 0.3) +
      #   geom_rect(xmin=0,xmax=20,ymin=NOF$Max.Ammoniacal.N[1], ymax=NOF$Max.Ammoniacal.N[2], aes(fill = "B"), alpha = 0.3) +
      #   geom_rect(xmin=0,xmax=20,ymin=NOF$Max.Ammoniacal.N[2], ymax=NOF$Max.Ammoniacal.N[3], aes(fill = "C"), alpha = 0.3) +
      #   geom_rect(xmin=0,xmax=20,ymin=NOF$Max.Ammoniacal.N[3], ymax=2000, aes(fill = "D"), alpha = 0.3) +
      #   scale_fill_manual("Band :", breaks = c("A", "B", "C", "D"),values = c("green", "yellow", "orange", "red")) +
      #   geom_point() +
      #   theme_bw() +    
      #   labs(x = "Year", y = "95th percentage Ammoniacal", face="bold") +    
      #   scale_y_continuous(expand = c(0,0)) + expand_limits(y = NOF$Max.Ammoniacal.N[3] + 0.5)  
      # print(pp); 
      #  dev.off()
      
      #------------------Finding the band for Ammonia Toxicity-------------------------------
      
      NT <- subset(Com_NOF,select = c(Year, M_Ammoniacal_Band, Max_Ammoniacal_Band))
      NT["result"] <- as.numeric()
      
      #Changing the band to number
      NT$M_Ammoniacal_Band <- NOF_BtoN(NT$M_Ammoniacal_Band)
      NT$Max_Ammoniacal_Band <- NOF_BtoN(NT$Max_Ammoniacal_Band)
      
      # comparing the value to find the Toxicity value 
      NT$result <- NOF_Compare(NT$M_Ammoniacal_Band, NT$Max_Ammoniacal_Band, NT$result)
      
      # Adding the result to the summary table 
      Com_NOF$Ammonia_Toxicity_Band <- NOF_NtoB(NT$result,Com_NOF$Ammonia_Toxicity_Band)
    }  
    ######################  E.Coli #########################################
    rawEcoli=data.frame(year=format(sub_swq$Date[sub_swq$Parameter=="ECOLI" & sub_swq$SiteName==Name_swq[i]],'%Y'),
                        value=sub_swq$Value[sub_swq$Parameter=="ECOLI" & sub_swq$SiteName==Name_swq[i]])
    rawEcoli=rawEcoli[!is.na(rawEcoli$value),]
    
    if(dim(rawEcoli)[1]>60){ #data requirement for band determination, footnote 1, table  
      for(yy in 1:length(Com_NOF$Year)){
        ecv=rawEcoli$value[which(rawEcoli$year==Com_NOF$Year[yy])]
        Com_NOF$E_coliRecHealth540[yy]=sum(ecv>540)/length(ecv)*100
        Com_NOF$E_coliRecHealth260[yy]=sum(ecv>260)/length(ecv)*100
        Com_NOF$E_coliRecHealth540band[yy]=cut(x = Com_NOF$E_coliRecHealth540[yy],
                                               breaks = c(0,NOF$EcoliRec540),labels=NOF$Band,right=FALSE)
        Com_NOF$E_coliRecHealth260band[yy]=cut(Com_NOF$E_coliRecHealth260[yy],
                                               breaks = c(0,NOF$EcoliRec260),labels=NOF$Band,right=FALSE)
      }
      Com_NOF$E_coliRecHealth540band[is.na(Com_NOF$E_coliRecHealth540band)&!is.na(Com_NOF$E_coliRecHealth540)]="E"
      Com_NOF$E_coliRecHealth260band[is.na(Com_NOF$E_coliRecHealth260band)&!is.na(Com_NOF$E_coliRecHealth260)]="E"
      #Catch for this band which isn't defined via NOF file
      stopifnot(Com_NOF$E_coliRecHealth540[Com_NOF$E_coliRecHealth540band=="E"&!is.na(Com_NOF$E_coliRecHealth260)]>=30)
      #Catch for this band which isn't defined via NOF file
      stopifnot(Com_NOF$E_coliRecHealth260[Com_NOF$E_coliRecHealth260band=="E"&!is.na(Com_NOF$E_coliRecHealth260)]>=50)
    }else{
      Com_NOF$E_coliRecHealth540=NA
      Com_NOF$E_coliRecHealth260=NA
      Com_NOF$E_coliRecHealth540band=NA
      Com_NOF$E_coliRecHealth260band=NA
    }
    
    #E coli median ####
    Value <- tapply(sub_swq$Value[sub_swq$Parameter=="ECOLI" & sub_swq$SiteName==Name_swq[i]], 
                    format(sub_swq$Date[sub_swq$Parameter=="ECOLI" & sub_swq$SiteName==Name_swq[i]], '%Y'), 
                    na.rm=TRUE, quantile,prob=c(0.5),type=5)
    
    if(length(Value)!=0){
      #adding values into Com_NOF table
      Com_NOF$E_coli <- NOF_AddValue(Value, Com_NOF$E_coli, Com_NOF$Year)
      
      #calculate the overall median
      #Com_NOF$E_coli[nrow(Com_NOF)] <- median(Com_NOF$E_coli[1:(nrow(Com_NOF)-1)], na.rm=TRUE)
      if(length(sub_swq$Value[sub_swq$Parameter=="ECOLI"& sub_swq$SiteName==Name_swq[i]])>=30){
        Com_NOF$E_coli[nrow(Com_NOF)] <- quantile(sub_swq$Value[sub_swq$Parameter=="ECOLI"& sub_swq$SiteName==Name_swq[i]],prob=c(0.5),type=5,na.rm=T)
      }
      
      
      #find the band which each value belong to
      Com_NOF$E_coli_band <- NOF_FindBand(Com_NOF$E_coli, NOF$E..coli, Com_NOF$E_coli_band)
      
      #make a graph
      #jpeg('E Coli.jpg')  
      # pp <- ggplot (Com_NOF, aes(x = Year[1:(nrow(Com_NOF))], y = E_coli[1:(nrow(Com_NOF))]), size= 5) +    
      #   geom_rect(xmin=0,xmax=20,ymin=0, ymax=NOF$E..coli[1], aes(fill = "A"), alpha = 0.3) +
      #   geom_rect(xmin=0,xmax=20,ymin=NOF$E..coli[1], ymax=NOF$E..coli[2], aes(fill = "B"), alpha = 0.3) +
      #   geom_rect(xmin=0,xmax=20,ymin=NOF$E..coli[2], ymax=NOF$E..coli[3], aes(fill = "C"), alpha = 0.3) +
      #   geom_rect(xmin=0,xmax=20,ymin=NOF$E..coli[3], ymax=2000, aes(fill = "D"), alpha = 0.3) +
      #   scale_fill_manual("Band :", breaks = c("A", "B", "C", "D"),values = c("green", "yellow", "orange", "red")) +
      #   geom_point() +
      #   theme_bw() +    
      #   labs(x = "Year", y = "E Coli", face="bold") +    
      #   scale_y_continuous(expand = c(0,0)) + expand_limits(y = NOF$E..coli[3] + 200)  
      # print(pp); 
      #dev.off()
    }
    
    #-------95th percentile Ecoli-----------------------------------------------------
    Value <- tapply(sub_swq$Value[sub_swq$Parameter=="ECOLI" & sub_swq$SiteName==Name_swq[i]], 
                    format(sub_swq$Date[sub_swq$Parameter=="ECOLI" & sub_swq$SiteName==Name_swq[i]], '%Y'), 
                    na.rm=TRUE, quantile,prob=c(0.95),type=5)
    
    if(length(Value)!=0){
      #adding values into Com_NOF table
      Com_NOF$E_coli95 <- NOF_AddValue(Value, Com_NOF$E_coli95, Com_NOF$Year)
      
      #calculate the overall 95 percentile
      if(length(sub_swq$Value[sub_swq$Parameter=="ECOLI"& sub_swq$SiteName==Name_swq[i]])>=30){
        Com_NOF$E_coli95[nrow(Com_NOF)] <- quantile(sub_swq$Value[sub_swq$Parameter=="ECOLI"& sub_swq$SiteName==Name_swq[i]],prob=c(0.95),type=5,na.rm=T)
      }
      
      #find the band which each value belong to
      Com_NOF$E_coli95_band <- NOF_FindBand(Com_NOF$E_coli95, NOF$Ecoli95, Com_NOF$E_coli95_band)  
    }  
    #---------------------------------------------------------------------------------
      
    Com_NOF$SiteName <- Name_swq[i]
    if(!exists("NOFSummaryTable")){
      NOFSummaryTable <- Com_NOF
    } else {
      NOFSummaryTable <- rbind.data.frame(NOFSummaryTable,Com_NOF,stringsAsFactors = FALSE)
    }
    
}


#############################Save the output table ############################
write.csv(NOFSummaryTable, file = "h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOF Summary Table.csv")
load(file="h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawa_sitetable.RData")
NOFSummaryTable <- merge(NOFSummaryTable, catSiteTable, by.x="SiteName",by.y="CouncilSiteID",all.x=TRUE) # Using LAWAIDs to join tables
NOFSummaryTableSubset <- NOFSummaryTable[NOFSummaryTable$Year=="Overall",]
NOFSummaryTableSubset <- NOFSummaryTableSubset%>%select("SiteName","SiteID","LawaSiteID",
                                                        Year:E_coliRecHealth260band,
                                                        "SWQAltitude","SWQLanduse","Agency")
# NOFSummaryTableSubset <- NOFSummaryTableSubset[,c(20,1:6,8:11,15:18)]  # LAWASiteID,SiteName:Per_Nitrate_Band,Median_Ammon:Max_Ammon_Band,Ecoli's...
#NOFSummaryTableSubset <- NOFSummaryTableSubset[,c(1:2,3:6,8:11,15:16)]

write.csv(NOFSummaryTableSubset, file = "h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOF Summary TableSubset.csv")

# Reshape Output
require(reshape2)
NOFSummaryTableLong <- melt(data=NOFSummaryTableSubset,id.vars=c("SiteName","Year"))

#drop LawaSiteID cases
NOFSummaryTableLong <- NOFSummaryTableLong[NOFSummaryTableLong$variable!="LawaSiteID",]

# Add LAWA IDs to sites
# load(file="h:/ericg/16666LAWA/2018/WaterQuality/ROutput/lawa_sitetable.RData")

NOFSummaryTableLong <- merge(NOFSummaryTableLong, catSiteTable, by.x="SiteName",by.y="CouncilSiteID",all.x=TRUE) # Using LAWAIDs to join tables
NOFSummaryTableLong <- select(NOFSummaryTableLong,"LawaSiteID", "variable", "SiteName", "Year", "value")
# NOFSummaryTableLong <- NOFSummaryTableLong[,c(6,3,1:2,4)]  # LawaSiteId, variable, SiteName, Year, value

write.csv(NOFSummaryTableLong, file = "h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOF Summary Table Long.csv")

NOFSummaryTableLongSubset <- NOFSummaryTableLong[NOFSummaryTableLong$Year=="Overall",]
NOFSummaryTableLongSubset <- NOFSummaryTableLong[!is.na(NOFSummaryTableLong$LawaSiteID),]
#NOFSummaryTableLongSubset <- NOFSummaryTableLongSubset[grepl(pattern = "[ABCD]",NOFSummaryTableLongSubset$value),]

write.csv(NOFSummaryTableLongSubset, file = "h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOF_STATE_2018.csv")

#read data
#NOFSummaryTableLongSubset <- read.csv(file = "//file/herman/R/OA/08/02/207/WaterQuality/ROutput/NOF_STATE_2015.csv", stringsAsFactors=FALSE)

nofRound <- NOFSummaryTableLongSubset
vars<-as.character(unique(NOFSummaryTableLongSubset$variable))
vars <- vars[order(vars)]
#vars
# [1] "Agency"                 "Ammonia_Toxicity_Band"  "Chl_a_band"             "Chl_a_max"              "E_coli"                
# [6] "E_coli95"               "E_coli95_band"          "E_coliRecHealth260"     "E_coliRecHealth260band" "E_coliRecHealth540"    
# [11] "E_coliRecHealth540band" "E_coli_band"            "M_Ammoniacal_Band"      "M_Nitrate_Band"         "Max_Ammoniacal"        
# [16] "Max_Ammoniacal_Band"    "Median_Ammoniacal"      "Median_Nitrate"         "Nitrate_Toxicity_Band"  "Per_Nitrate"           
# [21] "Per_Nitrate_Band"       "SWQAltitude"            "SWQLanduse"             "SiteID"          

nofRound$variable <- as.character(nofRound$variable)
# Decimal places for variables
#dp <- c(4,0,NA,NA,NA,4,NA,4,NA,4)
dp <- c(0,NA,0,NA,NA,NA,4,NA,4,4,4,NA)
dp <- rep(NA,length(vars))
dp[vars%in%c("E_coli", "E_coli95", "E_coliRecHealth260", "E_coliRecHealth540")] <- 0
dp[vars%in%c("Max_Ammoniacal", "Median_Ammoniacal", "Median_Nitrate", "Per_Nitrate")] <- 4

#p <- c("Median_Nitrate","Median_Ecoli","Max_AmmoniacalN","Median_Ammoniacal","Per_Nitrate","Max_AmmoniacalN","Median_Nitrate","Per_Nitrate","Median_Ecoli","Median_Ammoniacal")
p <- c("Median_Ecoli","Median_Ecoli","Per_Ecoli","Per_Ecoli","Median_Ammoniacal","Median_Nitrate","Max_AmmoniacalN","Max_AmmoniacalN","Median_Ammoniacal","Median_Nitrate","Per_Nitrate","Per_Nitrate")
p=vars #EG: I dont see why this should be different!?
#desc <- c("value","value","band","band","band","value","band","value","band","value")
desc <- c("value","band","value","band","band","band","value","band","value","value","value","band")
desc = rep('value',length(vars))
desc[grepl(vars,pattern = 'band',ignore.case = T)] <- 'band'
desc[vars%in%c("Agency", "SWQAltitude","SWQLanduse","SiteID")] <- 'meta'

dfp <- data.frame(vars,p,desc,stringsAsFactors=FALSE,row.names=NULL)
nofRound <- merge(nofRound,dfp,by.x="variable",by.y="vars",all=TRUE)


# POST PROCESSING NOF RESULTS
# Round values to appropriate DP's

# Note that for rounding off a 5, the IEC 60559 standard is expected to be used, ‘go to the even digit’. Therefore round(0.5) is 0 and round(-1.5) is -2
# This is not the desired behaviour here. It is expected that 0.5 rounds to 1, 2.5 rounds, to 3 and so on.
# Therefore for all even values followed by exactly .5 needs to have a small number added (like 0.00001) to get them rounding in the right direction (unless there is 
# a flag in the function that provides for this behaviour), or to redefine the round function. 
# (see http://theobligatescientist.blogspot.co.nz/2010/02/r-i-still-love-you-but-i-hate-your.html)

# As all values are positive, we'll add a small number, related to the degree of rounding required.
# If I was smarter, I would redefine the round function


for(i in 1:length(vars)){
  if(!is.na(dp[i])){
    nofRound$value[nofRound$variable==vars[i]] <- as.character(as.numeric(nofRound$value[nofRound$variable==vars[i]]) + 0.000001)
    nofRound$value[nofRound$variable==vars[i]] <- as.character(round(as.numeric(nofRound$value[nofRound$variable==vars[i]]),digits = dp[i]))
  } else {
    
  }
}


nofRound$value[is.na(nofRound$value)] <- "NA"
nofRound <- nofRound[order(nofRound$LawaSiteID,nofRound$p,nofRound$desc),]
write.csv(nofRound, file = "h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOF_STATE_2018_NAs.csv")

# Transform (tidyr::spread) data in nofRound to the following form to supply to IT Effect
# LawaSiteID,SiteName,Year,Parameter,value,Band
# ARC-00001,44603,Overall,Max_AmmoniacalN,NA,NA
# ARC-00001,44603,Overall,Median_Ammoniacal,NA,NA
# ARC-00001,44603,Overall,Median_Ecoli,28,A
# ARC-00001,44603,Overall,Median_Nitrate,0.0079,A

nof_value <- nofRound[nofRound$desc=="value",c(2,3,4,6,5)]
names(nof_value) <- c("LawaSiteID","SiteName","Year","Parameter","value")
nof_band  <- nofRound[nofRound$desc=="band",c(2,3,4,6,5)]
names(nof_band) <- c("LawaSiteID","SiteName","Year","Parameter","Band")

nof_wide <- dplyr::left_join(nof_value,nof_band,by = c("LawaSiteID", "SiteName", "Year", "Parameter"))
nof_wide <- unique(nof_wide)

write.csv(nof_wide, file = "h:/ericg/16666LAWA/2018/WaterQuality/ROutput/NOF_STATE_2018_ITEFFECT.csv")
