#=======================================================================================================
#Create a function to adjust Ammonia to pH8
NH4adj<-function(sub_swq,vars=c("NH4","PH"),csv){
  A<-read.csv(csv,stringsAsFactors = FALSE)
  A <- rbind(A,c(NA,1))
  
  D <- sub_swq[sub_swq$parameter%in%vars,] 
  D <- D%>%spread(parameter,Value)          
  
  D$Ratio=approx(x = A$PH,y = A$Ratio,xout = D$PH)$y
  D$NH4adj<-D$NH4/D$Ratio
  
  D <- D%>%select("LawaSiteID","SiteName","Date","NH4adj")
  Dg <- D%>%gather(key="parameter",value="Value",4)%>%drop_na()
  return(Dg)
}
#=======================================================================================================
#Create a function for finding the band for each value
#colValue: the column name that we want to find the band for each value
#parameter: the column name that content the value need to compare in NOF table   (Band Thresholds)
#colBand: the column name that we will insert the band in .
#We took band C as a special case as inclusive of the upper value as this is the National Bottom line.

# Band Median.Nitrate X95th.Percentile.Nitrate Median.Ammoniacal.N Max.Ammoniacal.N Chl.a.max E..coli Ecoli95 EcoliRec540 EcoliRec260
# 1    A           <1.0                     <1.5               <0.03            <0.05       <50    <260     540          <5         <20
# 2    B            2.4                      3.5                0.24              0.4       120     540    1000         <10         <30
# 3    C            6.9                      9.8                 1.3              2.2       200    1000    1200         <20         <34
# 4    D           >6.9                     >9.8               >1.30             >2.2      >200   >1000   >1200         <30         <50

NOF_FindBand_orig <- function(colValue, parameter, colBand)
{
  for ( i in 1:length(colValue))
  {
    if(is.na(colValue[i]) == FALSE)
    {
      if (colValue[i] < parameter[1])
      {
        colBand[i] = "A"
      }
      if (colValue[i] >= parameter[1] && colValue[i] < parameter[2])
      {
        colBand[i] = "B"
      }
      if (colValue[i] > parameter[2] && colValue[i] <= parameter[3])  #Note the inclusion of the upper value for this band#
      {
        colBand[i] = "C"
      }
      if (colValue[i] > parameter[3])
      {
        colBand[i] = "D"
      }
    }
  }
  return(colBand)
}
#=======================================================================================================
NOF_FindBand <- function(value, bandColumn){
  # This takes rules from the NOFbandDefinition file, 
  # which are expressed in terms of thresholds such as <=1.0, <=2.4, <=6.9 >6.9
  # and pastes the values to be assessed in front of each, then evaluates to see the first that evaluates as true.
  # e.g. 0.6 is true only for the first (0.6<=1.0 is TRUE but 0.6<=2.4 is FALSE etc)
  # e.g. 2.2 is true only for the second (2.2<=1.0 is FALSE byt 2.2<=2.4 is TRUE etc)
  # e.g. 5 is true only for the third (5<=1.0 is FALSE, 5<=2.4 is FALSE but 5<=6.9 is TRUE)
  # e.g. 8 is true only for the fourth
  paste(LETTERS[which(unlist(lapply(gsub(pattern = 'x',replacement = value,x = bandColumn),FUN = function(x){eval(parse(text=x))})))],collapse='')
}
#=======================================================================================================

#Create a function to add value into the table of compliance (created later as each site is processed) with proposed National Objectives Framework
#value : the table that has values to be added 
#column: the column that the values will be added into
#year: the year column to comcapre
NOF_AddValue <- function(value, column, year)
{
  for(i in 1:length(value))
  {
    for( e in 1:(length(column)-1))
    {
      if(!is.na(rownames(Value)[e])){
        if(rownames(value)[i] == year[e])
        {
          column[e] = value[i]
        }
      } else{
        column[e] = NA
      }
    }
  }
  return(column)
}


#Create a fuction to compare the median and 95th percantage 
#column1 and column2: the two columns that we want to compare 
#column3: the column that we will insert the result into
NOF_Compare <- function(column1, column2, column3)
{
  for(i in 1:length(column1))
  {
    if(is.na(column1[i]) == FALSE && is.na(column2[i]) == FALSE)
    {
      if (column1[i] >= column2[i])
      {
        column3[i] = column1[i]
      }
      if (column1[i] <= column2[i])
      {
        column3[i] = column2[i]
      }
    }
    if(is.na(column1[i]) == FALSE && is.na(column2[i]) == TRUE)
    {
      column3[i] = column1[i]
    }
    if(is.na(column1[i]) == TRUE && is.na(column2[i]) == FALSE)
    {
      column3[i] = column2[i]
    }
  }
  return(column3)
}

#Create a function to change the band to number so it will will be easier for find comparation.
#Band A, B, C and D will be 1, 2, 3, and 4
#Column : the column that you want to conver to number
NOF_BtoN <- function(column)
{
  column <- as.character(column)
  for(i in 1:length(column))
  {
    if(is.na(column[i]) == FALSE)
    {
      if(column[i] == "A")
      {
        column[i] = "1"
      }
      if(column[i] == "B")
      {
        column[i] = "2"
      }
      if(column[i] == "C")
      {
        column[i] = "3"
      }
      if(column[i] == "D")
      {
        column[i] = "4"
      }
    }
  }
  column <- as.numeric(column)
  return(column)
}


#Create a fuction to change the number back to the band and add them into the summary table 
#column1: the column we want to change
#column2: the column of summary table that you want to insert the result band into 
NOF_NtoB <- function(column1, column2)
{
  column <- as.character(column1)
  for(i in 1:length(column1))
  {
    if(is.na(column1[i]) == FALSE)
    {
      if(column1[i] == "1")
      {
        column2[i] = "A"
      }
      if(column1[i] == "2")
      {
        column2[i] = "B"
      }
      if(column1[i] == "3")
      {
        column2[i] = "C"
      }
      if(column1[i] == "4")
      {
        column2[i] = "D"
      }
    }
  }
  return(column2)
}


