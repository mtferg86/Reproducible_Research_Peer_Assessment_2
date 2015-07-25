###########################################################
#title: 'Exploratory Data Analysis: Course Project 2'
#author: "Matthew Fergusson"
#date: "July 22, 2015"
#output: Storm Data Processing and Results 
###########################################################

#Remove all objects in [r] environment
#rm(list = ls(all.names = TRUE))

###########################################
# Set Libraries 
###########################################
library(tidyr)
library(ggplot2)
library(dplyr)
library(sqldf)
library(lubridate)

###########################################
# Download data 
###########################################

#file URL
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
#Destination file path and file name
fileDest <- "C:/Users/mfergusson/Desktop/MTF Personal/Education - Coursera Data Science Specialization/05 - Reproducible Research/Peer Assessment 2/repdata-data-StormData.csv.bz2"

#directory for R scripts and other work
Base_wd = "C:/Users/mfergusson/Desktop/MTF Personal/Education - Coursera Data Science Specialization/05 - Reproducible Research/Peer Assessment 2"
setwd(Base_wd)

#download file
  #download.file(fileUrl, destfile = fileDest)
#unzip all to location

StormData <- read.csv("repdata-data-StormData.csv.bz2", header=TRUE, sep = "," , quote = "\"")

names(StormData)
#Keep: 
# "BGN_DATE" / "EVTYPE" / "FATALITIES" / "INJURIES" / "PROPDMG" / "PROPDMGEXP" /  "CROPDMG" / "CROPDMGEXP"

StormData_2 <- select(StormData, BGN_DATE,  EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP )
  StormData_2$FATALITIES <- as.numeric(StormData_2$FATALITIES) 
  StormData_2$INJURIES <- as.numeric(StormData_2$INJURIES)
  StormData_2$PROPDMG <- as.numeric(StormData_2$PROPDMG)
  StormData_2$CROPDMG <- as.numeric(StormData_2$CROPDMG)
  StormData_2$BGN_DATE <- as.POSIXct(StormData_2$BGN_DATE, format = "%m/%d/%Y %H:%M:%S")
  StormData_2$year <- year(StormData_2$BGN_DATE)

#look at data distribution by year
  par(mfcol = c(1,2))
  par(mar = c(4.5,4.5,3,2))
  hist(StormData_2$year
       , main=""
       , xlab="year"
       , ylab="Yearly Event Count")
  par(mar = c(3,3,3,2))
  boxplot(StormData_2$year)
  mtext("Yearly Event Observations", side = 3, line = -2, outer = TRUE, cex = 2)

#Filder out loans before 1985
    #looks like modern reading start to come into play around 1990 but I would like to
    #look at all the data points within my lifetime to make things interesting for me :)
  StormData_2 <- filter(StormData_2, StormData_2$year >= 1985)
    
#examing the "EXP" fields
  unique(StormData_2$PROPDMGEXP)
  unique(StormData_2$CROPDMGEXP)

#create modified "PROPDMGEXP" and "CROPDMGEXP" fields to treat them as exponential values of the damages
  StormData_3 <- sqldf(" SELECT *
        ,  case when PROPDMGEXP in ('B','b') then 9
                when PROPDMGEXP in ('M','m') then 6
                when PROPDMGEXP in ('K','k') then 3
                when PROPDMGEXP in ('H','h') then 2
                when PROPDMGEXP in ('?','-','', '+') then 0
                else PROPDMGEXP
           end as PROPDMGEXP_mod
        ,  case when CROPDMGEXP in ('B','b') then 9
                when CROPDMGEXP in ('M','m') then 6
                when CROPDMGEXP in ('K','k') then 3
                when CROPDMGEXP in ('H','h') then 2
                when CROPDMGEXP in ('?','-','', '+') then 0
                else CROPDMGEXP
           end as CROPDMGEXP_mod
        FROM StormData_2")

#create modified damages fields to make values comparable for top 10
  StormData_3$PROPDMG_mod <- StormData_3$PROPDMG * 10^StormData_3$PROPDMGEXP_mod
  StormData_3$CROPDMG_mod <- StormData_3$CROPDMG * 10^StormData_3$CROPDMGEXP_mod
  
#Take sum of all injuries, fatalities and economic damages by event type
  #using modified fields for damages
StormData_4 <- sqldf("SELECT EVTYPE 
      ,sum(FATALITIES) as FATALITIES_TOT 
      ,sum(INJURIES) as INJURIES_TOT 
      ,sum(FATALITIES) + sum(INJURIES) as FAT_INJ_TOT
      ,sum(PROPDMG_mod) as PROPDMG_TOT 
      ,sum(CROPDMG_mod) as CROPDMG_TOT 
      ,sum(PROPDMG_mod) + sum(CROPDMG_mod) as ECONDMG_TOT
      ,count(*) as OCCURENCES
      FROM StormData_3
      GROUP BY EVTYPE")

#Isolate Health related data
  StormData_Health <- select(StormData_4, EVTYPE, FATALITIES_TOT, INJURIES_TOT, FAT_INJ_TOT)
#Top 10 Inuries and Fatalities Combined 
  StormData_Health <- arrange(StormData_Health, desc(FAT_INJ_TOT))
  StormData_Health_Top10_Both <- StormData_Health[1:10,]
#Top 10 fatalities
  StormData_Health <- arrange(StormData_Health, desc(FATALITIES_TOT))
  StormData_Health_Top10_Fat <- StormData_Health[1:10,]
#Top 10 Injuries
    StormData_Health <- arrange(StormData_Health, desc(INJURIES_TOT))
    StormData_Health_Top10_Inj <- StormData_Health[1:10,]

#Combine for table display
 Health_Impact_comp <- cbind(as.character(StormData_Health_Top10_Both$EVTYPE), as.numeric(StormData_Health_Top10_Both$FAT_INJ_TOT)
        , as.character(StormData_Health_Top10_Fat$EVTYPE), as.numeric(StormData_Health_Top10_Fat$FATALITIES_TOT)
        , as.character(StormData_Health_Top10_Inj$EVTYPE), as.numeric(StormData_Health_Top10_Inj$INJURIES_TOT))
 colnames(Health_Impact_comp) <- c("Top 10 Combined","Fatalities and Injuries","Top 10 Fatalities","Fatalities", "Top 10 Injuries","Injuries"  )    
 Health_Impact_comp <- data.frame(Health_Impact_comp)  
 names(Health_Impact_comp) 
 select (Health_Impact_comp, Top.10.Combined, Fatalities.and.Injuries)
 select (Health_Impact_comp, Top.10.Fatalities, Fatalities)
 select (Health_Impact_comp, Top.10.Injuries, Injuries)
 
# for figuring out ggplot    
  # StormData_Health_Top10_vert <- gather(StormData_Health_Top10, "Measure", "Count", -EVTYPE)

par(mfcol = c(1,1))
par(mar =c( 8, 4, 2, 2))
barplot(StormData_Health_Top10_Both$FAT_INJ_TOT
        , las=2
        , names = StormData_Health_Top10_Both$EVTYPE
        , cex.names = 0.7
        , cex.axis = 0.7
        , xlab = ""
        , ylab = "Event Occurences"
        , main = "Top 10: Combined Fatality and Injury Occurences")

#Isolate Econ damage related data
StormData_EconDMG <- select(StormData_4, EVTYPE, PROPDMG_TOT, CROPDMG_TOT, ECONDMG_TOT)
#Top 10 Inuries and Fatalities Combined 
StormData_EconDMG <- arrange(StormData_EconDMG, desc(ECONDMG_TOT))
StormData_EconDMG_Top10_Both <- StormData_EconDMG[1:10,]
#Top 10 fatalities
StormData_EconDMG <- arrange(StormData_EconDMG, desc(PROPDMG_TOT))
StormData_EconDMG_Top10_PROP <- StormData_EconDMG[1:10,]
#Top 10 Injuries
StormData_EconDMG <- arrange(StormData_EconDMG, desc(CROPDMG_TOT))
StormData_EconDMG_Top10_CROP <- StormData_EconDMG[1:10,]

#Combine for table display
EconDMG_Impact_comp <- cbind(as.character(StormData_EconDMG_Top10_Both$EVTYPE), as.numeric(StormData_EconDMG_Top10_Both$ECONDMG_TOT)
                            , as.character(StormData_EconDMG_Top10_PROP$EVTYPE), as.numeric(StormData_EconDMG_Top10_PROP$PROPDMG_TOT)
                            , as.character(StormData_EconDMG_Top10_CROP$EVTYPE), as.numeric(StormData_EconDMG_Top10_CROP$CROPDMG_TOT))
colnames(EconDMG_Impact_comp) <- c("Top 10 Combined","Property and Crop Damage","Top 10 Property Damage","Property Damage", "Top 10 Crop Damage","Crop Damage")    
EconDMG_Impact_comp <- data.frame(EconDMG_Impact_comp)

names(EconDMG_Impact_comp)

select(EconDMG_Impact_comp, Top.10.Combined,Property.and.Crop.Damage)
select(EconDMG_Impact_comp, Top.10.Property.Damage,Property.Damage)
select(EconDMG_Impact_comp, Top.10.Crop.Damage,Crop.Damage)

StormsData_EconDMG_Top10_Both_Graph <- StormData_EconDMG_Top10_Both
StormData_EconDMG_Top10_Both_Graph$ECONDMG_TOT <- StormData_EconDMG_Top10_Both_Graph$ECONDMG_TOT / 10^9

par(mfcol = c(1,1))
par(mar =c( 8, 5, 2, 2))
barplot(StormData_EconDMG_Top10_Both_Graph$ECONDMG_TOT
        , las=2
        , names = StormData_EconDMG_Top10_Both_Graph$EVTYPE
        , cex.names = 0.7
        , cex.axis = 0.7
        , xlab = ""
        , ylab = "Damages (in Billions of USD)"
        , main = "Top 10: Combined Property and Crop Damages")

