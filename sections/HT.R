## ----setuTHT, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
#file.copy("sections/images", ".", recursive = TRUE)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
# Load holding times reference table
holdingtimes <- read.csv("data/Holding.Times.csv")
holdingtimes$Holding<-as.numeric(holdingtimes$Holding)

# Make simpler holding time lookup table with only two columns
holdingtimes <- unique(holdingtimes[c("chemical_name","Holding")])
# Merge data frame and holding times lookup table
HT <- merge(HT, holdingtimes, by=c("chemical_name"), all.x = TRUE)
rm(holdingtimes)

#format dates
library(lubridate)

#Added on 2/8/19 to account for new handing of date formatting at beginning of script (QAQC.Rmd)
HT$sample_date<-format(HT$sample_date, "%m-%d-%Y %H:%M")
HT$analysis_date<-format(HT$analysis_date, "%m-%d-%Y %H:%M")

HT$sample_date<-mdy_hm(HT$sample_date)
HT$analysis_date<-mdy_hm(HT$analysis_date)
# Calculate difference between sample datetimes and analysis datetimes (actual holding times)
HT$HTactual <- difftime(HT$analysis_date ,HT$sample_date , units = c("hours"))

# Calculate holding time exceedances 
HT$HTpass <- HT$Holding - HT$HTactual
HT$HTpass <- as.numeric(HT$HTpass)
HT$HTpass<-ifelse(HT$HTpass<0,"R","A")
HT<-unique(HT[c('chemical_name','sys_sample_code','lab_sdg','sample_date','short','HTpass')])
#add these flags to the larger data set
# data$sample_date<-as.Date(data$sample_date,"%m/%d/%Y")
data<-merge(data,HT,by=c('sys_sample_code','chemical_name','short','sample_date','lab_sdg'),all=TRUE)
data<-data[!is.na(data$chemical_name),]

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#plotting the sample failures
#count fails and passes
library(plyr)
HTfails<-count(HT[c('short','HTpass')])
names(HTfails)[names(HTfails)=='HTpass']<-'legend'

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

 print(HTfails<-(ggplot() +
    geom_col(data=HTfails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())))
rm(list=c('HTfails','HT'))

