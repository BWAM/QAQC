## ----setuPP, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
#file.copy("sections/images", ".", recursive = TRUE)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
component<-c('NITROGEN, NITRATE (AS N)','NITROGEN, NITRITE','Nitrate+Nitrite as Nitrogen','Nitrogen, ammonia (As N)','NITROGEN, KJELDAHL, TOTAL','Orthophosphate as Phosphorus, Dissolved')
of<-c('Nitrate+Nitrite as Nitrogen','Nitrate+Nitrite as Nitrogen','Nitrogen','Nitrogen','Nitrogen','PHOSPHORUS, TOTAL (AS P)')
table<-data.frame(component,of)
knitr::kable(table)
rm(list=c('component','of','table'))

## ----message=FALSE, warning=FALSE, results='asis'------------------------

#pull parameter pairs
#the pair is the total param that should be larger. The param that remains chemical_name is the param that should be smaller
#we are only flagging the param that should be smaller
pair<-unique(data[c('sys_sample_code','lab_sdg','sample_date','chemical_name','short','result_value','result_unit','smaller')])
paird<-pair[!is.na(pair$smaller),]
#remove the smaller and short columns in pair
pair<-unique(pair[c('sys_sample_code','lab_sdg','sample_date','chemical_name','result_value','result_unit')])
#rename the result value and chemical name in pair so can merge to smaller column in paird
names(pair)[names(pair)=='result_value']<-'result_value.total'
names(pair)[names(pair)=='result_unit']<-'result_unit.total'
names(pair)[names(pair)=='chemical_name']<-'smaller'
#now merge the two only keeping params in paird
pair<-merge(paird,pair,by=c('sys_sample_code','lab_sdg','sample_date','smaller'),all=FALSE)
rm(paird)
#now remove rows where the component parameter is NA
pair<-pair[!is.na(pair$result_value),]
#check that the units are the same and flagg values larger than their total
pair$pairpass<-ifelse(pair$result_unit!=pair$result_unit.total,"units don't match",ifelse(pair$result_value>pair$result_value.total,"R","A"))
#restrict just to 
pair<-unique(pair[c('sys_sample_code','chemical_name','short','sample_date','pairpass')])

#add these flags to the larger data set
# data$sample_date<-as.Date(data$sample_date,"%m/%d/%Y")
# pair$sample_date<-as.Date(pair$sample_date,"%m/%d/%Y")
data<-merge(data,pair,by=c('sys_sample_code','chemical_name','short','sample_date'),all=TRUE)
data<-data[!is.na(data$chemical_name),]

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#plotting the sample failures
#count fails and passes
library(plyr)
pairfails<-count(pair[c('short','pairpass')])
names(pairfails)[names(pairfails)=='pairpass']<-'legend'

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

 print(pairfails<-(ggplot() +
    geom_col(data=pairfails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())))
rm(list=c('pairfails','pair'))

