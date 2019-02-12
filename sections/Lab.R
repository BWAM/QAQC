## ----setuL, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
#file.copy("sections/images", ".", recursive = TRUE)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
qualifiers<-unique(data[c('lab_sdg','lab_qualifiers')])
qualifiers$lab_qualifiers<-NA
qualifiers<-unique(qualifiers[c('lab_sdg','lab_qualifiers')])
names(qualifiers)[names(qualifiers)=='lab_qualifiers']<-'list'

knitr::kable(qualifiers)
rm(qualifiers)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
knitr::kable(errors)
rm(errors)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
flags<-unique(data[c('lab_qualifiers','fraction')])
flags$fraction<-"present"
names(flags)[names(flags)=='fraction']<-'present'
knitr::kable(flags)
rm(flags)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
data$labpass<-data$lab_qualifiers
data$labpass<-as.character(data$labpass)
data$labpass<-ifelse(grepl("B",data$labpass),"R",data$labpass)
data$labpass<-ifelse(grepl("N",data$labpass),"R",data$labpass)
data$labpass<-ifelse(grepl("\\*",data$labpass),"R",data$labpass)
data$labpass<-ifelse(grepl("D",data$labpass),"R",data$labpass)
data$labpass<-ifelse(grepl("W",data$labpass),"R",data$labpass)
data$labpass<-ifelse(grepl("U",data$labpass),"U",data$labpass)
data$labpass<-ifelse(grepl("E",data$labpass),"J",data$labpass)
data$labpass<-ifelse(grepl("J",data$labpass),"J",data$labpass)
data$labpass<-ifelse(data$labpass=="","A",data$labpass)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#plotting the sample failures
#count fails and passes
library(plyr)
labfails<-count(data[c('short','labpass')])
names(labfails)[names(labfails)=='labpass']<-'legend'

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

 print(labfails<-(ggplot() +
    geom_col(data=labfails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())))
rm(list=c('labfails'))

