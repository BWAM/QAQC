## ----setupE, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
#file.copy("sections/images", ".", recursive = TRUE)

## ---- echo=FALSE---------------------------------------------------------
OHIO<-read.csv("data/Ohio.Qualifiers.csv")
knitr::kable(OHIO)
rm(OHIO)

## ----message=FALSE, results='asis'---------------------------------------
#create pass column - pass if less than reporting limit; reject if more than reporting limit
#convert non-detects to 0
EB$lab_qualifiers<-as.character(EB$lab_qualifiers)
EB$result_value <- ifelse(grepl("U",EB$lab_qualifiers),0,EB$result_value)

#create pass column
EB$pass<-ifelse(EB$result_value>EB$quantitation_limit,"fail","pass")
#format date
# EB$sample_date <- as.Date(EB$sample_date,"%m/%d/%Y")

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#plot the EBt result value / day for each param 
#pull only those parameters with failures
params<-EB[EB$pass!="pass",]
params<-params[!is.na(params$chemical_name),]
params<-unique(params$chemical_name)
nparams<-length(params)

for(i in 1:nparams){
  temp<-EB[EB$chemical_name==params[i],]
  # temp$sample_date<-as.Date(temp$sample_date,"%m/%d/%Y")
  reporting<-temp$quantitation_limit[1]
  #plot the result values
  plot(temp$sample_date,temp$result_value,type="p",lwd=4,col="black",main=params[i],xlab="date",ylab="equipment blank result value")
  abline(h=c(reporting))
  rm(list=c('temp','reporting'))
}
rm(list=c('params','nparams','i'))

## ---- echo=FALSE,warning=FALSE-------------------------------------------
#truncate the EB file
EBs<-unique(EB[c('chemical_name','result_value','sample_date','pass')])
names(EBs)[names(EBs)=='result_value']<-'blank'
EBs$blank<-ifelse(EBs$pass=="fail",EBs$blank,NA)
# EBs$sample_date<-as.Date(EBs$sample_date,"%m/%d/%Y")
EBs<-unique(EBs[c('chemical_name','blank','sample_date')])

samples<-unique(data[c('sys_sample_code','chemical_name','sample_date')])
# samples$sample_date<-as.Date(samples$sample_date,"%m/%d/%Y")

params<-unique(EBs$chemical_name)
nparams<-length(params)

dataset <- bind_nearest_date(sample.df = samples,
                          match.df = EBs,
                          match.col = "blank")

#merge with final data dataset
#first convert data date to a date
# data$sample_date<-as.Date(data$sample_date,"%m/%d/%Y")
data<-merge(data,dataset,by=c('sys_sample_code','chemical_name','sample_date'),all=TRUE)
data<-data[!is.na(data$chemical_name),]
rm(dataset)

#finally, calculate error flag if any
data$EBpass<-ifelse(is.na(data$blank),"A",ifelse(data$result_value<=3*(data$blank),"R",ifelse(data$result_value<=5*(data$blank),"T",ifelse(data$result_value<=10*(data$blank),"J","A"))))

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#plotting the sample failures
#count fails and passes
library(plyr)
blankfails<-count(EB[c('short','pass')])
samplefails<-count(data[c('short','EBpass')])
names(samplefails)[names(samplefails)=='EBpass']<-'legend'
names(blankfails)[names(blankfails)=='pass']<-'legend'
blankfails$legend<-gsub("pass","A",blankfails$legend)
blankfails$legend<-gsub("fail","R",blankfails$legend)

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

 samplefails<-(ggplot() +
    geom_col(data=samplefails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank()))
 blankfails<-(ggplot() +
    geom_col(data=blankfails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank()))
 print(ggarrange(
  samplefails, blankfails, labels = c("associated sample fails", "individual EB fails"),
  common.legend = TRUE, legend = "bottom"
  ))
 rm(list=c('blankfails','samplefails'))
 rm(EB)

