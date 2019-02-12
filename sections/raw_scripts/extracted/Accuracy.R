## ----setuA, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
#file.copy("sections/images", ".", recursive = TRUE)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#calculate the failures myself rather than depending on the lab
spike$percrecovery<-((abs(spike$qc_spike_measured-spike$qc_original_conc))/spike$qc_spike_added)*100
spike$spikeamount<-spike$qc_original_conc/spike$qc_spike_added
spike$pass<-ifelse(spike$spikeamount>4,NA,
            ifelse(spike$percrecover>(100+spike$accuracy)|spike$percrecovery<(100-spike$accuracy),"fail","pass"))

#create a separate file with the samples flagged as pass / fail to merge with parent file later (safety precaution)
#format date to date/time because multiple spike samples in one day
library(lubridate)
# spike$sample_date <- mdy_hm(spike$sample_date)
spikes<-unique(spike[c('chemical_name','sample_date','pass')])

samples<-unique(data[c('sys_sample_code','chemical_name','sample_date')])
# samples$sample_date <- mdy_hm(samples$sample_date)

params<-unique(spikes$chemical_name)
nparams<-length(params)

dataset <- bind_nearest_date(sample.df = samples,
                          match.df = spikes,
                          match.col = "pass") %>% 
  dplyr::rename(spikepass = pass)
#merge with final dataset
#first convert data date to a date
# data$sample_date<-mdy_hm(data$sample_date)
data<-merge(data,dataset,by=c('sys_sample_code','chemical_name','sample_date'),all=TRUE)
data<-data[!is.na(data$chemical_name),]
#convert date back to simple date
# data$sample_date<-as.Date(data$sample_date,"%m/%d/%Y")
rm(dataset)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#plotting the sample failures
#count fails and passes
library(plyr)
spikefails<-count(spike[c('short','pass')])
samplefails<-count(data[c('short','spikepass')])
names(samplefails)[names(samplefails)=='spikepass']<-'legend'
names(spikefails)[names(spikefails)=='pass']<-'legend'

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

 samplefails<-(ggplot() +
    geom_col(data=samplefails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank()))
 spikefails<-(ggplot() +
    geom_col(data=spikefails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank()))
 print(ggarrange(
  samplefails, spikefails, labels = c("associated sample fails", "individual spike fails"),
  common.legend = TRUE, legend = "bottom"
  ))
 rm(list=c('spikefails','samplefails','spike'))

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#This bit of script converts pass faill to R (reject) and A (Accept)
data$spikepass<-gsub("pass","A",data$spikepass)
data$spikepass<-gsub("fail","R",data$spikepass)


