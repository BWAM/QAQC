## ----setupP, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
#file.copy("sections/images", ".", recursive = TRUE)

## ---- echo=FALSE---------------------------------------------------------
#converting non-detects to 0
dup$result_value.dup <- ifelse(dup$lab_qualifiers.dup=="U",0,dup$result_value.dup)
dup$result_value.dup <- ifelse(dup$lab_qualifiers.dup=="UN",0,dup$result_value.dup)
dup$result_value.dup <- ifelse(dup$lab_qualifiers.dup=="UE",0,dup$result_value.dup)
dup$result_value.parent <- ifelse(dup$lab_qualifiers.parent=="U",0,dup$result_value.parent)
dup$result_value.parent <- ifelse(dup$lab_qualifiers.parent=="UN",0,dup$result_value.parent)
dup$result_value.parent <- ifelse(dup$lab_qualifiers.parent=="UE",0,dup$result_value.parent)
#Calculating RPD
dup$RPD<-((dup$result_value.parent-dup$result_value.dup))
#can't divide by 0 so I calculate the denominator first 
dup$RPD<-ifelse(dup$RPD==0,0,((dup$RPD)/(dup$result_value.parent+dup$result_value.dup/2))*100)
dup$RPD<-abs(dup$RPD)
#remove parameters wihtout method detection limits
dup<-dup[!is.na(dup$method_detection_limit.parent),]
#this step is necessary to make sure you're using the max sample value
dup$ratio<-ifelse(pmax(dup$result_value.parent, dup$result_value.dup)==dup$result_value.parent,
                  ifelse(dup$result_value.parent==0,0,(dup$result_value.parent/dup$method_detection_limit.parent)),
                  ifelse(dup$result_value.dup==0,0,(dup$result_value.dup/dup$method_detection_limit.dup)))
dup$eq<-ifelse(dup$ratio!=0,(100*0.9465*(dup$ratio^-0.344))+5,0)
dup$pass<-ifelse((dup$eq>dup$RPD)|(dup$RPD==0),"pass","fail")
#ohio's line of acceptability
eq = function(x){(0.9465*(x^(-0.344)))*100+5}

params<-dup[dup$pass=="fail",]
params<-params[!is.na(params$pass),]
params<-unique(params$chemical_name)
nparams<-length(params)

if (nparams > 0) {
  for(i in 1:nparams){
  temp<-dup[dup$chemical_name==params[i],]
  #plot the result values
  plot(temp$ratio,temp$RPD,type="p",lwd=4,col="forestgreen",main=params[i],xlab="sample/DL ratio",ylab="RPD")
  abline(h=20)
  lines(eq(1:1000),type="l",lwd=4,col="deepskyblue")
 rm(temp) 
  }
  rm("i")
}
rm(list=c('params','nparams','eq'))

## ---- echo=FALSE,warning=FALSE-------------------------------------------
#truncate the dup file
dups<-unique(dup[c('chemical_name','pass','sample_date.parent')])
names(dups)[names(dups)=='sample_date.parent']<-'sample_date'
# dups$sample_date<-as.Date(dups$sample_date,"%m/%d/%Y")

samples<-unique(data[c('sys_sample_code','chemical_name','sample_date')])
# samples$sample_date<-as.Date(samples$sample_date,"%m/%d/%Y")

params<-unique(dups$chemical_name)
nparams<-length(params)

dataset <- bind_nearest_date(sample.df = samples,
                          match.df = dups,
                          match.col = "pass") %>% 
  dplyr::rename(duppass = pass)

#merge with final data dataset
#first convert data date to a date
# data$sample_date<-as.Date(data$sample_date,"%m/%d/%Y")
data<-merge(data,dataset,by=c('sys_sample_code','chemical_name','sample_date'),all=TRUE)
data<-data[!is.na(data$chemical_name),]
rm(dataset)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#plotting the sample failures
#count fails and passes
library(plyr)
dupfails<-count(dup[c('short','pass')])
samplefails<-count(data[c('short','duppass')])
names(samplefails)[names(samplefails)=='duppass']<-'legend'
names(dupfails)[names(dupfails)=='pass']<-'legend'

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

 samplefails<-(ggplot() +
    geom_col(data=samplefails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank()))
 dupfails<-(ggplot() +
    geom_col(data=dupfails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank()))
 print(ggarrange(
  samplefails, dupfails, labels = c("associated sample fails", "individual dup fails"),
  common.legend = TRUE, legend = "bottom"
  ))
 rm(list=c('dupfails','samplefails'))
 rm(dup)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#This bit of script converts pass faill to R (reject) and A (Accept)
data$duppass<-gsub("pass","A",data$duppass)
data$duppass<-gsub("fail","R",data$duppass)


