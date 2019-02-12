## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
#file.copy("sections/images", ".", recursive = TRUE)

## ---- echo=FALSE---------------------------------------------------------
#associate the data with the reporting limts file
rlimits<-read.csv("sections/data/reporting.limits.csv")
data<-merge(data,rlimits,by=c('chemical_name'),all=FALSE)
rm(rlimits)

# Convert dates from factors to POSIXct objects
library(lubridate)
if (all(stringr::str_count(data$sample_date, ':') == 2)){
 data$sample_date <- mdy_hms(data$sample_date)
 data$analysis_date <- mdy_hms(data$analysis_date)
} else{
 data$sample_date <- mdy_hm(data$sample_date)
 data$analysis_date <- mdy_hm(data$analysis_date)
}

#pulling spike dataset
spike<-data[data$sample_type_code=="MS",]
spike<-unique(spike[c('sys_sample_code','lab_sdg','sample_date','chemical_name','short','fraction','result_value','result_unit','quantitation_limit','method_detection_limit','detection_limit_unit','lab_qualifiers','qc_original_conc','qc_spike_added','qc_spike_measured','accuracy')])
spike<-spike[!is.na(spike$chemical_name),]

#for all other data sets I need to first restrict the raw data to only that listed as "field"
data<-data[data$sample_source=="Field",]
data<-data[!is.na(data$chemical_name),]

#pulling equipment blank data set
EB<-data[data$DEC_sample_type=="EB",]
EB<-unique(EB[c('sys_sample_code','lab_sdg','sample_date','chemical_name','short','fraction','result_value','result_unit','quantitation_limit','method_detection_limit','detection_limit_unit','lab_qualifiers','reporting_limit','reporting_units')])
EB<-EB[!is.na(EB$chemical_name),]

#pulling duplicate data set
dup<-data[data$DEC_sample_type=="DUP"|data$DEC_sample_type=="N_DUPPARENT",]
dup<-unique(dup[c('DEC_sample_type','sys_sample_code','lab_sdg','sample_date','chemical_name','short','fraction','result_value','result_unit','quantitation_limit','method_detection_limit','detection_limit_unit','lab_qualifiers')])
#separate the parent and duplicate data sets
dupparent<-dup[dup$DEC_sample_type=="N_DUPPARENT",]
dup<-dup[dup$DEC_sample_type=="DUP",]
#remove -DUP from sys sample code for merge, including all instances of upper and lower case.
dup$sys_sample_code<-gsub("-[Dd][Uu][Pp]","",dup$sys_sample_code)
dup$sys_sample_code<-gsub(" [Dd][Uu][Pp]","",dup$sys_sample_code)
dup$sys_sample_code<-gsub("WS[Dd][Uu][Pp]","WS",dup$sys_sample_code)
dup$sys_sample_code<-gsub("W[Dd][Uu][Pp]","W",dup$sys_sample_code)

#convert both sys_sample_code fields to character
dup$sys_sample_code<-as.character(dup$sys_sample_code)
dupparent$sys_sample_code<-as.character(dupparent$sys_sample_code)
#rename duplicate fields so can merge the two tables
names(dupparent)[names(dupparent)=='result_value']<-'result_value.parent'
names(dupparent)[names(dupparent)=='result_unit']<-'result_unit.parent'
names(dupparent)[names(dupparent)=='sample_date']<-'sample_date.parent'
names(dupparent)[names(dupparent)=='quantitation_limit']<-'quantitation_limit.parent'
names(dupparent)[names(dupparent)=='method_detection_limit']<-'method_detection_limit.parent'
names(dupparent)[names(dupparent)=='detection_limit_unit']<-'detection_limit_unit.parent'
names(dupparent)[names(dupparent)=='lab_qualifiers']<-'lab_qualifiers.parent'
names(dup)[names(dup)=='result_value']<-'result_value.dup'
names(dup)[names(dup)=='result_unit']<-'result_unit.dup'
names(dup)[names(dup)=='sample_date']<-'sample_date.dup'
names(dup)[names(dup)=='quantitation_limit']<-'quantitation_limit.dup'
names(dup)[names(dup)=='method_detection_limit']<-'method_detection_limit.dup'
names(dup)[names(dup)=='detection_limit_unit']<-'detection_limit_unit.dup'
names(dup)[names(dup)=='lab_qualifiers']<-'lab_qualifiers.dup'
#remove DEC_sample_type for merge
dup<-unique(dup[c('sys_sample_code','lab_sdg','chemical_name','short','sample_date.dup','result_value.dup','result_unit.dup','quantitation_limit.dup','method_detection_limit.dup','detection_limit_unit.dup','lab_qualifiers.dup')])
dupparent<-unique(dupparent[c('sys_sample_code','lab_sdg','chemical_name','short','sample_date.parent','result_value.parent','result_unit.parent','quantitation_limit.parent','method_detection_limit.parent','detection_limit_unit.parent','lab_qualifiers.parent')])
#merge the two into one data set
dup<-merge(dup,dupparent,by=c('sys_sample_code','lab_sdg','chemical_name','short'),all=TRUE)
rm(dupparent)


#remove qc from complete data set
data<-data[data$DEC_sample_type=="N"|data$DEC_sample_type=="N_DUPPARENT",]

#creating a holding time subset
HT<-unique(data[c('sys_sample_code','lab_sdg','sample_date','chemical_name','short','analysis_date')])



## ---- echo=FALSE---------------------------------------------------------
samples<-unique(data[c('sample_date','lab_sdg')])
samples$freq<-1
samples$type<-"standard samples"
samples$sample_date<-as.Date(samples$sample_date,"%m/%d/%Y")
blanksamples<-unique(EB[c('sample_date','lab_sdg')])
blanksamples$freq<-2
blanksamples$type<-"equipment blanks"
blanksamples$sample_date<-as.Date(blanksamples$sample_date,"%m/%d/%Y")
dupsamples<-unique(dup[c('sample_date.parent','lab_sdg')])
dupsamples$freq<-3
dupsamples$type<-"duplicates"
names(dupsamples)[names(dupsamples)=="sample_date.parent"]<-"sample_date"
dupsamples$sample_date<-as.Date(dupsamples$sample_date,"%m/%d/%Y")
spikesamples<-unique(spike[c('sample_date','lab_sdg')])
spikesamples$freq<-4
spikesamples$type<-"spike samples"
spikesamples$sample_date<-as.Date(spikesamples$sample_date,"%m/%d/%Y")
#merge together
samples<-merge(samples,blanksamples,all=TRUE)
samples<-merge(samples,dupsamples,all=TRUE)
samples<-merge(samples,spikesamples,all=TRUE)

library(ggplot2)
print(ggplot() +
  geom_point(data=samples,aes(sample_date,freq,color=type)) +
  ylab("sample types")+
  xlab("sample date"))
rm(list=c('samples','blanksamples','dupsamples','spikesamples'))

## ---- child = 'sections/Lab.Rmd'-----------------------------------------

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


## ------------------------------------------------------------------------

bind_nearest_date <- function(sample.df, match.df, match.col) {

final.df <- lapply(unique(match.df$chemical_name), function(param.i) {
  sample.param.i <- sample.df[sample.df$chemical_name == param.i, ]
  match.param.i <- match.df[match.df$chemical_name == param.i,]
  
  site.df <- lapply(unique(sample.param.i$sys_sample_code), function(site.i) {
    sample.sub <- sample.param.i[sample.param.i$sys_sample_code == site.i, ]
    match.param.i$abs <- abs(match.param.i$sample_date - sample.sub$sample_date) 
    match.param.i$min <- min(abs(match.param.i$sample_date - sample.sub$sample_date))
    match.param.i <- match.param.i[which(match.param.i$abs == match.param.i$min), ]
    sample.sub[, match.col] <- match.param.i[, match.col][1]
    return(sample.sub)
  }) %>% 
    dplyr::bind_rows()

}) %>% 
 dplyr::bind_rows() 

return(final.df)
}

## ---- child = 'sections/Accuracy.Rmd'------------------------------------

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



## ---- child = 'sections/Precision.Rmd'-----------------------------------

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



## ---- child = 'sections/Equipment.Blanks.Rmd'----------------------------

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


## ---- child = 'sections/Parameter.pairs.Rmd'-----------------------------

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


## ---- child = 'sections/HT.Rmd'------------------------------------------

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


## ---- child = 'sections/Conclusions.Rmd'---------------------------------

## ----setupC, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
#file.copy("sections/images", ".", recursive = TRUE)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#creating one flag to rule them all and interpetation of this flag

#convert all NA values in pass fields to A
data$labpass<-ifelse(is.na(data$labpass),"A",data$labpass)
data$spikepass<-ifelse(is.na(data$spikepass),"A",data$spikepass)
data$duppass<-ifelse(is.na(data$duppass),"A",data$duppass)
data$EBpass<-ifelse(is.na(data$EBpass),"A",data$EBpass)
data$pairpass<-ifelse(is.na(data$pairpass),"A",data$pairpass)
data$HTpass<-ifelse(is.na(data$HTpass),"A",data$HTpass)
#create an overall QA field
data$validator_qualifiers<-"A"
data$validator_qualifiers<-ifelse((data$labpass=="R"|data$spikepass=="R"|data$duppass=="R"|data$EBpass=="R"|data$pairpass=="R"|data$HTpass=="R"),"R",
           ifelse(data$EBpass=="T","T",
           ifelse((data$labpass=="J"|data$EBpass=="J"),"J",
           ifelse(data$labpass=="U","U",data$validator_qualifiers))))

#add explanation for flagged data for each type
#first create the field
data$interpreted_qualifiers<-"Normal"

#For lab errors
#R flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$labpass=="R","lab error","Normal"),
                                ifelse(data$labpass=="R",paste(data$interpreted_qualifiers," ; lab error",sep=""),data$interpreted_qualifiers))
#J flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$labpass=="J","estimated value due to lab error","Normal"),
                                ifelse(data$labpass=="J",paste(data$interpreted_qualifiers," ; estimated value due to lab error",sep=""),data$interpreted_qualifiers))
#U flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$labpass=="U","Analyte was analyzed for but not detected","Normal"),
                                ifelse(data$labpass=="U",paste(data$interpreted_qualifiers," ; Analyte was analyzed for but not detected",sep=""),data$interpreted_qualifiers))

#For EB errors
#R flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$EBpass=="R","Equipment Blank error","Normal"),
                                ifelse(data$EBpass=="R",paste(data$interpreted_qualifiers," ; Equipment Blank error",sep=""),data$interpreted_qualifiers))
#J flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$EBpass=="J","estimated value due to Equipment Blank error","Normal"),
                                ifelse(data$EBpass=="J",paste(data$interpreted_qualifiers," ; estimated value due to Equipment Blank error",sep=""),data$interpreted_qualifiers))

#For spike errors
#R flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$spikepass=="R","Accuracy error","Normal"),
                                ifelse(data$spikepass=="R",paste(data$interpreted_qualifiers," ; Accuracy error",sep=""),data$interpreted_qualifiers))

#For dup errors
#R flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$duppass=="R","Precision error","Normal"),
                                ifelse(data$duppass=="R",paste(data$interpreted_qualifiers," ; Precision error",sep=""),data$interpreted_qualifiers))


#For pair errors
#R flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$pairpass=="R","Result should be less than the total","Normal"),
                                ifelse(data$pairpass=="R",paste(data$interpreted_qualifiers," ; Result should be less than the total",sep=""),data$interpreted_qualifiers))

#For HT errors
#R flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$HTpass=="R","Holding Time error","Normal"),
                                ifelse(data$HTpass=="R",paste(data$interpreted_qualifiers," ; Holding Time error",sep=""),data$interpreted_qualifiers))

#changing the 'Normal' interpreted flag to ""
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",'',data$interpreted_qualifiers)


## ----message=FALSE, warning=FALSE, results='asis'------------------------
flags<-read.csv("data/validator_flags.csv")

knitr::kable(flags)
rm(flags)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#plotting the sample failures
#count fails and passes
library(plyr)
labfails<-count(data[c('short','validator_qualifiers')])
names(labfails)[names(labfails)=='validator_qualifiers']<-'legend'

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

 print(labfails<-(ggplot() +
    geom_col(data=labfails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())))
rm(list=c('labfails'))

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#plot the result value / day for each param 
#color the points based on flags
labfails<-unique(data$chemical_name)
labfails<-count(data[c('short','interpreted_qualifiers')])
names(labfails)[names(labfails)=='interpreted_qualifiers']<-'legend'


library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())


 #plot the graph without the legend
 print((ggplot() +
    geom_col(data=labfails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")))
#plot just the legend
 legend<-(ggplot() +
                  geom_col(data=labfails,aes(short,freq,fill=legend)) +
                  coord_flip() +
                  theme_void())
legend<-get_legend(legend)
as_ggplot(legend)
rm(legend)

rm(list=c('labfails'))



## ---- echo=FALSE---------------------------------------------------------
#creating final data set
forprint<-unique(data[c('sys_sample_code','chemical_name','sample_date','cas_rn','fraction','result_value','result_unit','method_detection_limit','detection_limit_unit','quantitation_limit','lab_qualifiers','validator_qualifiers','interpreted_qualifiers')])

