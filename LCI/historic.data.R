#LCI Equipment Blank Script
#Alene Onion
#December 2018

#first run this script before running the QAQC master script

#set working directory
setwd("L:/DOW/StreamDatabase/Lakes/data")

#read data tables
profiles<-read.csv("Depth.Profile.csv",stringsAsFactors = FALSE)
location<-read.csv("Location.csv",stringsAsFactors = FALSE)
results<-read.csv("Test.Results.csv",stringsAsFactors = FALSE)
sample<-read.csv("Sample.csv",stringsAsFactors = FALSE)
lake<-read.csv("Lake.Master.csv",stringsAsFactors = FALSE)

#set working directory
setwd("C:/Rscripts/QAQC/LCI")

source('Lakes.R')
databackup<-data

#restrict to non-HABs LCI data
data<-data[data$INFO_TYPE=="OW",]
###^Do we need to include "QA" up here^
data<-data[data$DATA_PROVIDER=="LCI",]
#pull only records since 2017
data$SAMPLE_DATE<-as.Date(data$SAMPLE_DATE,"%m-%d-%Y")
data<-data[data$SAMPLE_DATE>'2017-01-01',]
data<-data[!is.na(data$SAMPLE_DATE),]
###
#we need to restrict to non HABs samples
data<-data[data$Characteristic.Name=="PHOSPHORUS"|
             data$Characteristic.Name=="NITROGEN, NITRATE-NITRITE"|
             data$Characteristic.Name=="AMMONIA"|
             data$Characteristic.Name=="NITROGEN, KJELDAHL, TOTAL"|
             data$Characteristic.Name=="ALKALINITY, TOTAL (AS CACO3)"|
             data$Characteristic.Name=="CHLOROPHYLL A"|
             data$Characteristic.Name=="TOTAL ORGANIC CARBON"|
             data$Characteristic.Name=="TRUE COLOR"|
             data$Characteristic.Name=="INORGANIC NITROGEN (NITRATE AND NITRITE)"|
             data$Characteristic.Name=="CALCIUM"|
             data$Characteristic.Name=="DISSOLVED ORGANIC CARBON"|
             data$Characteristic.Name=="UV 254"|
             data$Characteristic.Name=="SULFATE"|
             data$Characteristic.Name=="SULFATE (AS SO4)"|
             data$Characteristic.Name=="MANGANESE"|
             data$Characteristic.Name=="IRON"|
             data$Characteristic.Name=="ARSENIC"|
             data$Characteristic.Name=="COPPER"|
             data$Characteristic.Name=="MAGNESIUM"|
             data$Characteristic.Name=="CHLORIDE (AS CL)"|
             data$Characteristic.Name=="CHLORIDE"|
             data$Characteristic.Name=="BORON"|
             data$Characteristic.Name=="ALUMINUM"|
             data$Characteristic.Name=="SILICA"|
             data$Characteristic.Name=="POTASSIUM"|
             data$Characteristic.Name=="SODIUM"|
             data$Characteristic.Name=="CHLORIDE"|
             data$Characteristic.Name=="NITROGEN, NITRATE (AS N)"|
             data$Characteristic.Name=="TOTAL DISSOLVED SOLIDS"|
             data$Characteristic.Name=="TOTAL HARDNESS",]


#pull the lake ids for each location and use these to replace the lake id field which is incomplete
#this removes all the data without location ids but I'm ok with that
lakeids<-unique(data[c('LOCATION_ID','LAKE_ID')])
lakeids<-lakeids[!is.na(lakeids$LOCATION_ID),]
lakeids<-lakeids[!is.na(lakeids$LAKE_ID),]
data<-data[,!(names(data) %in% c('LAKE_ID'))]
data<-merge(data,lakeids,by=c('LOCATION_ID'),all=TRUE)
rm(lakeids)

#pull the dates for each sampleid and use these to replace the date field which is incomplete
#this removes all the data without sample ids but I'm ok with that
lakeids<-unique(data[c('SAMPLE_ID','SAMPLE_DATE')])
lakeids<-lakeids[!is.na(lakeids$SAMPLE_ID),]
lakeids<-lakeids[!is.na(lakeids$SAMPLE_DATE),]
data<-data[,!(names(data) %in% c('SAMPLE_DATE'))]
data<-merge(data,lakeids,by=c('SAMPLE_ID'),all=TRUE)
rm(lakeids)

#read QA data file
QA<-read.csv("QAsampleIDsforR.csv")
#remove spaces in names
QA$SAMPLE_NAME<-trimws(QA$SAMPLE_NAME)
QA$DEC_sample_type<-trimws(QA$DEC_sample_type)
#remove rows with NA in QA sample column
QA<-QA[!is.na(QA$SAMPLE_NAME),]
#remove rows with NA in sample type sample column
#there are some QA samples that we can't identify as DUP or EB
QA<-QA[!is.na(QA$DEC_sample_type),]
#remove dup in the QA samples field
#QA$SAMPLE_NAME<-gsub("-DUP","",QA$SAMPLE_NAME)
#QA$SAMPLE_NAME<-gsub(" DUP","",QA$SAMPLE_NAME)

#merge data and QA file so we only have the QA samples in the QA file
data<-merge(data,QA,by=('SAMPLE_NAME'),all=TRUE)
data$DEC_sample_type<-ifelse(is.na(data$DEC_sample_type),"N",data$DEC_sample_type)
rm(QA)
data<-data[!is.na(data$LOCATION_ID),]

###########################################################
#we need to pull data without lakeids and locationids and fix them!!
#junk<-data[is.na(data$LOCATION_ID)|is.na(data$LAKE_ID)|is.na(data$SAMPLE_DATE),]
#junk<-unique(junk[c('LAKE_ID','LOCATION_ID','SAMPLE_NAME','SAMPLE_DATE','DEC_sample_type','Characteristic.Name')])
#junk<-junk[order(junk$SAMPLE_NAME),]

##############################################################
#I need to subset dupes, remerge with "data" based on sample date and location id
DUPES<-unique(data[c('DEC_sample_type','SAMPLE_DATE','LAKE_ID','SAMPLE_NAME')])
dups<-DUPES[DUPES$DEC_sample_type=="DUP",]
N<-DUPES[DUPES$DEC_sample_type=="N",]
DUPES<-merge(N,dups,by=c('SAMPLE_DATE','LAKE_ID'),all=FALSE)


#rename column headers to match EDD
colnames(data)[colnames(data)=="Characteristic.Name"] <- "chemical_name"
colnames(data)[colnames(data)=="Result.Value"] <- "result_value"
colnames(data)[colnames(data)=="Result.Unit"] <- "result_unit"
colnames(data)[colnames(data)=="SAMPLE_DATE"] <- "sample_date"
#^sample data from EDD has time in the same cell, is that a problem?
colnames(data)[colnames(data)=="METHOD_DETECTION_LIMIT"] <- "method_detection_limit"
colnames(data)[colnames(data)=="Result.Detection.Quantitation.Limit.Unit"] <- "detection_limit_unit"
colnames(data)[colnames(data)=="LAB_QUALIFIERS"] <- "lab_qualifiers"
colnames(data)[colnames(data)=="SAMPLE_SOURCE"] <- "sample_source"
colnames(data)[colnames(data)=="SAMPLE_NAME"] <- "sys_sample_code"
colnames(data)[colnames(data)=="QUANTITATION_LIMIT"] <- "quantitation_limit"
colnames(data)[colnames(data)=="Analysis.Start.Date"] <- "analysis_date"
colnames(data)[colnames(data)=="Fraction"] <- "fraction"
colnames(data)[colnames(data)=="Lab_Sample_Name"] <- "lab_sdg"


#pull only column headers that are needed
data<-unique(data[c('sys_sample_code','sample_date','chemical_name',
                    'result_value','result_unit','analysis_date','fraction','method_detection_limit','detection_limit_unit',
                    'quantitation_limit','lab_sdg','lab_qualifiers','sample_source','DEC_sample_type',
                    'sample_date')])
data<-data[!is.na(data$sample_date),]


#subset the "data" file into known EBs, known FBs, and other
#EB<-data[data$DEC_sample_type=="EB",]
#FD<-data[data$DEC_sample_type=="FD",]
#Other<-data[data$DEC_sample_type=="NA",]

#read reporting limits file
RL<-read.csv("reporting.limits.csv")

#pull EB data that exceed the reporting limits in RL
EB<-merge(EB,RL,by=c('chemical_name'),all=TRUE)
EB$diff<-ifelse(EB$result_value>EB$reporting_limit,"fail","pass")

#remove NA sample rows in EB table
EB<-EB[!is.na(EB$SAMPLE_ID),]



#run the rmarkdown script for this list
library(rmarkdown)
render("LCIqaqccomparisons.Rmd", params = inputs)


