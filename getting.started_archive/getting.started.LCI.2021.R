#getting started
#Alene Onion
#December 2018


#The purpose of this script is to QAQC data files #This script requires:
#data.csv: raw data files with the added DEC_sample_type column which identifies the same as a normal sample, blank, matrix spike, duplicate, or duplicate parent
#reporting.limits.csv: reporting limits file that includes accuracy limits, paired parameters that components should be samler than, and an abbreviated name column
#Ohio.Qualifiers.csv: A table of Ohio data qualifiers
#Holding.Times_v2.csv: a lookup table for holding times
#ALSflags.png: to see the ALs flags
#laberrors.csv: a list of all the errors noted in the written lab reports
#validator_flags: a list of all the possible flags applied to the validator_flag column

#This script associates samples with the nearest qc samples by date
#It is possible to run this script for a large or small data set 
#but I recommend dividing your data into regions (or samplers if you want to be very fine) to refine your application of QC data
#as long as the subset has a complete QC data set (spikes, duplicates, and equipment blanks) it can be run separately

#The output is a flagged data set and a quality report summarizing the flagging process
setwd("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/QAQC")
library(tidyverse)
library(plyr)
library(lubridate)

###### User-defined variables ######

rm(list=ls())

# Used for naming report file and adding Project_name field to Streams data. 
#   Include project name type. (e.g, "Susquehanna RIBS Screening" or "Ramapo RAS")
project.name <- "LCI.2021.HYP"
project.dir <- "C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/QAQC/sections/data/projectData/LCI.2021/"
input.data <- "LCI.2021.raw.with.DEC.INFO.TYPE.HYP.csv"
output.filename <- "LCI.2021.HYP_qaqcd.csv"
name.i<-project.name

####################################This was the only dataset we’ve encountered so far with only totals in the fraction column (and no dissolved), so it is the only one affected by the logical class issue. I added a fix for this before rerunning. 

# Load input data
# Must classify "fraction" column as character because if only T (total) is present, read.csv will classify as logical and convert all to "TRUE".
data<-read.csv(paste0(project.dir,input.data), colClasses = c(fraction="character"))
# data$sample_date <- as.Date(data$sample_date, "%m-%d-%Y %H:%M:%S")
# data$analysis_date <- as.Date(data$analysis_date, "%m-%d-%Y %H:%M:%S")


# Load list of lab errors extracted from the ALS PDF reports on the first page of "Narrative Documents". Copy both General Chemistry and Metals sections (not both always present).
errors<-read.csv(paste0(project.dir,"laberrors.csv"))

# Trim to only necessary fields. Checks if SiteID and Project_name fields exist (streams data) and include if yes. These fieds are not used in QAQC process but are carried through to the final data output.
  data<-unique(data[c('sys_sample_code','Info.Type','Site.ID','lab_anl_method_name','chemical_name','cas_rn','fraction','lab_qualifiers','lab_sdg','sample_date',
                      'result_value','result_unit','qc_original_conc','qc_spike_added','qc_spike_measured',
                      'method_detection_limit','detection_limit_unit','quantitation_limit','sample_source','sample_type_code',
                      'DEC_sample_type','analysis_date')]) 

#remove QC samples that were decided to be not valuable (see QC.ANalysis.xlsx file for explaination)
data<-data %>% 
  filter(#precision flags removed
         !(sys_sample_code=="1501SWA0984A_DH_OW_06/15/2021"&sample_type_code=="MS"),
         !(sys_sample_code=="1301VLY0262A_DH_OW_07/28/2021"&sample_type_code=="MS"),
         !(sys_sample_code=="1104RIC0152D_DH_BS_08/11/2021"&sample_type_code=="MS"),
         #these are removed to be added as individual flags afterwards
         !(sys_sample_code=="0702ONO0154_SDH_BS_08/25/2021"&sample_type_code=="MS"),
         #precision flags removed
         !(sys_sample_code=='1201UWB0940A_DH_OW_07/27/2021'&chemical_name=='Nitrate+Nitrite as Nitrogen'&DEC_sample_type=='DUP'),
         !(sys_sample_code=='1201UWB0940A_DH_OW_09/14/2021'&chemical_name=='Nitrogen, ammonia (As N)'&DEC_sample_type=='DUP'),
         !(sys_sample_code=='1201UWB0940A_DH_OW_09/14/2021'&chemical_name=='NITROGEN, KJELDAHL, TOTAL'&DEC_sample_type=='DUP'),
         !(sys_sample_code=='1301MEL0331_DH_OW_08/30/2021'&chemical_name=='Arsenic'&DEC_sample_type=='DUP'),
         !(sys_sample_code=='1301MEL0331_DH_OW_08/30/2021'&chemical_name=='CHLORIDE (AS CL)'&DEC_sample_type=='DUP'),
         !(sys_sample_code=='1004NIC0314_DH_BS_06/30/2021'&chemical_name=='PHOSPHORUS, DISSOLVED (AS P)'&DEC_sample_type=='DUP'),
         !(sys_sample_code=='1201UWB0940A_DH_BS_07/27/2021'&chemical_name=='Manganese'&DEC_sample_type=='DUP'),
         !(sys_sample_code=='1201UWB0940A_DH_BS_07/27/2021'&chemical_name=='Nitrate+Nitrite as Nitrogen'&DEC_sample_type=='DUP'),
         !(sys_sample_code=='1301MEL0331_DH_BS_08/30/2021'&chemical_name=='Arsenic'&DEC_sample_type=='DUP'),
         !(sys_sample_code=='1301MEL0331_DH_BS_08/30/2021'&chemical_name=='CHLORIDE (AS CL)'&DEC_sample_type=='DUP'),
         !(sys_sample_code=='1301MEL0331_DH_BS_08/30/2021'&chemical_name=='Manganese'&DEC_sample_type=='DUP'),
         !(sys_sample_code=='1301MEL0331_DH_BS_08/30/2021'&chemical_name=='Nitrogen, ammonia (As N)'&DEC_sample_type=='DUP'),
         #Equipment blank flags removed
         !(sys_sample_code=="1202COB0583_DH_BS_06/23/2021"&DEC_sample_type=="EB"&chemical_name=="Color, True"),
         !(sys_sample_code=="1202COB0583_DH_OW_06/23/2021"&DEC_sample_type=="EB"&chemical_name=="NITROGEN, KJELDAHL, TOTAL"),
         !(sys_sample_code=="1202COB0583_DH_BS_06/23/2021"&DEC_sample_type=="EB"&chemical_name=="NITROGEN, KJELDAHL, TOTAL"),
         !(sys_sample_code=="0801DAR0750_DH_OW_06/29/2021"&DEC_sample_type=="EB"&chemical_name=="Color, True"),
         !(sys_sample_code=="0801DAR0750_DH_BS_06/29/2021"&DEC_sample_type=="EB"&chemical_name=="Color, True"),
         !(sys_sample_code=="1301WIC0183A_DH_OW_07/21/2021"&DEC_sample_type=="EB"&chemical_name=="NITROGEN, KJELDAHL, TOTAL"),
         #the following phosphorus flag is removed to be added as individual flag afterwards
         !(sys_sample_code=="1202COB0583_DH_BS_07/28/2021"&DEC_sample_type=="EB"&chemical_name %in% c("Color, True","NITROGEN, KJELDAHL, TOTAL","PHOSPHORUS, DISSOLVED (AS P)")),
         !(sys_sample_code=="1202COB0583_DH_OW_07/28/2021"&DEC_sample_type=="EB"&chemical_name %in% c("Color, True","NITROGEN, KJELDAHL, TOTAL")),
         !(sys_sample_code=="1104DAV0127A_DH_OW_08/16/2021"&DEC_sample_type=="EB"&chemical_name=="NITROGEN, KJELDAHL, TOTAL"),
         !(sys_sample_code=="1301WIC0183A_DH_OW_08/30/2021"&DEC_sample_type=="EB"&chemical_name=="Chlorophyll A"),
         !(sys_sample_code=="1301WIC0183A_DH_BS_08/30/2021"&DEC_sample_type=="EB"&chemical_name=="NITROGEN, KJELDAHL, TOTAL"),
         #and all of the following are removed to be added as individual flags afterwards
         !(sys_sample_code=="0202BEA0131_DH_OW_08/03/2021"&DEC_sample_type=="EB"&chemical_name=="Color, True"),
         !(sys_sample_code=="0202BEA0131_DH_BS_08/03/2021"&DEC_sample_type=="EB"&chemical_name=="Color, True"),
         
         !(sys_sample_code=="0202BEA0131_DH_OW_08/03/2021"&DEC_sample_type=="EB"&chemical_name=="PHOSPHORUS, TOTAL (AS P)")  )
  
  
#run the rmarkdown script for this list
rmarkdown::render("QAQC.Rmd",
                  output_file=paste0(project.dir,project.name,"_QAQC-report_",Sys.Date(),".html"),
                  output_dir = project.dir)

#Modifications to ALS data based on field data results
forprint<-forprint %>%
  mutate(sample_date=as.character(sample_date),
         sample_date=ifelse(sys_sample_code=="1104ROC0637_DH_OW_08/11/2021","2021-08-10 12:06:00 UTC",sample_date),
         sys_sample_code=ifelse(sys_sample_code=="1104ROC0637_DH_OW_08/11/2021","1104ROC0637_DH_OW_08/10/2021",sys_sample_code),
         sample_date=ifelse(sys_sample_code=="1104ROC0637_DH_BS_08/11/2021","2021-08-10 12:06:00 UTC",sample_date),
         sys_sample_code=ifelse(sys_sample_code=="1104ROC0637_DH_BS_08/11/2021","1104ROC0637_DH_BS_08/10/2021",sys_sample_code)) %>% 
  mutate(sample_date=ifelse(sys_sample_code=="0704UWB5422_DH_OW_09/15/2021","2021-09-14 13:30:00 UTC",sample_date),
         sys_sample_code=ifelse(sys_sample_code=="0704UWB5422_DH_OW_09/15/2021","0704UWB5422_DH_OW_09/14/2021",sys_sample_code))
#these are data flags added because QC samples were removed above for QA analysis but Jason felt the individual value should be flagged
forprint<-forprint %>% 
  mutate(
    #accuracy flags
    validator_qualifiers=ifelse(sys_sample_code=="0702ONO0154_SDH_BS_08/25/2021"&chemical_name=="nitrate+nitrite as nitrogen","R",validator_qualifiers),
    #precision flags
    validator_qualifiers=ifelse(sys_sample_code=='1201UWB0940A_DH_OW_07/27/2021'&chemical_name=='nitrate+nitrite as nitrogen','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='1201UWB0940A_DH_OW_09/14/2021'&chemical_name=='nitrogen, ammonia (as n)','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='1201UWB0940A_DH_OW_09/14/2021'&chemical_name=='nitrogen, kjeldahl, total','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='1301MEL0331_DH_OW_08/30/2021'&chemical_name=='arsenic','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='1301MEL0331_DH_OW_08/30/2021'&chemical_name=='chloride (as cl)','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='1004NIC0314_DH_BS_06/30/2021'&chemical_name=='phosphorus, dissolved (as p)','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='1201UWB0940A_DH_BS_07/27/2021'&chemical_name=='manganese','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='1201UWB0940A_DH_BS_07/27/2021'&chemical_name=='nitrate+nitrite as nitrogen','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='1301MEL0331_DH_BS_08/30/2021'&chemical_name=='arsenic','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='1301MEL0331_DH_BS_08/30/2021'&chemical_name=='chloride (as cl)','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='1301MEL0331_DH_BS_08/30/2021'&chemical_name=='manganese','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='1301MEL0331_DH_BS_08/30/2021'&chemical_name=='nitrogen, ammonia (as n)','R',validator_qualifiers),
    #EB flags
    validator_qualifiers=ifelse(sys_sample_code=='0202BEA0131_DH_BS_08/03/2021'&chemical_name=='color, true','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='0202BEA0131_DH_OW_08/03/2021'&chemical_name=='color, true','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='0202BEA0131_DH_OW_08/03/2021'&chemical_name=='phosphorus, total (as p)','R',validator_qualifiers),
    validator_qualifiers=ifelse(sys_sample_code=='1202COB0583_DH_BS_07/28/2021'&chemical_name=='phosphorus, dissolved (as p)','R',validator_qualifiers),
    
    #and now repeat to fix interpreted qualifiers
    #accuracy flags
    interpreted_qualifiers=ifelse(sys_sample_code=="0702ONO0154_SDH_BS_08/25/2021"&chemical_name=="nitrate+nitrite as nitrogen",paste(interpreted_qualifiers,"accuracy error",sep=", "),interpreted_qualifiers),
    #precision flags
    interpreted_qualifiers=ifelse(sys_sample_code=='1201UWB0940A_DH_OW_07/27/2021'&chemical_name=='nitrate+nitrite as nitrogen',paste(interpreted_qualifiers,"precision error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='1201UWB0940A_DH_OW_09/14/2021'&chemical_name=='nitrogen, ammonia (as n)',paste(interpreted_qualifiers,"precision error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='1201UWB0940A_DH_OW_09/14/2021'&chemical_name=='nitrogen, kjeldahl, total',paste(interpreted_qualifiers,"precision error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='1301MEL0331_DH_OW_08/30/2021'&chemical_name=='arsenic',paste(interpreted_qualifiers,"precision error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='1301MEL0331_DH_OW_08/30/2021'&chemical_name=='chloride (as cl)',paste(interpreted_qualifiers,"precision error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='1004NIC0314_DH_BS_06/30/2021'&chemical_name=='phosphorus, dissolved (as p)',paste(interpreted_qualifiers,"precision error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='1201UWB0940A_DH_BS_07/27/2021'&chemical_name=='manganese',paste(interpreted_qualifiers,"precision error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='1201UWB0940A_DH_BS_07/27/2021'&chemical_name=='nitrate+nitrite as nitrogen',paste(interpreted_qualifiers,"precision error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='1301MEL0331_DH_BS_08/30/2021'&chemical_name=='arsenic',paste(interpreted_qualifiers,"precision error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='1301MEL0331_DH_BS_08/30/2021'&chemical_name=='chloride (as cl)',paste(interpreted_qualifiers,"precision error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='1301MEL0331_DH_BS_08/30/2021'&chemical_name=='manganese',paste(interpreted_qualifiers,"precision error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='1301MEL0331_DH_BS_08/30/2021'&chemical_name=='nitrogen, ammonia (as n)',paste(interpreted_qualifiers,"precision error",sep=", "),interpreted_qualifiers),
    #EB flags
    interpreted_qualifiers=ifelse(sys_sample_code=='0202BEA0131_DH_BS_08/03/2021'&chemical_name=='color, true',paste(interpreted_qualifiers,"equipment blank error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='0202BEA0131_DH_OW_08/03/2021'&chemical_name=='color, true',paste(interpreted_qualifiers,"equipment blank error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='0202BEA0131_DH_OW_08/03/2021'&chemical_name=='phosphorus, total (as p)',paste(interpreted_qualifiers,"equipment blank error",sep=", "),interpreted_qualifiers),
    interpreted_qualifiers=ifelse(sys_sample_code=='1202COB0583_DH_BS_07/28/2021'&chemical_name=='phosphorus, dissolved (as p)',paste(interpreted_qualifiers,"equipment blank error",sep=", "),interpreted_qualifiers))

# Write the data output
# For streams, manually copy to L:/DOW\SMAS\StreamDatabase\Chemistry\final_QAQCd_data\[year]\
  # Will automate this step after L drive is reorganized.
write.csv(forprint,file=paste0(project.dir,output.filename),row.names = FALSE)

#merge the two files into one comprehensive project
epi<-read.csv("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/QAQC/sections/data/projectData/LCI.2021/LCI.2021.EPI_qaqcd.csv")
hyp<-read.csv("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/QAQC/sections/data/projectData/LCI.2021/LCI.2021.HYP_qaqcd.csv")
new<-merge(epi,hyp,all=TRUE)
write.csv(new,file=paste(project.dir,"LCI.2021.comprehensive.qaqcd.data.csv"))
write.csv(new,file="C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data/2021/LCI.2021.comprehensive.qaqcd.data.csv",row.names=FALSE)
#check that we aren't missing files
field1<-read.csv("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/QAQC/sections/data/projectData/LCI.2021/alene_keys.csv")
field<-field1 %>% mutate(LOCATION=sub("_2021.*","",EVENT_LMAS_ID),
                        DATETIME=sub(".*_2021-","",EVENT_LMAS_ID),
                        DATETIME=sub("_.*","",DATETIME),
                        DATETIME=sub("-","/",DATETIME),
                        DATETIME=paste(DATETIME,"/2021",sep=""))
fieldepi<-field %>% mutate(sys_sample_code=paste(LOCATION,"OW",DATETIME,sep="_"),key=KEY_OW) %>% select(sys_sample_code,key) %>% distinct() 
fieldhypo<-field %>% mutate(sys_sample_code=paste(LOCATION,"BS",DATETIME,sep="_"),key=KEY_BS) %>% select(sys_sample_code,key) %>% distinct() 
field<-merge(fieldhypo,fieldepi,all=TRUE)
field<-field %>% filter(!is.na(key)) %>% mutate(sys_sample_code=trimws(sys_sample_code))
field<-merge(field,new,by=c("sys_sample_code"),all=TRUE)
missing<-field %>% filter(is.na(lab_sdg)) %>% select(sys_sample_code,key) %>% distinct() %>% arrange(sys_sample_code) %>% 
  mutate(date_sampled=sub(".*_","",sys_sample_code)) %>% 
  #remove these because water samples weren't collected
  filter(!(key %in% c('21L0460','21L0465','21L0471','21L0472','21L0474','21L0475','21L0476','21L0477','21L0478','21L0479','21L0496','21L0312','21L0339',
                      '21L0313','21L0326','21L0511'))) %>% 
  #lost coolers
  filter(!(key %in% c('21L0218','21L0083','21L0336','21L0368','21L0412'))) %>% 
  #didn't collect
  filter(!(key=="21L0218"))
notinfield<-field %>% filter(is.na(key)) %>% select(sys_sample_code) %>% distinct() %>% arrange(sys_sample_code)
write.csv(missing,file=paste(project.dir,"LCI.2021.missing.ALS.data.csv"),row.names=FALSE)
write.csv(notinfield,file=paste(project.dir,"LCI.2021.missing.field.data.csv"),row.names=FALSE)
#create file for Sabrina to confirm in the COCs
sabrina<-field %>% filter(!is.na(lab_sdg)) %>% select(sys_sample_code,key,lab_sdg) %>% distinct() %>% mutate(date_sampled=sub(".*_","",sys_sample_code)) %>% arrange(date_sampled)
write.csv(sabrina,file=paste(project.dir,"sabrina.checks.of.cocs.csv",sep=""),row.names=FALSE)
rm(list=ls())
