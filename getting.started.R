#getting started
#Alene Onion
#December 2018

#the purpose of this script is to prepare the raw data set for QAQC analysis

#first load the raw data
wallkill<-read.csv("sections/data/Wallkill_2018_chem.csv")

#associate it with the reporting limts file
rlimits<-read.csv("sections/data/reporting.limits.csv")
wallkill<-merge(wallkill,rlimits,by=c('chemical_name'),all=FALSE)
rm(rlimits)

#pulling spike dataset
spike<-wallkill[wallkill$sample_type_code=="MS",]
spike<-unique(spike[c('sys_sample_code','lab_sdg','sample_date','chemical_name','short','fraction','result_value','result_unit','quantitation_limit','method_detection_limit','detection_limit_unit','lab_qualifiers','qc_original_conc','qc_spike_added','qc_spike_measured','qc_spike_status','accuracy')])
spike<-spike[!is.na(spike$chemical_name),]

#for all other data sets I need to first restrict the raw data to only that listed as "field"
wallkill<-wallkill[wallkill$sample_source=="Field",]
wallkill<-wallkill[!is.na(wallkill$chemical_name),]

#pulling equipment blank data set
EB<-wallkill[wallkill$DEC_sample_type=="EB",]
EB<-unique(EB[c('sys_sample_code','lab_sdg','sample_date','chemical_name','short','fraction','result_value','result_unit','quantitation_limit','method_detection_limit','detection_limit_unit','lab_qualifiers','reporting_limit','reporting_units')])
EB<-EB[!is.na(EB$chemical_name),]

#pulling duplicate data set
dup<-wallkill[wallkill$DEC_sample_type=="DUP"|wallkill$DEC_sample_type=="N_DUPPARENT",]
dup<-unique(dup[c('DEC_sample_type','sys_sample_code','lab_sdg','sample_date','chemical_name','short','fraction','result_value','result_unit','quantitation_limit','method_detection_limit','detection_limit_unit','lab_qualifiers')])
#separate the parent and duplicate data sets
dupparent<-dup[dup$DEC_sample_type=="N_DUPPARENT",]
dup<-dup[dup$DEC_sample_type=="DUP",]
#remove -DUP from sys sample code for merge
dup$sys_sample_code<-gsub("-DUP","",dup$sys_sample_code)
dup$sys_sample_code<-gsub(" DUP","",dup$sys_sample_code)
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
wallkill<-wallkill[wallkill$DEC_sample_type=="N"|wallkill$DEC_sample_type=="N_DUPPARENT",]

#read field data
field<-read.csv("sections/data/Short_wallkill_field_data_2017_2018.csv")
#I'm remvoing the field data because several sites are missing so I can't use this to associate QC samples 
rm(field)


#run the rmarkdown script for this list
rmarkdown::render("WallkillQAQC.Rmd", params = inputs)

