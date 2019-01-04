#getting started
#Alene Onion
#December 2018

#The purpose of this script is to QAQC data files #This script requires:
#data.csv: raw data files with the added DEC_sample_type column which identifies the same as a normal sample, blank, matrix spike, duplicate, or duplicate parent
#reporting.limits.csv: reporting limits file that includes accuracy limits, paired parameters that components should be samler than, and an abbreviated name column
#Ohio.Qualifiers.csv: A table of Ohio data qualifiers
#Holding.Times.csv: a lookup table for holding times
#ALSflags.png: to see the ALs flags
#laberrors.csv: a list of all the errors noted in the written lab reports
#validator_flags: a list of all the possible flags applied to the validator_flag column

#This script associates samples with the nearest qc samples by date
#It is possible to run this script for a large or small data set 
#but I recommend dividing your data into regions (or samplers if you want to be very fine) to refine your application of QC data
#as long as the subset has a complete QC data set (spikes, duplicates, and equipment blanks) it can be run separately

#The output is a flagged data set and a quality report summarizing the flagging process

#first load the data file
data<-read.csv("sections/data/projectData/wallkill2018/Wallkill_2018_chem.csv")
errors<-read.csv("sections/data/projectData/wallkill2018/laberrors.csv")
#truncate this file to only the necessary fields
#this shortened file is saved as Wallkill.short.csv
data<-unique(data[c('sys_sample_code','chemical_name','cas_rn','fraction','lab_qualifiers','lab_sdg','sample_date',
                    'result_value','result_unit','qc_original_conc','qc_spike_added','qc_spike_measured','qc_spike_status',
                    'method_detection_limit','detection_limit_unit','quantitation_limit','sample_source','sample_type_code',
                    'DEC_sample_type','analysis_date')])

#first a QC check to make sure this script accoutns for all the possible lab error codes:
#run the rmarkdown script for this list
rmarkdown::render("QAQC.Rmd", params = inputs)

#write the data output
write.csv(forprint,file="sections/data/projectData/wallkill2018/output/dataqqaqcd.csv",row.names = FALSE)
rm(forprint)

