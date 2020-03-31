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

library(tidyverse)
library(plyr)
library(lubridate)

###### User-defined variables ######

# Used for naming report file and adding Project_name field to Streams data. 
#   Include project name type. (e.g, "Susquehanna RIBS Screening" or "Ramapo RAS")
project.name <- "SBU-Mohawk chem 2019"
project.dir <- "sections/data/projectData/Streams/ITS_tables_2020-03-27/"
input.data <- "2019_chem_preqaqc_ALL-SBU_Mohawk_complete_2020-03-27.csv"
output.filename <- paste0("2019_chem_qaqc_ALL-SBU_Mohawk_complete_",Sys.Date(),".csv")

# project.name <- "Finger Lakes Tribs 2019"
# project.dir <- "sections/data/projectData/Streams/2019/fingerlakes/"
# input.data <- "2019_FingerLakes_chem_preqaqc_2020-01-03.csv"
# output.filename <- "2019_fingerlakes_chem_qaqcd-2020-02-04_TEST_2.csv"

####################################This was the only dataset we’ve encountered so far with only totals in the fraction column (and no dissolved), so it is the only one affected by the logical class issue. I added a fix for this before rerunning. 

# Load input data
# Must classify "fraction" column as character because if only T (total) is present, read.csv will classify as logical and convert all to "TRUE".
data<-read.csv(paste0(project.dir,input.data), colClasses = c(fraction="character"), stringsAsFactors = FALSE)

# (For streams data) Add project name field for carrying through to final output
if("SITE_ID" %in% colnames(data)){
  data$Project_name <- project.name
}

# Load list of lab errors extracted from the ALS PDF reports on the first page of "Narrative Documents". Copy both General Chemistry and Metals sections (not both always present).
errors<-read.csv(paste0(project.dir,"laberrors.csv"))

# Trim to only necessary fields. Checks if SITE_ID and Project_name fields exist (streams data) and include if yes. These fieds are not used in QAQC process but are carried through to the final data output.
  # Added in SDG for streams data 2/25/2020
if("SITE_ID" %in% colnames(data) & "Project_name" %in% colnames(data)){
  data<-unique(data[c('sys_sample_code','sample_delivery_group','lab_anl_method_name','chemical_name','cas_rn','fraction','lab_qualifiers','lab_sdg','sample_date',
                      'result_value','result_unit','qc_original_conc','qc_spike_added','qc_spike_measured',
                      'method_detection_limit','detection_limit_unit','quantitation_limit','sample_source','sample_type_code',
                      'DEC_sample_type','analysis_date','SITE_ID','Project_name')]) 
} else{
  data<-unique(data[c('sys_sample_code','lab_anl_method_name','chemical_name','cas_rn','fraction','lab_qualifiers','lab_sdg','sample_date',
                      'result_value','result_unit','qc_original_conc','qc_spike_added','qc_spike_measured',
                      'method_detection_limit','detection_limit_unit','quantitation_limit','sample_source','sample_type_code',
                      'DEC_sample_type','analysis_date')]) 
}

# Change turbidity quanitation limit to 1.0 NTU, as per Jason Fagel, 3/12/20
data <- data %>% 
  mutate(quantitation_limit = ifelse(chemical_name == "TURBIDITY", 1.0, quantitation_limit))

#run the rmarkdown script for this list
rmarkdown::render("QAQC.Rmd")

# Write the data output
# For streams, manually copy to L:\DOW\SMAS\StreamDatabase\Chemistry\final_QAQCd_data\[year]\
  # Will automate this step after L drive is reorganized.
write.csv(forprint,file=paste0(project.dir,output.filename),row.names = FALSE)

# Copy report file to output directory and rename
file.copy("QAQC.html",paste0(project.dir,project.name,"_QAQC-report_",Sys.Date(),".html"), overwrite = TRUE)
