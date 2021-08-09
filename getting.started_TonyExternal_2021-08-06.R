#getting started
#Alene Onion
#December 2018


#The purpose of this script is to QAQC data files #This script requires:
#data.csv: raw data files with the added DEC_sample_type column which identifies the same as a normal sample, blank, matrix spike, duplicate, or duplicate parent
#reporting.limits.csv: reporting limits file that includes accuracy limits, paired parameters that components should be samler than, and an abbreviated name column
#Ohio.Qualifiers.csv: A table of Ohio data qualifiers
#Holding.Times_v2.csv: a lookup table for holding times
#ALSflags.png: to see the ALS flags
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
project.dir <- "sections/data/projectData/Streams/"
input.dir <- "2021/tony_external/"
input.data <- "EXT_FLX_CHEM_OWAS.csv"
### ^^^ FILTER BY SDG BELOW in "data" DF if needed ^^^ ###
project.name <- "Owasco_2021_ext"
name.i <- project.name
output.dir <- "2021/tony_external/"
output.filename <- paste0("Owasco_2021_ext_QAQCd",Sys.Date(),".csv")

# Load input data and filter if needed
  # Must classify "fraction" column as character because if only T (total) is present, read.csv will classify as logical and convert all to "TRUE".
data <- read.csv(paste0(project.dir, input.dir, input.data), colClasses = c(fraction="character"), stringsAsFactors = FALSE) 
  # filter(sample_delivery_group %in% c(
  #   "R1905772",
  #   "R1906451",
  #   "R1907499",
  #   "R1909548"
  # ))


### Mods to Tony's input files:

# Remove empty rows
data <- data[!apply(is.na(data) | data == "", 1, all),]

# Convert sample_source to "Field" and convert dates as needed
data <- data %>% 
  mutate(sample_source = "Field",
         sample_date = as.Date(sample_date, format = "%m/%d/%y"),
         sample_date = format(sample_date, "%m/%d/%Y"),
         analysis_date = as.POSIXct(analysis_date, format = "%m/%d/%y"),
         chemical_name = case_when(
           chemical_name == "CARBON, DISSOLVED ORGANIC (DOC)" ~ "Carbon, Dissolved Organic (DOC)",
           chemical_name == "NITROGEN, AMMONIA (AS N)" ~ "Nitrogen, ammonia (As N)",
           chemical_name == "NITROGEN, NITRATE-NITRITE" ~ "Nitrate+Nitrite as Nitrogen",
           chemical_name == "NITROGEN, TOTAL" ~ "Nitrogen",
           chemical_name == "PHOSPHORUS, DISSOLVED" ~ "PHOSPHORUS, DISSOLVED (AS P)",
           chemical_name == "PHOSPHORUS, DISSOLVED ORTHOPHOSPHATE (AS P)" ~ "Orthophosphate as Phosphorus, Dissolved",
           chemical_name == "PHOSPHORUS, TOTAL (AS P)" ~ "PHOSPHORUS, TOTAL (AS P)",
           chemical_name == "TOTAL SUSPENDED SOLIDS" ~ "Total Suspended Solids",
           TRUE ~ chemical_name) 
  )





####################################

# (For streams data) Add project name field for carrying through to final output
if("SITE_ID" %in% colnames(data)){
  data$project_name <- project.name
}

# Load list of lab errors extracted from the ALS PDF reports on the first page of "Narrative Documents". Copy both General Chemistry and Metals sections (not both always present).
errors<-read.csv(paste0(project.dir, input.dir, "laberrors.csv"))

# Trim to only necessary fields. Checks if SITE_ID and Project_name fields exist (streams data) and include if yes. These fieds are not used in QAQC process but are carried through to the final data output.
  # Added in SDG for streams data 2/25/2020
if("SITE_ID" %in% colnames(data) & "project_name" %in% colnames(data)){
  data<-unique(data[c('sys_sample_code','sample_delivery_group','lab_anl_method_name','chemical_name','cas_rn','fraction','lab_qualifiers','lab_sdg','sample_date',
                      'result_value','result_unit','qc_original_conc','qc_spike_added','qc_spike_measured',
                      'method_detection_limit','detection_limit_unit','quantitation_limit','sample_source','sample_type_code',
                      'DEC_sample_type','analysis_date','SITE_ID', 'SITE_ID_CORR_IND', 'project_name')]) 
} else{
  data<-unique(data[c('sys_sample_code','lab_anl_method_name','chemical_name','cas_rn','fraction','lab_qualifiers','lab_sdg','sample_date',
                      'result_value','result_unit','qc_original_conc','qc_spike_added','qc_spike_measured',
                      'method_detection_limit','detection_limit_unit','quantitation_limit','sample_source','sample_type_code',
                      'DEC_sample_type','analysis_date')]) 
}

# Change turbidity quanitation limit to 1.0 NTU, as per Jason Fagel, 3/12/20
# Commented out 4/9/21. Already done on ALS end.
# data <- data %>% 
#   mutate(quantitation_limit = ifelse(chemical_name == "TURBIDITY", 1.0, quantitation_limit))

#run the rmarkdown script for this list
rmarkdown::render("QAQC.Rmd")

# Write the data output
# For streams, manually copy to L:\DOW\SMAS\StreamDatabase\Chemistry\final_QAQCd_data\[year]\
  # Will automate this step after L drive is reorganized.
write.csv(forprint,file=paste0(project.dir, output.dir, output.filename),row.names = FALSE)

# Copy report file to output directory and rename
file.copy("QAQC.html",paste0(project.dir, output.dir, project.name,"_QAQC-report_",Sys.Date(),".html"), overwrite = TRUE)
