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
library(furrr)

###### User-defined variables ######

# Used for naming report file and adding Project_name field to Streams data. 
project.dir <- "sections/data/projectData/Streams/"

input.dir <- "2021/all_subset_assmnts/"
input.data <- "2021_chem_preqaqc_JOIN-all_subset_assmnts_v2_2021-10-26.csv"
proj.list.file <- "2021_smas_qc_batching_assmnts_2021-10-26.csv"
proj.year <- "2021"


####################################

# Load input data and loop through by project 

proj.list <- read_csv(paste0(project.dir, input.dir, proj.list.file))  
  # select("Folder#", "project_QAQC") 
  # dplyr::rename("sample_delivery_group" = "Folder#")
  # filter(project_QAQC != "Lake_Biomonitoring") %>% 
  # mutate(project_QAQC = str_replace_all(project_QAQC, "/", "-")) %>%
  # mutate(project_QAQC = toupper(project_QAQC))  
  # filter(project_QAQC %in% c("FINGER_LAKES_ADV_MON", "SCREENING_DELAWARE"))

data.proj <- read.csv(paste0(project.dir, input.dir, input.data), colClasses = c(fraction="character"), stringsAsFactors = FALSE) %>% 
  left_join(proj.list, by = "SDG_team") %>% 
  select(project_QAQC, everything()) %>% 
  filter(!is.na(project_QAQC)) 
  # filter(!project_QAQC %in% "MOHK_TSS")
  # mutate(chemical_name = ifelse(chemical_name %in% "magnesium", "Magnesium", chemical_name))

data.proj.tss <- data.proj %>% 
  filter(project_QAQC %in% "MOHK_TSS",
         sample_type_code %in% "MS")
  

# Determine which projects are missing certain QC sample types so batches can be regrouped
data.proj.dup <- data.proj %>% filter(DEC_sample_type %in% "DUP")
proj.list.dupmissing <- proj.list %>% 
  filter(!project_QAQC %in% data.proj.dup$project_QAQC) %>% 
  distinct(project_QAQC)

data.proj.eb <- data.proj %>% filter(DEC_sample_type %in% "EB")
proj.list.ebmissing <- proj.list %>% 
  filter(!project_QAQC %in% data.proj.eb$project_QAQC) %>% 
  distinct(project_QAQC)

# Identify how many parameters received MS/MSDs per project 
#   (Ones with as many MS params as overall params have their own full MS. Ones with only a few are just the random lab MS tests run)
proj.list.mscount <- data.proj %>%
  filter(sample_type_code %in% "MS") %>% 
  dplyr::group_by(project_QAQC) %>% 
  dplyr::mutate(ms_param_count = sum(length(unique(chemical_name)))) %>% 
  dplyr::ungroup() %>% 
  distinct(project_QAQC, ms_param_count)


# Look for SDGs with no project name match
data.proj.na <- data.proj %>%
  filter(is.na(project_QAQC)) %>% 
  distinct(project_QAQC, sample_delivery_group, SDG_team, SITE_ID)

# look for missing sdgs
missing.sdgs <- anti_join(proj.list, data.proj, by = "SDG_team")
extra.sdgs <- anti_join(data.proj, proj.list, by = "SDG_team")

proj.names <- unique(data.proj$project_QAQC)

# For running single project
name.i <- proj.names[13]
# name.i <- proj.names[17]
### Must also change in loop below at "future_map(proj.names[x]"

future::plan(transparent)
# furrr::future_map(proj.names, .progress = TRUE, function(name.i){
furrr::future_map(proj.names[1:28], .progress = TRUE, function(name.i){
    # lapply(proj.names[1:2], function(name.i){
    
  cat(name.i)
  
  data <- data.proj %>% 
    filter(project_QAQC %in% name.i)
  
  # Load list of lab errors extracted from the ALS PDF reports on the first page of "Narrative Documents". Copy both General Chemistry and Metals sections (not both always present).
  errors<-read.csv(paste0(project.dir, input.dir, "laberrors.csv"))
  
  # Trim to only necessary fields. Checks if SITE_ID and Project_name fields exist (streams data) and include if yes. These fieds are not used in QAQC process but are carried through to the final data output.
  # Added in SDG for streams data 2/25/2020
  if("SITE_ID" %in% colnames(data)){
    data<-unique(data[c('sys_sample_code','sample_delivery_group','lab_anl_method_name','chemical_name','cas_rn','fraction','lab_qualifiers','lab_sdg','sample_date',
                        'result_value','result_unit','qc_original_conc','qc_spike_added','qc_spike_measured',
                        'method_detection_limit','detection_limit_unit','quantitation_limit','sample_source','sample_type_code',
                        'DEC_sample_type','analysis_date','SITE_ID','SITE_ID_CORR_IND')]) 
  } else{
    data<-unique(data[c('sys_sample_code','lab_anl_method_name','chemical_name','cas_rn','fraction','lab_qualifiers','lab_sdg','sample_date',
                        'result_value','result_unit','qc_original_conc','qc_spike_added','qc_spike_measured',
                        'method_detection_limit','detection_limit_unit','quantitation_limit','sample_source','sample_type_code',
                        'DEC_sample_type','analysis_date')]) 
  }
  
  # Create project name field with current name.i
  data$project_name <- name.i
  
  # Change turbidity quanitation limit to 1.0 NTU, as per Jason Fagel, 3/12/20
  # ALS changed the QLs in the EDDs for turbidity, but found one still at 0.1 in 2021 data, so keeping this line.
  data <- data %>%
    mutate(quantitation_limit = ifelse(chemical_name == "TURBIDITY", 1.0, quantitation_limit))
  
  library(rmarkdown)
  
  #run the rmarkdown script for this list
  rmarkdown::render("QAQC.Rmd", output_file = paste0(project.dir, input.dir, proj.year, "_chem_QAQC_report-",name.i,"_",Sys.Date(),".html"))
  
  # Write the data output
  # For streams, manually copy to L:\DOW\SMAS\StreamDatabase\Chemistry\final_QAQCd_data\[year]\
  # Will automate this step after L drive is reorganized.
  write.csv(forprint,file=paste0(project.dir, input.dir, proj.year, "_chem_qaqc-",name.i,"_",Sys.Date(),".csv"),row.names = FALSE)
  
  # Copy report file to output directory and rename
  # file.copy("QAQC.html",paste0(project.dir, input.dir, proj.year, "_chem_QAQC_report-",name.i,"_",Sys.Date(),".html"), overwrite = TRUE)
  
})
