library(tidyverse)
library(plyr)
library(lubridate)

name.i <- "CoE HABs"
errors <- data.frame()
# Establish the file path directory.
data_dir <- file.path(
  "C:",
  "Users",
  "zmsmith.000",
  "New York State Office of Information Technology Services",
  "BWAM - ALS",
  "coe_habs_technologies"
)
# Vector of ALS Zip file names.
als_files <- list.files(data_dir,
                        full.names = TRUE,
                        pattern = ".zip$")

# Read in ALS data
als_list <- lapply(als_files, ALS::as_als)
# Append the ALS objects together.
als <- ALS::append_als_objs(als_list)
# Drop intermediate objects.
rm("als_files", "als_list")
merge_df <- merge(als$sample,
              als$result,
              by = "sys_sample_code",
              all = TRUE)

names(merge_df)[names(merge_df) %in% "nysdec_sample_type"] <- "DEC_sample_type"


keep_vec <- c(
    'sys_sample_code',
    'lab_anl_method_name',
    'chemical_name',
    'cas_rn',
    'fraction',
    'lab_qualifiers',
    'lab_sdg',
    'sample_date',
    'result_value',
    'result_unit',
    'qc_original_conc',
    'qc_spike_added',
    'qc_spike_measured',
    'method_detection_limit',
    'detection_limit_unit',
    'quantitation_limit',
    'sample_source',
    'sample_type_code',
    'DEC_sample_type',
    'analysis_date'
  )

data <- merge_df[keep_vec]
data$DEC_sample_type <- toupper(data$DEC_sample_type)
data$DEC_sample_type[is.na(data$DEC_sample_type)] <- "N"
data$sample_date <- format(data$sample_date, "%m-%d-%Y %H:%M:%S")
data$analysis_date <- format(data$analysis_date, "%m-%d-%Y %H:%M:%S")

dups <- data %>% 
  filter(grepl("dup", sys_sample_code)) %>% 
  pull(sys_sample_code) %>% 
  gsub("dup", "", .)

data$DEC_sample_type <- ifelse(data$sys_sample_code %in% dups,
                               "N_DUPPARENT",
                               data$DEC_sample_type)


#run the rmarkdown script for this list
rmarkdown::render("QAQC.Rmd")

# Write the data output
# For streams, manually copy to L:\DOW\SMAS\StreamDatabase\Chemistry\final_QAQCd_data\[year]\
# Will automate this step after L drive is reorganized.
write.csv(forprint,
          file = file.path(
            "C:",
            "Users",
            "zmsmith.000",
            "Downloads",
            paste0(Sys.Date(),"_coe-habs_qaqc-data.csv")
          ),
          row.names = FALSE)

# Copy report file to output directory and rename
file.copy(
  "QAQC.html",
  file.path(
    "C:",
    "Users",
    "zmsmith.000",
    "Downloads",
    paste0(Sys.Date(),
           "_coe-habs_qaqc-report",
    
    ".html"
  )),
  overwrite = TRUE
)
