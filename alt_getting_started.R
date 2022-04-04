library(tidyverse)
library(plyr)
library(lubridate)
library(ALS)

als_example <- as_als(file.path("sections", "data", "R2004299.zip"))
join_df <- full_join(als_example$sample,
                  als_example$result,
                  by = "sys_sample_code")

keep_vec <-
  c(
    'sys_sample_code',
    'sample_delivery_group',
    'lab_anl_method_name',
    "chemical_name",
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
    # 'DEC_sample_type',
    'analysis_date'#,
    # 'SITE_ID',
    # 'Project_name'
  )

data <- join_df[keep_vec]

#run the rmarkdown script for this list
rmarkdown::render("QAQC.Rmd")
