
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
# Join the sample and result tables together by sys_sample_code.
merge_df <- merge(als$sample,
                  als$result,
                  by = "sys_sample_code",
                  all = TRUE)
merge_df <- merge_df %>%
  mutate(
    nysdec_sample_type = sample_type_desc,
    nysdec_sample_type = case_when(
      sample_type_code %in% "N" &
        endsWith(sys_sample_code, "eb") ~ "equipment_blank",
      sample_type_code %in% "N" &
        endsWith(sys_sample_code, "dup") ~ "field_duplicate",
      TRUE ~ nysdec_sample_type
    )
  )

# Identify columns to retain from merge_df.
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
  'nysdec_sample_type',
  'analysis_date'
)

# Retain only the specified columns.
als_df <- unique(merge_df[keep_vec])
# Replace all NA dec sample types with N (standard sample).
als_df$nysdec_sample_type[is.na(als_df$nysdec_sample_type)] <- "standard_sample"
# Reformat the date fields.
als_df$sample_date <- format(als_df$sample_date, "%m-%d-%Y %H:%M:%S")
als_df$analysis_date <- format(als_df$analysis_date, "%m-%d-%Y %H:%M:%S")
# Convert all non-detects to 0 in the result field.
als_df$result_value <- ifelse(als_df$lab_qualifiers %in% "U",
                              0,
                              als_df$result_value)
als_df$sys_sample_code <- gsub("efff", "eff", als_df$sys_sample_code)
als_df <- als_df %>% 
  mutate(
    sample_date = as.POSIXct(sample_date, format = "%m-%d-%Y %H:%M:%S"),
    site = case_when(
      grepl("esf", sys_sample_code) ~ "esf",
      grepl("clk", sys_sample_code) ~ "clarkson",
      grepl("con", sys_sample_code) ~ "control",
      TRUE ~ "lab"
    ),
    location = case_when(
      grepl("eff", sys_sample_code) ~ "effluent",
      grepl("inf", sys_sample_code) ~ "influent",
      grepl("amb", sys_sample_code) ~ "ambient",
      TRUE ~ "lab"
    ),
    lab_flag = case_when(
      lab_qualifiers %in% c("B", "N", "*", "D", "W") ~ "reject",
      lab_qualifiers %in% c("E", "J") ~ "estimated",
      lab_qualifiers %in% c("U") ~ "undetected", # "nondetect"
      is.na(lab_qualifiers) ~ "no_flag",
      TRUE ~ lab_qualifiers
    ),
    accuracy = if_else(
      nysdec_sample_type %in% "matrix_spike",
      lab_flag,
      NA_character_
    ),
    accuracy = case_when(
      accuracy %in% "no_flag" ~ "pass",
      accuracy %in% "reject" ~ "fail",
      is.na(accuracy) ~ NA_character_,
      TRUE ~ "ERROR"
    ),
    # Identify the parent sample.
    parent = get_parent(.vec = sys_sample_code),
    chem_frac = paste(fraction,
                       chemical_name,
                       sep = "_")
  ) %>% 
  relocate(sys_sample_code,
           site,
           location,
           sample_date,
           parent,
           lab_flag,
           accuracy)




als_split <- split(als_df, als_df$nysdec_sample_type)

# dups <- als_df %>% 
#   filter(grepl("dup", sys_sample_code)) %>% 
#   pull(sys_sample_code) %>% 
#   gsub("dup", "", .)
# 
# als_df$DEC_sample_type <- ifelse(als_df$sys_sample_code %in% dups,
#                                "N_DUPPARENT",
#                                als_df$DEC_sample_type)

