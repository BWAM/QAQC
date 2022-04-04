# test_split <- split(als_bind, als_bind$chem_frac)
# .x <- test_split$`T_PHOSPHORUS, TOTAL (AS P)`
# .y <- als_split$equipment_blank
# .flag_col <- "equipment_blank"
merge_nearest_date <- function(.x, .y, .flag_col) {
  day_limit <- 5
  y_df <- .y[.y$chem_frac %in% unique(.x$chem_frac), ]
  
  if (nrow(y_df) == 0) {
    .x[.flag_col] <- "Test not available for this parameter."
    return(.x)
  }
  .x$join_date <- vapply(.x$sample_date, function(d) {
    # print(d)
    y_df$sample_date[which.min(abs(y_df$sample_date - d))]
  },
  FUN.VALUE = NA_real_)
  .x$join_date <- as.POSIXct(.x$join_date, origin = "1970-01-01")
  .x$day_diff <- round(abs(difftime(.x$sample_date,
                                    .x$join_date,
                                    units = "days")), 1)
  # .x$join_date <- ifelse(
  #   test = .x$day_diff > day_limit,
  #   yes = NA_real_,
  #   no = .x$join_date)
  # .x$join_date <- as.POSIXct(.x$join_date, origin = "1970-01-01")

  x_sub <- .x[!names(.x) %in% .flag_col]
  
  if (.flag_col == "precision") {
    y_df$result_value <- paste0(y_df$rpd, "%")
  }
  y_sub <- y_df[c("sys_sample_code", "chemical_name", 
                  "fraction", "sample_date", .flag_col,
                  "result_value")]
  names(y_sub)[names(y_sub) %in% "sys_sample_code"] <- paste(
    .flag_col,
    "ssc",
    sep = "_"
  ) 
  
  names(y_sub)[names(y_sub) %in% "result_value"] <- paste(
    .flag_col,
    "result_value",
    sep = "_"
  ) 
  merge_df <- merge(x = x_sub,
                    y = y_sub,
                    by.x = c("chemical_name",
                             "fraction",
                             "join_date"),
                    by.y = c("chemical_name",
                             "fraction",
                             "sample_date"),
                    all.x = TRUE)
  if (.flag_col %in% "equipment_blank") {
    merge_df$equipment_blank <- ifelse(
      test = merge_df$day_diff > day_limit,
      yes = paste("Nearest QC sample collected plus or minus",
                  merge_df$day_diff,
                  "days (>",
                  day_limit,
                  "day limit)."),
      no = merge_df$equipment_blank)
  }

  
  merge_df <- merge_df[!names(merge_df) %in% "day_diff"]
  # merge_df2 <- merge_df %>% 
  #   select(sys_sample_code, contains(.flag_col))
  # 
  # agg_df2 <- aggregate(
  #   . ~ sys_sample_code,
  #   data = merge_df2,
  #   FUN = toString
  # )
  # agg_df2 <- aggregate(
  #   x = merge_df$equipment_blank_ssc,
  #   # by = lapply(names(merge_df)[!grepl(.flag_col, names(merge_df))],
  #   #             function(i) merge_df[[i]]),
  #   by = list(merge_df$sys_sample_code, merge_df$chemical_name),
  #   FUN = toString
  # )
  # 
  # sub_df <- unique(merge_df[!grepl(.flag_col, names(merge_df))])
  # 
  # agg_test <- merge(
  #   sub_df,
  #   agg_df2,
  #   by = "sys_sample_code"
  # )
  if (.flag_col == "equipment_blank") {
    fail_vec <- merge_df$equipment_blank %in% "fail"
    greater_vec <- merge_df$result_value > 3 * merge_df$equipment_blank_result_value
    merge_df$equipment_blank <- ifelse(
      test =  fail_vec & greater_vec,
      yes = "pass",
      no = merge_df$equipment_blank)
  }

# Make sure that target QC samples are not assigned other QC sample --------
# It does not make sense for an EB sample to be associated with another EB sample.
  ftype <- switch(.flag_col,
                  "equipment_blank" = "equipment_blank",
                  "precision" = "field_duplicate",
                  "accuracy" = "matrix_spike")
  split_merge <- split(merge_df, merge_df$nysdec_sample_type)
  target_df <- split_merge[[ftype]]
  target_sub <- target_df[target_df$sys_sample_code == target_df[paste0(.flag_col, "_ssc")], ]
  split_merge[[ftype]] <- target_sub
    
  bound_df <- do.call(rbind, split_merge)
  agg_df <- bound_df %>%
    dplyr::group_by(dplyr::across(-dplyr::contains(.flag_col))) %>%
    dplyr::summarize(dplyr::across(dplyr::contains(.flag_col), paste, collapse = "; "),
                     .groups = "drop")
  
  final_df <- agg_df[!names(agg_df) %in% c("indices", "join_date")]
  
  return(final_df)
}
