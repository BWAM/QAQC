# .df <- als_chla_appended
# .id_colname <- "sample"
# .vec <- no_doc[[.id_colname]]
parse_join_event_id <- function(.df, .id_colname) {
  # Remove event ids that end in "DOC". 
  # These do not follow the standard event id format.
  no_doc <- .df[!grepl(".DOC$", .df[[.id_colname]]), ]
  # Parse the id into its various elements
  id_df <- parse_event_id(no_doc[[.id_colname]])
  # Make sure each row is unique.
  id_df <- unique(id_df)
  # Join the parsed ids with the original table.
  final_df <- full_join(x = id_df,
                        y = .df,
                        by = c("supplied_id" = .id_colname))
  # Intended to add the DOC samples to the event_id column.
  final_df$event_id <- ifelse(is.na(final_df$event_id),
                              tolower(final_df$supplied_id),
                              final_df$event_id)
  # End of function. Return a data frame.
  return(final_df)
}


parse_event_id <- function(.vec) {
  # Extract ID Elements from the supplied vector into a DF.
  extracted_df <- extract_id_elements(.vec)
  # Correct date: mmddyy -> R date
  extracted_df$id_date <- id_date_correction(extracted_df$id_date)
  # Correct site: check against expected and provide more informative names.
  extracted_df$site <- id_site_correction(extracted_df$site)
  # Correct time: HHMM -> R POSIXct using extracted_df$date.
  extracted_df$id_time <- id_time_correction(extracted_df$id_date,
                                             extracted_df$id_time)
  # Correct location: check against expected and provide more informative names.
  extracted_df$location <- id_location_correction(extracted_df$location)
  # Correct sample_type: check against expected and provide more informative names.
  extracted_df$sample_type <- id_type_correction(extracted_df$sample_type)
  # End of function. Return a data frame.
  return(extracted_df)
}

extract_id_elements <- function(.vec) {
  # All to lowercase to make it easier to clean up later.
  event_id <- tolower(.vec)
  # Remove any spaces or dashes from the vector.
  event_id <- gsub(" |-", "", event_id)
  # Create a data frame with the extracted info.
  final_df <- data.frame(
    # The original values are used here to enable a merge back to the
    # original table.
    supplied_id = .vec,
    # Supply a clean version of the Id.
    event_id = event_id,
    # Remove the location and sample type.
    # Makes it simple to compare by location.
    event_time_id = substr(event_id, 1, 13),
    event_date_id = substr(event_id, 1, 9),
    # 1-6 represent date as mmddyy.
    id_date = substr(x = event_id,
                     start = 1,
                     stop = 6),
    # 7-9 represent site (school) as 3-letter string.
    site = substr(x = event_id,
                  start = 7,
                  stop = 9),
    # 10-13 represent time as HHMM.
    id_time = substr(x = event_id,
                     start = 10,
                     stop = 13),
    # 14-16 represent location as a 3-letter string.
    location = substr(x = event_id,
                      start = 14,
                      stop = 16),
    # 17-19, if present, represent sample type strings of length 2 or 3.
    sample_type = substr(x = event_id,
                         start = 17,
                         stop = 19)
  )
  # End of function. Return a data frame.
  return(final_df)
}

id_date_correction <- function(.vec) {
  as.Date(.vec, "%m%d%y",
          tz = "America/New_York")
}

id_site_correction <- function(.vec) {
  # Check the supplied site names matches the expected site names.
  hermes::check_match(.supplied = unique(.vec),
                      .expected = c("clk", "con", "esf"),
                      .type = "'site' column values")
  # Provide more informative names.
  site_vec <- ifelse(
    test = .vec %in% "con",
    yes = "Control",
    no = ifelse(
      test = .vec %in% "clk",
      yes = "Clarkson",
      no = ifelse(
        test = .vec %in% "esf",
        yes = "ESF",
        no = .vec
      )
    )
  )
  # Make sites a factor to order appropriately when plotting.
  final_vec <- factor(site_vec, levels = c("Clarkson",
                                           "ESF",
                                           "Control"))
  return(final_vec)
}

id_time_correction <- function(.date_vec, .time_vec) {
  fixit::correct_times(.date_vec,
                       as.POSIXct(.time_vec,
                                  tz = "America/New_York",
                                  "%H%M"))
}

id_location_correction <- function(.vec) {
  # Check the supplied location names matches the expected location names.
  hermes::check_match(
    .supplied = unique(.vec),
    .expected = c("amb", "eff", "inf", "oc"),
    .type = "'location' column values"
  )
  # Provide more informative names.
  location_vec <- ifelse(
    test = .vec %in% "amb",
    yes = "Ambient",
    no = ifelse(
      test = .vec %in% "eff",
      yes = "Effluent",
      no = ifelse(
        test = .vec %in% "inf",
        yes = "Influent",
        no = "ERROR"
      )
    )
  )
  # Make location a factor to order appropriately when plotting.
  final_vec <- factor(location_vec, levels = c("Ambient",
                                               "Influent",
                                               "Effluent"))
  return(final_vec)
}

id_type_correction <- function(.vec) {
  # Check the supplied location names matches the expected location names.
  hermes::check_match(
    .supplied = unique(.vec),
    .expected = c("", "dup", "eb", "72"),
    .type = "'sample_type' column values"
  )
  # Provide more informative names.
  ifelse(
    test = .vec %in% "dup",
    yes = "duplicate",
    no = ifelse(
      test = .vec %in% "eb",
      yes = "equipment_blank",
      no = ifelse(
        test = .vec %in% "72",
        yes = "72_hr_hold",
        no = ifelse(
          test = .vec %in% "",
          yes = "sample",
          no = "ERROR"
        )
      )
    )
  )
}

