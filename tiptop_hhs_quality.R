library(kableExtra)
library(redcapAPI)
library(dplyr)

# Auxiliar functions -------------------------------------------------------------------------------

# Data timestamp
dataTimestamp = function(data_retrieval_mode, file_date = "", file_time = "") {
  if(data_retrieval_mode == "file") {
    data_timestamp = paste(file_date, file_time)
  } else if(data_retrieval_mode == "api") {
    data_timestamp = Sys.time()
  }
  
  return(data_timestamp)
}

# Read data from csv (downloaded from REDCap) or directly through the API
readData = function(data_retrieval_mode, file_prefix = "", file_date = "", file_time = "",
                    api_url = "", api_token = "") {
  if(data_retrieval_mode == "file") {
    hhs_data_file =paste0(file_prefix, "_DATA_", file_date, "_", gsub(":", "", file_time), ".csv")
    hhs_data = read.csv(hhs_data_file)
  } else if(data_retrieval_mode == "api") {
    rcon = redcapConnection(api_url, api_token)
    hhs_data = exportRecords(rcon, factors = F)
  }
  
  return(hhs_data)
}

# Get the timestamp of the last collected record
lastRecordDate = function(hhs_data) {
  return(max(as.character(hhs_data$interview_date), na.rm = T))
}

# Get the number of records uploaded to REDCap
numberOfRecords = function(hhs_data) {
  return(nrow(hhs_data))
}

# Behaves as MySQL UNION statement. Appends a list just below the other.
union = function(...) {
  aux = list(...)
  dat = data.frame()
  for(i in seq(along = aux)) {
    if(length(aux[[i]]) == 0) {
      dat[i,] = rep(0, ncol(dat))
    } else {
      for(j in names(aux[[i]]))
        dat[i,j] = aux[[i]][j] 
    }
  }
  dat = rapply(dat, f = function(x) ifelse(is.na(x), 0, x), how = "replace")
  return(dat)
}

# Converts a data frame of two columns in a list in which one column is used as keys 
# and the other as values.
pivot <- function(indexes, index_column, value_column, df) {
  l = list()
  for(i in indexes) {
    if(length(df[value_column][df[index_column] == i]) == 0)
      l[i] = 0
    else
      l[i] = df[value_column][df[index_column] == i]
  }
  
  return(l)
}

# Compute number of participants who consented the interview
numberOfparticipantsWhoConsented = function(hhs_data) {
  consented_area_1 = table(hhs_data$consent[hhs_data$district == 1])[2]
  consented_area_2 = table(hhs_data$consent[hhs_data$district == 2])[2]
  consented = c(
    if(is.na(consented_area_1)) 0 else consented_area_1,
    if(is.na(consented_area_2)) 0 else consented_area_2
  )
  names(consented) = c(1, 2)
  
  return(consented)
}

# Compute recruitment rate
recruitmentRate = function(hhs_data, sample_size_area_1, sample_size_area_2) {
  consented = numberOfparticipantsWhoConsented(hhs_data)
  
  recruitment = c(
    if(is.na(consented[1])) 0 else floor((consented[1] / sample_size_area_1) * 100),
    if(is.na(consented[2])) 0 else floor((consented[2] / sample_size_area_2) * 100)
  )
  names(recruitment) = c(1, 2)
  
  return(recruitment)
}

# Compute c-IPTp knowledge rate
cIPTpKnowledgeRate = function(hhs_data) {
  consented = numberOfparticipantsWhoConsented(hhs_data)
  
  know_ciptp_area_1 = table(hhs_data$know_about_ciptp[hhs_data$district == 1])
  know_ciptp_area_2 = table(hhs_data$know_about_ciptp[hhs_data$district == 2])
  
  ciptp_knowledge = c(
    if(is.na(consented[1])) 0 else floor((
      if(is.na(know_ciptp_area_1[2])) 0 else know_ciptp_area_1[2] / consented[1]) * 100),
    if(is.na(consented[2])) 0 else floor((
      if(is.na(know_ciptp_area_2[2])) 0 else know_ciptp_area_2[2] / consented[2]) * 100)
  )
  names(ciptp_knowledge) = c(1, 2)
  
  return(ciptp_knowledge)
}

# Compute c-IPTp administration: Women who took SP at community level
cIPTpAdministrationRate = function(hhs_data) {
  consented = numberOfparticipantsWhoConsented(hhs_data)
  
  administration_ciptp_area_1 = table(hhs_data$sp_community[hhs_data$district == 1])
  administration_ciptp_area_2 = table(hhs_data$sp_community[hhs_data$district == 2])
  
  ciptp_administration = c(
    if(is.na(consented[1])) 0 else floor((
      if(is.na(administration_ciptp_area_1[2])) 0 else administration_ciptp_area_1[2] / consented[1]) * 100),
    if(is.na(consented[2])) 0 else floor((
      if(is.na(administration_ciptp_area_2[2])) 0 else administration_ciptp_area_2[2] / consented[2]) * 100)
  )
  names(ciptp_administration) = c(1, 2)
  
  return(ciptp_administration)
}

# Plots --------------------------------------------------------------------------------------------
# Color palette
color_palette = c("gray8", "gray35", "gray90")

# Visited Households per Area
visitedHouseholdsArea = function(hhs_data, household_to_be_visited_area_1, 
                                 household_to_be_visited_area_2, study_areas) {
  interval = 100
  max_x_axis = max(household_to_be_visited_area_1, household_to_be_visited_area_2) + interval * 5
  
  consented = numberOfparticipantsWhoConsented(hhs_data)
  
  visits_area_1 = table(hhs_data$district)[1]
  visits_area_2 = table(hhs_data$district)[2]
  visits_number = c(
    if(is.na(visits_area_1)) 0 else visits_area_1,
    if(is.na(visits_area_2)) 0 else visits_area_2
  )
  completeness = c(
    if(is.na(visits_number[1])) 0 
    else floor((visits_number[1] / household_to_be_visited_area_1) * 100), 
    if(is.na(visits_number[2])) 0 
    else floor((visits_number[2] / household_to_be_visited_area_2) * 100)
  )
  names(completeness) = c(1, 2)
  par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.05)
  visits_progress = barplot(
    height      = matrix(c(consented, visits_number), nrow = 2, ncol = 2, byrow = T), 
    horiz       = T, 
    names.arg   = study_areas, 
    main        = "Visited Households per Area",
    xlab        = "Number of households",
    ylab        = "Study areas",
    xlim        = c(0, max_x_axis),
    axes        = F,
    beside      = T,
    col = color_palette[2:3]
  )
  axis(1, seq(0, max_x_axis, interval))
  legend("topright", legend = c("Interviewed", "Visited"), fill = color_palette[2:3], cex = 1.5)
  text(
    x      = c(visits_number,consented), 
    y      = c(2.5, 5.5, 1.5, 4.5), 
    labels = paste0(c(completeness, recruitment), '%'), 
    pos    = 4, 
    col = "red",
    cex = 1.5
  )
}

# Visited Households per Cluster in a concrete Area
progressOfArea = function(hhs_data, study_area, interval, required_visits_mean) {
  study_area_column = paste0("cluster_", study_areas_ids[study_area])
  
  visits_number = table(hhs_data[study_area_column])
  if(length(visits_number) > 0) {
    max_y_axis = max(visits_number) + interval
    consented_number = table(hhs_data[hhs_data$consent == 1, study_area_column])
    
    dat = union(visits_number, consented_number)
    par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.05, mar = c(8, 8, 4, 0))
    visits_progress = barplot(
      height = matrix(c(dat[2,], dat[1,] - dat[2,]), nrow = 2, byrow = T),
      main   = paste("Visited Households per Cluster in", study_areas[study_area]),
      xlab   = paste("Cluster in", study_areas[study_area]),
      ylab   = "Number of households",
      ylim   = c(0, max_y_axis),
      axes   = F,
      col = color_palette[2:3],
      mgp = c(4, 1, 0)
    )
    axis(1, visits_progress, paste0("C", rownames(visits_number)), las = 2)
    axis(2, seq(0, max_y_axis, interval))
    abline(h = required_visits_mean, lwd = 1, lty = 2, col = "red")
    legend("topright", legend = c("Interviewed", "Visited"), fill = color_palette[2:3], cex = 1.5)
    text(x = visits_progress, y = dat[2,], labels = dat[2,], pos = 3, col = color_palette[1])
  } else {
    print("There is no data.") 
  }
}

# Tables -------------------------------------------------------------------------------------------
trialProfileOfArea = function(hhs_data, study_area) {
  maximum_number_of_columns = 29
  font_size = 10
  study_area_column = paste0("cluster_", study_areas_ids[study_area])
  
  number_hh_selected_visited = table(hhs_data[study_area_column])
  if(length(number_hh_selected_visited) > 0) {
    number_hh_selected_interviewed = table(hhs_data[hhs_data$hh_acceptance == 1, study_area_column])
    
    number_women_childbearing_age_df = setNames(
      aggregate(childbearing_age_women ~ get(study_area_column), FUN = sum, data = hhs_data), 
      c(study_area_column, "childbearing_age_women")
    )
    number_women_childbearing_age_list = pivot(
      indexes = names(number_hh_selected_visited), 
      index_column = study_area_column, 
      value_column = "childbearing_age_women", 
      df = number_women_childbearing_age_df
    )
    
    number_eligible_women_df = setNames(
      aggregate(residents_during_pregnancy ~ get(study_area_column), FUN = sum, data = hhs_data),
      c(study_area_column, "residents_during_pregnancy")
    )
    number_eligible_women_list = pivot(
      indexes = names(number_hh_selected_visited),
      index_column = study_area_column,
      value_column = "residents_during_pregnancy",
      df = number_eligible_women_df
    )
    
    childbearing_age_women_profile = union(
      number_women_childbearing_age_list, 
      number_eligible_women_list
    )
    
    number_women_interviewed = table(hhs_data[hhs_data$consent == 1, study_area_column])
    number_women_non_interviewed = table(hhs_data[hhs_data$consent == 0, study_area_column])
    
    eligible_women_selected = union(number_women_interviewed, number_women_non_interviewed)
    
    number_women_denied_consent = table(hhs_data[hhs_data$why_not_consent == 0, study_area_column])
    number_women_absent = table(hhs_data[hhs_data$why_not_consent == 2, study_area_column])
    number_women_unabled = table(hhs_data[hhs_data$why_not_consent == 1, study_area_column])
    number_women_other_reason = table(hhs_data[hhs_data$why_not_consent == 88, study_area_column])
    
    number_hh_empty = table(hhs_data[hhs_data$hh_available == 2, study_area_column])
    number_hh_head_not_found = table(hhs_data[hhs_data$hh_available == 0, study_area_column])
    number_hh_head_refused = table(hhs_data[hhs_data$hh_acceptance == 0, study_area_column])
    
    hh_selected_not_interviewed = union(number_hh_empty, number_hh_head_refused)
    
    trial_profile = union(
      number_hh_selected_visited, 
      number_hh_selected_interviewed, 
      number_women_childbearing_age_list, 
      childbearing_age_women_profile[1,] - childbearing_age_women_profile[2,], 
      number_eligible_women_list,
      eligible_women_selected[1,] + eligible_women_selected[2,],
      number_women_interviewed,
      number_women_non_interviewed,
      number_women_denied_consent,
      number_women_absent,
      number_women_unabled,
      number_women_other_reason,
      hh_selected_not_interviewed[1,] + hh_selected_not_interviewed[2,],
      number_hh_empty,
      number_hh_head_not_found,
      number_hh_head_refused
    )
    row.names(trial_profile) = c(
      "HH selected visited", 
      "HH selected interviewed", 
      "Women of childbearing age",
      paste0("NON eligible women", footnote_marker_symbol(1, "html")),
      "Eligible women",
      "Eligible women selected",
      "Women interviewed",
      "Women NON interviewed",
      "Denied signed consent/assent",
      "Absent",
      "Not able to respond",
      "Other reason",
      "HH selected NOT interviewed",
      "Empty/destroyed",
      paste0("HH head not found", footnote_marker_symbol(2, "html")),
      "HH head/other refused to consent the interview"
    )
    colnames(trial_profile) = paste0("C", colnames(trial_profile))
    #browser()
    # Consistency checks within the trial profile
    trial_profile_checked = trial_profile
    for(i in colnames(trial_profile)) {
      # non_interviewed HH = empty + refused
      trial_profile_checked[c(13, 14, 16), i] = cell_spec(
        x        = trial_profile[c(13, 14, 16),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[14, i] + trial_profile[16, i] != trial_profile[13, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[14, i] + trial_profile[16, i] != trial_profile[13, i], 
                 "NOT interviewed HH must be equal to the sum of empty/destroyed + refused", "")
      )
      
      # women = eligible + non_eligible
      trial_profile_checked[c(3, 4, 5), i] = cell_spec(
        x        = trial_profile[c(3, 4, 5),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[4, i] + trial_profile[5, i] != trial_profile[3, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[4, i] + trial_profile[5, i] != trial_profile[3, i], 
                 "Women must be equal to the sum of eligibles + NON eligibles", "")
      )
      
      # non_interviwed women = denied + absent + unabled + other
      trial_profile_checked[c(8, 9, 10, 11, 12), i] = cell_spec(
        x        = trial_profile[c(8, 9, 10, 11, 12),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[9, i] + trial_profile[10, i] + trial_profile[11, i] + 
                   trial_profile[12, i] != trial_profile[8, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[9, i] + trial_profile[10, i] + trial_profile[11, i] + 
                   trial_profile[12, i]  != trial_profile[8, i], 
                 "NON interviewed women must be equal to the sum of denied + absent + unabled", "")
      )
      
      # women selected = interviewed + non_interviewed
      trial_profile_checked[c(6, 7, 8), i] = cell_spec(
        x        = trial_profile[c(6, 7, 8),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[7, i] + trial_profile[8, i] != trial_profile[6, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[7, i] + trial_profile[8, i] != trial_profile[6, i], 
                 "Women selected must be equal to the sum of interviewed + NON interviewed", "")
      )
      
      # visited HH = interviewed + non_interviewed
      trial_profile_checked[c(1, 2, 13), i] = cell_spec(
        x        = trial_profile[c(1, 2, 13),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[2, i] + trial_profile[13, i] != trial_profile[1, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[2, i] + trial_profile[13, i] != trial_profile[1, i], 
                 "Visited HH must be equal to the sum of interviewed + NOT interviewed", "")
      )
    }
    #browser()
    if(ncol(trial_profile_checked) > maximum_number_of_columns) {
      number_of_columns = ncol(trial_profile_checked)
      middle = as.integer(number_of_columns / 2)
      
      print(kable(trial_profile_checked[,1:(middle + 2)], "html", escape = F) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                        font_size = font_size) %>%
        row_spec(0, bold = T, color = "white", background = "#494949") %>%
        row_spec(c(1, 2, 3, 13), bold = T) %>%
        add_indent(c(9, 10, 11, 12))
      )
      print(kable(trial_profile_checked[,(middle + 3):number_of_columns], "html", escape = F) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "responsive"), font_size = 
                        font_size) %>%
        row_spec(0, bold = T, color = "white", background = "#494949") %>%
        row_spec(c(1, 2, 3, 13), bold = T) %>%
        add_indent(c(9, 10, 11, 12)) %>%
        footnote(
          general_title = "Notes:",
          general = "Colored cells are consistency errors. Hover over these cells to display a 
          tooltip with the error message. Please, refer to the provided Data Queries Sheet.", 
          symbol = c(
            "Eligible woman: woman that meets selection criteria 1 and selection criteria 2", 
            "HH head availability is not required to proceed with the interview as long as any other 
            adult consents"
          )
        )
      )
    } else {
      print(kable(trial_profile_checked, "html", escape = F) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                        font_size = font_size) %>%
        row_spec(0, bold = T, color = "white", background = "#494949") %>%
        row_spec(c(1, 2, 3, 13), bold = T) %>%
        add_indent(c(9, 10, 11, 12)) %>%
        footnote(
          general_title = "Notes:",
          general = "Colored cells are consistency errors. Hover over these cells to display a 
          tooltip with the error message. Please, refer to the provided Data Queries Sheet.", 
          symbol = c(
            "Eligible woman: woman that meets selection criteria 1 and selection criteria 2", 
            "HH head availability is not required to proceed with the interview as long as any other 
            adult consents"
          )
        )
      )
    }
  } else {
    print("There is no data.")
  }
  
}

SPIndicators = function(hhs_data, study_areas) {
  consented = numberOfparticipantsWhoConsented(hhs_data)
  
  sp_area_1 = table(hhs_data$sp[hhs_data$district == 1])
  sp_area_2 = table(hhs_data$sp[hhs_data$district == 2])
  sp = t(union(sp_area_1, sp_area_2))
  
  sp_doses_area_1 = table(hhs_data$sp_doses_number[hhs_data$district == 1])
  sp_doses_area_2 = table(hhs_data$sp_doses_number[hhs_data$district == 2])
  sp_doses = t(union(sp_doses_area_1, sp_doses_area_2))
  #browser()
  sp_adherence = union(
    consented,
    if('1' %in% rownames(sp)) sp['1',] else c(0, 0),
    if('1' %in% rownames(sp_doses)) sp_doses['1',] else c(0, 0),
    if('2' %in% rownames(sp_doses)) sp_doses['2',] else c(0, 0),
    if('3' %in% rownames(sp_doses)) sp_doses['3',] else c(0, 0),
    if('4' %in% rownames(sp_doses)) sp_doses['4',] else c(0, 0),
    if('5' %in% rownames(sp_doses)) sp_doses['5',] else c(0, 0),
    if('6' %in% rownames(sp_doses)) sp_doses['6',] else c(0, 0),
    if('0' %in% rownames(sp)) sp['0',] else c(0, 0), 
    if('2' %in% rownames(sp)) sp['2',] else c(0, 0)
  )
  for(i in 2:10) {
    sp_adherence[i,] = paste(
      sp_adherence[i,],
      paste0("(", round((as.integer(sp_adherence[i,]) / consented) * 100, 2), "%", ")")
    )
  }
  
  row.names(sp_adherence) = c(
    "Women interviewed",
    "Women that took SP",
    "Women that took exactly 1 dose of SP",
    "Women that took exactly 2 doses of SP",
    "Women that took exactly 3 doses of SP",
    "Women that took exactly 4 doses of SP",
    "Women that took exactly 5 doses of SP",
    "Women that took exactly 6 doses of SP",
    "Women that didn't take SP",
    "Women that didn't know if they took SP"
  )
  colnames(sp_adherence) = study_areas
  
  kable(sp_adherence, "html", escape = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "responsive"), font_size = 12, 
                  full_width = F, position = "float_right") %>%
    row_spec(0, bold = T, color = "white", background = "#494949") %>%
    row_spec(c(1, 2, 9, 10), bold = T) %>%
    add_indent(c(3, 4, 5, 6, 7, 8))
}

duplicatedRecords = function() {
  #id_columns = hhs_data[c("cluster_kenge", "cluster_bulungu", "household")]
  study_area_columns = paste0("cluster_", study_areas_ids)
  
  duplicated_records = hhs_data[duplicated(hhs_data[2:ncol(hhs_data)]) | 
                                  duplicated(hhs_data[2:ncol(hhs_data)], fromLast = T), ]
  
  duplicated_records$cluster[!is.na(duplicated_records[study_area_columns[1]])] = 
    duplicated_records[!is.na(duplicated_records[study_area_columns[1]]), study_area_columns[1]]
  duplicated_records$cluster[!is.na(duplicated_records[study_area_columns[2]])] = 
    duplicated_records[!is.na(duplicated_records[study_area_columns[2]]), study_area_columns[2]]
  
  columns = c("district", "cluster", "household", "latitude", "longitude", "hh_initials", "consent", 
              "interviewer_id", "interview_date")
  duplicated_records_summary = duplicated_records[
    order(duplicated_records$district, duplicated_records$cluster, duplicated_records$household), 
    columns]
  
  duplicated_records_summary$consent[is.na(duplicated_records_summary$consent)] = "Not asked"
  duplicated_records_summary$consent[duplicated_records_summary$consent == 0]   = "No"
  duplicated_records_summary$consent[duplicated_records_summary$consent == 1]   = "Yes"
  
  duplicated_records_summary$district[duplicated_records_summary$district == 1] = study_areas[1]
  duplicated_records_summary$district[duplicated_records_summary$district == 2] = study_areas[2]
  
  colnames(duplicated_records_summary) = c("District", "Cluster", "HH ID", "Latitude", "Longitude", 
                                           "Head Initials", "Consent", "Int. ID", "Int. Date")
  #browser()
  kable(duplicated_records_summary, "html", escape = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                  font_size = 12) %>%
    row_spec(0, bold = T, color = "white", background = "#494949") %>%
    scroll_box(height = "250px")
}

duplicatedHouseholds = function() {
  #browser()
  study_area_columns = paste0("cluster_", study_areas_ids)
  
  duplicated_records = hhs_data[duplicated(hhs_data[2:ncol(hhs_data)]) | 
                                  duplicated(hhs_data[2:ncol(hhs_data)], fromLast = T), ]
  
  id_columns = hhs_data[c(study_area_columns[1], study_area_columns[2], "household")]
  duplicated_hh = hhs_data[duplicated(id_columns) | duplicated(id_columns, fromLast = T), ]
  rerecorded_hh = duplicated_hh[!(duplicated_hh$record_id %in% duplicated_records$record_id), ]
  
  rerecorded_hh$cluster[!is.na(rerecorded_hh[study_area_columns[1]])] = 
    rerecorded_hh[!is.na(rerecorded_hh[study_area_columns[1]]), study_area_columns[1]]
  rerecorded_hh$cluster[!is.na(rerecorded_hh[study_area_columns[2]])] = 
    rerecorded_hh[!is.na(rerecorded_hh[study_area_columns[2]]), study_area_columns[2]]
  
  columns = c("district", "cluster", "household", "latitude", "longitude", "hh_initials", "consent", 
              "end_last_pregnancy", "reported_age", "interviewer_id", "interview_date")
  rerecorded_hh_summary = rerecorded_hh[
    order(rerecorded_hh$district, rerecorded_hh$cluster, rerecorded_hh$household), 
    columns]
  
  rerecorded_hh_summary$consent[is.na(rerecorded_hh_summary$consent)] = "Not asked"
  rerecorded_hh_summary$consent[rerecorded_hh_summary$consent == 0]   = "No"
  rerecorded_hh_summary$consent[rerecorded_hh_summary$consent == 1]   = "Yes"
  
  rerecorded_hh_summary$district[rerecorded_hh_summary$district == 1] = study_areas[1]
  rerecorded_hh_summary$district[rerecorded_hh_summary$district == 2] = study_areas[2]
  
  colnames(rerecorded_hh_summary) = c("District", "Cluster", "HH ID", "Latitude", "Longitude", 
                                      "Head Initials", "Consent", "End Pregnancy", "Age", "Int. ID", 
                                      "Int. Date")
  #browser()
  kable(rerecorded_hh_summary, "html", escape = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                  font_size = 12) %>%
    row_spec(0, bold = T, color = "white", background = "#494949") %>%
    scroll_box(height = "250px")
}