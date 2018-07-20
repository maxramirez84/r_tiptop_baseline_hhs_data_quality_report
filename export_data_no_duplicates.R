
# INPUTS: API TOKEN, STUDY_AREA_1_ID, STUDY_AREA_1_NAME, STUDY_AREA_2_ID, STUDY_AREA_2_NAME
# OUTPUT: CSV FILE WITH DATA SET WITHOUT DUPLICATES

library(stringr)
source("tiptop_hhs_quality.R")

# Auxiliar functions
renameRecords = function (data, record_ids_to_rename) {
  #browser()
  if(length(record_ids_to_rename) > 0) {
    previous_household_id = -1
    for(i in 1:length(record_ids_to_rename)) {
      new_household_id = paste0(
        data$household[data$record_id == record_ids_to_rename[i]], '_01')
      
      if(new_household_id == previous_household_id | new_household_id %in% data$household)
        new_household_id = paste0(
          data$household[data$record_id == record_ids_to_rename[i]], '_02')
      
      data$household[data$record_id == record_ids_to_rename[i]] = new_household_id
      
      previous_household_id = new_household_id
    }
  }
  
  return(data)
}

# Read arguments
args = commandArgs(T)
api_token = args[1]
study_area_1_id = args[2]
study_area_1    = args[3]
study_area_2_id = args[4]
study_area_2    = args[5]
study_areas_ids = c(study_area_1_id, study_area_2_id)
study_areas     = c(study_area_1, study_area_2)

# Read data set from REDCap by using the provided token
redcap_api_url = "https://tiptop.isglobal.org/redcap/api/"
hhs_data = readData("api", api_url = redcap_api_url, api_token = api_token)
hhs_data = removeSpecialCharacters(hhs_data, study_areas_ids)
hhs_data = removeEmptyRecords(hhs_data)

# Remove duplicated records (where all variables contain exactly the same values except record_id)
duplicated_records = duplicatedRecords(hhs_data, study_areas_ids, study_areas)
record_ids_to_remove = duplicated_records$record_id[
  duplicated(duplicated_records[2:ncol(duplicated_records)])]
hhs_data_with_no_dups = hhs_data[!(hhs_data$record_id %in% record_ids_to_remove), ]

# Analyze reused household IDs, where they are duplicates or not
duplicated_households = duplicatedHouseholds(hhs_data, study_areas_ids, study_areas)

# When they are not duplicates, reassing a new household ID (TO THE LATEST RECORDS)
non_duplicated_households = duplicated_households[duplicated_households$duplicated == 'F', ]
record_ids_to_rename = non_duplicated_households$record_id[
  duplicated(non_duplicated_households[2:4])]
hhs_data_with_no_dups = renameRecords(hhs_data_with_no_dups, record_ids_to_rename)

# When they are duplicates, remove the OLDEST RECORDS and keep the LAST ONE
in_fact_duplicated_households = duplicated_households[duplicated_households$duplicated == 'T', ]
record_ids_to_drop = in_fact_duplicated_households$record_id[
  duplicated(in_fact_duplicated_households[2:4], fromLast = T)]
hhs_data_with_no_dups = hhs_data_with_no_dups[!(hhs_data_with_no_dups$record_id %in% 
                                                  record_ids_to_drop), ]

# After renaming non duplicated households and dropping duplicated households, could happen that 
# still few duplicates remain in the dataset, concretely those household IDs which have occurrecies 
# in both non duplicated and duplicated. They have been porcessed independently, now they must be
# treated on the whole by renaming again.
duplicated_households = duplicatedHouseholds(hhs_data_with_no_dups, study_areas_ids, study_areas)
record_ids_to_rename = duplicated_households$record_id[duplicated(duplicated_households[2:4])]
hhs_data_with_no_dups = renameRecords(hhs_data_with_no_dups, record_ids_to_rename)

# Report what have been done
# Pending...

# Write file
redcap_project_info = getProjectInfo(redcap_api_url, api_token)
filename = str_replace_all(redcap_project_info$project_title, "[^[:alnum:]]", "")
filename = paste0(filename, "_DATA_WITH_NO_DUPS_")
filename = paste0(filename, Sys.Date())
filename = paste0(filename, "_", format(Sys.time(), "%H%M"))
filename = paste0(filename, ".csv")
write.csv(hhs_data_with_no_dups, file = filename, row.names = F, na = "")