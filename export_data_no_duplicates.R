
# INPUTS: API TOKEN, STUDY_AREA_1_ID, STUDY_AREA_1_NAME, STUDY_AREA_2_ID, STUDY_AREA_2_NAME
# OUTPUT: CSV FILE WITH DATA SET WITHOUT DUPLICATES

library(stringr)
source("tiptop_hhs_quality.R")

# Read arguments
args = commandArgs(T)
#api_token = args[1]
api_token = "1C249EF7F123F4DEB09EAAS7E4E9EDFS99" # TIPTOP HHS Baseline Madagascar (MALGACHE) TEMP
#study_area_1_id = args[2]
#study_area_1    = args[3]
#study_area_2_id = args[4]
#study_area_2    = args[5]
#study_areas_ids = c(study_area_1_id, study_area_2_id)
#study_areas     = c(study_area_1, study_area_2) 
study_areas_ids = c("mananjary", "toliary_2") # TEMP
study_areas = c("Mananjary", "Toliary 2") # TEMP

# Read data set from REDCap by using the provided token
redcap_api_url = "https://tiptop.isglobal.org/redcap/api/"
hhs_data = readData("api", api_url = redcap_api_url, api_token = api_token)

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

previous_household_id = -1
for(i in 1:length(record_ids_to_rename)) {
  new_household_id = paste0(
    hhs_data_with_no_dups$household[hhs_data_with_no_dups$record_id == record_ids_to_rename[i]], 
    '_01')
  
  if(new_household_id == previous_household_id)
    new_household_id = paste0(
      hhs_data_with_no_dups$household[hhs_data_with_no_dups$record_id == record_ids_to_rename[i]], 
      '_02')
  
  hhs_data_with_no_dups$household[
    hhs_data_with_no_dups$record_id == record_ids_to_rename[i]] = new_household_id
  
  previous_household_id = new_household_id
}

# When they are duplicates, remove them
duplicated_households = duplicated_households[duplicated_households$duplicated == 'T', ]
record_ids_to_drop = duplicated_households$record_id[duplicated(duplicated_households[2:4])]
hhs_data_with_no_dups = hhs_data_with_no_dups[!(hhs_data_with_no_dups$record_id %in% 
                                                  record_ids_to_drop), ]

# Report what have been done
# Pending...

# Write file
redcap_project_info = getProjectInfo(redcap_api_url, api_token)
filename = str_replace_all(redcap_project_info$project_title, "[^[:alnum:]]", "")
filename = paste0(filename, "_DATA_WITH_NO_DUPS_")
filename = paste0(filename, Sys.Date())
filename = paste0(filename, "_", format(Sys.time(), "%H%M"))
filename = paste0(filename, ".csv")
write.csv(hhs_data_with_no_dups, file = filename, row.names = F)