
# INPUTS: API, <API TOKEN>, <STUDY_AREA_1_ID>, <STUDY_AREA_1_NAME>, <STUDY_AREA_2_ID>, 
#         <STUDY_AREA_2_NAME>
# INPUTS: FILE, <API TOKEN>, <STUDY_AREA_1_ID>, <STUDY_AREA_1_NAME>, <STUDY_AREA_2_ID>, 
#         <STUDY_AREA_2_NAME>, <FILE_PREFIX>, <FILE_CONTENT>, <FILE_DATE>, <FILE_TIME>
#
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
source          = args[1]  #= "FILE"
api_token       = args[2]  #= "XXXXXX"
study_area_1_id = args[3]  #= "nhamatanda"
study_area_1    = args[4]  #= "Nhamatanda"
study_area_2_id = args[5]  #= "meconta"
study_area_2    = args[6]  #= "Meconta"
if(source == "FILE") {
  file_prefix   = args[7]  #= "DATA/DATA/TIPTOPHHSBaselineMoz"
  file_content  = args[8]  #= "_DATA_"
  file_date     = args[9]  #= "2018-09-05"
  file_time     = args[10] #= "08:35"
}
  
study_areas_ids = c(study_area_1_id, study_area_2_id)
study_areas     = c(study_area_1, study_area_2)

# Read data set from REDCap by using the provided token
redcap_api_url = "https://tiptop.isglobal.org/redcap/api/"

if(source == "API") {
  hhs_data = readData("api", api_url = redcap_api_url, api_token = api_token)
} else {
  hhs_data = readData("file", file_prefix = file_prefix, file_content = file_content, 
                      file_date = file_date, file_time = file_time)
}
# In the Mozambique case, cluster values are scattered in multiple variables. So we need to collapse them
hhs_data$cluster_nhamatanda[!is.na(hhs_data$district) & hhs_data$district == 1] = 
  rowSums(hhs_data[!is.na(hhs_data$district) & hhs_data$district == 1, grepl("cluster_", names(hhs_data))], na.rm = T)
hhs_data$cluster_meconta[!is.na(hhs_data$district) & hhs_data$district == 2] = 
  rowSums(hhs_data[!is.na(hhs_data$district) & hhs_data$district == 2, grepl("cluster_", names(hhs_data))], na.rm = T)
###

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

# In the Mozambique case, cluster values are scattered in multiple variables. So they werecollapsed 
# before. Now removed to avoid confusion a keep the data set as it was originally
hhs_data_with_no_dups$cluster_nhamatanda = NULL
hhs_data_with_no_dups$cluster_meconta = NULL
###

# Write file
redcap_project_info = getProjectInfo(redcap_api_url, api_token)
filename = str_replace_all(redcap_project_info$project_title, "[^[:alnum:]]", "")
filename = paste0(filename, "_DATA_WITH_NO_DUPS_")
filename = paste0(filename, Sys.Date())
filename = paste0(filename, "_", format(Sys.time(), "%H%M"))
filename = paste0(filename, ".csv")
write.csv(hhs_data_with_no_dups, file = filename, row.names = F, na = "")