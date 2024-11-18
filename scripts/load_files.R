##LOAD FILES
#This script navigates to where the outputs of each BigBacter run are saved and loads the contents of
#the most recent created folder.
#This script also navigates to where the metadata db is saved and loads the file.

#Load the file paths that are saved in the paths.txt file
paths <-readLines("paths.txt")

#Load each path separately
main_folder <- paths[1]
wabacteriamaster_metadata <- paths[2]



##EXTRACTING OUTPUTS FROM MAIN FOLDER##
#List all subfolders saved in the main folder
subfolders <- list.dirs(main_folder, full.names = TRUE, recursive = FALSE)

#Get the creation time of each subfolder
folder_info <- data.frame(
  folder = subfolders,
  creation_time = file.info(subfolders)$ctime
)

#Find the most recently created folder
most_recent_folder <- folder_info[which.max(folder_info$creation_time), "folder"]

#List all CSV files in the most recent folder
csv_files_in_recent_folder <- list.files(most_recent_folder, pattern = "\\.csv$", full.names = TRUE)



##IDENTIFY SNP DIST CSV FILES AND LOAD##
#Filter the CSV files that contain both "core-snps_dist" and "long" in the file name
snps_dist_longfiles <- csv_files_in_recent_folder[grepl("core-snps_dist", csv_files_in_recent_folder) & 
                                                   grepl("long", csv_files_in_recent_folder)]


#Check if there are any SNP dist files in the folder
if (length(snps_dist_longfiles) == 0) {
  stop("No SNP dist files found in the most recent folder.")
}


#Initialize an empty list to store the loaded dataframes
snp_dist_long_list <- list()

#Load each SNP dist long file and store it in the list with the file name as the element name
for (file in snps_dist_longfiles) {
  #Generate a valid name for the list element from the file name (without the extension)
  file_name <- gsub(".csv$", "", basename(file))
  
  #Read the CSV file and assign it to the list, using the file name as the list element name
  snp_dist_long_list[[file_name]] <- read.csv(file)
  
}

##IDENTIFY AND LOAD SUMMARY TSV FILE##
#Identify the summary.tsv file
#List all files in the most recent folder
files_in_most_recent_folder <- list.files(most_recent_folder, full.names = TRUE)

#Filter for .tsv files that contain the string "summary"
summary_tsv_files<- files_in_most_recent_folder[grepl("summary", files_in_most_recent_folder) & 
                                                  grepl("\\.tsv$", files_in_most_recent_folder)]

#Load the summary tsv file assuming there is only per folder
summary_tsv <- read.delim(summary_tsv_files[1])

#DATA CLEANING
#BigBacter sometimes adds a _T1 to the samples. Remove and clean IDs

summary_tsv_cleaned<- summary_tsv %>%
  mutate(ID = case_when(
    str_starts(summary_tsv$ID, "WA") ~ substr(summary_tsv$ID, 1, 9),
    str_starts(summary_tsv$ID, regex("[0-9]{4}")) ~ substr(summary_tsv$ID, 1, 12),
    str_ends(summary_tsv$ID, "_T1") ~ str_remove(summary_tsv$ID, "_T1"),
    TRUE ~ summary_tsv$ID
  ))

##LOAD WABACTERIAMASTER_METADATA#
wabacteriamaster_meta_df<- read.csv(wabacteriamaster_metadata)
