##LOAD FILES
#This script navigates to where the outputs of each BigBacter run are saved and loads the contents of
#the most recent created folder.
#This script also navigates to where the metadata db is saved and loads the file.

#Create a folder to save the new results by taxa for the rendering of independent reports
outputs_script_dir <- "outputs_scripts"
if (!dir.exists(outputs_script_dir)) {
  dir.create(outputs_script_dir)
}

#Load the file paths that are saved in the paths.txt file
paths <-readLines("paths.txt")

#Load each path separately
main_folder <- paths[1]
bacteriatracker.wa <- paths[2]

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

#DATA CLEANING FUNCTION
# Function for column cleanup
clean_column <- function(column) {
  case_when(
    str_starts(column, "WA") ~ substr(column, 1, 9),
    str_starts(column, regex("[0-9]{4}")) ~ substr(column, 1, 12),
    str_ends(column, "_T1") ~ str_remove(column, "_T1"),
    TRUE ~ column
  )
}

#BigBacter sometimes adds a _T1 to the samples. Remove and clean IDs
summary_tsv_cleaned<- summary_tsv %>%
  mutate(ID = clean_column(ID))
         
#LOAD BACTERIA-TRACKER METADATA that originates in the new tracker#
bacteriatrackerwa_meta_df<- read.csv(bacteriatracker.wa)

#Identify TAXA for generation of reports by taxa
results_by_taxa<-summary_tsv_cleaned %>% 
  filter(STATUS == "NEW") %>% 
  select(TAXA) %>% 
  unique()

write.csv(results_by_taxa, file = file.path(outputs_script_dir, "results_by_taxa.csv"), row.names = FALSE)

##IDENTIFY AND LOAD MICROREACT FILES##
#Identify the .microreact files
# Create a folder to save the extracted microreact files
microreact_dir <- "microreact"
if (!dir.exists(microreact_dir)) {
  dir.create(microreact_dir)
}

# Locate all .microreact files in the most recent folder
files_in_most_recent_folder <- list.files(most_recent_folder, full.names = TRUE)
microreact_files <- files_in_most_recent_folder[grepl("\\.microreact$", files_in_most_recent_folder)]

# Extract each .microreact file to its respective subfolder
invisible(lapply(microreact_files, function(microreact_file) {
  
  # Create a subfolder for the current file based on the file name
  subfolder_name <- tools::file_path_sans_ext(basename(microreact_file))  # Remove .microreact extension for folder name
  subfolder_path <- file.path(microreact_dir, subfolder_name)
  
  # Clean the subfolder by removing its contents if it exists
  if (dir.exists(subfolder_path)) {
    unlink(subfolder_path, recursive = TRUE)  # Delete the subfolder and its contents
  }
  
  # Create the subfolder again
  dir.create(subfolder_path)
  
  # Modify the file name: remove leading digits and dash (e.g., "1749579764-")
  original_filename <- basename(microreact_file)
  modified_filename <- sub("^[0-9]+-", "", original_filename)
  
  # Copy the original .microreact file into the subfolder
  output_file <- file.path(subfolder_path, basename(modified_filename))
  
  # Ensure the .microreact file is copied as is (no modification)
  file.copy(microreact_file, output_file)
  
}))