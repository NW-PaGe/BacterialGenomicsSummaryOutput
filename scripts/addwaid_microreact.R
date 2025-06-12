#WA ID ADD TO MICROREACT
#This script identifies the documents that contain the metadata inside the microreact files, parses the data,
#adds the WA ID, and resaves the microreact files


## PART I: Extract and parse the metadata from the microreact files##

# Folder that contains the microreact files
microreact_dir <- "microreact"

parse_microreact_metadata <- function(mr_file_path) {
  message("Checking: ", mr_file_path)
  
  # Read full .microreact JSON as text
  json_text <- paste(readLines(mr_file_path, warn = FALSE), collapse = "\n")
  
  # Parse JSON
  mr_data <- fromJSON(json_text)
  
  # Check for metadata blob
  if (!is.null(mr_data$files$metadata$blob)) {
    blob_text <- mr_data$files$metadata$blob
    
    df.meta <- tryCatch({
      read_csv(I(blob_text), show_col_types = FALSE)
    }, error = function(e) {
      message("ERROR while reading CSV from blob: ", e$message)
      return(NULL)
    })
    
    if (!is.null(df.meta)) {
      output_dir <- dirname(mr_file_path)
      output_file <- file.path(output_dir, paste0("parsed_", file_path_sans_ext(basename(mr_file_path)), "_metadata.csv"))
      write_csv(df.meta, output_file)
      message("Parsed metadata saved to: ", output_file)
    }
  } else {
    warning("Metadata blob not found in ", mr_file_path)
  }
}

# Loop through subfolders and parse each .microreact file
subfolders <- list.dirs(microreact_dir, full.names = TRUE, recursive = FALSE)

for (subfolder in subfolders) {
  mr_files <- list.files(subfolder, pattern = "\\.microreact$", full.names = TRUE)
  lapply(mr_files, parse_microreact_metadata)
}

# PART II: Add the WA ID to the metadata microreact file and output the edited .microreact file
# Locate all parsed metadata CSVs
parsed_files <- list.files(
  path = "microreact",
  pattern = "^parsed_.*_metadata\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Iterate through each parsed metadata file
for (file in parsed_files) {
  df <- read_csv(file, show_col_types = FALSE)
  
  # Preserve 'Reference' rows
  df_reference <- df %>% filter(ID == "Reference")
  df_non_reference <- df %>% filter(ID != "Reference")
  
  # Prepare metadata for join: rename ID to WA_ID
  wa_meta_df <- bacteriatrackerwa_meta_df %>%
    select(ID_BB, WA_ID)
  
  # Join only non-Reference rows
  joined_non_reference <- df_non_reference %>%
    left_join(wa_meta_df, by = c("ID" = "ID_BB"))
  
  # Add Reference rows back in (with WA_ID = "Reference")
  df_reference$WA_ID <- "Reference"
  joined_df <- bind_rows(joined_non_reference, df_reference)
  
  # Get the subfolder path where the .microreact file is located
  subfolder_path <- dirname(file)
  
  # Write new metadata CSV use write.table instead of write_csv for compatibility
  output_csv <- file.path(subfolder_path, paste0("waid-", file_path_sans_ext(basename(file)), ".csv"))
  
  write.table(joined_df, file = output_csv, quote = FALSE, row.names = FALSE, sep = ",")
  
  message("Parsed and joined metadata saved to: ", output_csv)
  
  # Find original .microreact file in same subfolder
  mr_file <- list.files(subfolder_path, pattern = "\\.microreact$", full.names = TRUE)
  
  if (length(mr_file) == 1) {
    mr.file <- fromJSON(file = mr_file)
    
    # Update the 'metadata' section of the original file
    mr.file$files$metadata$name <- basename(output_csv)
    mr.file$files$metadata$blob <- readChar(output_csv, file.info(output_csv)$size)
    
    # Read the CSV content correctly and assign as string blob
    csv_content <- paste(readLines(output_csv), collapse = "\n")
    mr.file$files$metadata$blob <- csv_content
    
    new_mr_path <- file.path(subfolder_path, paste0("waid-", file_path_sans_ext(basename(mr_file)), ".microreact"))
    write(toJSON(mr.file), file = new_mr_path)
    
    message("Updated .microreact file saved to: ", new_mr_path)
  } else {
    warning("No or multiple .microreact files found in: ", subfolder_path)
  }
}

