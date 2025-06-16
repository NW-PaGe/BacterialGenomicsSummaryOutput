#SNP DISTANCES
#This script loads a long format distance file and outputs the MIN and MAX SNP distances and
#Outputs files with strong genomic linkages (0-10 SNPs) and intermediate linkages (11-50 SNPs) detected 
#among the new isolates and the rest in the same genomic cluster

# Create a folder to save the outputs of this script
outputs_script_dir <- "outputs_scripts"
if (!dir.exists(outputs_script_dir)) {
  dir.create(outputs_script_dir)
}

# Check that the snp dist long files are loaded
if (!exists("snp_dist_long_list")) {
  stop("snp_dist_long_list not found. Make sure the list of dataframes is loaded.")
}

# Extract TAXA from file names (defined outside the loop)
extract_taxa <- function(filename) {
  taxa <- str_extract(filename, "(?<=-)[A-Za-z_]+(?=-)")  # Handle underscore in taxa
  return(taxa)
}

# Define the taxa to filter
desired_taxa <- params$taxa

# SNP DISTANCES MIN AND MAX
# Create an empty list to store the results
summary_snp_minmax <- list()

# Iterate over each dataframe to calculate max and min
for (summary_Dists in names(snp_dist_long_list)) {
  
  # Extract TAXA from filename using the extract_taxa function
  taxa_in_file <- extract_taxa(summary_Dists)
  
  # Trim any leading/trailing spaces to ensure matching
  taxa_in_file <- str_trim(taxa_in_file)
  desired_taxa <- str_trim(desired_taxa)
  
  # Check if the taxa matches the desired taxa (case-insensitive comparison)
  if (grepl(desired_taxa, taxa_in_file, ignore.case = TRUE)) {
    
    # Extract the dataframe from the list
    df_snp <- snp_dist_long_list[[summary_Dists]]
    
    # Check if the dataframe has more than 4 rows and calculate max and min SNP distance
    if (nrow(df_snp) > 4) {
      snp_minmax <- df_snp %>%
        filter(ID1 != ID2) %>%
        filter(ID1 != "Reference") %>%
        filter(ID2 != "Reference") %>%
        summarize(
          MAX = max(dist, na.rm = TRUE),
          MIN = min(dist, na.rm = TRUE)
        )
      
      # Add the source file name as a column
      snp_minmax <- snp_minmax %>%
        mutate(Source = summary_Dists) %>%
        select(Source, MAX, MIN)
      
      # Store the summary results
      summary_snp_minmax[[summary_Dists]] <- snp_minmax
      
      # Print the result for verification
      cat("Summary of Min and Max SNPs for", summary_Dists, ":\n")
      print(snp_minmax)
      cat("\n")
    } else {
      cat("Skipping", summary_Dists, "as it has 4 or fewer rows.\n")
    }
  } else {
    cat("Skipping", summary_Dists, "as it does not match the desired taxa.\n")
  }
}

save(summary_snp_minmax, file = file.path(outputs_script_dir, "summary_snp_minmax.RData"))

#SNP DISTANCES STRONG AND INTERMEDIATE GENOMIC LINKAGES

#Identify new isolates using the summary_tsv file
isolates_run_summ<-summary_tsv_cleaned %>% 
  filter(ID!="Reference") %>%
  select(ID, STATUS)

#Empty list to store results
summary_snp_linkages <- list()

#Iterate over each dataframe to identify strong and intermediate genomic linkages
for (summary_Linkages in names(snp_dist_long_list)) {
  
  # Extract TAXA from filename
  taxa_in_file <- extract_taxa(summary_Linkages)
  
  # Trim any leading/trailing spaces to ensure matching
  taxa_in_file <- str_trim(taxa_in_file)
  desired_taxa <- str_trim(desired_taxa)
  
  # Check if the taxa matches the desired taxa (case-insensitive comparison)
  if (grepl(desired_taxa, taxa_in_file, ignore.case = TRUE)) {
  
  #Extract the dataframes from list
  df_links <- snp_dist_long_list[[summary_Linkages]]
  
  if (nrow(df_links) > 4) {
    snp_links <- df_links %>%
    filter(ID1 != ID2) %>%
    filter(ID1 != "Reference") %>%
    filter(ID2 != "Reference") %>%
    mutate(ID1 = case_when(str_starts(ID1, "WA") ~ substr(ID1, 1, 9),
        str_starts(ID1, regex("[0-9]{4}")) ~ substr(ID1, 1, 12),
        str_ends(ID1, "_T1") ~ str_remove(ID1, "_T1"),
        TRUE ~ ID1)) %>% 
     mutate(ID2 = case_when(str_starts(ID2, "WA") ~ substr(ID2, 1, 9),
        str_starts(ID2, regex("[0-9]{4}")) ~ substr(ID2, 1, 12),
        str_ends(ID2, "_T1") ~ str_remove(ID2, "_T1"),
        TRUE ~ ID2)) %>%
    mutate(VeryStrongGenLinkage= ifelse(dist >=0 & dist<=5, ID2, ""),
           StrongGenLinkage= ifelse(dist >=6 & dist<=10, ID2, ""),
           InterGenLinkage= ifelse(dist >=11 & dist<=50, ID2, "")) %>%
    filter(VeryStrongGenLinkage != "" | StrongGenLinkage != "" | InterGenLinkage != "") %>%
    mutate(Source = summary_Linkages) %>%
    select(Source, ID1, VeryStrongGenLinkage, StrongGenLinkage, InterGenLinkage)
    
    snp_links <- snp_links %>%
    left_join(isolates_run_summ, by = c("ID1" = "ID"))
    
    #Store the summary results
    summary_snp_linkages[[summary_Linkages]] <-snp_links
    
    #Print the result for verification
    cat("Summary of Linkages SNPs for", summary_Linkages, ":\n")
    print(snp_links)
    cat("\n")
  } else {
    cat("Skipping", summary_Linkages, "as it has 4 or fewer rows.\n")
  }
  } else {
    cat("Skipping", summary_Linkages, "as it does not match the desired taxa.\n")
  }
}

save(summary_snp_linkages, file = file.path(outputs_script_dir, "summary_snp_linkages.RData"))