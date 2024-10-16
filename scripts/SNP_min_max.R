#SNP DISTANCES MIN AND MAX
#This script loads a long format distance file and outputs the MIN and MAX SNP distances

#Create a folder to save the outputs of this script
outputs_script_dir <- "outputs_scripts"
if (!dir.exists(outputs_script_dir)) {
  dir.create(outputs_script_dir)
}

#Check that the snp dist long files are loaded
if (!exists("snp_dist_long_list")) {
  stop("csv_list not found. Make sure the list of dataframes is loaded.")
}

#Create an empty list to store the results
summary_snp_minmax <- list()

#Iterate over each dataframe to calculate max and min
for (summary_Dists in names(snp_dist_long_list)) {
  
  #Extract the dataframe from the list
  df_snp <- snp_dist_long_list[[summary_Dists]]
  
  #Check if the dataframe has more than 4 rows
  if (nrow(df_snp) > 4) {
    snp_minmax <- df_snp %>%
      filter(ID1 != ID2) %>%
      filter(ID1 != "Reference") %>%
      filter(ID2 != "Reference") %>%
      summarize(
        MAX = max(dist, na.rm = TRUE),
        MIN = min(dist, na.rm = TRUE)
      )
   
    #Add the source file name as a column
      snp_minmax <- snp_minmax %>%
      mutate(Source = summary_Dists)%>%
      select(Source, MAX, MIN) 
    
    #Store the summary results
    summary_snp_minmax[[summary_Dists]] <-snp_minmax
    
    #Print the result for verification
    cat("Summary of Min and Max SNPs for", summary_Dists, ":\n")
    print(snp_minmax)
    cat("\n")
  } else {
    cat("Skipping", summary_Dists, "as it has 4 or fewer rows.\n")
  }
}

save(summary_snp_minmax, file = file.path(outputs_script_dir, "summary_snp_minmax.RData"))