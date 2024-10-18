##METADATA BY GENETIC CLUSTER
#This script summarizes the metadata information by genetic cluster

#Create a folder to save the metadata summaries
results_dir <- "metadata_summ"
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
}

#Wrangle the data
current_run_summary<-summary_tsv %>% 
  filter(ID!="Reference") %>% 
  select(ID, 
         STATUS,
         TAXA,
         CLUSTER,
         ISO_IN_CLUSTER,
         ISO_PASS_QC)

bacteriamastermeta_summ<-wabacteriamaster_meta_df %>% 
  mutate(BigBacter_Status='STATUS') %>% 
  mutate(CollectionDate=format(as.Date(CollectionDate,"%Y-%m-%d"),"%m-%d-%Y")) %>%
  mutate(DOB=format(as.Date(DOB,"%Y-%m-%d"),"%m-%d-%Y")) %>%
  select(ID,
         WA_ID,
         CASE_ID,
         BigBacter_Status, 
         SPECIES,
         CollectionDate,
         PatientFirstName,
         PatientLastName,
         DOB,
         PatientAddressCounty,
         SubmitterCounty,
         SubmitterName,
         SpecimenSource)

#Add the metadata to the selected information from the summary tsv so the resulting dataframe only pertains
#to the current run
current_run_metadata<-left_join(current_run_summary, bacteriamastermeta_summ, by='ID') %>% 
  select(ID,
         WA_ID,
         CASE_ID,
         TAXA,
         SPECIES,
         STATUS,
         BigBacter_Status, 
         CLUSTER,
         ISO_IN_CLUSTER,
         ISO_PASS_QC,
         CollectionDate,
         PatientFirstName,
         PatientLastName,
         DOB,
         PatientAddressCounty,
         SubmitterCounty,
         SubmitterName,
         SpecimenSource)

#Group the df by species and then by cluster and split into different dfs
metadata_grouped <- current_run_metadata %>% 
  group_by(TAXA, CLUSTER) %>% 
  group_split()

#Name the dfs using TAXA and CLUSTER
unique_names <- unique(current_run_metadata %>% 
                         select(TAXA, CLUSTER))

names(metadata_grouped) <- paste(unique_names$TAXA, unique_names$CLUSTER, sep = "_")


#Create separate data frames for each cluster and assign them to the global environment
for (i in seq_along(metadata_grouped)) {
  df_name <- paste(unique_names$TAXA[i], unique_names$CLUSTER[i], sep = "_")
  assign(df_name, metadata_grouped[[i]], envir = .GlobalEnv)  
}


#Calculate Min and Max Collection Date and identify ID and WA_ID where STATUS is NEW for each df
results <- lapply(metadata_grouped, function(df) {
  
  #Calculate min and max collection dates
  min_date <- min(df$CollectionDate, na.rm = TRUE)
  max_date <- max(df$CollectionDate, na.rm = TRUE)
  
  #Extract ID and WA_ID of the new isolates
  new_IDs <- df %>%
    filter(STATUS == "NEW", !is.na(ID), !is.na(WA_ID)) %>%
    select(ID, WA_ID)
  
  #Extract ID and WA_ID of all isolates
  all_ids <- df %>%
    filter(!is.na(ID), !is.na(WA_ID)) %>%
    select(ID, WA_ID)
  
  #Extract unique counties from Submitter County
  all_counties <- unique(na.omit(df$SubmitterCounty))
  
  #Extract unique counties where STATUS is NEW
  new_counties <- unique(na.omit(df %>%
                           filter(STATUS == "NEW") %>%
                           select(SubmitterCounty)))
  
  #Identify isolates from cases with the same DOB and extract their ID and WA_ID
  same_dob <- df %>%
    group_by(DOB) %>%
    filter(n() > 1 & !is.na(DOB)) %>%
    select(ID, WA_ID, DOB)
  
  #Format the duplicate DOB results
  if (nrow(same_dob) > 0) {
    duplicate_dob_str <- paste(same_dob$ID, same_dob$WA_ID, collapse = "; ")
  } else {
    duplicate_dob_str <- "No isolates from the same case"
  }
  
  
  
  #Limit All_IDs to no more than 10
  if (nrow(all_ids) > 10) {
    all_ids_str <- paste(paste(all_ids$ID[1:10], all_ids$WA_ID[1:10], collapse = "; "), "Limited output to 10 IDs, but there are more isolates in this genetic cluster")
  } else {
    all_ids_str <- paste(all_ids$ID, all_ids$WA_ID, collapse = "; ")
  }
  
  #Create a data frame for the combined results
  combined_df <- data.frame(
    Min_CollDate = min_date,
    Max_CollDate = max_date,
    All_Counties = paste(all_counties, collapse = ", "),
    New_Counties = paste(new_counties, collapse = ", "),
    stringsAsFactors = FALSE
  )
  
  #Attach New Status Data and All IDs as separate columns
  combined_df$All_IDs <- all_ids_str 
  combined_df$New_IDs <- paste(new_IDs$ID, new_IDs$WA_ID, collapse = "; ")
  combined_df$ISOs_SameCase = duplicate_dob_str

  
  return(combined_df)
})

#Combine results into single dataframes and save as .RData files
for (name in names(metadata_grouped)) {
  combined_df <- results[[name]]
  
  #Define name for the output file
  output_name <- paste(metadata_grouped[[name]]$TAXA[1], metadata_grouped[[name]]$CLUSTER[1], sep = "_")
  
  save(combined_df, file = file.path(results_dir, paste0(output_name, ".RData")))
}