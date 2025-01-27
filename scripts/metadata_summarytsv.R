##METADATA BY GENETIC CLUSTER
#This script summarizes the metadata information by genetic cluster

#Create a folder to save the mapping of sequencing ID and WDRS Case ID
outputs_script_dir <- "outputs_scripts"
if (!dir.exists(outputs_script_dir)) {
  dir.create(outputs_script_dir)
}


#Create a folder to save the metadata summaries for the metadata table
results_dir <- "metadata_summ"
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
}

#Wrangle the data
#Select needed columns from summary_tsv
current_run_summary<-summary_tsv_cleaned %>% 
  filter(ID!="Reference") %>%
  select(ID, 
         STATUS,
         TAXA,
         CLUSTER,
         ISO_IN_CLUSTER,
         ISO_PASS_QC)

#Identify observations in the current run that don't have a WA ID
notwaids_current_run<-current_run_summary %>% 
  filter(!str_starts(ID,"WA"))

#Identify observations in the current run that are left once those without WA ID are filtered out
remaining_current_run<- anti_join(current_run_summary, notwaids_current_run, by='ID') %>% 
  rename(WA_ID = 'ID')

#Wrangle historical metadata
historical_metadata_summ<-historical_metadata %>%
  mutate(SpecimenDateCollected=as.Date(CollectionDate, format = "%Y-%m-%d")) %>%
  mutate(PatientBirthDate=as.Date(DOB, format = "%Y-%m-%d")) %>% 
  select(ID,
         WA_ID,
         CASE_ID,
         SpecimenDateCollected,
         PatientFirstName,
         PatientLastName,
         PatientBirthDate,
         PatientAddressCounty,
         SubmitterCounty,
         SubmitterName,
         SpecimenSource)

#Add the historical metadata to observations that don't have WA IDs in the current run
current_run_histmetadata<-left_join(notwaids_current_run, historical_metadata_summ, by='ID') %>% 
  mutate(ID = ifelse(!is.na(WA_ID), WA_ID, ID)) %>%
  select(ID,
         STATUS,
         CASE_ID,
         TAXA,
         CLUSTER,
         ISO_IN_CLUSTER,
         ISO_PASS_QC,
         SpecimenDateCollected,
         PatientFirstName,
         PatientLastName,
         PatientBirthDate,
         PatientAddressCounty,
         SubmitterCounty,
         SubmitterName,
         SpecimenSource) %>%
  unique()

#Wrangle metadata from new tracker
bacteriamastermeta_summ<-wabacteriamaster_meta_df %>%
  mutate(SpecimenDateCollected=as.Date(SpecimenDateCollected, format = "%Y-%m-%d")) %>%
  mutate(PatientBirthDate=as.Date(PatientBirthDate, format = "%Y-%m-%d")) %>% 
  select(PHLAccessionNumber,
         CASE_ID,
         SpecimenDateCollected,
         PatientFirstName,
         PatientLastName,
         PatientBirthDate,
         PatientAddressCounty,
         SubmitterCounty,
         SubmitterName,
         SpecimenSource)


#Add the metadata to the selected information from the summary tsv
current_run_nonhist_metadata<-left_join(remaining_current_run, bacteriamastermeta_summ, by=c('WA_ID'='PHLAccessionNumber')) %>% 
  rename(ID = WA_ID) %>% 
  select(ID,
         STATUS,
         CASE_ID,
         TAXA,
         CLUSTER,
         ISO_IN_CLUSTER,
         ISO_PASS_QC,
         SpecimenDateCollected,
         PatientFirstName,
         PatientLastName,
         PatientBirthDate,
         PatientAddressCounty,
         SubmitterCounty,
         SubmitterName,
         SpecimenSource)

# Append the two dataframes
current_run_metadata <- rbind(current_run_nonhist_metadata, current_run_histmetadata)

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
  min_date <- format(min(df$SpecimenDateCollected, na.rm = TRUE), format = "%m-%d-%Y")
  max_date <- format(max(df$SpecimenDateCollected, na.rm = TRUE), format = "%m-%d-%Y")

  #Extract ID and WA_ID of the new isolates
  new_IDs <- df %>%
    filter(STATUS == "NEW", !is.na(ID)) %>%
    select(ID)
  
  #Extract ID and WA_ID of all isolates
  all_ids <- df %>%
    filter(!is.na(ID)) %>%
    select(ID)
  
  #Extract unique counties from Submitter County
  all_counties <- unique(na.omit(df$SubmitterCounty))
  
  #Extract unique counties where STATUS is NEW
  new_counties <- unique(na.omit(df %>%
                           filter(STATUS == "NEW") %>%
                           select(SubmitterCounty))) %>%
                           unlist()
  
  #Extract unique submitting facilities' names from Submitter facility
  all_names <- unique(na.omit(df$SubmitterName))
  
  #Extract unique submitting facilities' names where STATUS is NEW
  new_names <- unique(na.omit(df %>%
                                   filter(STATUS == "NEW") %>%
                                   select(SubmitterName))) %>%
                                   unlist()
  
  
  #Identify isolates from cases with the same DOB and extract their ID and WA_ID
  same_dob <- df %>%
    group_by(PatientBirthDate) %>%
    filter(n() > 1 & !is.na(PatientBirthDate)) %>%
    select(ID, PatientBirthDate)%>%
    summarise(IDs = paste(ID, collapse = ", "))
  
  #Format the duplicate DOB results
  if (nrow(same_dob) > 0) {
    duplicate_dob_str <- paste(
      paste0("DOB: ",  same_dob$PatientBirthDate, " IDs: ",  same_dob$IDs),
      collapse = paste0(";\n")
    )
  } else {
    duplicate_dob_str <- "No isolates from the same case"
  }
  
  
  #Limit All_IDs to no more than 10
  #If the run only has WA IDs comment out the two lines (140,144) all_ids$ID
  if (nrow(all_ids) > 10) {
    all_ids_str <- paste(paste(all_ids$ID[1:10], 
                               collapse = "; "), "Limited output to 10 IDs, but there are more isolates in this genetic cluster")
  } else {
    all_ids_str <- paste(all_ids$ID, 
                         collapse = "; ")
  }
  
  #Create a data frame for the combined results
  combined_df <- data.frame(
    Min_CollDate = min_date,
    Max_CollDate = max_date,
    All_Counties = paste(all_counties, collapse = ", "),
    New_Counties = paste(new_counties, collapse = ", "),
    All_Facilities = paste(all_names, collapse = ", "),
    New_Facilities = paste(new_names, collapse = ", "),
    stringsAsFactors = FALSE
  )
  
  #Attach New Status Data and All IDs as separate columns
  #If the run only has WA IDs comment out the line new_IDs$ID
  combined_df$All_IDs <- all_ids_str 
  combined_df$New_IDs <- paste(new_IDs$ID, 
                               collapse = "; ")
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


#Mapping of sequencing ID and CASE_ID
mapping_case_ID<-current_run_metadata%>% 
  filter(STATUS=="NEW") %>% 
  mutate(CASE_ID=as.character(CASE_ID)) %>% 
  select(ID,
         CASE_ID)

#Remove row names
rownames(mapping_case_ID) <- NULL

save(mapping_case_ID, file = file.path(outputs_script_dir, "mapping_case_ID.RData"))