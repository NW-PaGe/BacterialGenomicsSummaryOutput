# This script looks up where new samples have been classified in terms of partition and counts then number of isolates in each partition
# The script compares partitions in both Snippy and Gubbins outputs, if they are the same one table with single columns gets generated otherwise
# columns for Snippy and Gubbins partitions and counts get output.

taxa_filter <- params$taxa

### Load SNIPPY and GUBBINS Files

# Load Snippy metadata
snippy_file <- list.files(
  path = "microreact",
  pattern = paste0("waid-parsed_", taxa_filter, ".*snippy_metadata\\.csv$"),
  recursive = TRUE,
  full.names = TRUE
)


# Load Gubbins metadata
gubbins_file <- list.files(
  path = "microreact",
  pattern = paste0("waid-parsed_", taxa_filter, ".*gubbins_metadata\\.csv$"),
  recursive = TRUE,
  full.names = TRUE
)


# Extract Snippy partitions

if (length(snippy_file) > 0) {
  
  snippy_meta <- read_csv(snippy_file, show_col_types = FALSE) %>%
    mutate(ID_clean = str_remove(ID, "_T[0-9]+$"))
  
  snippy_new_partition <- snippy_meta %>%
    filter(STATUS == "NEW") %>%
    select(ID_clean, CLUSTER, PARTITION) %>%
    distinct()
  
  snippy_partition_counts <- snippy_meta %>%
    filter(
      PARTITION %in% snippy_new_partition$PARTITION,
      ID_clean != "Reference"
    ) %>%
    group_by(CLUSTER, PARTITION) %>%
    summarise(PARTITION_Snippy_Counts = n(), .groups = "drop")
  
  snippy_partition_info <- snippy_new_partition %>%
    rename(PARTITION_Snippy = PARTITION) %>%
    left_join(snippy_partition_counts,
              by = c("CLUSTER", "PARTITION_Snippy" = "PARTITION"))
  
} else {
  
  snippy_partition_info <- data.frame(
    ID_clean = character(),
    PARTITION_Snippy = character(),
    PARTITION_Snippy_Counts = integer()
  )
}


# Extract Gubbins partitions
if (length(gubbins_file) > 0) {
  
  gubbins_meta <- read_csv(gubbins_file, show_col_types = FALSE) %>%
    mutate(ID_clean = str_remove(ID, "_T[0-9]+$"))
  
  gubbins_new_partition <- gubbins_meta %>%
    filter(STATUS == "NEW") %>%
    select(ID_clean, CLUSTER, PARTITION) %>%
    distinct()
  
  gubbins_partition_counts <- gubbins_meta %>%
    filter(
      PARTITION %in% gubbins_new_partition$PARTITION,
      ID_clean != "Reference"
    ) %>%
    group_by(CLUSTER, PARTITION) %>%
    summarise(PARTITION_Gubbins_Counts = n(), .groups = "drop")
  
  gubbins_partition_info <- gubbins_new_partition %>%
    rename(PARTITION_Gubbins = PARTITION) %>%
    left_join(gubbins_partition_counts,
              by = c("CLUSTER", "PARTITION_Gubbins" = "PARTITION"))
  
} else {
  
  
  gubbins_partition_info <- data.frame(
    ID_clean = character(),
    PARTITION_Gubbins = character(),
    PARTITION_Gubbins_Counts = integer()
  )
}

# Combine Snippy and Gubbins outputs

combined_partition_info <- full_join(
  snippy_partition_info,
  gubbins_partition_info,
  by = "ID_clean"
)

expected_cols <- c(
  "ID_clean",
  "PARTITION_Snippy",
  "PARTITION_Snippy_Counts",
  "PARTITION_Gubbins",
  "PARTITION_Gubbins_Counts"
)

for (col in expected_cols) {
  if (!col %in% colnames(combined_partition_info)) {
    combined_partition_info[[col]] <- NA
  }
}

save(combined_partition_info,
     file = file.path("outputs_scripts", "partition_info.RData"))