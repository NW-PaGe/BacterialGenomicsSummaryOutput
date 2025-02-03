
# Define input file path
taxa_file <- "outputs_scripts/results_by_taxa.csv"

#Check if the file exists before reading
if (!file.exists(results_by_taxa)) {
  stop("Error: Taxa file not found: ", results_by_taxa)
}

#Load the taxa dataset
results_by_taxa <- read.csv("results_by_taxa.csv", stringsAsFactors = FALSE)

#Check that TAXA column exists
if (!"TAXA" %in% colnames(results_by_taxa)) {
  stop("Error: The TAXA column is missing in results_by_taxa.csv")
}

#Get the unique taxa list, removing any NA or empty values
taxa_list <- unique(na.omit(results_by_taxa$TAXA))
taxa_list <- taxa_list[taxa_list != ""]  #Remove empty strings

#Check if there are taxa to process
if (length(taxa_list) == 0) {
  stop("Error: No valid taxa found in results_by_taxa.csv")
}

#Create output directory
output_dir <- "species_reports"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Generate reports for each taxa
for (sp in taxa_list) {
  # Clean taxa name for file output (remove special characters)
  clean_sp <- gsub("[^a-zA-Z0-9_]", "_", sp)  # Replace special chars with '_'
  
  output_file <- file.path(output_dir, paste0("report_", clean_sp, ".html"))
  
  quarto_render(input = "BacteriaGenomicsSummaryOutput/BacteriaGenomicsReports.qmd",
                output_file = output_file,
                execute_params = list(taxa = sp))
  
  message("Report generated for: ", sp)
}