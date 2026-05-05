#RENDERING Script
#This script renders the .qmd to an HTML file and saves the report with the taxa name and date


library(quarto)
library(dplyr)
library(knitr)
library(kableExtra)
library(tidyverse)
library(lubridate)
library(tools)
library(htmltools)
library(fs)
library(rjson)


#Before rendering remember to clear all your output folders: metadata_summ, microreact, and output_scripts

#Enter the param taxa
taxa <- "Klebsiella_pneumoniae"

#Enter the param methods either "all", "Snippy", or "Gubbins"
phylogeny<- "all"   

#Enter the patient address county (data will be filtered by patient address county ex. King) or "all"
PatientAddressCounty<- "all"   

#Add date
todays_date <- format(Sys.Date(), "%Y-%m-%d")

#Render the report
quarto_render(
  input = "BacteriaGenomicsReports.qmd",
  execute_params = list(
    taxa = taxa, 
    phylogeny = phylogeny,
    PatientAddressCounty = PatientAddressCounty),
  output_file = paste0("BacteriaGenomicsReports_",  
                       PatientAddressCounty, "_", 
                       taxa, "_", 
                       phylogeny, "_", 
                       todays_date, ".html")
)

