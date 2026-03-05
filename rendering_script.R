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


#Enter the param taxa
taxa <- "Shigella_flexneri"

#Enter the param methods either "all", "Snippy", or "Gubbins"
phylogeny<- "all"   


#Add date
todays_date <- format(Sys.Date(), "%Y-%m-%d")

#Render the report
quarto_render(
  input = "BacteriaGenomicsReports.qmd",
  execute_params = list(taxa = taxa, phylogeny = phylogeny),
  output_file = paste0("BacteriaGenomicsReports_", taxa, "_", todays_date, ".html")
)

