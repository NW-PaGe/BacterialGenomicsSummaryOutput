#OVERVIEW BIGBACTER RESULTS
#This script uses the summary.tsv file to output a summary of the isolates (all and new) by genomic cluster and species

#Create a folder to save the outputs of this script
outputs_script_dir <- "outputs_scripts"
if (!dir.exists(outputs_script_dir)) {
  dir.create(outputs_script_dir)
}

##OVERVIEW NEW SEQUENCES
#Identify new results to get an overview of the new sequences
newseq<-summary_tsv_cleaned %>%
  filter(STATUS=="NEW", TAXA == params$taxa) %>%
  mutate(GENOMIC_CLUSTER=CLUSTER) %>% 
  select(ID,
         QUAL,
         TAXA,
         GENOMIC_CLUSTER,
         ISO_IN_CLUSTER,
         ISO_PASS_QC)

save(newseq, file = file.path(outputs_script_dir, "newseq.RData"))

##RECOMBINATION
#Data wrangling
recomb<-summary_tsv_cleaned %>%  
  filter(TAXA == params$taxa) %>% 
  mutate(GENOMIC_CLUSTER=CLUSTER)%>% 
  filter(ISO_IN_CLUSTER!= 1 & ISO_IN_CLUSTER!=2) %>% 
  filter(QUAL!="FAIL") %>% 
  mutate(RECOMB= as.numeric(RECOMB))

#Calculate % recombination
recomb_calc <- recomb %>%
  mutate("%Recomb" = round((RECOMB / LENGTH) * 100,3))

#Summarize results by taxa, cluster, and max recombination detected in that genomic cluster
recomb_summary <- recomb_calc %>%
  filter(!is.na("%Recomb")) %>% 
  group_by(TAXA, GENOMIC_CLUSTER) %>%
  summarize('MAX_%Recomb_Detected'= max(`%Recomb`, na.rm = TRUE))

save(recomb_summary, file = file.path(outputs_script_dir, "recomb_summary.RData"))


##FAILED SEQUENCES
#Data wrangling
failed<-summary_tsv_cleaned %>% 
  filter(QUAL=="FAIL", TAXA == params$taxa) %>% 
  mutate(GENOMIC_CLUSTER=CLUSTER) %>% 
  select(ID,
         STATUS,
         TAXA,
         GENOMIC_CLUSTER,
         LOWCOV, 
         PER_GENFRAC,
         PER_LOWCOV,
         PER_HET)

save(failed, file = file.path(outputs_script_dir, "failed.RData"))