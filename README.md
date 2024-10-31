# Bacterial Genomics Summary Output

This is a Quarto project that uses multiple R scripts to load and wrangle the output files from BigBacter runs wiht the goal of producing an HTML summary report.
This project does not conduct genomic analysis but it provides an overview of the categorization of new sequenced isolates in genomic clusters.

## Rendering

Before rendering the report double check the following:
1. The project expects a paths.txt file which should contain two file paths pointing to the location of the BigBacter folder with the latest outputs `Ran Through BigBacter` and in the second line a path to the file containing metadata information `wa-bacteria-master-metadata.csv`
2. If you have rendered the report previously, please delete the folders `metadata_summ` and `outputs_scripts`. Otherwise, the report will render by adding new information to the outputs saved in these files from the last time it was rendered.
