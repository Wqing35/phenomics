# get_online_paper_metrices
devtools::install_github('Telogen/ASNJ')

library(ASNJ)
library(dplyr)
library(stringr)
library(ggplot2)


online_data <- read.csv('../Phenomics/weekly_online_paper_metrices/SearchResults.csv')
online_data <- filter(online_data,Content.Type != 'Journal')

all_Phenomics_paper_metrics <- get_all_Phenomics_paper_metrics(online_data)



head(all_Phenomics_paper_metrics)
sum(all_Phenomics_paper_metrics$access)
sum(all_Phenomics_paper_metrics$citation)

sys.time <- Sys.time() %>% as.character()
update_time <- strsplit(sys.time,' ')[[1]][1]

out_file_name <- paste0('./weekly_online_paper_metrices/output/',update_time,'.xlsx')
out_file_name

writexl::write_xlsx(all_Phenomics_paper_metrics,out_file_name)




