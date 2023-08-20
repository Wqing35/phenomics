# get_online_paper_metrices
# devtools::install_github('Telogen/ASNJ')
setwd('/mdshare/node8/txmdata/Phenomics/')
library(ASNJ)
library(tidyverse)


online_data <- read.csv('./weekly_online_paper_metrices/SearchResults.csv')
online_data <- filter(online_data,Content.Type != 'Journal')

all_Phenomics_paper_metrics <- get_all_Phenomics_paper_metrics(online_data,sleep_seconds = 1)



head(all_Phenomics_paper_metrics)
sum(all_Phenomics_paper_metrics$access)
sum(all_Phenomics_paper_metrics$citation)
sum(all_Phenomics_paper_metrics$altmetric)

sys.time <- Sys.time() %>% as.character()
update_time <- strsplit(sys.time,' ')[[1]][1]

out_file_name <- paste0('./weekly_online_paper_metrices/output/',update_time,'.xlsx')
out_file_name

writexl::write_xlsx(all_Phenomics_paper_metrics,out_file_name)

writexl::write_xlsx(all_Phenomics_paper_metrics,
                    './weekly_online_paper_metrices/all_Phenomics_paper_metrics.xlsx')



