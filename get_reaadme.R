# get_reaadme

plot_file_names <- list.files('./weekly_online_paper_metrices/output/')
file_name <- plot_file_names[1]
file_name
full_file_name <- paste0('/mdshare/node8/txmdata/test/Phenomics/weekly_online_paper_metrices/output/',file_name)
out_df <- read.table(full_file_name,sep = '\t',header = T)
out_df$update_time <- NULL
knitr::kable(out_df)


