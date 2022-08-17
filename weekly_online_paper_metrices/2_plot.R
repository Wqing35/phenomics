# plot
plot_file_names <- list.files('./weekly_online_paper_metrices/output/')
plot_file_names

df <- pbmcapply::pbmclapply(plot_file_names,function(file_name){
  file_name <- plot_file_names[1]
  full_file_name <- paste0('/mdshare/node8/txmdata/test/Phenomics/weekly_online_paper_metrices/output/',file_name)
  out_df <- read.table(full_file_name,sep = '\t',header = T)
  total_access <- sum(out_df$access)
  total_citation <- sum(out_df$citation)
  total_altmetric <- sum(out_df$altmetric)
  
  
},mc.cores = 10) %>% do.call(what = rbind)

# test
library(ggplot2)
ggplot(out_df) +
  geom_boxplot(aes(y = access)) + 
  geom_boxplot(aes(y = citation)) + 
  geom_boxplot(aes(y = altmetric)) 


knitr::kable(out_df)


