# plot
library(ggplot2)
library(ggpubr)
library(dplyr)
library(txmBioinfoToolkit)


plot_file_names <- list.files('./weekly_online_paper_metrices/output/')
plot_file_names

df <- pbmcapply::pbmclapply(plot_file_names,function(file_name){
  # file_name <- plot_file_names[1]
  full_file_name <- paste0('/mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/output/',file_name)
  out_df <- read.table(full_file_name,sep = '\t',header = T)
  total_access <- sum(out_df$access)
  total_citation <- sum(out_df$citation)
  total_altmetric <- sum(out_df$altmetric)
  
  out <- data.frame(total_access = total_access,
                   total_citation = total_citation,
                   total_altmetric = total_altmetric,
                   date = substr(file_name,1,10))
  colnames(out) <- c('Total access','Total citations','Total altmetrics','Date')
  return(out)
},mc.cores = 10) %>% do.call(what = rbind)


ggplot(df,aes(x = Date,y = `Total access`)) + 
  geom_point(stat = "identity") +
  geom_text(aes(label = `Total access`),hjust = -.2,vjust = -.2) + 
  geom_smooth(method = 'lm', se = F, color = 'red') +
  stat_cor(method = "spearman") +
  xlim(0,100000) +
  ylim(0,100000) 
  
  
ggplot(df,aes(x = Date,y = `Total citations`)) + 
  geom_point(stat = "identity") +
  geom_text(aes(label = `Total citations`),hjust = -.2,vjust = -.2) + 
  geom_smooth(method = 'lm', se = F, color = 'red') +
  stat_cor(method = "spearman") 

ggplot(df,aes(x = Date,y = `Total altmetrics`)) + 
  geom_point(stat = "identity") +
  geom_text(aes(label = `Total altmetrics`),hjust = -.2,vjust = -.2)


