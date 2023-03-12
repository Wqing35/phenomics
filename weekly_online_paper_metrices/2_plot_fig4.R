# fig4
library(ggplot2)
library(stringr)
library(ggpubr)
library(aplot)
library(dplyr)
setwd('/mdshare/node8/txmdata/Phenomics/')

plot_file_names <- list.files('./weekly_online_paper_metrices/output/')
plot_file_names <- rev(plot_file_names)[1:15]

df <- pbmcapply::pbmclapply(plot_file_names,function(file_name){
  # file_name <- rev(plot_file_names)[1]
  file_name
  full_file_name <- paste0('./weekly_online_paper_metrices/output/',file_name)
  if(str_detect(file_name,'xlsx')){
    out_df <- readxl::read_excel(full_file_name)
  }else{
    out_df <- read.table(full_file_name,sep = '\t',header = T)
  }
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


access_plot <- ggplot(df,aes(x = Date,y = `Total access`)) + 
  geom_point(stat = "identity") +
  geom_line(aes(group = 1)) +
  geom_text(aes(label = `Total access`),hjust = .5,vjust = -.8) + 
  ylim((min(df$`Total access`) - 1000),(max(df$`Total access`) + 6000)) + 
  theme_light() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14,color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = .5)) 

citations_plot <- ggplot(df,aes(x = Date,y = `Total citations`)) + 
  geom_point(stat = "identity") +
  geom_line(aes(group = 1)) +
  geom_text(aes(label = `Total citations`),hjust = .5,vjust = -.8) + 
  ylim((min(df$`Total citations`) - 2),(max(df$`Total citations`) + 10)) + 
  theme_light() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14,color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = .5)) 

altmetrics_plot <- ggplot(df,aes(x = Date,y = `Total altmetrics`)) + 
  geom_point(stat = "identity") +
  geom_line(aes(group = 1)) +
  geom_text(aes(label = `Total altmetrics`),hjust = .5,vjust = -.8) + 
  ylim((min(df$`Total altmetrics`) - 2),(max(df$`Total altmetrics`) + 5)) + 
  theme_light() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12,color = 'black',angle = 45,vjust = 1,hjust = 1),
        axis.title.y = element_text(size = 14,color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = .5)) 


fig4 <- access_plot %>% 
  insert_bottom(citations_plot, height = 1) %>% 
  insert_bottom(altmetrics_plot, height = 1)

ggsave('./figures/fig4.png',fig4,width = 12,height = 6)


