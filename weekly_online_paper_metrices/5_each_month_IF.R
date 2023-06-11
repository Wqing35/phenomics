# 5_each_month_IF


library(ggplot2)
library(stringr)
library(ggpubr)
library(aplot)
library(dplyr)
library(Seurat)
setwd('/mdshare/node8/txmdata/Phenomics/')

plot_file_names <- list.files('./weekly_online_paper_metrices/output/')
plot_file_names

month_list <- list(Jan = c('2022-12-26','2023-01-30'),
                   Feb = c('2023-01-30','2023-02-27'),
                   Mar = c('2023-02-27','2023-03-26'),
                   Apr = c('2023-03-26','2023-04-30'),
                   May = c('2023-04-30','2023-05-29'),
                   Jun = c(),
                   Jul = c(),
                   Aug = c(),
                   Sep = c(),
                   Oct = c(),
                   Nov = c(),
                   Dec = c())

each_month_pred_if <- sapply(names(month_list),function(month){
  print(month)
  # month <- 'Jan'
  begin_day <- month_list[[month]][1]
  if(!is.null(begin_day)){
    begin_df <- readxl::read_excel(paste0('./weekly_online_paper_metrices/output/',begin_day,'.xlsx'))
    begin_citation <- sum(begin_df$citation)
    begin_citation
    
    end_day <- month_list[[month]][2]
    end_df <- readxl::read_excel(paste0('./weekly_online_paper_metrices/output/',end_day,'.xlsx'))
    end_citation <- sum(end_df$citation)
    end_citation
    print(end_citation - begin_citation)
    this_month_if <- (end_citation - begin_citation)/77
  } else{
    this_month_if <- NA
  }
  return(this_month_if)
})

each_month_pred_if_df <- data.frame(each_month_pred_if)
each_month_pred_if_df$Month <- factor(rownames(each_month_pred_if_df),levels = rownames(each_month_pred_if_df))

Monthly_cumulative_impact_factor_vector <- c()
Monthly_cumulative_impact_factor <- c()
for(month in rownames(each_month_pred_if_df)){
  print(month)
  Monthly_cumulative_impact_factor_vector <- c(Monthly_cumulative_impact_factor_vector,each_month_pred_if_df[month,'each_month_pred_if'])
  Monthly_cumulative_impact_factor <- c(Monthly_cumulative_impact_factor,sum(Monthly_cumulative_impact_factor_vector))
}
each_month_pred_if_df$Monthly_cumulative_impact_factor <- Monthly_cumulative_impact_factor
each_month_pred_if_df
each_month_IF <- ggplot(each_month_pred_if_df,aes(x = Month)) + 
  geom_bar (aes(y = each_month_pred_if,fill = Month),stat = "identity") + 
  geom_text(aes(y = each_month_pred_if,label = round(each_month_pred_if,2)),vjust = -0.8) + 
  geom_line(aes(y = Monthly_cumulative_impact_factor,group = 1)) + 
  geom_point(aes(y = Monthly_cumulative_impact_factor)) + 
  geom_text(aes(y = Monthly_cumulative_impact_factor, label = round(Monthly_cumulative_impact_factor,2)),vjust = -0.8) + 
  theme_bw() + 
  NoLegend() + 
  ylim(0,1) + 
  ylab('Monthly Impact Factor') + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12,color = 'black',angle = 45,vjust = 1,hjust = 1),
        axis.title.y = element_text(size = 14,color = 'black'),
        axis.text.y = element_text(size = 10,color = 'black'),
        plot.title = element_text(hjust = .5)) 
each_month_IF
  
ggsave('./figures/each_month_IF.png',each_month_IF,width = 8,height = 3)



