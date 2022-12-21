# 4_pred_IF

library(ggplot2)
library(stringr)
library(ggpubr)
library(aplot)
library(dplyr)
setwd('/mdshare/node8/tianlejin/Phenomics/')


# ref: https://zhuanlan.zhihu.com/p/138831192
# IF2023 = Citation2023/(N2022+N2021)


begin_day <- "2022-10-03"
end_day <- strsplit((Sys.time() %>% as.character()),' ')[[1]][1]
diff_day <- difftime(end_day, begin_day, units = c("days")) %>% as.numeric()
diff_day
time_fold <- 365/diff_day
time_fold

plot_file_names <- list.files('../Phenomics/weekly_online_paper_metrices/output/')
plot_file_names

begin_df <- readxl::read_excel('/mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/output/2022-10-03.xlsx')
begin_citation <- sum(begin_df$citation)
begin_citation

end_df <- readxl::read_excel(
  paste0('/mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/output/',
         rev(plot_file_names)[1]))
end_citation <- sum(end_df$citation)
end_citation

Citation2023 <- (end_citation - begin_citation) * time_fold

N2021_plus_2022 <- length(which(begin_df$type %in% c('Article','Review')))

Pred_IF <- round(Citation2023/N2021_plus_2022,3)

pred_if_df <- data.frame(time = end_day,Pred_IF = Pred_IF)

fig_IF <- ggplot(pred_if_df,aes(x = time,y = Pred_IF)) + 
  geom_point(stat = "identity") +
  geom_line(aes(group = 1)) +
  geom_text(aes(label = Pred_IF),hjust = .5,vjust = -.8) + 
  ylim((min(pred_if_df$Pred_IF) - 2),(max(pred_if_df$Pred_IF) + 2)) + 
  theme_light() + 
  ylab('Predicted IF') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12,color = 'black',angle = 45,vjust = 1,hjust = 1),
        axis.title.y = element_text(size = 14,color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = .5)) 

write.csv(pred_if_df,'./weekly_online_paper_metrices/pred_if_df.csv')

ggsave('./figures/fig_IF.png',fig_IF,width = 8,height = 3)

