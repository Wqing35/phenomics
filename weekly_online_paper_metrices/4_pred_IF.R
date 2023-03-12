# 4_pred_IF

library(ggplot2)
library(stringr)
library(ggpubr)
library(aplot)
library(dplyr)
setwd('/mdshare/node8/txmdata/Phenomics/')


# ref: https://zhuanlan.zhihu.com/p/138831192
# IF2023 = Citation2023/(N2022+N2021)

plot_file_names <- list.files('./weekly_online_paper_metrices/output/')
plot_file_names

begin_day <- "2022-12-26"
end_day <- str_sub(rev(plot_file_names)[1],1,10)
diff_day <- difftime(end_day, begin_day, units = c("days")) %>% as.numeric()
diff_day
time_fold <- 365/diff_day
time_fold


begin_df <- readxl::read_excel('./weekly_online_paper_metrices/output/2022-12-26.xlsx')
begin_citation <- sum(begin_df$citation)
begin_citation

end_df <- readxl::read_excel(
  paste0('./weekly_online_paper_metrices/output/',
         rev(plot_file_names)[1]))
end_citation <- sum(end_df$citation)
end_citation

Citation2023 <- (end_citation - begin_citation) * time_fold

N2021_plus_2022 <- length(which(begin_df$type %in% c('Article','Review')))

Pred_IF <- round(Citation2023/N2021_plus_2022,3)
Pred_IF

pred_if_df <- data.frame(day = end_day,Pred_IF = Pred_IF)
pred_if_df


pred_if_df_orig <- read.csv('./weekly_online_paper_metrices/pred_if_df.csv')
pred_if_df_orig
pred_if_df <- rbind(pred_if_df_orig,pred_if_df)
pred_if_df

write.csv(pred_if_df,'./weekly_online_paper_metrices/pred_if_df.csv',row.names = F)


fig_IF <- ggplot(pred_if_df,aes(x = day,y = Pred_IF)) + 
  geom_point(stat = "identity") +
  geom_line(aes(group = 1)) +
  geom_text(aes(label = Pred_IF),hjust = .5,vjust = -.8) + 
  ylim((min(pred_if_df$Pred_IF) - 0.5),(max(pred_if_df$Pred_IF) + 0.5)) + 
  theme_light() + 
  ylab('Predicted IF') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12,color = 'black',angle = 45,vjust = 1,hjust = 1),
        axis.title.y = element_text(size = 14,color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = .5)) 
fig_IF

ggsave('./figures/fig_IF.png',fig_IF,width = 8,height = 3)

