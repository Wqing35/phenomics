# fig2
library(ggpubr)
library(aplot)
library(tidyverse)
setwd('/mdshare/node8/txmdata/Phenomics/')

plot_file_names <- list.files('./weekly_online_paper_metrices/output/')
plot_file_names

full_file_name <- paste0('./weekly_online_paper_metrices/output/',
                         rev(plot_file_names)[1])
out_df <- readxl::read_excel(full_file_name)
out_df <- filter(out_df,type != 'Correction')
three_known_types <- c('Article','Review','Protocol')
other_types <- setdiff(unique(out_df$type),three_known_types)
out_df$mytype <- factor(out_df$type,
                        levels = c(three_known_types,other_types),
                        labels = c('Article','Review','Protocol',rep('Others',length(other_types))))
table(out_df$mytype)
# 发文量
p1 <- ggplot(out_df,aes(x = year)) +
  geom_bar(width = 0.6, aes(fill = mytype)) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5,size = 4) +
  scale_x_continuous(breaks=c(2021,2022,2023)) +
  scale_fill_discrete(name = "Article type") +
  ggtitle('Published paper distribution') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12,color = 'black'),
        axis.text.y = element_text(size = 12,color = 'black'),
        plot.title = element_text(hjust = 0.5)) +
  ylab('Number')+
  ylim(0,52)

# citations
out_df$citation_range <- '0'
out_df[which(out_df$citation > 0 & out_df$citation <= 5),]$citation_range <- '1-5'
out_df[which(out_df$citation > 5 & out_df$citation <= 10),]$citation_range <- '6-10'
out_df[which(out_df$citation > 10 & out_df$citation <= 50),]$citation_range <- '11-50'

table(out_df$citation_range)
out_df$citation_range <- factor(out_df$citation_range,
                                levels = c('0','1-5','6-10','11-50','51-100','101-500','>500'))

p2 <- ggplot(out_df,aes(x = citation_range)) +
  geom_bar(width = 0.6, aes(fill = mytype)) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5,size = 4) +
  ggtitle("Citation distribution") +
  theme_bw() +
  xlab('Citation ranges') +
  ylab('Number') +
  theme(axis.text.x = element_text(size = 12,colour = 'black'),
        axis.text.y = element_text(size = 12,colour = 'black'),
        plot.title = element_text(hjust = 0.5)) +
  ylim(0,52)

fig2 <- ggpubr::ggarrange(plotlist = list(p1,p2),nrow = 1,ncol = 2,common.legend = T,legend = 'right')
fig2
ggsave('./figures/fig2.png',fig2,width = 12,height = 6)



