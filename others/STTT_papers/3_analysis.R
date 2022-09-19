# 3_analysis
library(dplyr,verbose = F,warn.conflicts = F)
library(ggplot2,verbose = F,warn.conflicts = F)
library(txmBioinfoToolkit,verbose = F,warn.conflicts = F)

out_df <- readRDS('./others/STTT_papers/STTT_all_papers_info_df.RDS')
head(out_df)

# View(out_df)
out_df$IF <- NA
out_df[which(out_df$year == 2016),]$IF <- 0
out_df[which(out_df$year == 2017),]$IF <- 5.65
out_df[which(out_df$year == 2018),]$IF <- 6.02
out_df[which(out_df$year == 2019),]$IF <- 13.03
out_df[which(out_df$year == 2020),]$IF <- 12.73
out_df[which(out_df$year == 2021),]$IF <- 18.19
out_df[which(out_df$year == 2022),]$IF <- 38.10

# 发文量
ggplot(out_df,aes(x = year)) + 
  geom_bar(width = 0.8, aes(fill = type)) + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) + 
  geom_point(aes(y = IF)) + 
  geom_line(aes(y = IF)) + 
  geom_text(aes(y = IF,label = IF),vjust = -.6,size = 5) +
  scale_x_continuous(breaks=c(2016,2017,2018,2019 , 2020, 2021,2022)) + 
  scale_fill_discrete(name = "Article type") +
  ggtitle('STTT articles') + 
  theme_light() + 
  theme(axis.text.x = element_text(size = 13,color = 'black'),
        axis.text.y = element_text(size = 10,color = 'black'),
        plot.title = element_text(hjust = 0.5)) + 
  ylab('')
  
  
  

# subjects
out_df_high_cited <- out_df[which(out_df$citation > 10),]
all_subjects <- strsplit(out_df_high_cited$subjects,', ') %>% unlist(use.names = F)
plot_data_all <- table(all_subjects) %>% sort(T) %>% as.data.frame()
head(plot_data_all)
colnames(plot_data_all) <- c('Subject','Frequency')
plot_data <- head(plot_data_all,20)

ggplot(plot_data, aes(x = Subject, y = Frequency,color = Subject,fill = Subject)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8, preserve = "single"), width = 0.7) + 
  geom_text(aes(label = round(Frequency,2)), size = 5, position = position_dodge(0.8), vjust = -0.5) + 
  ggtitle("Top 20 STTT highly cited (citation > 10) papers' subjects") + 
  ylim(0,80) +
  theme_light() + 
  theme(legend.position = "none",plot.margin = unit(c(.2,.2,.2,1.2),units = 'cm'),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,size = 12,colour = 'black'),
        plot.title = element_text(hjust = 0.5)) 


# citations
out_df$citation_range <- '0'
out_df[which(out_df$citation > 0 & out_df$citation <= 5),]$citation_range <- '1-5'
out_df[which(out_df$citation > 5 & out_df$citation <= 10),]$citation_range <- '6-10'
out_df[which(out_df$citation > 10 & out_df$citation <= 50),]$citation_range <- '11-50'
out_df[which(out_df$citation > 50 & out_df$citation <= 100),]$citation_range <- '51-100'
out_df[which(out_df$citation > 100 & out_df$citation <= 500),]$citation_range <- '101-500'
out_df[which(out_df$citation > 500),]$citation_range <- '>500'

table(out_df$citation_range)
out_df$citation_range <- factor(out_df$citation_range,
                                levels = c('0','1-5','6-10','11-50','51-100','101-500','>500'))


ggplot(out_df,aes(x = citation_range)) + 
  geom_bar(width = 0.8, aes(fill = type)) + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) + 
  theme_light() + 
  xlab('Citation ranges') + 
  ylab('Number')



