# plot
library(ggplot2)
library(stringr)
library(ggpubr)
library(aplot)
library(dplyr)
setwd('~/Phenomics/')

plot_file_names <- list.files('../Phenomics/weekly_online_paper_metrices/output/')
plot_file_names

df <- pbmcapply::pbmclapply(plot_file_names,function(file_name){
  # file_name <- rev(plot_file_names)[1]
  file_name
  full_file_name <- paste0('/mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/output/',file_name)
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
  ylim((min(df$`Total access`) - 1000),(max(df$`Total access`) + 5000)) + 
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
  ylim((min(df$`Total citations`) - 2),(max(df$`Total citations`) + 5)) + 
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
  ylim((min(df$`Total altmetrics`) - 2),(max(df$`Total altmetrics`) + 2)) + 
  theme_light() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12,color = 'black',angle = 45,vjust = 1,hjust = 1),
        axis.title.y = element_text(size = 14,color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = .5)) 


access_plot %>% 
  insert_bottom(citations_plot, height = 1) %>% 
  insert_bottom(altmetrics_plot, height = 1)




full_file_name <- paste0('/mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/output/',
                         rev(plot_file_names)[1])
out_df <- readxl::read_excel(full_file_name)
out_df <- filter(out_df,type != 'Correction')
out_df$mytype <- factor(out_df$type,
                        levels = c('Article','Review','Commentary','Correspondence','Editorial','Meeting Report'),
                        labels = c('Article','Review','Others','Others','Others','Others'))
# 发文量
p1 <- ggplot(out_df,aes(x = year)) +
  geom_bar(width = 0.6, aes(fill = mytype)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5,size = 5) +
  scale_x_continuous(breaks=c(2021,2022)) +
  scale_fill_discrete(name = "Article type") +
  ggtitle('Published paper distribution') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12,color = 'black'),
        axis.text.y = element_text(size = 12,color = 'black'),
        plot.title = element_text(hjust = 0.5)) +
  ylab('Number')

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
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5,size = 5) +
  ggtitle("Citation distribution") +
  theme_bw() +
  xlab('Citation ranges') +
  ylab('Number') +
  theme(axis.text.x = element_text(size = 12,colour = 'black'),
        axis.text.y = element_text(size = 12,colour = 'black'),
        plot.title = element_text(hjust = 0.5))

ggpubr::ggarrange(plotlist = list(p1,p2),nrow = 1,ncol = 2,common.legend = T,legend = 'right')

