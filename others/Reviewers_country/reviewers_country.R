library(dplyr)
library(ggplot2)
library(cowplot)
library(Seurat)


data <- read.table('./monthly_report/Editorial_Search_Submissions_results_20220831T193100918.tab',sep = '\t',quote = '',header=T)
data$Classifications <- NULL
data$Country <- unlist(strsplit(sapply(strsplit(data$Author.Name,'\\('),function(i){i[2]}),'\\)'))
data$is_China <- factor(data$Country == 'CHINA',labels = c('Overseas','China'))

papers_data <- data

reviewer_data <- read.table('./monthly_report/Search_People_results_20220831T194554640.tab',sep = '\t',quote = '',header=T)
head(reviewer_data)
reviewer_data <- reviewer_data %>% 
  select(Name,Country,Reviewer.Role) %>%
  filter(Reviewer.Role == 'Reviewer')

unique_reviewers <- strsplit(papers_data$Reviewers,';') %>% 
  unlist() %>% 
  gsub(pattern = ' ',replacement = '') %>% 
  gsub(pattern = '\\*',replacement = '') %>% 
  unique()

reviewer_idx <- sapply(unique_reviewers,function(one_reviewer){
  agrep(one_reviewer,reviewer_data$Name)
}) %>% unlist(use.names = F)

reviewers_df <- table(reviewer_data[reviewer_idx,]$Country) %>% sort(T) %>% as.matrix() %>% as.data.frame()
colnames(reviewers_df) <- 'Number'
reviewers_df$Proportion <- paste0(round(reviewers_df$Number/length(reviewer_idx)*100,1),'%')

write.table(reviewers_df,'./monthly_report/reviewers.txt',quote = F,sep = '\t')



