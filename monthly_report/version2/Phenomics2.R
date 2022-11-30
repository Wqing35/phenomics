library(dplyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(Seurat)
setwd('~/Phenomics/')

this_year <- '2022'
this_month <- '11'
this_date <- '30'

data <- read.table('../Phenomics/monthly_report/version2/Editorial_Search_Submissions_results_20221130T222848371.tab',
                   sep = '\t',quote = '',header=T)
data$Classifications <- NULL
data$Country <- unlist(strsplit(sapply(strsplit(data$Author.Name,'\\('),function(i){i[2]}),'\\)'))
data$is_China <- factor(data$Country == 'CHINA',labels = c('Overseas','China'))
data <- data[order(data$Manuscript.Number),]
write.csv(data,paste0('../Phenomics/monthly_report/20221130_out.csv'),fileEncoding = 'UTF-8')

################################################################
data <- readxl::read_xlsx('../Phenomics/monthly_report/version2/Phenomics_1130.xlsx')
tail(data)
# data <- data[-nrow(data),]


'####################### 一、投稿情况 ###########################' %>% message()
'1. 投稿数量：' %>% message()
sub_year <- sapply(strsplit(as.character(data$Initial.Date.Submitted),'-'),function(i){i[1]})
data$sub_year <- sub_year
status_year <- sapply(strsplit(as.character(data$Status.Date),'-'),function(i){i[1]})
data$status_year <- status_year
month <- sapply(strsplit(as.character(data$Initial.Date.Submitted),'-'),function(i){paste0(i[1],'-',i[2])})
data$Month <- factor(month,levels = rev(unique(month)))
word1 <- paste0('截止',this_year,'年',this_month,'月',this_date,'日',
                '，累计收到投稿',(nrow(data)),'篇，含',
                length(which((data$Article.Type == 'Article'))),'篇研究论文、',
                length(which((data$Article.Type == 'Review'))),'篇综述、',
                length(which((data$Article.Type == 'Protocol'))),'篇Protocol、',
                length(which((data$Article.Type == 'Brief Communication'))),'篇简要通讯、',
                length(which((data$Article.Type == 'Commentary'))),'篇评论和',
                length(which((data$Article.Type == 'Correspondence/Letter to the Editor'))),'篇读者来信',
                '。自2020年7月开刊，2020年投稿平均',round(length(which(data$sub_year == '2020'))/6,1),
                '篇/月，2021年投稿平均',round(length(which(data$sub_year == '2021'))/12,1),
                '篇/月，2022年投稿平均',round(length(which(data$sub_year == '2022'))/as.numeric(this_month),1),
                '篇/月（如图1）。')
word1 %>% print()
table(data$Article.Type)

# 投稿分布
word2 <- paste0('如图2所示，国内投稿',length(which(data$Country == 'CHINA')),
                '篇（复旦及附属医院',length(which(data$`is Fudan` == 1)),
                '篇，其它国内单位',length(which(data$Institution == 'Non-Fudan')),
                '篇），国外',(length(unique(data$Country))-1),'个国家',
                (length(unique(data[which(data$Country != 'CHINA'),]$`Institution of the First Corresponding Author`))-1),
                '家科研机构投稿',length(which(data$Institution == 'Overseas')),
                '篇；具体单位情况详见附件1。含',
                length(which(data$`is invited` == 'Y')),'篇邀请稿（',round(length(which(data$`is invited` == 'Y'))/nrow(data)*100,1),
                '%）和',length(which(data$`is invited` == 'N')),'篇自投稿（',round(length(which(data$`is invited` == 'N'))/nrow(data)*100,1) ,'%）')
word2 %>% print()


# 图1
fig1 <- ggplot(data = data,aes(x = Month)) +
  geom_bar(width = 0.8,aes(fill = Institution)) +
  geom_text(stat='count', aes(label=..count..), vjust= -0.1,size = 6) + 
  theme_bw() +
  labs(y="Number of submissions") + 
  # ggtitle('Number of submissions each month') + 
  # theme(plot.title = element_text(hjust = 0.5)) +
  NoLegend() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12,color = 'black',angle = 45,vjust = 1,hjust = 1),
        axis.title.y = element_text(size = 14,color = 'black'),
        axis.text.y = element_text(size = 12,color = 'black'),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = .5)) 

fig1

# 图2
pie_data <- data.frame(table(data$Institution))
colnames(pie_data) <- c('Institutes','Freqency')
pie_data$Percentage = pie_data$Freq/sum(pie_data$Freq)*100
pie_data$label = rev(paste0(pie_data$Institutes,'\n',pie_data$Freqency,'\n',round(pie_data$Freq/sum(pie_data$Freq)*100,2),'%'))
fig2 <- ggplot(pie_data, aes(x = "", y = Percentage, fill = Institutes)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(y= 100-(cumsum(Percentage)-Percentage/2), x= 1.2),
            label = rev(pie_data$label),size = 5) +
  labs(x = '', y = '') +
  cowplot::theme_nothing() 
  # ggtitle('Submission distribution') + 
  # theme(plot.title = element_text(hjust = 0.5))
fig2
ggpubr::ggarrange(plotlist = list(fig1, fig2), ncol = 2, nrow = 1,widths = c(2.2, 1))



'####################### 二、审稿情况 ###########################' %>% message()
word3 <- paste0('累计接受文章',length(which(data$Current.Status == 'Final Decision Accept')),
                '篇，拒稿或transfer',length(which(data$Current.Status %in%
                                               c('Final Decision Reject','Submission Transferred',
                                                 'Content Files Deleted - Forced to Withdrawn '))),
                '篇，其它正在审稿中（如下表），具体审稿情况如附件2；2021年已上线6期（23篇+1篇开刊词），2022年已上线',
                (length(unique(data$Issue))-7),'期（共计',
                (length(unique(data$Issue))-7)*6,'篇），已上线文章相关参数见附件3。')
word3 %>% print()
table(data$Current.Status,data$status_year)

'####################### 表一 ###########################' %>% message()
names <- sapply(strsplit(data$Author.Name,split = '\\('),function(x){
  x[1]
})
article_type <- data$Article.Type
article_type[which(article_type == 'Article')] <- '研究'
article_type[which(article_type == 'Review')] <- '综述'
article_type[which(article_type == 'Brief Communication')] <- '简报'
article_type[which(article_type == 'Commentary')] <- '评论'
data$article_type_chinese <- paste0(names,'(',article_type,')')


'# Accept列'%>% message()
accept <- table(data$Current.Status,data$status_year)['Final Decision Accept',] %>% print()
accept_idx <- which(data$Current.Status == 'Final Decision Accept')

'# Reject/transfer列' %>% message()
RT <- colSums(table(data$Current.Status,data$status_year)[c(
  'Final Decision Reject','Submission Transferred'),]) %>% print()
RT_idx <- which(data$Current.Status %in% c('Final Decision Reject','Submission Transferred'))

'# 最后一列' %>% message()
last <- colSums(table(data$Current.Status,data$status_year)[c(
  'Content Files Deleted - Forced to Withdrawn','Sent Back to Author'),]) %>% print()
last_idx <- which(data$Current.Status %in% c('Content Files Deleted - Forced to Withdrawn',
                                             'Sent Back to Author'))


'# Under Review列' %>% message()
UR_idx <- which(data$Current.Status %in% 
                  c('New Submission','Reviewers Assigned','Revision Submitted',
                    'Under Review',  'Reviews Completed','Editor Assigned'))
UR <- length(UR_idx) %>% print()

'# Revise列' %>% message()
Revise <- (nrow(data) - sum(accept) - sum(RT) - sum(last) - UR) %>% print()
Revise_idx <- setdiff(1:nrow(data),c(accept_idx,RT_idx,last_idx,UR_idx))


'####################### 附件二 ###########################' %>% message()
word3.2 <- paste0('审稿情况补充')
word3.2 %>% message()

word3.5 <- paste0('已接受文章', length(accept_idx),'篇：')
paste0('# ',word3.5) %>% print()

word4 <- paste0('已集结成册',length(which(data$Issue != '')),'篇。')
paste0('1) ',word4) %>% print()

all_name_article_type <- data$article_type_chinese

online_not_issue_index <- which(!is.na(data$is_online) & is.na(data$Issue))
word5 <- paste0('已上线但尚未集合成期',length(online_not_issue_index),'篇：',
                stringr::str_c(all_name_article_type[online_not_issue_index],collapse = '、'),'。')
paste0('2) ',word5) %>% print()

not_online_index <- which(is.na(data$is_online) & data$Current.Status == 'Final Decision Accept')
word6 <- paste0('尚未上线发表',length(not_online_index),'篇：',
                stringr::str_c(all_name_article_type[not_online_index],collapse = '、'),'。')
paste0('3) ',word6) %>% print()

word8 <- paste0('在审文章',length(UR_idx),'篇：',
                stringr::str_c(all_name_article_type[UR_idx],collapse = '、'),'。')
paste0('# ',word8) %>% print()

word9 <- paste0('在修文章',length(UR_idx),'篇：',
                stringr::str_c(all_name_article_type[Revise_idx],collapse = '、'),'。')
paste0('# ',word9) %>% print()
















# citation
citation_df <- read.csv('./Phenomics/citation.csv')
table(citation_df$Article.Type,citation_df$Citations)

data <- as.data.frame(table(citation_df$Article.Type))
data$Percentage <- data$Freq/sum(data$Freq) * 100
data$label = rev(paste0(round(data$Freq/sum(data$Freq)*100,2),'%','\n',data$Var1))
data$label[2:5] <- ' '
ggplot(data, aes(x = "", y = Percentage, fill = Var1)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(y= 100-(cumsum(Percentage)-Percentage/2), x= 1.1),
            label = rev(data$label)) +
  labs(x = '', y = '') +
  cowplot::theme_nothing()

library(scales)
hue_pal()(6)

Article_df <- citation_df %>% filter(Article.Type == 'Article')
ggplot(Article_df, aes(x = Citations)) +
  geom_bar(color = '#F8766D',fill = '#F8766D') +
  geom_text(stat='count', aes(label=..count..), vjust = -.5) +
  ylim(0,25)

Review_df <- citation_df %>% filter(Article.Type == 'Review')
ggplot(Review_df, aes(x = Citations)) +
  geom_bar(color = '#F564E3',fill = '#F564E3') +
  geom_text(stat='count', aes(label=..count..), vjust = -.5) +
  ylim(0,25)










