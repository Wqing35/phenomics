library(dplyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(Seurat)


this_year <- '2022'
this_month <- '10'
this_date <- '31'

data <- read.table('./monthly_report/version1/Editorial_Search_Submissions_results_20221031T114750873.tab',
                   sep = '\t',quote = '',header=T)
data$Classifications <- NULL
data$Country <- unlist(strsplit(sapply(strsplit(data$Author.Name,'\\('),function(i){i[2]}),'\\)'))
data$is_China <- factor(data$Country == 'CHINA',labels = c('Overseas','China'))
data <- data[order(data$Manuscript.Number),]
write.csv(data,paste0('./monthly_report/version1/20221031_out.csv'),fileEncoding = 'UTF-8')

################################################################
data <- read.csv('./monthly_report/version2/Phenomics期刊工作进展数据表0930.csv')
tail(data,20)
data <- data[1:140,]


'####################### 一、投稿情况 ###########################' %>% message()
'1. 投稿数量：' %>% message()
sub_year <- sapply(strsplit(data$Initial.Date.Submitted,'-'),function(i){i[3]})
data$sub_year <- sub_year
status_year <- sapply(strsplit(data$Status.Date,'-'),function(i){i[3]})
data$status_year <- status_year
month <- sapply(strsplit(data$Initial.Date.Submitted,'-'),function(i){paste0(i[2],'-',i[3])})
data$Month <- factor(month,levels = rev(unique(month)))
word1 <- paste0('截止',this_year,'年',this_month,'月',this_date,'日',
                '，累计收到投稿',(nrow(data)),'篇，含',
                length(which((data$Article.Type == 'Article'))),'篇研究论文、',
                length(which((data$Article.Type == 'Review'))),'篇综述、',
                length(which((data$Article.Type == 'Protocol'))),'篇Protocol、',
                length(which((data$Article.Type == 'Brief Communication'))),'篇简要通讯、',
                length(which((data$Article.Type == 'Commentary'))),'篇评论和',
                length(which((data$Article.Type == 'Correspondence/Letter to the Editor'))),'篇读者来信',
                '。自2020年7月开刊，2020年投稿平均',round(length(which(data$sub_year == '20'))/6,1),
                '篇/月，2021年投稿平均',round(length(which(data$sub_year == '21'))/12,1),
                '篇/月，2022年投稿平均',round(length(which(data$sub_year == '22'))/as.numeric(this_month),1),
                '篇/月（如图1）。')
word1 %>% print()
table(data$Article.Type)

# 投稿分布
word2 <- paste0('如图2所示，国内投稿',length(which(data$Country == 'CHINA')),
                '篇（复旦及附属医院',length(which(data$is.Fudan == 1)),
                '篇，其它国内单位',length(which(data$Institution == 'Non-Fudan')),
                '篇），国外',(length(unique(data$Country))-1),'个国家',
                (length(unique(data[which(data$Country != 'CHINA'),]$Institution.of.the.First.Corresponding.Author))-1),
                '家科研机构投稿',length(which(data$Institution == 'Overseas')),
                '篇；具体单位情况详见附件1。含',
                length(which(data$is.invited == 'Y')),'篇邀请稿（',round(length(which(data$is.invited == 'Y'))/nrow(data)*100,1),
                '%）和',length(which(data$is.invited == 'N')),'篇自投稿（',round(length(which(data$is.invited == 'N'))/nrow(data)*100,1) ,'%）')
word2 %>% print()


# 图1
fig1 <- ggplot(data = data[-nrow(data),],aes(x = Month)) +
  geom_bar(width = 0.8,aes(fill = Institution)) +
  geom_text(stat='count', aes(label=..count..), vjust= -0.1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
  labs(x = '', y = '')
fig1

# 图2
pie_data <- data.frame(table(data$Institution))
colnames(pie_data) <- c('Institutes','Freqency')
pie_data$Percentage = pie_data$Freq/sum(pie_data$Freq)*100
pie_data$label = rev(paste0(pie_data$Institutes,'\n',pie_data$Freqency,'\n',round(pie_data$Freq/sum(pie_data$Freq)*100,2),'%'))
fig2 <- ggplot(pie_data, aes(x = "", y = Percentage, fill = Institutes)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(y= 100-(cumsum(Percentage)-Percentage/2), x= 1.1),
            label = rev(pie_data$label)) +
  labs(x = '', y = '') +
  cowplot::theme_nothing()
fig2
ggpubr::ggarrange(plotlist = list(fig1, fig2), ncol = 2, nrow = 1,widths = c(2, 1))



'####################### 二、审稿情况 ###########################' %>% message()
word3 <- paste0('累计接受文章',length(which(data$Current.Status == 'Final Decision Accept')),
                '篇，拒稿或transfer',length(which(data$Current.Status %in%
                                               c('Final Decision Reject','Submission Transferred',
                                                 'Content Files Deleted - Forced to Withdrawn '))),
                '篇，其它正在审稿中（如下表），具体审稿情况如附件2；2021年已上线6期，',
                '2022年已上线5期，已上线文章相关参数见https://github.com/Telogen/Journal-Phenomics。')
word3 %>% print()
table(data$Current.Status,data$status_year)


tmp <- table(data$Current.Status,data$status_year) %>% 
  as.matrix()
# under review
tmp[c('New Submission','Reviewers Assigned','Revision Submitted',
      'Under Review',  'Reviews Completed','Editor Assigned'),] %>% 
  colSums()
tmp[c('New Submission','Reviewers Assigned','Revision Submitted',
      'Under Review',  'Reviews Completed','Editor Assigned'),] %>% 
  colSums()

###### 附件📎2
names <- sapply(strsplit(data$Author.Name,split = '\\('),function(x){
  x[1]
})
article_type <- data$Article.Type
article_type[which(article_type == 'Article')] <- '研究'
article_type[which(article_type == 'Review')] <- '综述'
article_type[which(article_type == 'Brief Communication')] <- '简报'
article_type[which(article_type == 'Commentary')] <- '评论'
data$article_type_chinese <- paste0(names,'(',article_type,')')

review_articles <- filter(data,Current.Status %in% c('New Submission','Reviewers Assigned','Revision Submitted',
                                                     'Under Review',  'Reviews Completed','Editor Assigned'))$article_type_chinese
length(review_articles)
paste(review_articles,collapse = ', ')

revise_articles <- filter(data,Current.Status %in% c('Incomplete','Revise','Revision Needs Approval',
                                                     'Revision Submitted'))$article_type_chinese
length(revise_articles)
paste(revise_articles,collapse = ', ')


1






































'####################### 表一 ###########################' %>% message()
data$Round <- '1round'
data[which(stringr::str_detect(data$Manuscript.Number,'R')),]$Round <- '2/3round'
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

'# Accept but need final editing列' %>% message()
ABFE <- table(data$Decision,data$status_year)['Accept but needs final editing',] %>% print()
# 要排除Final Decision Accept的，这些不算做Accept but needs final editing
tmp_data <- data[which(data$Current.Status == 'Final Decision Accept' & data$Decision == 'Accept but needs final editing'),]
tmp_data$status_year
exclude <- nrow(tmp_data)

'# Reject/transfer列' %>% message()
RT <- colSums(table(data$Current.Status,data$status_year)[c(
  'Final Decision Reject','Submission Transferred'),]) %>% print()

'# 最后一列' %>% message()
last <- colSums(table(data$Current.Status,data$status_year)[c(
  'Content Files Deleted - Forced to Withdrawn','Sent Back to Author'),]) %>% print()

##################### UNDER REVIEW  和 EVISE 的new_data #####################
new_data <- filter(data,Current.Status != 'Final Decision Accept' &
                     Current.Status != 'Final Decision Reject' &
                     Current.Status != 'Submission Transferred' &
                     Current.Status != 'Sent Back to Author' &
                     Current.Status != 'Content Files Deleted - Forced to Withdrawn' &
                     Decision != 'Accept but needs final editing')
# 验证一下
nrow(data)
nrow(new_data) + sum(accept) + sum(ABFE) - exclude + sum(RT) + sum(last)

table(new_data$Current.Status)

'# Under Review列' %>% message()
review_data <- filter(new_data,Current.Status %in%
                        c('New Submission','Reviewers Assigned','Revision Submitted',
                          'Under Review',  'Reviews Completed','Editor Assigned'))
table(review_data$Round)
'# Revise列' %>% message()
revise_data <-filter(new_data,!Current.Status %in%
                       c('New Submission','Reviewers Assigned','Revision Submitted',
                         'Under Review',  'Reviews Completed','Editor Assigned') &
                       Current.Status != '')
table(revise_data$Decision,revise_data$Round)




'####################### 附件1. 已投文章的第一通讯作者的单位情况 ###########################' %>% message()
data$sup1 <- data$Institute
data$sup1[which(data$sup1 %in% c('Non-Fudan','Overseas'))] <-
  data$Institution.of.the.First.Corresponding.Author[which(data$sup1 %in% c('Non-Fudan','Overseas'))]
for(i in setdiff(names(sort(table(data$Country),T)),'')){
  tmp_data <- data[which(data$Country == i),]
  print(paste0(i,': ',nrow(tmp_data)))
  print(table(tmp_data$sup1))
  message('---------------------------------------------------------------')
}
'# 中国数量及比例' %>% message()
length(which(data$is_China == 'China')) %>% print()
round(length(which(data$is_China == 'China'))/(nrow(data)-1)*100,1) %>% paste0('%') %>% print()
'# 外国数量及比例' %>% message()
(length(which(data$is_China == 'Overseas'))-1) %>% print()
round(length(which(data$is_China == 'Overseas'))/(nrow(data)-1)*100,1) %>% paste0('%') %>% print()

'####################### 附件二 ###########################' %>% message()
word3.2 <- paste0('每篇文章审稿状态（截止',this_month,'月',this_date,'日）')
word3.2 %>% message()

word3.5 <- paste0('已接受文章',
                  length(which(data$Decision == 'Accept but needs final editing' |
                                 data$Current.Status == 'Final Decision Accept')),'篇：')
paste0('# ',word3.5) %>% print()

word4 <- paste0('已集结成册',length(which(data$Issue != '')),'篇。')
paste0('1) ',word4) %>% print()

all_name_article_type <- data$article_type_chinese

online_not_issue_index <- which(data$is.online == 1 & data$Issue == '')
word5 <- paste0('已上线但尚未集合成期',length(online_not_issue_index),'篇：',
                stringr::str_c(all_name_article_type[online_not_issue_index],collapse = '、'),'。')
paste0('2) ',word5) %>% print()

not_online_index <- which(is.na(data$is.online) & data$Current.Status == 'Final Decision Accept')
word6 <- paste0('尚未上线发表',length(not_online_index),'篇：',
                stringr::str_c(all_name_article_type[not_online_index],collapse = '、'),'。')
paste0('3) ',word6) %>% print()

accept_but_need_index <- which(data$Decision == 'Accept but needs final editing' &
                                 data$Current.Status != 'Final Decision Accept')
word7 <- paste0('已接收但在做最后编辑文章',length(accept_but_need_index),'篇：',
                stringr::str_c(all_name_article_type[accept_but_need_index],collapse = '、'),'。')
paste0('4) ',word7) %>% print()

message('---------------------------------------------------------------')

word8 <- paste0('在审文章',nrow(review_data),'篇：')
paste0('# ',word8) %>% print()

word9 <- paste0(length(which(review_data$Round == '1round')),'篇一审中：',
                stringr::str_c(filter(review_data,Round == '1round')$article_type_chinese,collapse = '、'),'。')
paste0('1) ',word9) %>% print()

word10 <- paste0(length(which(review_data$Round == '2/3round')),'篇二/三审中：',
                 stringr::str_c(filter(review_data,Round == '2/3round')$article_type_chinese,collapse = '、'),'。')
paste0('2) ',word10) %>% print()

message('---------------------------------------------------------------')

word11 <- paste0('在修文章',nrow(revise_data),'篇：')
paste0('# ',word11) %>% print()

word12 <- paste0(nrow(filter(revise_data,
                             Round == '1round' &
                               Decision %in% c('Major revisions','Major Revisions'))),'篇一审大修中：',
                 stringr::str_c(filter(revise_data,
                                       Round == '1round' &
                                         Decision %in% c('Major revisions','Major Revisions'))$article_type_chinese,collapse = '、'),'。')
paste0('1) ',word12) %>% print()

word13 <- paste0(nrow(filter(revise_data,
                             Round == '1round' &
                               Decision %in% c('Minor revisions','Minor Revisions'))),'篇一审小修中：',
                 stringr::str_c(filter(revise_data,
                                       Round == '1round' &
                                         Decision %in% c('Minor revisions','Minor Revisions'))$article_type_chinese,collapse = '、'),'。')
paste0('2) ',word13) %>% print()

word14 <- paste0(nrow(filter(revise_data,
                             Round == '2/3round' &
                               Decision %in% c('Major revisions','Major Revisions'))),'篇二/三审大修中：',
                 stringr::str_c(filter(revise_data,
                                       Round == '2/3round' &
                                         Decision %in% c('Major revisions','Major Revisions'))$article_type_chinese,collapse = '、'),'。')
paste0('3) ',word14) %>% print()

word15 <- paste0(nrow(filter(revise_data,
                             Round == '2/3round' &
                               Decision %in% c('Minor revisions','Minor Revisions'))),'篇二/三审小修中：',
                 stringr::str_c(filter(revise_data,
                                       Round == '2/3round' &
                                         Decision %in% c('Minor revisions','Minor Revisions'))$article_type_chinese,collapse = '、'),'。')
paste0('4) ',word15) %>% print()















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










