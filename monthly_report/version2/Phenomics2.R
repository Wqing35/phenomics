library(dplyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(Seurat)
setwd('/mdshare/node8/txmdata/Phenomics')

file_names <- list.files('../Phenomics/monthly_report/version2')
file_name <- (file_names[str_detect(string = file_names,pattern = 'Edit')] %>% sort(T))[1]
file_name

data <- read.table(paste0('../Phenomics/monthly_report/version2/',file_name),
                   sep = '\t',quote = '',header=T)
data$Classifications <- NULL
data$Country <- unlist(strsplit(sapply(strsplit(data$Author.Name,'\\('),function(i){i[2]}),'\\)'))
data$is_China <- factor(data$Country == 'CHINA',labels = c('Overseas','China'))
data <- data[order(data$Manuscript.Number),]

write.csv(data,paste0('../Phenomics/monthly_report/20230331_out.csv'),fileEncoding = 'UTF-8')







################################################################
this_year <- '2023'
this_month <- '3'
this_date <- '31'

data <- readxl::read_xlsx('../Phenomics/monthly_report/version2/Phenomics_0331.xlsx')
tail(data)
# data <- data[-nrow(data),]


'####################### 一、投稿情况 ###########################' %>% message()
'1. 投稿数量：' %>% message()
sub_year <- sapply(strsplit(as.character(data$Initial.Date.Submitted),'-'),function(i){i[1]})
data$sub_year <- sub_year
sub_month <- sapply(strsplit(as.character(data$Initial.Date.Submitted),'-'),function(i){i[2]})
data$sub_month <- sub_month
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
                '篇/月，2022年投稿平均',round(length(which(data$sub_year == '2022'))/12,1),
                '篇/月，2023年投稿平均',round(length(which(data$sub_year == '2023'))/as.numeric(this_month),1),
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
f1 <- ggplot(data = data,aes(x = Month)) +
  geom_bar(width = 0.8,aes(fill = Institution)) + 
  geom_text(stat='count', aes(label=..count..), vjust= -0.1,size = 3) + 
  facet_grid(. ~ sub_month, scales = 'free', space = 'free') +
  theme_bw() +
  labs(y="Number of submissions") + 
  # ggtitle('Number of submissions each month') + 
  # theme(plot.title = element_text(hjust = 0.5)) +
  NoLegend() + 
  scale_fill_manual(
    values=c("Fudan Hospital"="#F7B0E8", "Fudan University"="#FFB3B5", "Non-Fudan"="#6BDABC", "Overseas"="#77D3EC"), 
    labels=c("Fudan Hospital","Fudan University","Non-Fudan","Overseas"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10,color = 'black',angle = 45,vjust = 1,hjust = 1),
        axis.title.y = element_text(size = 14,color = 'black'),
        axis.text.y = element_text(size = 12,color = 'black'),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = .5)) 

f1


# 图2
dat_2021 <- data[data$sub_year=="2021",]
dat_2022 <- data[data$sub_year=="2022",]
dat_2020 <- data[data$sub_year=="2020",]
dat_2023 <- data[data$sub_year=="2023",]

percent_2021 <- round(table(dat_2021$Institution)/nrow(dat_2021),2)
percent_2022 <- round(table(dat_2022$Institution)/nrow(dat_2022),2)
percent_2023 <- round(table(dat_2023$Institution)/nrow(dat_2023),2)
percent_2020 <- c(0,round(table(dat_2020$Institution)/nrow(dat_2020),2))

percent_all <- c(percent_2020,percent_2021,percent_2022,percent_2023)
names(percent_all)[1]="Fudan Hospital"

count <- c(0.3,table(dat_2020$Institution),table(dat_2021$Institution),table(dat_2022$Institution),table(dat_2023$Institution),0.3)
year <- c(rep("2020",times=4),rep("2021",times=4),rep("2022",times=4),rep("2023",times=4))
color <- rep(c("#FFB3B5","#F7B0E8","#6BDABC","#77D3EC"),times=4)
propotion_dat <- as.data.frame(cbind(names(percent_all),year,paste0(percent_all*100,'%'),count,color))
colnames(propotion_dat) <- c("Institution","Year","Percent","Count","Color")

f2 <- ggplot(propotion_dat, aes(x=Year, y=count, group=Institution)) + 
  geom_col(aes(fill=Institution), position="dodge") + 
  #scale_x_discrete(limits=factor(rownames(propotion_dat))) +
  theme_bw() +
  scale_fill_manual(
    values=c("Fudan Hospital"="#F7B0E8", "Fudan University"="#FFB3B5", "Non-Fudan"="#6BDABC", "Overseas"="#77D3EC"), 
    labels=c("Fudan Hospital","Fudan University","Non-Fudan","Overseas"))+
  geom_text(size=3,aes(label=Percent, y=count+0.05), position=position_dodge(0.9), vjust=0) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10,color = 'black',angle = 45,vjust = 1,hjust = 1),
        axis.title.y = element_text(size = 14,color = 'black'),
        axis.text.y = element_text(size = 12,color = 'black'),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = .5)) +
  ylab(' ')
f2


fig1 <- ggpubr::ggarrange(plotlist = list(f1, f2), ncol = 2, nrow = 1,widths = c(1.2, 1))
fig1

ggsave('./figures/fig1.png',fig1,width = 15,height = 6)



'####################### 二、审稿情况 ###########################' %>% message()

word3 <- paste0('累计接受文章',length(which(data$Current.Status == 'Final Decision Accept')),
                '篇，拒稿或transfer',length(which(data$Current.Status %in%
                                               c('Final Decision Reject','Submission Transferred',
                                                 'Content Files Deleted - Forced to Withdrawn '))),
                '篇，其它正在审稿中（如下表），具体审稿情况如附件2；2021年已上线6期（23篇+1篇开刊词），',
                '2022年已上线6期（36篇），','2023年已上线',
                round(length(which(data$Volumn == '3'))/8),'期（',
                length(which(data$Volumn == '3')),'篇+1篇会议报告）。')
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
tmp_row_idx <- which(rownames(table(data$Current.Status,data$status_year)) %in% c('Content Files Deleted - Forced to Withdrawn','Sent Back to Author'))
table(data$Current.Status,data$status_year)[tmp_row_idx,]
last <- colSums(table(data$Current.Status,data$status_year)[tmp_row_idx,]) %>% print()
# last <- 1
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

word4 <- paste0('已集结成册',length(which(!is.na(data$Volumn))),'篇。')
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

word9 <- paste0('在修文章',length(Revise_idx),'篇：',
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










