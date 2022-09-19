# get_STTT_all_papers_info

library(dplyr,verbose = F,warn.conflicts = F)
library(rvest,verbose = F,warn.conflicts = F)
library(xml2,verbose = F,warn.conflicts = F)
library(stringr,verbose = F,warn.conflicts = F)
library(ggplot2,verbose = F,warn.conflicts = F)
library(writexl,verbose = F,warn.conflicts = F)


all_papers_urls <- readRDS('./others/STTT_papers/all_papers_urls.RDS')

# url <- all_papers_urls[1]
# url <- 'https://www.nature.com/articles/s41392-022-00980-6'


get_STTT_paper_info <- function(url,sleep_seconds = 10){
  webpage <- read_html(url,encoding = 'utf-8')
  
  # title
  title_line <- html_nodes(webpage,'header h1')[1] %>% as.character()
  title_start_loc <- str_locate(title_line,'data-article-title=\"\">')[2] + 1
  title_end_loc <- str_locate(title_line,'</h1>')[1] - 1
  title <- str_sub(title_line,title_start_loc,title_end_loc)
  title <- gsub('<i>','',title)
  title <- gsub('</i>','',title)
  title <- gsub('\n','',title)
  title <- gsub('<sup>','',title)
  title <- gsub('</sup>','',title)
  title
  
  # type
  type_line <- html_nodes(webpage,'ul li')[grep('article-category',html_nodes(webpage,'ul li'))] %>% as.character()
  type_start_loc <- str_locate(type_line,'"article-category\">')[2] + 1
  type_end_loc <- str_locate(type_line,'</li>')[1] - 1
  type <- str_sub(type_line,type_start_loc,type_end_loc)
  type
  if(length(type) == 0){
    type <- 'NA'
  }
  type
  
  # access
  access_line <- html_nodes(webpage,'li p')[grep('Accesses',html_nodes(webpage,'li p'))] %>% as.character()
  access_start_loc <- str_locate(access_line,'<p class=\"c-article-metrics-bar__count\">')[2] + 1
  access_end_loc <- str_locate(access_line,' <span class=\"c-article-metrics-bar__label\"')[1] - 1
  access <- str_sub(access_line,access_start_loc,access_end_loc)
  access
  if(length(access) == 0){
    access <- 0
  }
  access
  if(str_detect(access,'k')){
    access <- str_sub(access,1,str_locate(access,'k')[1,1]-1)
    access <- as.numeric(access) * 1000
  }
  access
  
  # citation
  citation_line <- html_nodes(webpage,'li p')[grep('Citations',html_nodes(webpage,'li p'))] %>% as.character()
  citation_start_loc <- str_locate(citation_line,'<p class=\"c-article-metrics-bar__count\">')[2] + 1
  citation_end_loc <- str_locate(citation_line,' <span class=\"c-article-metrics-bar__label\"')[1] - 1
  citation <- str_sub(citation_line,citation_start_loc,citation_end_loc)
  citation
  if(length(citation) == 0){
    citation <- 0
  }
  citation
  
  # altmetric
  altmetric_line <- html_nodes(webpage,'li p')[grep('Altmetric',html_nodes(webpage,'li p'))] %>% as.character()
  altmetric_start_loc <- str_locate(altmetric_line,'<p class=\"c-article-metrics-bar__count\">')[2] + 1
  altmetric_end_loc <- str_locate(altmetric_line,' <span class=\"c-article-metrics-bar__label\"')[1] - 1
  altmetric <- str_sub(altmetric_line,altmetric_start_loc,altmetric_end_loc)
  altmetric
  if(length(altmetric) == 0){
    altmetric <- 0
  }
  altmetric
  
  # correspond_authors
  correspond_authors_line <- html_nodes(webpage,'li a')[grep('corresp',html_nodes(webpage,'li a'))] %>% as.character()
  correspond_authors <- sapply(correspond_authors_line,function(line){
    # line <- correspond_authors_line[1]
    start_loc <- str_locate(line,'Read more about ')[2] + 1
    end_loc <- str_locate(line,'\" data-author-popup=\"')[1] - 1
    correspond_author <- str_sub(line,start_loc,end_loc)
    return(correspond_author)
  }) %>% unname() %>% paste(collapse = ', ')
  correspond_authors
  
  # subject
  subject_line <- html_nodes(webpage,'li a')[grep('data-track-action=\"view subject\" data-track-label=\"link\">',html_nodes(webpage,'li a'))] %>% as.character()
  subjects <- sapply(subject_line,function(line){
    # line <- subject_line[1]
    start_loc <- str_locate(line,'data-track-label=\"link\">')[2] + 1
    end_loc <- str_locate(line,'</a>')[1] - 1
    subject <- str_sub(line,start_loc,end_loc)
    return(subject)
  }) %>% unname() %>% paste(collapse = ', ')
  subjects
  
  # time
  time_line <- html_nodes(webpage,'li a')[grep('Published',html_nodes(webpage,'li a'))] %>% as.character()
  time_start_loc <- str_locate(time_line,'Published: <time datetime=\"')[2] + 1
  time_end_loc <- time_start_loc + 9
  time <- str_sub(time_line,time_start_loc,time_end_loc)
  time
  year <- strsplit(time,'-')[[1]][1]
  year
  year_month <- paste0(strsplit(time,'-')[[1]][1],'-',strsplit(time,'-')[[1]][2])
  year_month
  
  out <- c(year = year,
           year_month = year_month,
           type = type,
           access = access,
           citation = citation,
           altmetric = altmetric,
           subjects = subjects,
           online_time = time,
           url = url,
           correspond_authors = correspond_authors,
           title = title)
  
  Sys.sleep(sleep_seconds)
  
  return(out)
}


out <- c()
for(idx in 1:length(all_papers_urls)){
  # idx <- 1
  print(idx)
  url <- all_papers_urls[idx]
  this_out <- get_STTT_paper_info(url,sleep_seconds = 20)
  out <- rbind(out,this_out)
  # print(this_out)
}
out_df <- as.data.frame(out)
out_df <- out_df[order(out_df$online_time,decreasing = T),]

out_df$year <- as.numeric(out_df$year)
out_df$access <- as.numeric(out_df$access)
out_df$citation <- as.numeric(out_df$citation)
out_df$altmetric <- as.numeric(out_df$altmetric)
rownames(out_df) <- NULL
head(out_df)

saveRDS(out_df, './others/STTT_papers/STTT_all_papers_info_df.RDS')

write_xlsx(out_df,'./others/STTT_papers/STTT_all_papers_info.xlsx')







