# get_STTT_all_papers_urls
library(dplyr,verbose = F,warn.conflicts = F)
library(rvest,verbose = F,warn.conflicts = F)
library(xml2,verbose = F,warn.conflicts = F)
library(stringr,verbose = F,warn.conflicts = F)
library(ggplot2,verbose = F,warn.conflicts = F)


# 搜索结果的url
searched_article_urls <- c('https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2016&page=1',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2017&page=1',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2017&page=2',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2018&page=1',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2019&page=1',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2019&page=2',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2020&page=1',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2020&page=2',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2020&page=3',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2020&page=4',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2020&page=5',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2020&page=6',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2020&page=7',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2020&page=8',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2021&page=1',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2021&page=2',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2021&page=3',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2021&page=4',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2021&page=5',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2021&page=6',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2021&page=7',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2021&page=9',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2021&page=10',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2021&page=11',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2021&page=12',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2021&page=13',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2022&page=1',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2022&page=2',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2022&page=3',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2022&page=4',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2022&page=5',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2022&page=6',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2022&page=7',
                           'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2022&page=9')

searched_review_urls <-  c('https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2016&page=1',   # 2016:5
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2017&page=1',   # 2017:14
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2018&page=1',   # 2018:14
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2019&page=1',   # 2019:16
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2020&page=1', 
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2020&page=2', 
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2020&page=3', 
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2020&page=4',   # 2020:61
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2021&page=1',
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2021&page=2',
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2021&page=3',
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2021&page=4',
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2021&page=5',   # 2021:83
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2022&page=1',
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2022&page=2',
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2022&page=3',
                           'https://www.nature.com/sigtrans/reviews-and-analysis?searchType=journalSearch&sort=PubDate&year=2022&page=4')   # 2022:63


searched_comment_urls <- c('https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2016&page=1',   # 2016:1
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2017&page=1',   # 2017:2
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2019&page=1',   # 2019:11
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2020&page=1', 
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2020&page=2', 
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2020&page=3', 
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2020&page=4',   # 2020:75
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2021&page=1',
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2021&page=2',
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2021&page=3',
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2021&page=4',
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2021&page=5',   # 2021:98
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2022&page=1',
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2022&page=2',
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2022&page=3',
                           'https://www.nature.com/sigtrans/news-and-comment?searchType=journalSearch&sort=PubDate&year=2022&page=4')   # 2022:74

all_searched_urls <- c(searched_article_urls,searched_review_urls,searched_comment_urls)
print(length(all_searched_urls))

get_STTT_all_papers_urls <- function(searched_url,sleep_seconds = 10){
  # searched_url <- 'https://www.nature.com/sigtrans/research-articles?searchType=journalSearch&sort=PubDate&year=2020&page=4'
  webpage <- read_html(searched_url,encoding = 'utf-8')
  paper_urls_tmp <- html_nodes(webpage,'h3 a') %>% as.character()
  paper_urls_tmp
  paper_urls <- sapply(paper_urls_tmp,function(paper_url_tmp){
    # paper_url_tmp <- paper_urls_tmp[1]
    start_loc <- str_locate(paper_url_tmp,'<a href=\"/articles/')[1,2] + 1
    end_loc <- str_locate(paper_url_tmp,'\" class=\"c-card__link')[1,1] - 1
    paper_url_tmp <- str_sub(paper_url_tmp,start_loc,end_loc)
    paper_url_tmp
    out <- paste0('https://www.nature.com/articles/',paper_url_tmp)
    return(out)
  },USE.NAMES = F)
  Sys.sleep(sleep_seconds)
  return(paper_urls)
}

all_papers_urls <- c()
for(idx in 1:length(all_searched_urls)){
  # idx <- 1
  print(idx)
  url <- all_searched_urls[idx]
  all_papers_urls <- c(all_papers_urls,get_STTT_all_papers_urls(url,sleep_seconds = 10))
  
}
length(all_papers_urls)


saveRDS(all_papers_urls,'./others/STTT_papers/all_papers_urls.RDS')








