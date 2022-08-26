# get_online_paper_metrices
library(dplyr,verbose = F,warn.conflicts = F)
library(rvest,verbose = F,warn.conflicts = F)
library(xml2,verbose = F,warn.conflicts = F)
library(stringr,verbose = F,warn.conflicts = F)
library(ggplot2,verbose = F,warn.conflicts = F)

sys.time <- Sys.time() %>% as.character()
update_time <- strsplit(sys.time,' ')[[1]][1]
update_time

urls <- read.table('/mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/0_urls.txt')$V1

get_online_paper_metrices <- function(url){
  # url <- urls[4]
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

  # time
  time_line <- html_nodes(webpage,'li a')[grep('Published',html_nodes(webpage,'li a'))] %>% as.character()
  time_start_loc <- str_locate(time_line,'Published: <time datetime=\"')[2] + 1
  time_end_loc <- time_start_loc + 9
  time <- str_sub(time_line,time_start_loc,time_end_loc)
  time

  return(c(title = title,
           type = type,
           time = time,
           access = access,
           citation = citation,
           altmetric = altmetric,
           correspond_authors = correspond_authors,
           update_time = update_time))
}

out <- c()
for(url in urls){
  print(url)
  this_out <- get_online_paper_metrices(url)
  out <- rbind(out,this_out)
  # print(this_out)
}
out_df <- as.data.frame(out)
rownames(out_df) <- NULL
out_df$access <- as.numeric(out_df$access)
out_df$citation <- as.numeric(out_df$citation)
head(out_df)
sum(out_df$access)
sum(out_df$citation)

out_file_name <- paste0('/mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/output/',update_time,'.txt')
out_file_name
write.table(x = out_df,file = out_file_name,quote = F,sep = '\t',row.names = F)





