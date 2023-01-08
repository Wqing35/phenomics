# phenomics_author_email

library(RISmed)
library(stringr)

get_one_year_papers <- function(year,keyword = 'phenomics'){
  # year <- 2013
  # keyword <- 'phenomics'
  all_PMIDs <- EUtilsSummary(keyword, type="esearch", db="pubmed", mindate=year, maxdate=year+1,datetype = 'pdat')
  print(paste0('Year: ',year,', total ',all_PMIDs@count,' papers'))
  all_papers <- EUtilsGet(all_PMIDs)
  all_papers
  
  have_email_paper_idx <- grep('@',all_papers@Affiliation)
  print(paste0("Total ",length(have_email_paper_idx)," papers have authors' emails"))
  
  out_df <- lapply(have_email_paper_idx,function(idx){
    # idx = 5
    affiliation <- all_papers@Affiliation[[idx]]
    have_email_author_idx <- grep('@',affiliation)
    have_email_author_idx
    Email_tmps <- str_extract(affiliation[have_email_author_idx],pattern = "\\S*@.*")
    Email_tmps
    if(str_sub(Email_tmps,start = str_length(Email_tmps)) == '.'){
      Emails <- str_sub(Email_tmps,start = 0,end = str_length(Email_tmps)-1)
    } else{
      Emails <- Email_tmps
    }

    this_out_df <- data.frame(AuthorFamilyName = all_papers@Author[[idx]][have_email_author_idx,'LastName'],
                              AuthorForeName = all_papers@Author[[idx]][have_email_author_idx,'ForeName'],
                              Email = Emails,
                              PMID = rep(all_papers@PMID[idx],length(have_email_author_idx)),
                              Journal = rep(all_papers@ISOAbbreviation[idx],length(have_email_author_idx)),
                              PubYear = rep(all_papers@YearPubDate[idx],length(have_email_author_idx)),
                              Keywords = paste0(all_papers@Keywords[[idx]],collapse = ', '),
                              ArticleTitle = rep(all_papers@ArticleTitle[idx],length(have_email_author_idx)),
                              Institute = affiliation[have_email_author_idx],
                              row.names = NULL)
    return(this_out_df)
  }) %>% do.call(what = 'rbind')
  
  return(out_df)
  
}



df_2012 <- get_one_year_papers(year = 2012)
df_2013 <- get_one_year_papers(year = 2013)
df_2014 <- get_one_year_papers(year = 2014)
df_2015 <- get_one_year_papers(year = 2015)
df_2016 <- get_one_year_papers(year = 2016)
df_2017 <- get_one_year_papers(year = 2017)
df_2018 <- get_one_year_papers(year = 2018)
df_2019 <- get_one_year_papers(year = 2019)
df_2020 <- get_one_year_papers(year = 2020)
df_2021 <- get_one_year_papers(year = 2021)
df_2022 <- get_one_year_papers(year = 2022)

phenomics_author_email <- rbind(df_2022,df_2021,df_2020,df_2019,df_2018,
                                df_2017,df_2016,df_2015,df_2014,df_2013,df_2012)

write.table(phenomics_author_email,'./Desktop/phenomics_author_email.txt',sep = '\t',quote = F,row.names = F)


