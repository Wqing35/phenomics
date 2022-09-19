# paper_journals
library(stringr,verbose = F,warn.conflicts = F)
library(ggplot2)
library(dplyr)
library(txmBioinfoToolkit)
library(writexl)

file_names <-list.files('./others/Pubmed_phenomics/download_files/')

all_papers_df <- pbmcapply::pbmclapply(file_names,function(file_name){
  # file_name <- file_names[2]
  gold_year <- str_sub(file_name,1,4)
  full_file_name <- paste0('./others/Pubmed_phenomics/download_files/',file_name)
  string <- scan(full_file_name,what = 'c')
  # string <- readLines(full_file_name)
  string <- paste(string,collapse = '==') 
  string_li <- str_split(string,'PMID-') 
  papers <- string_li[[1]]
  papers <- papers[-1]
  print(length(papers))
  # 不要直接查看papers，会卡
  one_year_df <- sapply(papers,function(paper){
    # paper <- papers[10]
    
    # journal
    start_loc <- str_locate(paper,'==TA==-==')[2] + 1
    end_loc <- str_locate(paper,'==JT==-==')[1] - 1
    journal_tmp <- str_sub(paper,start_loc,end_loc)
    journal <- str_split(journal_tmp,'==')[[1]] %>% paste0(collapse = ' ')
    
    # PMID
    start_loc <- 3
    end_loc <- str_locate(paper,'==OWN==-==')[1] - 1
    PMID <- str_sub(paper,start_loc,end_loc)
    
    # title
    start_loc <- str_locate(paper,'==TI==-==')[2] + 1
    end_loc <- str_locate(paper,'==PG==-==')[1] - 1
    title_tmp <- str_sub(paper,start_loc,end_loc)
    title <- str_split(title_tmp,'==')[[1]] %>% paste0(collapse = ' ')
    
    # time
    start_loc <- str_locate(paper,'==DEP==-==')[2] + 1
    end_loc <- start_loc + 7
    time <- str_sub(paper,start_loc,end_loc)
    
    # year
    year <- str_sub(time,1,4)
    if(is.na(year)){year <- gold_year}
    
    # is_China
    if(str_detect(paper,'[C,c]hina')){
      is_China <- T
    } else{
      is_China <- F
    }
    
    # is_Fudan
    if(str_detect(paper,'[F,f]udan')){
      is_Fudan <- T
    } else{
      is_Fudan <- F
    }
    
    # Institution
    Institution <- 'China'
    if(is_China == F){Institution <- 'Overseas'}
    if(is_Fudan == T){Institution <- 'Fudan'}
    
    out <- c(PMID = PMID,
             Institution = Institution,
             Journal = journal,
             year = year,
             title = title,
             is_China = is_China,
             is_Fudan = is_Fudan,
             time = time)
    return(out)
  },USE.NAMES = F) %>% t() %>% as.data.frame()
  
  return(one_year_df)
},mc.cores = 10) %>% do.call(what = 'rbind')

head(all_papers_df)

all_papers_df$PMID <- as.numeric(all_papers_df$PMID)
all_papers_df$year <- as.numeric(all_papers_df$year)
all_papers_df$time <- as.numeric(all_papers_df$time)

# institution_barplot
ggplot(all_papers_df,aes(x = year)) + 
  geom_bar(width = 0.8, aes(fill = Institution)) + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

saveRDS(all_papers_df,'./others/Pubmed_phenomics/all_papers_df.RDS')
write_xlsx(all_papers_df,'./others/Pubmed_phenomics/PubMed_Phenomics_papers.xlsx')


# journal_barplot
plot_data_all <- table(all_papers_df$Journal) %>% sort(T) %>% as.data.frame()
colnames(plot_data_all) <- c('Journal','Frequency')
plot_data <- head(plot_data_all,20)

ggplot(plot_data, aes(x = Journal, y = Frequency,color = Journal,fill = Journal)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8, preserve = "single"), width = 0.7) + 
  geom_text(aes(label = round(Frequency,2)), size = 5, position = position_dodge(0.8), vjust = -0.5) + 
  theme_light() + 
  ggtitle('Top 20 Phenomics journals') + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,size = 12,colour = 'black'),
        plot.title = element_text(hjust = 0.5))

saveRDS(plot_data_all,'./others/Pubmed_phenomics/plot_data_all.RDS')
write_xlsx(plot_data_all,'./others/Pubmed_phenomics/PubMed_Phenomics_journals.xlsx')

