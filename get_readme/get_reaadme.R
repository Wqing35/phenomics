# get_reaadme
library(kableExtra)
library(knitr)

plot_file_names <- list.files('./weekly_online_paper_metrices/output/')
file_name <- plot_file_names[1]
file_name
full_file_name <- paste0('/mdshare/node8/txmdata/test/Phenomics/weekly_online_paper_metrices/output/',file_name)
out_df <- read.table(full_file_name,sep = '\t',header = T)
out_df$update_time <- NULL


# knitr::kable(out_df)

out_df$title <- strsplit(out_df$title,' ') %>% 
  sapply(function(title){
    paste(c(title[1:5],'...'),collapse = ' ')
  })

urls <- read.table('/mdshare/node8/txmdata/test/Phenomics/weekly_online_paper_metrices/0_urls.txt')$V1

out_df %>% 
  mutate(title2 = cell_spec(title, "html", link = urls)) %>%
  kable(escape = FALSE) %>%
  kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
  cat(., file = "df.html")

colnames(out_df) <- c("Article title","Article type","Online time","Access","Citation","Altmetric","Corresponding authors")





