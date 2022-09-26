# get_reaadme
# download csv:
# https://link.springer.com/search?query=&search-within=Journal&facet-journal-id=43657
library(kableExtra)
library(knitr)
library(dplyr)

plot_file_names <- list.files('./weekly_online_paper_metrices/output/')
file_name <- rev(plot_file_names)[1]
file_name
full_file_name <- paste0('/mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/output/',file_name)
out_df <- read.table(full_file_name,sep = '\t',header = T)
# knitr::kable(out_df)
head(out_df)

out_df <- mutate(out_df,
                 short_title = strsplit(title,' ') %>% 
                   sapply(function(one_title){
                     paste(c(one_title[1:5],'...'),collapse = ' ')
                   }),
                 short_title_with_link = cell_spec(short_title, "html", link = url)) 
colnames(out_df)

readme_table <- select(out_df,
                       `Article title` = short_title_with_link,
                       `Article type` = type,
                       `Online time` = time,
                       Access = access,
                       Citation = citation,
                       Altmetric = altmetric,
                       `Corresponding authors` = correspond_authors,
                       Volume = volume,
                       Issue = issue)
readme_kable <- kable(readme_table,escape = F)
cat(readme_kable, file = "./weekly_online_paper_metrices/readme_kable.txt")




