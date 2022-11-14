library(dplyr)

plot_file_names <- list.files('../Phenomics/weekly_online_paper_metrices/output/')
file_name <- rev(plot_file_names)[1]
file_name
full_file_name <- paste0('/mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/output/',file_name)
out_df <- readxl::read_excel(full_file_name)
# knitr::kable(out_df)
head(out_df)

subject <- c('Metabonomics','','Metabonomics','Traditional Chinese Medicine','Phenomics')

Subjects <- c(rep('Phenomics',19),
              rep('Bioinformatics/Biostatics',12),
              rep('Genomics',8),
              rep('Imageology',7),
              rep('Botany/Agronomy',5),
              rep('Metabonomics',5),
              rep('Proteomics',3),
              rep('Traditional Chinese Medicine',2),
              rep('Anthropology',2),
              rep('Molecular biology',2),
              rep('Microbiology',2),
              rep('Population genetics',1),
              rep('Epigenomics',1),
              rep('Biomedical engineering',1),
              rep('Healthcare',1))
df <- as.data.frame(sort(table(Subjects),T))

ggplot(df, aes(x = Subjects, y = Freq)) + 
  geom_bar(aes(color = Subjects,fill = Subjects), stat = "identity", 
           position = position_dodge(width = 0.8, preserve = "single"), width = 0.7) + 
  geom_text(aes(label = Freq), size = 4, position = position_dodge(0.8), vjust = -0.2) + 
  theme_light() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12,color = 'black',angle = 45,vjust = 1,hjust = 1),
        axis.title.y = element_text(size = 14,color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = .5,size = 15),
        plot.margin = unit(c(.2,.2,.2,1.2),units = 'cm'),
        legend.position = "none") +
  ggtitle("Phenomics Paper Subject Distribution") 

