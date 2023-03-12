# fig3
library(ggplot2)
library(stringr)
library(ggpubr)
library(aplot)
library(dplyr)
setwd('/mdshare/node8/txmdata/Phenomics/')


Subjects <- c(rep('Phenomics',20),
              rep('Bioinformatics/Biostatics',13),
              rep('Genomics',10),
              rep('Imageology',8),
              rep('Botany/Agronomy',5),
              rep('Metabonomics',5),
              rep('Proteomics',4),
              rep('Traditional Chinese Medicine',2),
              rep('Anthropology',2),
              rep('Molecular biology',2),
              rep('Microbiology',2),
              rep('Population genetics',1),
              rep('Epigenomics',1),
              rep('Biomedical engineering',1),
              rep('Healthcare',1))
df <- as.data.frame(sort(table(Subjects),T))

fig3 <- ggplot(df, aes(x = Subjects, y = Freq)) + 
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


ggsave('./figures/fig3.png',fig3,width = 12,height = 6)



