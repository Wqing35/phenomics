# PubMed_Phenomics

file_names <-list.files('../txmdata/test/0830Phenomics/')
full_file_names <- paste0('../txmdata/test/0830Phenomics/',file_names)
full_file_names

data <- sapply(full_file_names,function(full_file_name){
  # full_file_name <- full_file_names[1]
  a <- scan(full_file_name,what = 'c')
  a <- paste(a,collapse='') 
  a_li <- strsplit(a,'PMID-') 
  papaers <- a_li[[1]]
  total_Phenomics_papers <- length(papaers)-1
  China_Phenomics_papers <- length(grep('China',papaers,ignore.case = T)) 
  Fudan_Phenomics_papers <- length(grep('Fudan',papaers,ignore.case = T))
  
  China_NonFudan_Phenomics_papers <- China_Phenomics_papers - Fudan_Phenomics_papers
  Overseas_Phenomics_papers <- total_Phenomics_papers - China_Phenomics_papers
  
  out <- c(total_Phenomics_papers = total_Phenomics_papers,
           China_Phenomics_papers = China_Phenomics_papers,
           Fudan_Phenomics_papers = Fudan_Phenomics_papers)
  return(out)
}) 
df <- as.data.frame(t(data))
rownames(df) <- substr(file_names,1,4)

df$China_NonFudan_Phenomics_papers <- df$China_Phenomics_papers - df$Fudan_Phenomics_papers
df$Overseas_Phenomics_papers <- df$total_Phenomics_papers - df$China_Phenomics_papers
df$year <- rownames(df)
df <- df[,c('Fudan_Phenomics_papers', 'China_NonFudan_Phenomics_papers', 'Overseas_Phenomics_papers', 'year')]



library(ggplot2)
library(dplyr)
plot_data <- reshape2::melt(df)
DATA <- plot_data %>% 
  group_by(year) %>%
  mutate(total = sum(value),
         prop = value/total) %>%
  ungroup() 
DATA$variable <- factor(DATA$variable,levels = rev(levels(DATA$variable)))

ggplot(DATA) + 
  geom_bar(aes(x = year, y = value,fill = variable, group = variable,color = variable), stat = "identity") +
  geom_text(aes(x = year,y = value,label = paste0(round(prop*100,0),'%')),position = position_stack(.5)) + 
  geom_text(aes(x = year,y = total,label = total),position = position_dodge(0.8), vjust = -0.5)




