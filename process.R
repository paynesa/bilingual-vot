library(dplyr)
library(ggplot2)
#Read in the data from the CSV files 
english <- read.csv("eng_vot_data.csv")
spanish <- read.csv("spa_vot_data.csv")

#get the spanish data for each speaker separately 
fer1_spa <- spanish %>% filter(Speaker == 'ferfulice_1')
fer2_spa <- spanish %>% filter(Speaker == 'ferfulice_2')
deu_spa <- spanish %>% filter(Speaker == 'deuchar')

#get the english data for each speaker separately 
fer1_eng <- english %>% filter(Speaker == 'ferfulice_1')
fer2_eng <- english %>% filter(Speaker == 'ferfulice_2')
deu_eng <- english %>% filter(Speaker == 'deuchar')

#extract segments individually 
d_eng <- english %>% filter(Consonant == 'd')
d_spa <- spanish %>% filter(Consonant == 'd')
t_eng <- english %>% filter(Consonant == 't')
t_spa <- spanish %>% filter(Consonant == 't')
b_eng <- english %>% filter(Consonant == 'b')
b_spa <- spanish %>% filter(Consonant == 'b')
p_eng <- english %>% filter(Consonant == 'p')
p_spa <- spanish %>% filter(Consonant == 'p')

#weird ggplot stuff from the internet
specie <- c(rep("d", 2), rep("t", 2), rep("b", 2), rep("p", 2))
condition <- rep(c("Spanish" , "English") , 4)
value <- c(mean(d_spa$VOT), mean(d_eng$VOT), mean(t_spa$VOT),mean(t_eng$VOT),  mean(b_spa$VOT),mean(b_eng$VOT),  mean(p_spa$VOT),  mean(p_eng$VOT))
sd <- c(sd(d_spa$VOT), sd(d_eng$VOT), sd(t_spa$VOT), sd(t_eng$VOT), sd(b_spa$VOT), sd(b_eng$VOT), sd(p_spa$VOT),  sd(p_eng$VOT))
data <- data.frame(specie,condition,value)
theme_set(theme_bw())
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity") + xlab("") + scale_fill_brewer(palette="Dark2")+ ylab("VOT (seconds)") +  ggtitle("Average VOT across 3 Simultaneous Spanish-English Bilingual 2-year-olds") +geom_errorbar(aes(ymin=value-sd, ymax=value+sd), size=0.25, width=0.25, position=position_dodge(.9)) 

