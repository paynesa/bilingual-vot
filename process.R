library(dplyr)

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

#get the means
means_english <- c(mean(d_eng$VOT), mean(t_eng$VOT), mean(b_eng$VOT), mean(p_eng$VOT))
means_spanish <- c(mean(d_spa$VOT), mean(t_spa$VOT), mean(b_spa$VOT), mean(p_spa$VOT))
table_means <- c(means_english, means_spanish)



barplot(table_means, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        beside=TRUE)
