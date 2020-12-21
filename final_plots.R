library(dplyr)
library(ggplot2)
#Read in the data from the CSV files 
english <- read.csv("cleaned_eng.csv")
spanish <- read.csv("cleaned_spa.csv")

#bilabial stops
english_b <-english %>% filter(Consonant == 'b')
english_p <-english %>% filter(Consonant == 'p')
spanish_b <- spanish  %>% filter(Consonant == 'b')
spanish_p <- spanish %>% filter(Consonant == 'p')

#alveolar stops
english_d <-english %>% filter(Consonant == 'd')
english_t <-english %>% filter(Consonant == 't')
spanish_d <- spanish  %>% filter(Consonant == 'd')
spanish_t <- spanish %>% filter(Consonant == 't')

#overall plot 
specie <- c(rep("/d/", 2), rep("/t/", 2), rep("/b/", 2), rep("/p/", 2))
condition <- rep(c("Spanish" , "English") , 4)
value <- c(mean(spanish_d$VOT), mean(english_d$VOT), mean(spanish_t$VOT),mean(english_t$VOT),  mean(spanish_b$VOT),mean(english_b$VOT),  mean(spanish_p$VOT),  mean(english_p$VOT))
sd <- c(sd(spanish_d$VOT), sd(english_d$VOT), sd(spanish_t$VOT), sd(english_t$VOT),  sd(spanish_b$VOT),sd(english_b$VOT),  sd(spanish_p$VOT),  sd(english_p$VOT))
data <- data.frame(specie,condition,value)
theme_set(theme_bw())
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity") + xlab("") + scale_fill_brewer(palette="Dark2")+ ylab("VOT (seconds)") +  ggtitle("Average VOT across all Speakers") +geom_errorbar(aes(ymin=value-sd, ymax=value+sd), size=0.25, width=0.25, position=position_dodge(.9))+theme( plot.title = element_text(hjust = 0.5))

#now it's time for some t-tests
t.test(spanish_b$VOT, english_b$VOT, paired=FALSE, conf.level = 0.95)
t.test(spanish_p$VOT, english_p$VOT, paired=FALSE, conf.level = 0.95)
t.test(spanish_d$VOT, english_d$VOT, paired=FALSE, conf.level = 0.95)
t.test(spanish_t$VOT, english_t$VOT, paired=FALSE, conf.level = 0.95)


#bilabial stops for each speaker 
f1_eng_b <- english_b %>% filter(Speaker == 'ferfulice_1')
f1_eng_p <- english_p %>% filter(Speaker == 'ferfulice_1')
f2_eng_b <- english_b %>% filter(Speaker == 'ferfulice_2')
f2_eng_p <- english_p %>% filter(Speaker == 'ferfulice_2')
deuchar_eng_b <- english_b %>% filter(Speaker == 'deuchar')
deuchar_eng_p <- english_p %>% filter(Speaker == 'deuchar')
alberto_eng_b <- english_b %>% filter(Speaker == 'perez_alberto')
alberto_eng_p <- english_p %>% filter(Speaker == 'perez_alberto')
carla_eng_b <- english_b %>% filter(Speaker == 'perez_carla')
carla_eng_p <- english_p %>% filter(Speaker == 'perez_carla')
john_eng_b <- english_b %>% filter(Speaker == 'perez_john')
john_eng_p <- english_p %>% filter(Speaker == 'perez_john')
shiela_eng_b <- english_b %>% filter(Speaker == 'perez_shiela')
shiela_eng_p <- english_p %>% filter(Speaker == 'perez_shiela')
tina_eng_b <- english_b %>% filter(Speaker == 'perez_tina')
tina_eng_p <- english_p %>% filter(Speaker == 'perez_tina')

f1_spa_b <- spanish_b %>% filter(Speaker == 'ferfulice_1')
f1_spa_p <- spanish_p %>% filter(Speaker == 'ferfulice_1')
f2_spa_b <- spanish_b %>% filter(Speaker == 'ferfulice_2')
f2_spa_p <- spanish_p  %>% filter(Speaker == 'ferfulice_2')
deuchar_spa_b <- spanish_b %>% filter(Speaker == 'deuchar')
deuchar_spa_p <- spanish_p %>% filter(Speaker == 'deuchar')
alberto_spa_b <- spanish_b %>% filter(Speaker == 'perez_alberto')
alberto_spa_p <- spanish_p %>% filter(Speaker == 'perez_alberto')
carla_spa_b <- spanish_b %>% filter(Speaker == 'perez_carla')
carla_spa_p <- spanish_p %>% filter(Speaker == 'perez_carla')
john_spa_b <- spanish_b %>% filter(Speaker == 'perez_john')
john_spa_p <- spanish_p  %>% filter(Speaker == 'perez_john')
shiela_spa_b <- spanish_b %>% filter(Speaker == 'perez_shiela')
shiela_spa_p <- spanish_p  %>% filter(Speaker == 'perez_shiela')
tina_spa_b <- spanish_b %>% filter(Speaker == 'perez_tina')
tina_spa_p <- spanish_p  %>% filter(Speaker == 'perez_tina')

specie <- c(rep("F1", 4), rep("F2", 4), rep("M (Deuchar)", 4), rep("Alberto", 4), rep("Carla", 4), rep("John", 4),  rep("Sheila", 4),  rep("Tina", 4))
condition <- rep(c("English /b/", "English /p/", "Spanish /b/", "Spanish /p/") , 8)
value <- c(mean(f1_eng_b$VOT), mean(f1_eng_p$VOT), mean(f1_spa_b$VOT), mean(f1_spa_p$VOT),mean(f2_eng_b$VOT), mean(f2_eng_p$VOT), mean(f2_spa_b$VOT), mean(f2_spa_p$VOT), mean(deuchar_eng_b$VOT), mean(deuchar_eng_p$VOT), mean(deuchar_spa_b$VOT), mean(deuchar_spa_p$VOT),  mean(alberto_eng_b$VOT), mean(alberto_eng_p$VOT), mean(alberto_spa_b$VOT), mean(alberto_spa_p$VOT),  mean(carla_eng_b$VOT), mean(carla_eng_p$VOT), mean(carla_spa_b$VOT), mean(carla_spa_p$VOT),  mean(john_eng_b$VOT), mean(john_eng_p$VOT), mean(john_spa_b$VOT), mean(john_spa_p$VOT),  mean(shiela_eng_b$VOT), mean(shiela_eng_p$VOT), mean(shiela_spa_b$VOT), mean(shiela_spa_p$VOT), mean(tina_eng_b$VOT), mean(tina_eng_p$VOT), mean(tina_spa_b$VOT), mean(tina_spa_p$VOT))
sd <- c(sd(f1_eng_b$VOT), sd(f1_eng_p$VOT), sd(f1_spa_b$VOT), sd(f1_spa_p$VOT), sd(f2_eng_b$VOT), sd(f2_eng_p$VOT), sd(f2_spa_b$VOT), sd(f2_spa_p$VOT), sd(deuchar_eng_b$VOT), sd(deuchar_eng_p$VOT), sd(deuchar_spa_b$VOT), sd(deuchar_spa_p$VOT),  sd(alberto_eng_b$VOT), sd(alberto_eng_p$VOT), sd(alberto_spa_b$VOT), sd(alberto_spa_p$VOT), sd(carla_eng_b$VOT), sd(carla_eng_p$VOT), sd(carla_spa_b$VOT), sd(carla_spa_p$VOT), sd(john_eng_b$VOT), sd(john_eng_p$VOT), sd(john_spa_b$VOT), sd(john_spa_p$VOT),  sd(shiela_eng_b$VOT), sd(shiela_eng_p$VOT), sd(shiela_spa_b$VOT), sd(shiela_spa_p$VOT),sd(tina_eng_b$VOT), sd(tina_eng_p$VOT), sd(tina_spa_b$VOT),sd(tina_spa_p$VOT))
data <- data.frame(specie,condition,value)
theme_set(theme_bw())
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity") + xlab("") + scale_fill_brewer(palette="Dark2")+ ylab("VOT (seconds)") +  ggtitle("Bilabial Stop VOT, Separated by Speaker") +geom_errorbar(aes(ymin=value-sd, ymax=value+sd), size=0.25, width=0.25, position=position_dodge(.9))+theme( plot.title = element_text(hjust = 0.5))



#alveolar stops for each speaker 
f1_eng_d <- english_d %>% filter(Speaker == 'ferfulice_1')
f1_eng_t <- english_t %>% filter(Speaker == 'ferfulice_1')
f2_eng_d <- english_d %>% filter(Speaker == 'ferfulice_2')
f2_eng_t <- english_t %>% filter(Speaker == 'ferfulice_2')
deuchar_eng_d <- english_d %>% filter(Speaker == 'deuchar')
deuchar_eng_t <- english_t %>% filter(Speaker == 'deuchar')
carla_eng_d <- english_d %>% filter(Speaker == 'perez_carla')
carla_eng_t <- english_t %>% filter(Speaker == 'perez_carla')
shiela_eng_d <- english_d %>% filter(Speaker == 'perez_shiela')
shiela_eng_t <- english_t %>% filter(Speaker == 'perez_shiela')
tina_eng_d <- english_d %>% filter(Speaker == 'perez_tina')
tina_eng_t <- english_t %>% filter(Speaker == 'perez_tina')

f1_spa_d <- spanish_d %>% filter(Speaker == 'ferfulice_1')
f1_spa_t <- spanish_t %>% filter(Speaker == 'ferfulice_1')
f2_spa_d <- spanish_d %>% filter(Speaker == 'ferfulice_2')
f2_spa_t <- spanish_t  %>% filter(Speaker == 'ferfulice_2')
deuchar_spa_d <- spanish_d %>% filter(Speaker == 'deuchar')
deuchar_spa_t <- spanish_t %>% filter(Speaker == 'deuchar')
carla_spa_d <- spanish_d %>% filter(Speaker == 'perez_carla')
carla_spa_t <- spanish_t %>% filter(Speaker == 'perez_carla')
shiela_spa_d <- spanish_d %>% filter(Speaker == 'perez_shiela')
shiela_spa_t <- spanish_t  %>% filter(Speaker == 'perez_shiela')
tina_spa_d <- spanish_d %>% filter(Speaker == 'perez_tina')
tina_spa_t <- spanish_t  %>% filter(Speaker == 'perez_tina')

specie <- c(rep("F1", 4), rep("F2", 4), rep("M (Deuchar)", 4), rep("Carla", 4),  rep("Sheila", 4),  rep("Tina", 4))
condition <- rep(c("English /d/", "English /t/", "Spanish /d/", "Spanish /t/") , 6)
value <- c(mean(f1_eng_d$VOT), mean(f1_eng_t$VOT), mean(f1_spa_d$VOT), mean(f1_spa_t$VOT),mean(f2_eng_d$VOT), mean(f2_eng_t$VOT), mean(f2_spa_d$VOT), mean(f2_spa_t$VOT), mean(deuchar_eng_d$VOT), mean(deuchar_eng_t$VOT), mean(deuchar_spa_d$VOT), mean(deuchar_spa_t$VOT), mean(carla_eng_d$VOT), mean(carla_eng_t$VOT), mean(carla_spa_d$VOT), mean(carla_spa_t$VOT), mean(shiela_eng_d$VOT), mean(shiela_eng_t$VOT), mean(shiela_spa_d$VOT), mean(shiela_spa_t$VOT), mean(tina_eng_d$VOT), mean(tina_eng_t$VOT), mean(tina_spa_d$VOT), mean(tina_spa_t$VOT))
sd <- c(sd(f1_eng_d$VOT), sd(f1_eng_t$VOT), sd(f1_spa_d$VOT), sd(f1_spa_t$VOT), sd(f2_eng_d$VOT), sd(f2_eng_t$VOT), sd(f2_spa_d$VOT), sd(f2_spa_t$VOT), sd(deuchar_eng_d$VOT), sd(deuchar_eng_t$VOT), sd(deuchar_spa_d$VOT), sd(deuchar_spa_t$VOT), sd(carla_eng_d$VOT), sd(carla_eng_t$VOT), sd(carla_spa_d$VOT), sd(carla_spa_t$VOT), sd(shiela_eng_d$VOT), sd(shiela_eng_t$VOT), sd(shiela_spa_d$VOT), sd(shiela_spa_t$VOT),sd(tina_eng_d$VOT), sd(tina_eng_t$VOT), sd(tina_spa_d$VOT),sd(tina_spa_t$VOT))
data <- data.frame(specie,condition,value)
theme_set(theme_bw())
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity") + xlab("") + scale_fill_brewer(palette="Dark2")+ ylab("VOT (seconds)") +  ggtitle("Alveolar Stop VOT, Separated by Speaker") +geom_errorbar(aes(ymin=value-sd, ymax=value+sd), size=0.25, width=0.25, position=position_dodge(.9))+theme( plot.title = element_text(hjust = 0.5))

#birthplace t-tests for English 
american_eng_b <- english_b %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' & Speaker != 'perez_tina')
spanish_eng_b <- english_b %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' | Speaker == 'perez_tina')
t.test(american_eng_b$VOT, spanish_eng_b$VOT, paired = FALSE, conf.level = 0.95)
american_eng_p <- english_p %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' & Speaker != 'perez_tina')
spanish_eng_p <- english_p %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' | Speaker == 'perez_tina')
t.test(american_eng_p$VOT, spanish_eng_p$VOT, paired = FALSE, conf.level = 0.95)
american_eng_d <- english_d %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' & Speaker != 'perez_tina')
spanish_eng_d <- english_d %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' | Speaker == 'perez_tina')
t.test(american_eng_d$VOT, spanish_eng_d$VOT, paired = FALSE, conf.level = 0.95)
american_eng_t <- english_t %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' & Speaker != 'perez_tina')
spanish_eng_t <- english_t %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' | Speaker == 'perez_tina')
t.test(american_eng_t$VOT, spanish_eng_t$VOT, paired = FALSE, conf.level = 0.95)


#birthplace t-tests for Spanish
american_spa_b <- spanish_b %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' & Speaker != 'perez_tina')
spanish_spa_b <- spanish_b %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' | Speaker == 'perez_tina')
t.test(american_spa_b$VOT, spanish_spa_b$VOT, paired = FALSE, conf.level = 0.95)
american_spa_p <- spanish_p %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' & Speaker != 'perez_tina')
spanish_spa_p <- spanish_p %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' | Speaker == 'perez_tina')
t.test(american_spa_p$VOT, spanish_spa_p$VOT, paired = FALSE, conf.level = 0.95)
american_spa_d <- spanish_d %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' & Speaker != 'perez_tina')
spanish_spa_d <- spanish_d %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' | Speaker == 'perez_tina')
t.test(american_spa_d$VOT, spanish_spa_d$VOT, paired = FALSE, conf.level = 0.95)
american_spa_t <- spanish_t %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' & Speaker != 'perez_tina')
spanish_spa_t <- spanish_t %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' | Speaker == 'perez_tina')
t.test(american_spa_t$VOT, spanish_spa_t$VOT, paired = FALSE, conf.level = 0.95)

#nationality t-tests for English 
american_eng_b <- english_b %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' )
spanish_eng_b <- english_b %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1')
t.test(american_eng_b$VOT, spanish_eng_b$VOT, paired = FALSE, conf.level = 0.95)
american_eng_p <- english_p %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1')
spanish_eng_p <- english_p %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' )
t.test(american_eng_p$VOT, spanish_eng_p$VOT, paired = FALSE, conf.level = 0.95)
american_eng_d <- english_d %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' )
spanish_eng_d <- english_d %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' )
t.test(american_eng_d$VOT, spanish_eng_d$VOT, paired = FALSE, conf.level = 0.95)
american_eng_t <- english_t %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' )
spanish_eng_t <- english_t %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' )
t.test(american_eng_t$VOT, spanish_eng_t$VOT, paired = FALSE, conf.level = 0.95)


#nationality t-tests for Spanish
american_spa_b <- spanish_b %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' )
spanish_spa_b <- spanish_b %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' )
t.test(american_spa_b$VOT, spanish_spa_b$VOT, paired = FALSE, conf.level = 0.95)
american_spa_p <- spanish_p %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' )
spanish_spa_p <- spanish_p %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' )
t.test(american_spa_p$VOT, spanish_spa_p$VOT, paired = FALSE, conf.level = 0.95)
american_spa_d <- spanish_d %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1')
spanish_spa_d <- spanish_d %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1')
t.test(american_spa_d$VOT, spanish_spa_d$VOT, paired = FALSE, conf.level = 0.95)
american_spa_t <- spanish_t %>% filter(Speaker != 'ferfulice_2' & Speaker != 'ferfulice_1' )
spanish_spa_t <- spanish_t %>% filter(Speaker == 'ferfulice_2' | Speaker == 'ferfulice_1' )
t.test(american_spa_t$VOT, spanish_spa_t$VOT, paired = FALSE, conf.level = 0.95)





#mother's L1 t-tests for English 
american_eng_b <- english_b %>% filter(Speaker != 'perez_shiela' & Speaker != 'perez_carla' & Speaker != 'perez_tina')
spanish_eng_b <- english_b %>% filter(Speaker == 'perez_shiela' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
t.test(american_eng_b$VOT, spanish_eng_b$VOT, paired = FALSE, conf.level = 0.95)
american_eng_p <- english_p %>% filter(Speaker != 'perez_shiela' & Speaker != 'perez_carla' & Speaker != 'perez_tina')
spanish_eng_p <- english_p %>% filter(Speaker == 'perez_shiela' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
t.test(american_eng_p$VOT, spanish_eng_p$VOT, paired = FALSE, conf.level = 0.95)
american_eng_d <- english_d %>% filter(Speaker != 'perez_shiela' & Speaker != 'perez_carla' & Speaker != 'perez_tina')
spanish_eng_d <- english_d %>% filter(Speaker == 'perez_shiela' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
t.test(american_eng_d$VOT, spanish_eng_d$VOT, paired = FALSE, conf.level = 0.95)
american_eng_t <- english_t %>% filter(Speaker != 'perez_shiela' & Speaker != 'perez_carla' & Speaker != 'perez_tina' )
spanish_eng_t <- english_t %>% filter(Speaker == 'perez_shiela' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
t.test(american_eng_t$VOT, spanish_eng_t$VOT, paired = FALSE, conf.level = 0.95)


#mother's L1 t-tests for Spanish
american_spa_b <- spanish_b %>% filter(Speaker != 'perez_shiela' & Speaker != 'perez_carla' & Speaker != 'perez_tina' )
spanish_spa_b <- spanish_b %>% filter(Speaker == 'perez_shiela' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
t.test(american_spa_b$VOT, spanish_spa_b$VOT, paired = FALSE, conf.level = 0.95)
american_spa_p <- spanish_p %>% filter(Speaker != 'perez_shiela' & Speaker != 'perez_carla' & Speaker != 'perez_tina' )
spanish_spa_p <- spanish_p %>% filter(Speaker == 'perez_shiela' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
t.test(american_spa_p$VOT, spanish_spa_p$VOT, paired = FALSE, conf.level = 0.95)
american_spa_d <- spanish_d %>% filter(Speaker != 'perez_shiela' & Speaker != 'perez_carla' & Speaker != 'perez_tina')
spanish_spa_d <- spanish_d %>% filter(Speaker == 'perez_shiela' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
t.test(american_spa_d$VOT, spanish_spa_d$VOT, paired = FALSE, conf.level = 0.95)
american_spa_t <- spanish_t %>% filter(Speaker != 'perez_shiela' & Speaker != 'perez_carla' & Speaker != 'perez_tina')
spanish_spa_t <- spanish_t %>% filter(Speaker == 'perez_shiela' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
t.test(american_spa_t$VOT, spanish_spa_t$VOT, paired = FALSE, conf.level = 0.95)



#father's L1 t-tests for English 
american_eng_b <- english_b %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
spanish_eng_b <- english_b %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'deuchar' | Speaker == 'perez_john' )
t.test(american_eng_b$VOT, spanish_eng_b$VOT, paired = FALSE, conf.level = 0.95)
american_eng_p <- english_p %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
spanish_eng_p <- english_p %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'deuchar' | Speaker == 'perez_john' )
t.test(american_eng_p$VOT, spanish_eng_p$VOT, paired = FALSE, conf.level = 0.95)
mean(american_eng_p$VOT)
american_eng_d <- english_d %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
spanish_eng_d <- english_d %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'deuchar' | Speaker == 'perez_john' )
t.test(american_eng_d$VOT, spanish_eng_d$VOT, paired = FALSE, conf.level = 0.95)
american_eng_t <- english_t %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
spanish_eng_t <- english_t %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'deuchar' | Speaker == 'perez_john' )
t.test(american_eng_t$VOT, spanish_eng_t$VOT, paired = FALSE, conf.level = 0.95)


#father's L1 t-tests for Spanish
american_spa_b <- spanish_b %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
spanish_spa_b <- spanish_b %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'deuchar' | Speaker == 'perez_john' )
t.test(american_spa_b$VOT, spanish_spa_b$VOT, paired = FALSE, conf.level = 0.95)
american_spa_p <- spanish_p %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
spanish_spa_p <- spanish_p %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'deuchar' | Speaker == 'perez_john' )
t.test(american_spa_p$VOT, spanish_spa_p$VOT, paired = FALSE, conf.level = 0.95)
american_spa_d <- spanish_d %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
spanish_spa_d <- spanish_d %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'deuchar' | Speaker == 'perez_john' )
t.test(american_spa_d$VOT, spanish_spa_d$VOT, paired = FALSE, conf.level = 0.95)
american_spa_t <- spanish_t %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_carla' | Speaker == 'perez_tina')
spanish_spa_t <- spanish_t %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'deuchar' | Speaker == 'perez_john' )
t.test(american_spa_t$VOT, spanish_spa_t$VOT, paired = FALSE, conf.level = 0.95)


#Language used by mother t-tests for English 
american_eng_b <- english_b %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'perez_john')
spanish_eng_b <- english_b %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_shiela' | Speaker == 'perez_tina' )
t.test(american_eng_b$VOT, spanish_eng_b$VOT, paired = FALSE, conf.level = 0.95)
american_eng_p <- english_p %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'perez_john')
spanish_eng_p <- english_p %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_shiela' | Speaker == 'perez_tina' )
t.test(american_eng_p$VOT, spanish_eng_p$VOT, paired = FALSE, conf.level = 0.95)
american_eng_d <- english_d %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'perez_john')
spanish_eng_d <- english_d %>%filter(Speaker == 'perez_carla' | Speaker == 'perez_shiela' | Speaker == 'perez_tina' )
t.test(american_eng_d$VOT, spanish_eng_d$VOT, paired = FALSE, conf.level = 0.95)
american_eng_t <- english_t %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'perez_john')
spanish_eng_t <- english_t %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_shiela' | Speaker == 'perez_tina' )
t.test(american_eng_t$VOT, spanish_eng_t$VOT, paired = FALSE, conf.level = 0.95)


#Language used by mother t-tests for Spanish
american_spa_b <- spanish_b %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'perez_john')
spanish_spa_b <- spanish_b %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_shiela' | Speaker == 'perez_tina' )
t.test(american_spa_b$VOT, spanish_spa_b$VOT, paired = FALSE, conf.level = 0.95)
american_spa_p <- spanish_p %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'perez_john')
spanish_spa_p <- spanish_p %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_shiela' | Speaker == 'perez_tina' )
t.test(american_spa_p$VOT, spanish_spa_p$VOT, paired = FALSE, conf.level = 0.95)
american_spa_d <- spanish_d %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'perez_john')
spanish_spa_d <- spanish_d %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_shiela' | Speaker == 'perez_tina' )
t.test(american_spa_d$VOT, spanish_spa_d$VOT, paired = FALSE, conf.level = 0.95)
american_spa_t <- spanish_t %>% filter(Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' | Speaker == 'perez_john')
spanish_spa_t <- spanish_t %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_shiela' | Speaker == 'perez_tina' )
t.test(american_spa_t$VOT, spanish_spa_t$VOT, paired = FALSE, conf.level = 0.95)


#Language used by father t-tests for English 
american_eng_b <- english_b %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_shiela')
spanish_eng_b <- english_b %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_tina' | Speaker == 'deuchar' | Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' )
t.test(american_eng_b$VOT, spanish_eng_b$VOT, paired = FALSE, conf.level = 0.95)
american_eng_p <- english_p %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_shiela')
spanish_eng_p <- english_p %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_tina' | Speaker == 'deuchar' | Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' )
t.test(american_eng_p$VOT, spanish_eng_p$VOT, paired = FALSE, conf.level = 0.95)
american_eng_d <- english_d %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_shiela')
spanish_eng_d <- english_d %>%filter(Speaker == 'perez_carla' | Speaker == 'perez_tina' | Speaker == 'deuchar' | Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' )
t.test(american_eng_d$VOT, spanish_eng_d$VOT, paired = FALSE, conf.level = 0.95)
american_eng_t <- english_t %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_shiela')
spanish_eng_t <- english_t %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_tina' | Speaker == 'deuchar' | Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' )
t.test(american_eng_t$VOT, spanish_eng_t$VOT, paired = FALSE, conf.level = 0.95)


#Language used by father t-tests for Spanish
american_spa_b <- spanish_b %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_shiela')
spanish_spa_b <- spanish_b %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_tina' | Speaker == 'deuchar' | Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' )
t.test(american_spa_b$VOT, spanish_spa_b$VOT, paired = FALSE, conf.level = 0.95)
american_spa_p <- spanish_p %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_shiela')
spanish_spa_p <- spanish_p %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_tina' | Speaker == 'deuchar' | Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' )
t.test(american_spa_p$VOT, spanish_spa_p$VOT, paired = FALSE, conf.level = 0.95)
american_spa_d <- spanish_d %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_shiela')
spanish_spa_d <- spanish_d %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_tina' | Speaker == 'deuchar' | Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' )
t.test(american_spa_d$VOT, spanish_spa_d$VOT, paired = FALSE, conf.level = 0.95)
american_spa_t <- spanish_t %>% filter(Speaker == 'perez_alberto' | Speaker == 'perez_shiela')
spanish_spa_t <- spanish_t %>% filter(Speaker == 'perez_carla' | Speaker == 'perez_tina' | Speaker == 'deuchar' | Speaker == 'ferfulice_1' | Speaker == 'ferfulice_2' )
t.test(american_spa_t$VOT, spanish_spa_t$VOT, paired = FALSE, conf.level = 0.95)





