# Computes and plots standard deviations of each team

library(tidyverse)
library(readr)
library(viridis)

# Load workspace from manipulation.R: mdsplit and scoreDf
load("manipulation.RData")

sdDf <- mdSplit[[1]] %>% 
  rename(score1=score)

for(i in 2:mdays){
  sdDf <- sdDf %>% select(-round) %>% 
    left_join(mdSplit[[i]], by = "team") %>%
    rename(!!paste0("score", i):=score)
}

sdDf <- sdDf %>% select(-round) 
sdDf <- as.data.frame(t(sdDf)) 
names(sdDf) <- sdDf[1,]
sdDf <- sdDf[-1,] %>%  
  mutate(across(where(is.character), ~ as.numeric(.)))

# Standard deviation
sdDfSum <- sdDf %>% 
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))

sdDfL <- sdDf %>% 
  pivot_longer(everything(), names_to = "group", values_to = "score") %>%
  group_by(group) %>%
  mutate(sd = sd(score)) %>%
  ungroup()

sdDfL$rank[sdDfL$group=="Picchio FC"] <- 1
sdDfL$rank[sdDfL$group=="FC Lausangeles Galaxy"] <- 2
sdDfL$rank[sdDfL$group=="Real Katenaccio"] <- 3
sdDfL$rank[sdDfL$group=="Atletico Focaccia"] <- 4
sdDfL$rank[sdDfL$group=="Brisolo"] <- 5
sdDfL$rank[sdDfL$group=="Askari"] <- 6
sdDfL$rank[sdDfL$group=="FC Sellaronda"] <- 7
sdDfL$rank[sdDfL$group=="L’Androne Inisvizzero"] <- 8
sdDfL$rank[sdDfL$group=="US Falco"] <- 9
sdDfL$rank[sdDfL$group=="Terra Promessa"] <- 10

# Boxplot
sdDfL %>%
  ggplot(aes(x=reorder(group, sd), y=score, fill=group)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("La variantsa, ordinata da sinistra verso destra in ordine crescente") +
  xlab("") + 
  ylab("") 

# Boxplot 2
sdDfL %>%
  ggplot(aes(x=reorder(group, rank), y=score, fill=group, alpha=0.6)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set3") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")+
  ggtitle("Distribuzione punteggi in ordine di classifica, da sinistra (primo) verso destra (ultimo)") +
  xlab("") + 
  ylab("") 
