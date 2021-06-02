library(tidyverse)
library(readr)


# This script imports an excel file called "Calendario" from fantaleghe.it and return a list with scores in each match day

# Initialise values
players <- 10                                   # Choose number of players in the fantasy football league
mdays <- 36                                     # Choose number of matchdays 

# Import excel file "Calendario" from fantaleghe.it and return a list with scores in each match day

data <- readxl::read_excel("./Calendario_Il-Gambionato.xlsx", skip = 2, col_names = F)

data1 <- data[,1:4]
data1 <- data1[,c(1,2,4,3)]
data2 <- data[,7:10]
data2 <- data2[,c(1,2,4,3)]

# Set number of splits 
splits <- nrow(data1)/(players/2+1)
break1 <- list()
break2 <- list()

# Split into matchdays
split1 <- split(data1,rep(1:splits,each=players/2+1))
split2 <- split(data2,rep(1:splits,each=players/2+1))
mdSplit <- c(split1, split2)
mdNames <- list()
break1 <- list()
break2 <- list()

for(i in 1:mdays){
  mdSplit[[i]][1,1] <- str_remove_all(mdSplit[[i]][1,1], "[^\\d]+")
  mdNames[[i]] <- mdSplit[[i]][1,1]
  mdSplit[[i]] <- mdSplit[[i]][-1,]
  mdSplit[[i]] <- mdSplit[[i]] %>% 
    mutate(across(where(is.double), ~ as.character(.)))
  break1[[i]] <- mdSplit[[i]][,1:2]
  names(break1[[i]]) <- c("team", "score")
  break2[[i]] <- mdSplit[[i]][,3:4]
  names(break2[[i]]) <- c("team", "score")
  mdSplit[[i]] <- rbind(break1[[i]],  break2[[i]])
  mdSplit[[i]] <- mdSplit[[i]] %>% arrange(team)
  mdSplit[[i]]$round <- as.character(i)
}

# Name list objects
mdNames <- unlist(mdNames)
names(mdSplit) <- mdNames

# Create unique dataframe
scoresDf <- mdSplit[[1]]
for(i in 2:length(mdSplit)){
  scoresDf <- rbind(scoresDf, mdSplit[[i]])
}
scoresDf <- scoresDf %>% 
  unite(roundTeam, round, team)
