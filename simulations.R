# Simulations 

#'
#'We start by creating all possible initial orders of the draw. 
#'The number of permutations of a vector of length 10, without repetitions is 
#'P(n,r) = \frac{n!}{(n-r)!}
#'Where $n$ is the number of objects (10) and $r$ is the length of the simulated vector (10), 
#'therefore $P(10,10) = 10!$. 
#'This returns a total of 10! permutations. 
#'We use round-robin combinations to create all possible schedules based on each initial simulated draw. 

  
# All permutations of length 10 of the teams vector
draws <- permutations(10, 10, 1:10) 
sim <- 10000

# Select random subset of permutations
set.seed(1234)
drawsSub <- draws[sample(nrow(draws), sim),] 
  
# Team names df
teamNames <- unique(mdSplit[[1]]$team) 
teamNames <- 
  as_tibble(cbind(teamNames, 1:players)) %>% 
  rename(team=V2)
  
# Loop: Round-robin tournaments, creates nrow(drawsSub) round robin schedules, adds scores  
rr <- list()
rr2 <- list()
rank1 <- list()
rank2 <- list()
finalTable <- list()
  
for(j in 1:nrow(drawsSub)){
  teams <- drawsSub[j,]
  rr[[j]] <- 
    roundrobin(teamvector=teams, second_round=T, randomize=F)
  rr2[[j]] <- rr[[j]] %>% 
    mutate(Matchday=Matchday+18)
  rr[[j]] <- rbind(rr[[j]], rr2[[j]]) %>%
    mutate(across(where(is.numeric), ~ as.character(.)))
  names(rr[[j]]) <- c("round", "team1", "team2")
  
  # Add score of each team in each round 
  rr[[j]] <- rr[[j]] %>% 
    left_join(teamNames, by=c("team1"="team")) %>% 
    rename(team1Name=teamNames) %>% 
    left_join(teamNames, by=c("team2"="team")) %>% 
    rename(team2Name=teamNames) %>%
    unite(roundTeam1Name, round, team1Name, remove = F) %>%
    unite(roundTeam2Name, round, team2Name) %>%
    select(roundTeam1Name, roundTeam2Name) %>%
    left_join(scoresDf, by=c("roundTeam1Name"="roundTeam")) %>%
    rename(score1=score) %>%
    left_join(scoresDf, by=c("roundTeam2Name"="roundTeam")) %>%
    rename(score2=score) %>% 
    separate(roundTeam1Name, c("round", "team1Name"), sep = "_") %>%
    separate(roundTeam2Name, c("round2", "team2Name"), sep = "_") %>%
    select(-round2) %>%
    mutate(across(score1:score2, ~ as.numeric(.)))
  
  # Convert scores into goals
  rr[[j]]$goal1 <- case_when(
    rr[[j]]$score1 < 66 ~ 0,
    rr[[j]]$score1 >= 66 & rr[[j]]$score1 < 71 ~ 1,
    rr[[j]]$score1 >= 71 & rr[[j]]$score1 < 76 ~ 2,
    rr[[j]]$score1 >= 76 & rr[[j]]$score1 < 81 ~ 3,
    rr[[j]]$score1 >= 81 & rr[[j]]$score1 < 86 ~ 4,
    rr[[j]]$score1 >= 86 & rr[[j]]$score1 < 91 ~ 5,
    rr[[j]]$score1 >= 91 & rr[[j]]$score1 < 96 ~ 6,
    rr[[j]]$score1 >= 96 ~ 7,
    TRUE ~ 0)
  
  rr[[j]]$goal2 <- case_when(
    rr[[j]]$score2 < 66 ~ 0,
    rr[[j]]$score2 >= 66 & rr[[j]]$score2 < 71 ~ 1,
    rr[[j]]$score2 >= 71 & rr[[j]]$score2 < 76 ~ 2,
    rr[[j]]$score2 >= 76 & rr[[j]]$score2 < 81 ~ 3,
    rr[[j]]$score2 >= 81 & rr[[j]]$score2 < 86 ~ 4,
    rr[[j]]$score2 >= 86 & rr[[j]]$score2 < 91 ~ 5,
    rr[[j]]$score2 >= 91 & rr[[j]]$score2 < 96 ~ 6,
    rr[[j]]$score2 >= 96 ~ 7,
    TRUE ~ 0)
  
  # Convert goals into league points
  rr[[j]]$points1 <- case_when(
    rr[[j]]$goal1 > rr[[j]]$goal2 ~ 3,
    rr[[j]]$goal1 < rr[[j]]$goal2 ~ 0,
    rr[[j]]$goal1 == rr[[j]]$goal2 ~ 1,
    TRUE ~ 99)
  
  rr[[j]]$points2 <- case_when(
    rr[[j]]$goal2 > rr[[j]]$goal1 ~ 3,
    rr[[j]]$goal2 < rr[[j]]$goal1 ~ 0,
    rr[[j]]$goal2 == rr[[j]]$goal1 ~ 1,
    TRUE ~ 99)
  
  # Sum up points by team and construct ranking for each simulation
  rank1[[j]] <- rr[[j]] %>% 
    select(team1Name, score1, points1) %>%
    group_by(team1Name) %>%
    summarise(points1=sum(points1), score1=sum(score1))
  
  rank2[[j]] <- rr[[j]] %>% 
    select(team2Name, score2, points2) %>%
    group_by(team2Name) %>%
    summarise(points2=sum(points2), score2=sum(score2))
  
  finalTable[[j]] <- rank1[[j]] %>%
    left_join(rank2[[j]], by=c("team1Name"="team2Name")) %>%
    mutate(points=points1+points2, score=score1+score2) %>%
    rename(team=team1Name) %>%
    select(team, score, points) %>%
    #mutate(rank = dense_rank(desc(points, score))) %>%
    arrange(desc(points, score)) 
  finalTable[[j]]$rank <- rownames(finalTable[[j]])
}

# Dataset for plot
plotDf <- finalTable[[1]]
for(j in 2:nrow(drawsSub)){
  plotDf <- rbind(plotDf, finalTable[[j]])
}
plotDf$rank <- as.numeric(plotDf$rank)

save(list="plotDf", file = "Simulation dataset.RData")
