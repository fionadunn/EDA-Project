# EDA-Project

require(tidyverse)
nfl <- nfl_teams_season_summary


## data structure
str(nfl)
typeof(nfl)
class(nfl)
nfl2 <- as_tibble(nfl)
head(nfl2)

## is the best offense truly a good defense

nfl2 %>%
  ggplot(aes(x = points_scored,
             y = wins)) +
  annotate(geom = "text", x = 500, y = -0.2, label = paste("r =", cor(nfl$points_scored, 
  nfl$wins))) +
  geom_point()

nfl2 %>%
  ggplot(aes(x = points_allowed,
             y = wins)) +
  annotate(geom = "text", x = 500, y = -0.2, label = paste("r =", cor(nfl$points_allowed, 
  nfl$wins))) +
  geom_point()

nfl2 %>%
  ggplot(aes(x = points_scored,
             y = losses)) +
  annotate(geom = "text", x = 500, y = -0.2, label = paste("r =", cor(nfl$points_scored,
  nfl$losses))) +
  geom_point()

nfl2 %>%
  ggplot(aes(x = points_allowed,
             y = losses)) +
  annotate(geom = "text", x = 500, y = -0.2, label = paste("r =", cor(nfl$points_allowed,
  nfl$losses))) +
  geom_point()



## is there one team/division that is best
winners <- nfl2 %>%
  filter(wins > (losses + ties)) %>%
  arrange(desc(wins))

table(winners$division)
summarize(winners, max(wins), min(wins), median(wins))

winners %>%
  ggplot(aes(x = division, fill = team)) +
  geom_bar() +
  theme_bw()


winners %>%
  ggplot(aes(x = wins, color = team)) +
  geom_freqpoly()  +
  facet_wrap(vars(division)) +
  theme_bw()  

nfl2 %>%
  group_by(team) %>%
  mutate(point_dif = points_scored - points_allowed) %>%
  arrange(desc(point_dif)) %>%
  ggplot(aes(x = season, y = point_dif, col = division)) + 
  geom_violin() +
  geom_boxplot(width = .2) +
  theme_bw()
  
  
nfl2 %>%
  group_by(team) %>%
  arrange(desc(wins)) %>%
  ggplot(aes(x = division, y = wins, color = division)) + 
  geom_violin() +
  geom_boxplot(width = .2) +
  theme_bw()


## is there a difference between running or passing the ball
nfl %>%
  ggplot(aes(x = run_off_epa_per_att,
             y = points_scored)) +
  annotate(geom = "text", x = .2, y = -0.2, label = paste("r =", cor(nfl$run_off_epa_per_att,
  nfl$points_scored))) +
  geom_point() +
  scale_x_continuous(limits = c(-.2, .2)) +
  geom_smooth() +
  theme_bw()

nfl %>%
  ggplot(aes(x =pass_off_epa_per_att ,
             y = points_scored)) +
  annotate(geom = "text", x = .2, y = -0.2, label = paste("r =", cor(nfl$pass_off_epa_per_att,
  nfl$points_scored))) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(limits = c(-.3, .2)) +
  theme_bw()

summarize(nfl, max(run_off_epa_per_att), min(run_off_epa_per_att))



## FINAL DIVISON CODE
##which divison is best
large_div <- nfl %>% 
  mutate(conference = substr(division, 1, 3)) %>%
  select(conference, wins, team, season)

head(large_div)

large_div %>%
  ggplot(aes(x = conference, fill = team), y = wins) +
  geom_bar() + 
  theme_bw() +
  labs(title = "Which is stronger: AFC vs. NFC?", 
       subtitle = "nfl_teams_season_summary.csv",
       x = "Conference",
       y = "Total Wins") 


## runs vs pass
nfl %>%
  ggplot(aes(x = pass_off_yards_gained_per_att,
             y = wins)) + 
  annotate(geom = "text", x = 8, y = 1, label = paste("r =", cor(nfl$pass_off_yards_gained_per_att,
  nfl$wins))) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(title = "Yards Gained per Pass Attempt Offensively vs Wins",
       subtitle = "nfl_teams_season_summary.csv",
       x = "Yards Gained per Pass Attempt",
       y = "Wins")


nfl %>%
  ggplot(aes(x = run_off_yards_gained_per_att,
             y = wins )) + 
  annotate(geom = "text", x = 6, y = 1, label = paste("r =", cor(nfl$run_off_yards_gained_per_att,
  nfl$wins))) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(title = "Yards Gained per Runn Attempt Offensively vs Wins",
       subtitle = "nfl_teams_season_summary.csv",
       x = "Yards Gained per Run Attempt",
       y = "Wins")

##cluster
nfl_strat <- nfl %>%
  mutate(matches = wins + losses,
         win_loss_ratio = wins/losses,
         run_def = ifelse(-1*run_def_total_epa >= run_off_total_epa, TRUE, FALSE),
         pass_def = ifelse(-1*pass_def_total_epa >= pass_off_total_epa, TRUE, FALSE),
         def_total_epa = run_def_total_epa + pass_def_total_epa, # overall "defensiveness"
         off_total_epa = run_off_total_epa + pass_off_total_epa, # overall "offensiveness"
         strategy = as.factor(ifelse(run_def & pass_def, "Pure Defense",
                                     ifelse(!run_def & pass_def, "Def Pass/Off Run",
                                            ifelse(run_def & !pass_def, "Off Pass/Def Run",
                                                   ifelse(!run_def & !pass_def, "Pure Offense", "")
                                            )
                                     ))
         ),
         strategy = fct_relevel(strategy, c("Pure Defense", "Def Pass/Off Run", "Off Pass/Def Run", "Pure Offense"))
  ) %>%
  group_by(season, division) %>%
  mutate( point_dif = points_scored - points_allowed) %>%
  mutate(final_rank = as.factor(dense_rank(desc(win_loss_ratio)))) %>%
  ungroup()

init_nfl_kmeans <- 
  kmeans(dplyr::select(nfl_strat,
                       pass_off_yards_gained_per_att, point_dif),
         3, nstart = 30)

nfl_strat <- nfl_strat %>%
  mutate(clusters = 
           as.factor(init_nfl_kmeans$cluster))
nfl_strat %>%
  ggplot(aes(x = pass_off_yards_gained_per_att, y = point_dif,
             color = clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")





nfl_minimax <- protoclust(dist(
  dplyr::select(nfl_strat,
                pass_off_yards_gained_per_att, point_dif)))
plot(nfl_minimax)


minimax_clusters <-
  protocut(nfl_minimax, k = 3)
nfl_strat <- nfl_strat %>%
  mutate(minimax_clusters = 
           as.factor(minimax_clusters$cl))
nfl_strat %>%
  ggplot(aes(x = pass_off_yards_gained_per_att, y = point_dif,
             color = minimax_clusters)) +
  geom_point() +
  ggthemes::scale_color_colorblind() +
  theme_bw()

nfl_multidim_clust <- protoclust(dist(dplyr::select(nfl_strat,
                                                    wins, point_dif,
                                                    pass_off_yards_gained_per_att, run_off_yards_gained_per_att)))
nfl_multidim_clust_cut <- protocut(nfl_multidim_clust, k = 5)
table("Final Rank" = nfl_strat$final_rank,
      "Clusters" = nfl_multidim_clust$cl)

library(GGally)
nfl_strat <- nfl_strat %>%
  mutate(full_minimax_clusters = 
           as.factor(nfl_multidim_clust_cut$cl))
ggpairs(nfl_strat,
        columns =
          c("wins", "point_dif",
            "pass_off_yards_gained_per_att", "run_off_yards_gained_per_att"),
        aes(color = full_minimax_clusters)) +
  theme_bw()
  
