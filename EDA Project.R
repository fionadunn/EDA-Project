require(tidyverse)
nfl <- nfl_teams_season_summary


## data structure
str(nfl)
typeof(nfl)
class(nfl)
nfl2 <- as_tibble(nfl)
head(nfl2)


## is there one team/division that is best
## had to combine teams that switched locations
nfl_team_rename <- nfl %>% 
  mutate(team = ifelse(team == "STL", "LA", team),
         team = ifelse(team == "SD", "LAC", team))

##merges all the sub-divisions into one larger division
large_div <- nfl_team_rename %>% 
  mutate(conference = substr(division, 1, 3)) %>%
  select(conference, wins, team, season) 

##code for bar chart
large_div %>%
  group_by(team, conference) %>%
  summarize(wins = sum(wins)) %>%
  ungroup() %>%
  mutate(team = fct_reorder(team, wins)) %>%
  ggplot(aes(x = team, y = wins, fill = conference)) +
  geom_bar(stat = "identity") + 
  theme_bw() +
  #facet_wrap(~season, ncol = 3) +
  labs(title = "Which is stronger: AFC vs. NFC? (2009-2019)", 
       subtitle = "Regular season wins during the 2009-2019 seasons",
       x = "Team",
       y = "Total Wins") +
  coord_flip() +
  geom_vline(xintercept = 16.5, 
             linetype = "dashed", 
             color = "black")

unique(large_div$team)

table(large_div$conference, large_div$wins)

NE <- large_div %>%
  filter(team == "NE")

sum(NE$wins)

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
  annotate(geom = "text", x = .2, y = -0.2, label = paste("R =", cor(nfl$pass_off_epa_per_att,
                                                                     nfl$points_scored))) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(limits = c(-.3, .2)) +
  theme_bw()

summarize(nfl, max(run_off_epa_per_att), min(run_off_epa_per_att))






## runs vs pass

summarize(nfl, max(pass_off_epa_per_att), min(pass_off_epa_per_att))
nfl_pass <- nfl %>%
  ggplot(aes(x = pass_off_epa_per_att,
             y = wins,
             col = team)) + 
  annotate(geom = "text", x = .35, y = 1, label = paste("r =", cor(nfl$pass_off_epa_per_att,
                                                                 nfl$wins))) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(limits = c(-.32, .38)) +
  labs(title = "Offensive EPA per Pass Attempt vs Wins",
       x = "EPA per Pass Attempt",
       y = "Wins")


nfl_run <- nfl %>%
  ggplot(aes(x = run_off_epa_per_att,
             y = wins,
             col = team)) + 
  annotate(geom = "text", x = .2, y = 1, label = paste("r =", cor(nfl$run_off_epa_per_att,
                                                                   nfl$wins))) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(limits = c(-.25, .2)) +
  labs(title = "Offensive EPA per Run Attempt vs Wins",
       x = "EPA per Run Attempt",
       y = "Wins")

nfl_pass + nfl_run


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


pass <- nfl_strat %>%
  ggplot(aes(-1*pass_def_epa_per_att, pass_off_epa_per_att, z = wins)) +
  stat_summary_hex(color = "black",
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  labs(x = "-1 * Pass Defense EPA per Attempt",
       y = "Pass Offense EPA per Attempt",
       title = "Mean Wins by EPA") +
  theme_bw() +
  theme(legend.position = "none") +
  coord_fixed()

run <- nfl_strat %>%
  ggplot(aes(-1*run_def_epa_per_att, run_off_epa_per_att, z = wins)) +
  stat_summary_hex(color = "black",
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  labs(x = "-1 * Run Defense EPA per Attempt",
       y = "Run Offense EPA per Attempt",
       fill = "Mean Wins") +
  theme_bw() +
  coord_fixed()

pass + run + plot_layout(guides = "collect")



def_nums <- nfl %>% 
  mutate(def_total = -1*(run_def_total_epa + pass_def_total_epa)/2,
         off_total = (run_off_total_epa + pass_off_total_epa)/2)

def <- def_nums %>%
  ggplot(aes(x = def_total,
             y = wins)) +
  geom_point() +
  annotate(geom = "text", x = 100, y = 1, label = paste("r =", cor(def_nums$def_total,
                                                                def_nums$wins))) +
  theme_bw()

off <- def_nums %>%
  ggplot(aes(x = off_total,
             y = wins)) +
  geom_point() +
  annotate(geom = "text", x = 100, y = 1, label = paste("r =", cor(def_nums$off_total,
                                                                   def_nums$wins))) +
  theme_bw()

off + def

