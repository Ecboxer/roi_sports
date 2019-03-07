library(tidyverse)
library(Lahman)
library(baseballr)
df_action <- read_csv('df_action.csv')
df_action %>% filter(Sport == 'Baseball')
action_dur <- df_action %>% filter(Sport == 'Baseball') %>% 
  select(action_duration)

df_mlb <- read_csv('mlb_misc.csv')
df_mlb %>% head()

br_teams <- c('LAA','ARI','ATL','BAL','BOS','CHC',
              'CHW','CIN','CLE','COL','DET','MIA',
              'HOU','KCR','LAD','MIL','MIN','NYM',
              'NYY','OAK','PHI','PIT','SDP','SEA',
              'SFG','STL','TBR','TEX','TOR','WSN')
mins_2018 <- c()
for (i in br_teams) {
  df_temp <- team_results_bref(i, 2018)
  t_avg <- df_temp %>% select(Time) %>%
    mutate(
    hrs = substring(Time, 0, 1) %>% as.integer(),
    min = substring(Time, 3, 4) %>% as.integer(),
    total = ((hrs * 60) + min) * 60
  ) %>% summarise(avg = mean(total))
  mins_2018 <- append(mins_2018, t_avg$avg)
}
ggplot() +
  geom_point(aes(x=reorder(br_teams, -mins_2018),
                 y=mins_2018)) +
  geom_hline(aes(yintercept=mean(mins_2018))) +
  geom_hline(aes(yintercept=action_dur$action_duration)) +
  ylim(c(0, 12000))