library(tidyverse)
library(Lahman)
library(baseballr)
library(plotly)

df_action <- read_csv('df_action.csv')
df_action %>% filter(Sport == 'Baseball')
action_dur <- df_action %>% filter(Sport == 'Baseball') %>% 
  select(action_duration)
action_min <- action_dur$action_duration / 60

df_mlb <- read_csv('mlb_misc.csv')
df_mlb %>% head()

br_teams <- c('LAA','ARI','ATL','BAL','BOS','CHC',
              'CHW','CIN','CLE','COL','DET','MIA',
              'HOU','KCR','LAD','MIL','MIN','NYM',
              'NYY','OAK','PHI','PIT','SDP','SEA',
              'SFG','STL','TBR','TEX','TOR','WSN')
mins_yrs <- c()
for (i in br_teams) {
  df_temp <- team_results_bref(i, 2014)
  t_avg_2014 <- df_temp %>% select(Time) %>%
    mutate(
    hrs = substring(Time, 0, 1) %>% as.integer(),
    min = substring(Time, 3, 4) %>% as.integer(),
    total = ((hrs * 60) + min) * 60
  ) %>% summarise(avg = mean(total))
  df_temp <- team_results_bref(i, 2015)
  t_avg_2015 <- df_temp %>% select(Time) %>%
    mutate(
      hrs = substring(Time, 0, 1) %>% as.integer(),
      min = substring(Time, 3, 4) %>% as.integer(),
      total = ((hrs * 60) + min) * 60
    ) %>% summarise(avg = mean(total))
  df_temp <- team_results_bref(i, 2016)
  t_avg_2016 <- df_temp %>% select(Time) %>%
    mutate(
      hrs = substring(Time, 0, 1) %>% as.integer(),
      min = substring(Time, 3, 4) %>% as.integer(),
      total = ((hrs * 60) + min) * 60
    ) %>% summarise(avg = mean(total))
  mins_yrs[i] <- sum(t_avg_2014$avg,
                     t_avg_2015$avg,
                     t_avg_2016$avg) / 3
}
mins_yrs

df_lens <- data.frame(matrix(ncol=30, nrow=1))
colnames(df_lens) <- br_teams
for (i in br_teams) {
  df_lens[i] <- mins_yrs[i]
}

df_plot <- df_lens %>%
  gather(key=team, value=time) %>% 
  mutate(time_min = time / 60,
         avg_time = mean(time_min),
         action_ratio = action_min / time_min)

df_plot %>% 
  ggplot() +
  geom_point(aes(x=reorder(team, -action_ratio),
                 y=action_ratio),
             size=10) +
  geom_hline(aes(yintercept=action_min / avg_time)) +
  coord_flip() +
  ggtitle('Proportion of action per game')

team_stats <- Teams %>% filter(yearID>2013) %>% 
  group_by(yearID) %>% 
  mutate(R_lg = sum(R),
         H_lg = sum(H),
         SO_lg = sum(SO),
         SB_lg = sum(SB)) %>% 
  ungroup() %>% 
  group_by(teamIDBR) %>% 
  mutate(R_prop = R / R_lg,
         H_prop = H / H_lg,
         SO_prop = SO / SO_lg,
         SB_prop = SB / SB_lg) %>% 
  select(yearID, teamIDBR, R_prop, H_prop, SO_prop, SB_prop, name) %>%
  group_by(teamIDBR) %>% 
  mutate(R_prop_mu = mean(R_prop),
         H_prop_mu = mean(H_prop),
         SO_prop_mu = mean(SO_prop),
         SB_prop_mu = mean(SB_prop)) %>% 
  select(yearID, teamIDBR, R_prop_mu, H_prop_mu, SO_prop_mu, SB_prop_mu, name)

order <- df_plot %>% merge(., team_stats,
                           by.x='team', by.y='teamIDBR') %>% 
  select(Team=name,
         `Proportion of action per game`=action_ratio,
         `Share of runs`=R_prop_mu,
         `Share of homeruns`=H_prop_mu,
         `Share of strikeouts`=SO_prop_mu,
         `Share of stolen bases`=SB_prop_mu) %>% 
  gather(key=Statistic, value=Value, -Team) %>% 
  filter(Statistic=='Proportion of action per game') %>% 
  dplyr::arrange(Value) %>% unique() %>%  
  select(Team) %>% pull()  # Prop of action

g <- df_plot %>% merge(., team_stats,
                  by.x='team', by.y='teamIDBR') %>% 
  select(Team=name,
         `Proportion of action per game`=action_ratio,
         `Share of runs per season`=R_prop_mu,
         `Share of homeruns per season`=H_prop_mu,
         `Share of strikeouts per season`=SO_prop_mu,
         `Share of stolen bases per season`=SB_prop_mu) %>% 
  gather(key=Statistic, value=Value, -Team) %>% 
  mutate(id=Statistic,
         Team=factor(Team, levels=order)) %>% 
  ggplot() +
  geom_point(aes(x=Team,
                 y=Value,
                 group=id,
                 fill=Statistic),
             size=6,
             pch=21) +
  xlab('') + ylab('') +
  ggtitle("Which team has the most exciting games?") +
  coord_flip() +
  scale_fill_brewer(type='qual', palette=6) +
  scale_y_continuous(breaks=seq(0.01,
                                0.1,
                                by=0.01)) +
  theme_eric() +
  theme(legend.title=element_blank())
ggplotly(g, tooltip=c('Team',
                      'Statistic',
                      'Value')) %>% 
  add_annotations(text="Statistic",
                  xref="paper", yref="paper",
                  x=1.03, xanchor="left",
                  y=0.81, yanchor="bottom",
                  legendtitle=T, showarrow=F) %>%
  layout(legend=list(y=0.8, yanchor='top'))

# Average times and metrics in period 2014-2016