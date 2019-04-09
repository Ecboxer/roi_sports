library(tidyverse)
library(Lahman)
library(baseballr)
library(ggridges)
library(teamcolors)

br_teams <- c('ARI','ATL','BAL','BOS','CHC','CHW',
              'CIN','CLE','COL','DET','HOU','KCR',
              'LAA','LAD','MIA','MIL','MIN','NYM',
              'NYY','OAK','PHI','PIT','SDP','SFG',
              'SEA','STL','TBR','TEX','TOR','WSN')
gm_lens <- c()
for (i in br_teams) {
  df_temp <- team_results_bref(i, 2018)
  n <- df_temp %>% nrow()
  t <- df_temp %>%
    mutate(
      hrs = substring(Time, 0, 1) %>% as.integer(),
      min = substring(Time, 3, 4) %>% as.integer(),
      total = ((hrs * 60) + min) * 60
    ) %>% select(total)
  while (n < 163) {
    t <- rbind(t, NaN)
    n <- n + 1
  }
  gm_lens[i] <- t
}
df_lens <- data.frame(matrix(ncol=30, nrow=163))
colnames(df_lens) <- br_teams
for (i in br_teams) {
  df_lens[i] <- gm_lens[i]
}

teamcolors[teamcolors$name=='Arizona Diamondbacks', 1] <- 'Arizona D-backs'
teamcolors[teamcolors$name=='Los Angeles Angels of Anaheim', 1] <- 'Los Angeles Angels'

mlb_colors <- teamcolors %>% filter(league=='mlb')
fillscale <- scale_fill_manual(name='Teams',
                               values=mlb_colors$primary,
                               guide=guide_legend(ncol=1))
colscale <- scale_color_manual(name='Teams',
                               values=mlb_colors$secondary,
                               guide=guide_legend(ncol=1))

order <- df_lens %>%
  gather(key=team, value=time) %>% 
  mutate(time_min=time / 60) %>% 
  group_by(team) %>% 
  summarise(med_time=median(time_min, na.rm=T)) %>% 
  merge(., team_names,
        by.x='team', by.y='teamIDBR') %>% 
  arrange(med_time) %>% 
  select(name) %>% pull()

team_names <- Teams %>%
  filter(yearID==2016) %>% 
  select(teamIDBR, name)

g_ridge <- df_lens %>%
  gather(key=team, value=time) %>% 
  merge(., team_names,
        by.x='team', by.y='teamIDBR') %>% 
  mutate(time_min=time / 60) %>% 
  ggplot(aes(x=time_min,
             y=factor(name, levels=order),
             fill=name),
         alpha=.5) +
  geom_density_ridges2(scale=2) +
  fillscale +
  xlab('Time (min)') + ylab('') +
  labs(title='Game lengths',
       subtitle='2018 season') +
  theme_eric()
g_ridge

# Secondary colors as color
h_ridge <- df_lens %>%
  gather(key=team, value=time) %>% 
  merge(., team_names,
        by.x='team', by.y='teamIDBR') %>% 
  mutate(time_min=time / 60) %>% 
  ggplot(aes(x=time_min,
             y=factor(name, levels=order),
             fill=name, color=name),
         alpha=.5) +
  geom_density_ridges2(scale=2, size=1) +
  fillscale + colscale +
  xlab('Time (min)') + ylab('') +
  labs(title='Game lengths',
       subtitle='2018 season') +
  theme_eric()
h_ridge

# df_lens %>%
#   gather(key=team, value=time) %>% 
#   merge(., team_names,
#         by.x='team', by.y='teamIDBR') %>% 
#   mutate(time_min=time / 60) %>% 
#   dplyr::select(name, time, time_min) %>% 
#   dplyr::group_by(name) %>% 
#   dplyr::summarise(mean_time_secs = mean(time,
#                                          na.rm=T),
#                    mean_time_min = mean(time_min,
#                                         na.rm=T)) %>% 
#   write_csv(path='avg_game_lengths.csv')