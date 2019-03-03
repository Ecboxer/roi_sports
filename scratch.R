library(tidyverse)

df_action <- read_csv('df_action.csv')
df_action %>% head()

df_action$perc_other <- 1 - (df_action$perc_action + df_action$perc_comm)

df_gather <- df_action %>% gather(key=key, value=value, -Sport)
df_gather %>% head()

df_gather %>% 
  filter(key %in% c('action_duration', 'clock_duration', 'comm_duration')) %>% 
  ggplot() +
  geom_point(aes(x=Sport, y=value, color=key), size=10) +
  coord_flip()

df_gather %>% 
  filter(key %in% c('perc_action', 'perc_comm', 'perc_other')) %>% 
  ggplot() +
  geom_bar(aes(x=factor(Sport,
                         levels=c('Soccer',
                                  'Hockey',
                                  'Basketball',
                                  'Baseball',
                                  'Football')),
               y=value, fill=key),
           stat='identity') +
  coord_flip()

df_action %>% 
  ggplot() +
  geom_bar(aes(x=reorder(Sport,-comm_per_hr),
               y=comm_per_hr),
           stat='identity')
