library(tidyverse)
library(Lahman)
library(baseballr)
df_action <- read_csv('df_action.csv')
df_action %>% filter(Sport == 'Baseball')
action_dur <- df_action %>% filter(Sport == 'Baseball') %>% 
  select(action_duration)
action_dur <- action_dur$action_duration
action_dur_min <- action_dur / 60

df_prices <- read_csv('mlb_prices.csv')
df_prices <- df_prices %>% mutate(
  pr_2017 = as.numeric(gsub("\\$", "", `2017`)),
  pr_2011 = as.numeric(gsub("\\$", "", `2011`)),
  perc_change = (pr_2017 - pr_2011) / pr_2011
) %>% 
  rename(teams = `MLB Teams`) %>% 
  select(teams, pr_2017, pr_2011, perc_change)
df_prices %>% head()

df_prices %>% ggplot() +
  geom_bar(aes(x=reorder(teams, pr_2017),
               y=pr_2017/action_dur_min),
           stat='identity') +
  coord_flip()
df_prices %>% ggplot() +
  geom_bar(aes(x=reorder(teams, pr_2011),
               y=pr_2011/action_dur_min),
           stat='identity') +
  coord_flip()
# Make interactive, switch from 2011 to 2017
# Tooltip to show actual prices