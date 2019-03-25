library(tidyverse)
library(Lahman)
library(baseballr)
library(teamcolors)

df_action <- read_csv('df_action.csv')
df_action %>% filter(Sport == 'Baseball')
action_dur <- df_action %>% filter(Sport == 'Baseball') %>% 
  select(action_duration)
action_dur <- action_dur$action_duration
action_dur_min <- action_dur / 60

df_prices <- read_csv('mlb_prices.csv')
df_prices %>% head()
df_prices <- df_prices %>% mutate(
  pr_2017 = as.numeric(gsub("\\$", "", `2017`)),
  pr_2011 = as.numeric(gsub("\\$", "", `2011`)),
  perc_change = (pr_2017 - pr_2011) / pr_2011
) %>% 
  rename(teams = `MLB Teams`) %>% 
  select(teams, pr_2017, pr_2011, perc_change)
df_prices %>% head()

df_prices_2019 <- read_csv('mlb_2019_fan_cost_index.csv')
df_prices_2019 <- df_prices_2019 %>% 
  select(teams=Team,
         pr_2019=`Avg. Ticket`)
df_prices_2019 %>% head()

df_prices_2019 %>% ggplot() +
  geom_bar(aes(x=reorder(teams, pr_2019),
               y=pr_2019/action_dur_min),
           stat='identity') +
  coord_flip()
df_prices %>% ggplot() +
  geom_bar(aes(x=reorder(teams, pr_2011),
               y=pr_2011/action_dur_min),
           stat='identity') +
  coord_flip()

teamcolors[teamcolors$name=='Arizona Diamondbacks', 1] <- 'Arizona D-backs'
teamcolors[teamcolors$name=='Los Angeles Angels of Anaheim', 1] <- 'Los Angeles Angels'

mlb_colors <- teamcolors %>% filter(league=='mlb')
fillscale <- scale_fill_manual(name='Teams',
                               values=mlb_colors$primary)

g <- df_prices_2019 %>%
  select(Team=teams,
         pr_2019) %>% 
  mutate(Ratio=action_dur_min/pr_2019,
         Price=paste('$',
                     as.character(pr_2019),
                     sep='')) %>% 
  ggplot(aes(label=Price)) +
  geom_bar(aes(x=reorder(Team, Ratio),
               y=Ratio,
               fill=Team),
           stat='identity') +
  fillscale +
  xlab('') +
  ylab('Ratio of minutes of action to ticket price') +
  ggtitle('Which teams have the best ROI?') +
  coord_flip() +
  theme_eric()
g
library(plotly)
g_topteams <- ggplotly(g, tooltip=c('Team', 'Ratio', 'Price'))
g_topteams

# Make interactive, switch from 2011 to 2017, in shiny
df_prices %>%
  gather(key=key, value=value,
         -c(teams, perc_change))