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

df_innings <- read_csv('innings.csv')
df_innings %>% head()

br_teams <- c('LAA','ARI','ATL','BAL','BOS','CHC',
              'CHW','CIN','CLE','COL','DET','MIA',
              'HOU','KCR','LAD','MIL','MIN','NYM',
              'NYY','OAK','PHI','PIT','SDP','SEA',
              'SFG','STL','TBR','TEX','TOR','WSN')

df_action %>% filter(Sport=='Baseball') %>% 
  select(perc_action, perc_comm) %>% 
  mutate(perc_other=1-perc_action-perc_comm) %>% 
  gather(key=Key, value=Value) %>% 
  ggplot() +
  geom_bar(aes(x=1, y=Value, fill=Key),
           stat='identity',
           position='fill')
df_mlb %>% filter(Year > 2013) %>% 
  select(Year, Time)

action_duration_sec <- df_action %>%
  filter(Sport=='Baseball') %>% 
  select(action_duration) %>% 
  pull

df_innings$`Seconds of Action` <- action_duration_sec

df_innings %>%
  filter(Inning==9) %>% 
  select(Year,
         `Commercials`=`Commercial time (game)`,
         `Action`=`Seconds of Action`,
         `Other`=`Time of game`)

g <- df_innings %>%
  filter(Inning==9) %>% 
  select(Year,
         `Commercials`=`Commercial time (game)`,
         `Action`=`Seconds of Action`,
         `Other`=`Time of game`) %>% 
  gather(key=Category, value=Value, -Year) %>% 
  ggplot() +
  geom_bar(aes(x=Year, y=Value, fill=Category),
           width=28,
           stat='identity', position='fill') +
  theme_eric() +
  xlab('Year') + ylab('Proportion of the game') +
  ggtitle('Have modern baseball broadcasts lost action?') +
  scale_fill_brewer(type='qual', palette=2, 'Share') +
  scale_x_discrete(limits=c(1984,2014)) +
  theme(legend.title=element_blank())
g_stacked <- ggplotly(g) %>% 
  add_annotations(text="Category",
                  xref="paper", yref="paper",
                  x=1.03, xanchor="left",
                  y=0.81, yanchor="bottom",
                  legendtitle=T, showarrow=F) %>%
  layout(legend=list(y=0.8, yanchor='top'))
g_stacked

# Slopegraph
h <- df_innings %>%
  filter(Inning==9) %>% 
  select(Year,
         `Commercials`=`Commercial time (game)`,
         `Action`=`Seconds of Action`,
         `Other`=`Time of game`) %>% 
  gather(key=Category, value=Value, -Year) %>% 
  group_by(Year) %>% 
  mutate(Total=sum(Value)) %>% 
  ungroup() %>% 
  mutate(Proportion=round(Value/Total,2)) %>% 
  ggplot() +
  geom_line(aes(x=Year,
                y=Proportion,
                color=Category),
            size=4, alpha=.5) +
  geom_point(aes(x=Year,
                 y=Proportion,
                 fill=Category),
             size=8, shape=21, color='black') +
  theme_eric() +
  xlab('Year') + ylab('Proportion of the game') +
  ggtitle('Have modern baseball broadcasts lost action?') +
  scale_color_brewer(type='qual',
                    palette=2, 'Share',
                    labels=c('Action',
                             'Commercials',
                             'Other')) +
  scale_fill_brewer(type='qual',
                    palette=2, 'Share',
                    labels=c('Action',
                             'Commercials',
                             'Other')) +
  scale_x_continuous(limits=c(1980,2018),
                     breaks=c(1984, 2014)) +
  scale_y_continuous(breaks=seq(0.1, 0.8, by=0.1)) +
  theme(legend.title=element_blank(),
        axis.title.x=element_text(size=20))
h_slope <- ggplotly(h) %>% 
  add_annotations(text="Category",
                  xref="paper", yref="paper",
                  x=1.023, xanchor="left",
                  y=0.81, yanchor="bottom",
                  legendtitle=T, showarrow=F) %>%
  layout(legend=list(y=0.8, yanchor='top'))
h_slope