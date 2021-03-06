library(tidyverse)
library(Lahman)
library(baseballr)
library(teamcolors)
library(plotly)

df_prices <- read_csv('earlyticketprices.csv')
df_prices %>% head()
df_prices_2019 <- read_csv('mlb_2019_fan_cost_index.csv')
df_prices_2019 <- df_prices_2019 %>%
  select(Teams=Team,
    `2019`=`Avg. Ticket`)
df_prices_2019 %>% head()
df_prices <- merge(df_prices, df_prices_2019,
      by.x='Teams', by.y='Teams')

df <- df_prices %>% select(-Division) %>%
  gather(key=year, value=price, -Teams)
df$Teams <- df$Teams %>% as.factor()
df %>% ggplot() +
  geom_line(aes(x=year,
                y=price,
                group=Teams, color=Teams))
# In rCharts or other
# Interpolate missing year or point out

teamcolors[teamcolors$name=='Arizona Diamondbacks', 1] <- 'Arizona D-backs'
teamcolors[teamcolors$name=='Los Angeles Angels of Anaheim', 1] <- 'Los Angeles Angels'

mlb_colors <- teamcolors %>% filter(league=='mlb')
colscale <- scale_color_manual(name='Teams',
                   values=mlb_colors$primary)
fillscale <- scale_fill_manual(name='Teams',
                               values=mlb_colors$secondary)
#teamcolors %>% filter(league == 'mlb') %>%
#  write_csv('team_colors.csv')
df <- df %>% mutate(id=substr(Teams, 0, 3))
df <- df %>% filter(year!='2001A') %>%
  mutate(year_num=as.numeric(year))

g_prices <- df %>% 
  select(Year=year_num,
         Price=price,
         id,
         Team=Teams) %>% 
  ggplot() +
  geom_line(aes(x=Year,
                y=Price,
                group=id,
                color=Team)) +
  geom_point(aes(x=Year,
                 y=Price,
                 group=id,
                 color=Team),
             alpha=0.5) +
  
  colscale +
  labs(title='Average ticket price',
       caption="Source: Doug's Business of Baseball and Forbes.com") +
  xlab('') + ylab('Regular season ticket price') +
  scale_y_continuous(labels=function(x) paste('$',
                                              x,
                                              sep=''),
                     breaks=round(seq(0, 
                                max(df$price, na.rm=T),
                                by=10), 1)) +
  scale_x_continuous(breaks=round(seq(min(df$year_num),
                                      2020,
                                      by=5), 0)) +
  theme_eric()
g_prices
g_prices_p <- ggplotly(g_prices,
                       tooltip=c('Team', 'Year', 'Price')) %>% 
  layout(title='Average ticket price')
g_prices_p

# Marlins lowering ticket prices for 2019
# https://www.usatoday.com/story/sports/mlb/marlins/2018/08/27/miami-marlins-ticket-strategy-prices/1108305002/

# ggsave(file='viz_prices.svg', plot=g,
#        width=20, height=8)
# 
# # Write to csv
# df %>% 
#   select(Year=year_num,
#          Price=price,
#          id,
#          Team=Teams) %>% 
#   write_csv('viz_prices_long.csv')
# df_prices %>%
#   write_csv('viz_prices_wide.csv')