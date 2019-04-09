library(tidyverse)
library(maps)
library(ggthemes)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
library(gdata)
library(rsconnect)

df_prices <- read_csv('earlyticketprices.csv')
df_prices %>% head()
df_stadiums <- read_csv('MLB Stadiums.csv')
df_prices_2019 <- read_csv('mlb_2019_fan_cost_index.csv')
df_prices_2019 <- df_prices_2019 %>%
  select(Teams=Team,
         `2019`=`Avg. Ticket`)
df_prices_2019 %>% head()
df_prices <- merge(df_prices, df_prices_2019,
                   by.x='Teams', by.y='Teams')

df <- df_prices %>% select(-Division) %>%
  gather(key=year, value=price, -Teams)

# Merge with location data
df_merge <- merge(x=df, y=df_stadiums,
      by.x='Teams', by.y='Team',
      all.x=T)
# Temp turn year into num
df_merge <- df_merge %>% filter(year!='2001A')
df_merge$year <- df_merge$year %>% as.numeric()

teams <- df_merge %>% select(Teams) %>% unique()

# Braves Mil>Atl
df_merge$Latitude[df_merge$Teams=='Atlanta Braves' & df_merge$year<1966] <- 43.030
df_merge$Longitude[df_merge$Teams=='Atlanta Braves' & df_merge$year<1966] <- -87.974
# Braves Bos>Mil
df_merge$Latitude[df_merge$Teams=='Atlanta Braves' & df_merge$year<1953] <- 42.353
df_merge$Longitude[df_merge$Teams=='Atlanta Braves' & df_merge$year<1953] <- -71.119
# SFG NY>SF
df_merge$Latitude[df_merge$Teams=='San Francisco Giants' & df_merge$year<1958] <- 40.830833
df_merge$Longitude[df_merge$Teams=='San Francisco Giants' & df_merge$year<1958] <- -73.9375
# Dodgers Bklyn>LA
df_merge$Latitude[df_merge$Teams=='Los Angeles Dodgers' & df_merge$year<1958] <- 40.665
df_merge$Longitude[df_merge$Teams=='Los Angeles Dodgers' & df_merge$year<1958] <- -73.958056
# Orioles StL>BAL
df_merge$Latitude[df_merge$Teams=='Baltimore Orioles' & df_merge$year<1954] <- 38.658
df_merge$Longitude[df_merge$Teams=='Baltimore Orioles' & df_merge$year<1954] <- -90.22
# LA Angels LA>Anaheim
df_merge$Latitude[df_merge$Teams=='Los Angeles Angels' & df_merge$year<1966] <- 34.073611
df_merge$Longitude[df_merge$Teams=='Los Angeles Angels' & df_merge$year<1966] <- -118.24
# Milwaukee Brewers Sea>Mil
df_merge$Latitude[df_merge$Teams=='Milwaukee Brewers' & df_merge$year<1970] <- 47.58
df_merge$Longitude[df_merge$Teams=='Milwaukee Brewers' & df_merge$year<1970] <- -122.298
# Minnesota Twins Was>Min
df_merge$Latitude[df_merge$Teams=='Minnesota Twins' & df_merge$year<1961] <- 38.9175
df_merge$Longitude[df_merge$Teams=='Minnesota Twins' & df_merge$year<1961] <- -77.020278
# Oakland As KC>Oak
df_merge$Latitude[df_merge$Teams=='Oakland Athletics' & df_merge$year<1968] <- 39.086
df_merge$Longitude[df_merge$Teams=='Oakland Athletics' & df_merge$year<1968] <- -94.558
# Oakland As PHI>KC
df_merge$Latitude[df_merge$Teams=='Oakland Athletics' & df_merge$year<1955] <- 39.996111
df_merge$Longitude[df_merge$Teams=='Oakland Athletics' & df_merge$year<1955] <- -75.165
# Texas Rangers Was>Txs
df_merge$Latitude[df_merge$Teams=='Texas Rangers' & df_merge$year<1972] <- 38.89
df_merge$Longitude[df_merge$Teams=='Texas Rangers' & df_merge$year<1972] <- -76.972
# Washington Nationals Mtl>Was
df_merge$Latitude[df_merge$Teams=='Washington Nationals' & df_merge$year<2005] <- 45.558
df_merge$Longitude[df_merge$Teams=='Washington Nationals' & df_merge$year<2005] <- -73.552

# Rename name changing teams
df_merge$popup <- df_merge$Teams
# Milwaukee Braves > Atlanta Braves
df_merge$popup[df_merge$Teams=='Atlanta Braves' & df_merge$year<1966] <- 'Milwaukee Braves'
# Boston Braves > Milwaukee Braves
df_merge$popup[df_merge$popup=='Milwaukee Braves' & df_merge$year<1953] <- 'Boston Braves'
# NYG > SF Giants
df_merge$popup[df_merge$Teams=='San Francisco Giants' & df_merge$year<1958] <- 'New York Giants'
# Cincinnati Redlegs > Reds
df_merge$popup[df_merge$Teams=='Cincinnati Reds' & df_merge$year<1959] <- 'Cincinnati Redlegs'
# Cincinnati Reds > Redlegs
df_merge$popup[df_merge$popup=='Cincinnati Redlegs' & df_merge$year<1954] <- 'Cincinnati Reds'
# Bklyn Dodgers > LA Dodgers
df_merge$popup[df_merge$Teams=='Los Angeles Dodgers' & df_merge$year<1958] <- 'Brooklyn Dodgers'
# StL Browns > Baltimore Orioles
df_merge$popup[df_merge$Teams=='Baltimore Orioles' & df_merge$year<1954] <- 'Milwaukee Brewers'
# KC As > Oak As
df_merge$popup[df_merge$Teams=='Oakland Athletics' & df_merge$year<1968] <- 'Kansas City Athletics'
# Phi As > KC As
df_merge$popup[df_merge$popup=='Kansas City Athletics' & df_merge$year<1955] <- 'Philadelphia Athletics'
# Was Senators > Min Twins
df_merge$popup[df_merge$Teams=='Minnesota Twins' & df_merge$year<1961] <- 'Washington Senators'
# Was Senators > Tex Rangers
df_merge$popup[df_merge$Teams=='Texas Rangers' & df_merge$year<1972] <- 'Washington Senators'
# LA Angels
df_merge$popup[df_merge$Teams=='Los Angeles Angels' & df_merge$year<2016] <- 'Los Angeles Angels of Anaheim'
df_merge$popup[df_merge$popup=='Los Angeles Angels of Anaheim' & df_merge$year<2005] <- 'Anaheim Angels'
df_merge$popup[df_merge$popup=='Anaheim Angels' & df_merge$year<1997] <- 'California Angels'
df_merge$popup[df_merge$popup=='California Angels' & df_merge$year<1965] <- 'Los Angeles Angels'
# Hou Colts > Astros
df_merge$popup[df_merge$Teams=='Houston Astros' & df_merge$year<1965] <- 'Houston Colt .45s'
# Sea Pilots > Mil Brewers
df_merge$popup[df_merge$Teams=='Milwaukee Brewers' & df_merge$year<1970 & df_merge$City=='Milwaukee, WI'] <- 'Seattle Pilots'
# Mtl Expos > Was Natl
df_merge$popup[df_merge$Teams=='Washington Nationals' & df_merge$year<2005] <- 'Montreal Expos'
# Flr Marlins > Mia
df_merge$popup[df_merge$Teams=='Miami Marlins' & df_merge$year<2012] <- 'Florida Marlins'
# Devil Rays > Rays
df_merge$popup[df_merge$Teams=='Tampa Bay Rays' & df_merge$year<2008] <- 'Tampa Bay Devil Rays'

# Change stadium names and cities for tooltips

df_leaflet <- df_merge %>% 
  select(Teams, price, year, Latitude, Longitude, Team_names_hist=popup) %>% 
  mutate(popup_p=paste(Team_names_hist,
                     as.character(year),
                     sep=' '),
         popup=paste(popup_p,
                     as.character(price),
                     sep=': $'))

m %>% addProviderTiles(providers$Stamen.Toner)
m %>% addProviderTiles(providers$Stamen.Watercolor)
m %>% addProviderTiles(providers$OpenStreetMap.BlackAndWhite)

teamcolors[teamcolors$name=='Arizona Diamondbacks', 1] <- 'Arizona D-backs'
teamcolors[teamcolors$name=='Los Angeles Angels of Anaheim', 1] <- 'Los Angeles Angels'

mlb_colors <- teamcolors %>% filter(league=='mlb')
mlb_teams <- mlb_colors %>% 
  select(name) %>% pull
mlb_primary <- mlb_colors %>% 
  select(primary) %>% pull
mlb_secondary <- mlb_colors %>% 
  select(secondary) %>% pull

pal <- colorFactor(mlb_primary,
                   domain=mlb_teams)
pal_stroke <- colorFactor(mlb_secondary,
                          domain=mlb_teams)

leaflet(df_leaflet) %>% setView(lng=-98.583,
                            lat=39.833,
                            zoom=4) %>% 
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
  addCircles(lng=~Longitude, lat=~Latitude,
             weight=1, radius=~price*5000,
             color=~pal(Teams),
             stroke=F)

years <- df_leaflet %>% arrange(year) %>% 
  select(year) %>% unique() %>% 
  mutate(year_num=as.numeric(year)) %>% 
  select(year_num) %>% pull
steps <- years %>% as.matrix() %>% 
  diff() %>% as.vector()

# Not in use
order <- c('Arizona D-backs',
           'Atlanta Braves',
           'Baltimore Orioles',
           'Boston Red Sox',
           'Chicago Cubs',
           'Chicago White Sox',
           'Cincinnati Reds',
           'Cleveland Indians',
           'Colorado Rockies',
           'Detroit Tigers',
           'Houston Astros',
           'Kansas City Royals',
           'Los Angeles Dodgers',
           'Los Angeles Angels',
           'Miami Marlins',
           'Milwaukee Breppwers',
           'Minnesota Twins',
           'New York Yankees',
           'New York Mets',
           'San Francisco Giants',
           'Oakland Athletics',
           'Philadelphia Phillies',
           'Pittsburgh Pirates',
           'San Diego Padres',
           'Seattle Mariners',
           'St. Louis Cardinals',
           'Tampa Bay Rays',
           'Texas Rangers',
           'Toronto Blue Jays',
           'Washington Nationals'))

ui <- fluidPage(
  sliderTextInput(inputId='year', label='Year',
                  choices=years,
                  selected=max(years),
                  grid=T),
  leafletOutput(outputId='map')
)
server <- function(input, output) {
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
      addCircles(data=df_leaflet[df_leaflet$year==input$year,],
                 lng=~jitter(Longitude, factor=2),
                 lat=~jitter(Latitude, factor=2),
                 weight=1, 
                 radius=~log(price)*50000,
                 color=~pal(Teams),
                 stroke=F, fillOpacity=.3,
                 popup=~popup)
  })
}

shinyApp(ui, server)
#rsconnect::deployApp('')