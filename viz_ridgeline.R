library(tidyverse)
library(Lahman)
library(baseballr)

df <- read_csv('tickets.csv')
df %>% head()