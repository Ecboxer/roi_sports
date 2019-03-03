library(tidyverse)

df_action = read_csv('length_of_action.csv')
df_action %>% head()
parse_time <- function(x) {
  temp_hrs <- sapply(strsplit(x, ' '), '[', 1)
  temp_mins <- sapply(strsplit(x, ' '), '[', 2)
  hrs <- temp_hrs %>%
    str_extract("\\d*") %>% as.numeric()
  mins <- temp_mins %>% 
    str_extract("\\d*") %>% as.numeric()
  length <- (hrs * 60 + mins) * 60
  return(length)
}
parse_action <- function(x) {
  mins <- x %>% 
    str_extract("^[0-9\\.]*") %>% as.numeric()
  length <- mins * 60
  return(length)
}
clock_duration <- df_action$`Clock Duration` %>%
  parse_time()
df_action <- cbind(df_action, clock_duration)

action_duration <- df_action$`Amt of Action` %>%
  parse_action()
df_action <- cbind(df_action, action_duration)

perc_action <- df_action$action_duration / df_action$clock_duration
df_action <- cbind(df_action, perc_action)

comm_duration <- df_action$`Amt of Commercial Time` * 60
df_action <- cbind(df_action, comm_duration)

perc_comm <- df_action$comm_duration / df_action$clock_duration
df_action <- cbind(df_action, perc_comm)

df_action$num_comm_30sec <- df_action$comm_duration / 30

df_action$comm_per_hr <- df_action$num_comm_30sec / (df_action$clock_duration / 3600)

df_action <- df_action[-c(2,3,4,5,6,7)] %>% head()
df_action %>% write_csv('df_action.csv')