library(skimr)
library(tidyverse)
df.final.all <- read.csv('C:/Users/ogmcd/Dropbox/00_2019_Class/Spring/BMI706/Project/final_all.csv')
df.final.all %>% skim
df.wo.duplicate <- df.final.all %>% distinct(County, State, Date,.keep_all=TRUE)

complete.COVID <- function(data, col) {
  completeVec <- complete.cases(data[, col])
  return(data[completeVec, ])
}

df <- complete.COVID(df.wo.duplicate, 'total_confirmed_cases')

df <- mutate(
  df, day = case_when(Date=='Mar 23' ~ 323,
                      Date=='Mar 24' ~ 324,
                      Date=='Mar 25' ~ 325,
                      Date=='Mar 26' ~ 326,
                      Date=='Mar 27' ~ 327,
                      Date=='Mar 28' ~ 328,
                      Date=='Mar 29' ~ 329,
                      Date=='Mar 30' ~ 330,
                      Date=='Mar 31' ~ 331,
                      Date=='Apr 1' ~ 401,
                      Date=='Apr 2' ~ 402,
                      Date=='Apr 3' ~ 403,
                      Date=='Apr 4' ~ 404,
                      Date=='Apr 5' ~ 405,
                      Date=='Apr 6' ~ 406,
                      Date=='Apr 7' ~ 407,
                      Date=='Apr 8' ~ 408,
                      Date=='Apr 9' ~ 409,
                      Date=='Apr 10' ~ 410,
                      Date=='Apr 11' ~ 411,
                      Date=='Apr 12' ~ 412,
                      Date=='Apr 13' ~ 413,
                      Date=='Apr 14' ~ 414,
                      Date=='Apr 15' ~ 415,
                      Date=='Apr 16' ~ 416,
                      Date=='Apr 17' ~ 417,
                      Date=='Apr 18' ~ 418,
                      Date=='Apr 19' ~ 419,
                      Date=='Apr 20' ~ 420,
                      Date=='Apr 21' ~ 421,
                      Date=='Apr 22' ~ 422,
                      Date=='Apr 23' ~ 423,
                      Date=='Apr 24' ~ 424,
                      Date=='Apr 25' ~ 425,
                      Date=='Apr 26' ~ 426)
)

df$day <- as.factor(df$day)

df %>% skim

df.orderd <- df[order(df$State, df$County, df$day),]
df.orderd %>% head(100)

states <- unique(df.orderd$State)

counties.state <- list(rep(NA, 11))
  
for (i in 1:11){
  counties <- unique(df.orderd$County[df.orderd$State==states[i]])
  counties.state[[i]] <- counties
}
counties.state[[1]]

df.state.county <- list(rep(NA, 11))

for (i in 1:11){
  state <- states[i]
  counties <- counties.state[[i]]
  n_counties <- length(counties)
  dfs <- list(rep(NA, n_counties))
  for (j in 1:n_counties){
    county <- counties[j]
    df.county <- df.orderd[(df.orderd$State==state) & (df.orderd$County==county), ]
    dfs[[j]] <- df.county
  }
  df.state.county[[i]] <- dfs
}

#sample <- df.state.county[[2]][[2]]

#case.plot <- ggplot(sample, aes(x = day, y = total_confirmed_cases, group=1))+
#  geom_line()+
#  xlab("Date") +
#  ylab("Total confirmed cases") +
#  ggtitle("Change in total confirmed cases over time")

#case.plot

get_time_trend <- function(state_num, county_num){
  plot <- ggplot(df.state.county[[state_num]][[county_num]], aes(x = day, y = total_confirmed_cases, group=1))+
    geom_line()+
    xlab("Date") +
    ylab("Total confirmed cases") +
    ggtitle("Change in total confirmed cases over time")
  return(plot)
}

get_time_trend(1, 3)