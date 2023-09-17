library(tidyverse)
library(mosaic)
library(here)
library(lubridate)
library(skimr)
library(ggiraph) # interactive graphs

#read the CSV file
bike <- read_csv(here::here("data", "london_bikes.csv"))

# fix dates using lubridate, and generate new variables for year, month, month_name, day, and day_of _week
bike <- bike %>%   
  mutate(
    month = month(date),
    month_name=month(date, label = TRUE)) 

# generate new variable season_name to turn seasons from numbers to Winter, Spring, etc
bike <- bike %>%  
  mutate(
    season_name = case_when(
      month_name %in%  c("Dec", "Jan", "Feb")  ~ "Winter",
      month_name %in%  c("Mar", "Apr", "May")  ~ "Spring",
      month_name %in%  c("Jun", "Jul", "Aug")  ~ "Summer",
      month_name %in%  c("Sep", "Oct", "Nov")  ~ "Autumn",
    ),
    season_name = factor(season_name, 
                         levels = c("Winter", "Spring", "Summer", "Autumn")),
    weekend = case_when(
      wday %in%  c("Sat", "Sun")  ~ "Weekend",
      TRUE  ~ "Weekday",
    )
  )


# Density, using facet_grid() year ~ month_name 

bike %>% 
  filter(year > 2013) %>% 

ggplot( aes(x=bikes_hired))+
  geom_density()+
  facet_grid(year  ~  month_name)+
  theme_bw()+
  labs(x=NULL, y = NULL)+
  NULL
