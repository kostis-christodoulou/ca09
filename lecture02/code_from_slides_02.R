library(tidyverse)
library(nycflights13)
glimpse(flights)

## `nycflights13` Some exploratory questions

# - Which `origin` airports had the greatest number of flights? 
# - What was the longest `arr_delay`? 
# - What was the average `arr_delay` for a `carrier`, say `UA`? 
# - What was the average `dep_delay` and `arr_delay` for all carriers, ranked in descending order of number of flights operated by each carrier?

## `dplyr::count()`

# Quickly count unique values of one or more variables. 
# Number of flights departing from each `origin` airport

flights %>% 
  
  # count() is the same as group_by()... summarise() 
  # `sort=TRUE` gives table in descending order
  count(origin, sort = TRUE) %>% 
  
  # mutate() generates a new column called `prop` which is the proportion of flights 
  # calculated as number of flights `n` divided by the `sum(n)`
  mutate(prop = n/sum(n))




# Flights in May
flights %>% 
  filter(month == 5)


# Flights in the first 8 days of May
flights %>% 
  filter(month == 5, day <= 8)


# Flights to LAX or SFO in August
flights %>% 
  filter(month == 8, dest %in% ("LAX", "SFO"))

# First 10 flights in dataframe

flights %>% 
  slice(1:10)


# Last 5 flights in dataframe

flights %>% 
  slice_tail(n = 5)


## `group_by()` and order of grouping{.smaller}
flights %>% 
  group_by(origin, month) %>% 
  summarise(median_arr_delay = median(arr_delay, na.rm=TRUE)) 

flights %>% 
  group_by(month,origin) %>% 
  summarise(median_arr_delay = median(arr_delay, na.rm=TRUE)) 

## `group_by()` and why we need to `ungroup()`

# How many groups do we have
flights %>% 
  group_by(origin, month) %>% 
  summarise(n = n()) 

# How many groups do we have
flights %>% 
  group_by(origin, month) %>% 
  summarise(n = n()) %>%
  slice(1)

# How many groups do we have now
flights %>% 
  group_by(origin, month) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  slice(1)



## `distinct()` and `sample()`

flights %>%
  select(origin, dest) %>%
  distinct() %>%
  arrange(origin, dest)

# randomly sample a number of flights
flights %>% 
  select(year, origin, dest, month, day) %>% 
  sample_n(10)

# randomly sample a fraction % of flights

flights %>% 
  select(year, origin, dest, month, day) %>% 
  sample_frac(0.02)


# Tidy (long) Data:`

# - Every <b style='color:#28a87d;'>column</b> is a <b style='color:#28a87d;'>variable</b>
# - Every <b style='color:#b82ca1;'>row</b> is an <b style='color:#b82ca1;'>observation</b>
# - Every <b style='color:#dcae1e;'>cell</b> is a <b style='color:#dcae1e;'>single value</b>
  

wide_data <- 
  tribble(
    ~student_id, ~final_exam,~midterm,~individual_project,~group_project,
    "2457625",   79, 68,71,83,
    "1758293",   92, 73, 67,56,
    "1622247",   71, 87,74,77
  )

wide_data

                  

## Wide and long data{.smaller}

# Variable `assignment` is spread across multiple columns:
  
wide_data

wide_data %>% 
  pivot_longer(
    cols = 2:5,
    names_to = "assignment",
    values_to = "score"
  ) 


## Pivoting in action 

#https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_German_federal_election

## `{rvest}` to scrape Wikipedia page
library(rvest) # to scrape wikipedia page
library(lubridate) # to handle conversions from characters to dates

url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_German_federal_election"

# get tables that exist on wikipedia page url
tables <- url %>% 
  read_html() %>% 
  html_nodes(css="table")

# parse HTML tables into a dataframe called polls 
# Use purr::map() to create a list of all tables in URL
polls <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               janitor::clean_names())


# list of opinion polls
german_election_polls <- 
  
  # the third table on the wikipedia page contains the list of all opinions polls
  polls[[3]] %>% 
  
  # drop the first two rows, as they contain variable names and actual election result 
  # and last row that contains 2017 results
  slice(3:(n()-1)) %>%  
  
  mutate(
    # polls are shown to run from-to, e.g. 9-13 Aug 2021. We keep the last date, 
    # 13 Aug here, as the poll date and we extract it by picking the last 11 
    # characters from that field
    end_date = str_sub(fieldwork_date, -11),
    
    # end_date is still a string, so we convert it into a date object using lubridate::dmy()
    end_date = dmy(end_date),
    
    # we also get the month and week number from the date, if we want to do analysis by month- week, etc.
    month = month(end_date),
    week = isoweek(end_date)
  )

dplyr::glimpse(german_election_polls)

#| label: german_polls_line_chart


german_election_polls_long <- german_election_polls %>% 
  select(end_date, month, week, samplesize, union, grune, spd, afd=af_d,fdp, linke) %>% 
  pivot_longer(cols = 5:10,
               names_to = "party",
               values_to = "percent") %>% 
  mutate(party = factor(party, 
                        levels = c("union", "spd", "grune", "fdp", "afd", "linke"),
                        labels = c("CDU/CSU", "SPD", "GRUNE", "FDP", "AFD", "LINKE")
  )
  ) %>% 
  arrange(end_date)


# use colour codes for parties
# source: https://gist.github.com/Fischaela/0cf760f17672e3eb399193e48d7c6104
# even though party colours is not straight-forward... 
# https://blog.datawrapper.de/partycolors/


my_colour_palette = c(
  "#000000", #CDU
  "#E3000F", #SPD
  "#1AA037", #GRUNE
  "#FFEF00", #FDP
  "#0489DB", #AFD
  "#951d7a"  #LINKE-- we need the pink hex code for Die Linke
)



ggplot(german_election_polls_long, aes(x=end_date, y= percent, colour=party))+
  geom_point(alpha=0.2)+
  scale_colour_manual(values=my_colour_palette)+
  geom_smooth(se=F)+
  theme_minimal()+
  NULL



## `pivot_wider()`

# `{ukbabynames}`  contains a listing of UK baby names occurring more than three times per year between 1974 and 2020


library(ukbabynames)
glimpse(ukbabynames)

## Plot % of males born every year

# Which dataframe makes it easier to calculate % of males born every year?

ukbabynames %>%
  group_by(year, sex) %>%
  summarise(count = sum(n)) %>% 
  ungroup()

ukbabynames %>%
  group_by(year, sex) %>%
  summarise(count = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = "sex",
    values_from = "count") 

#| code-line-numbers: "5,6"
ukbabynames %>%
  group_by(year, sex) %>%
  summarise(count = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "sex",
              values_from = "count") %>% 
  mutate(percent_male = M/(F+M))


ukbabynames %>%
  group_by(year, sex) %>%
  summarise(count = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "sex",
              values_from = "count") %>% 
  mutate(percent_male = M/(F+M)) %>% 
  ggplot(aes(x=year, y= percent_male))+
  geom_line()+
  theme_bw()

## `ggThemeAssist` add-in

library(ggThemeAssist)