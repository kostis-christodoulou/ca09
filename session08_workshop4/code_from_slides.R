library(tidyverse)
library(gapminder)
library(broom)
library(kableExtra)


yearMin <- 1952

many_models <- gapminder %>% 
  group_by(country,continent) %>% 
  
  nest()  %>% 
  
  mutate(simple_model = data %>% 
           map(
             ~lm(lifeExp ~ I(year - yearMin), 
                 data = .))) %>% 
  
  mutate(coefs = simple_model %>% 
           map(~ broom::tidy(., conf.int = TRUE)),
         details = simple_model %>% 
           map(broom::glance)) %>% 
  ungroup()

glimpse(many_models)
# too many lists!


# Unnesting flattens a list-column of data frames back into regular columns
intercepts <- 
  many_models %>% 
  
  # Unnesting flattens a list-column of data frames back into regular columns
  unnest(coefs) %>% 
  filter(term == "(Intercept)") %>% 
  arrange(estimate) %>% 
  mutate(country = fct_inorder(country)) %>% 
  select(country, continent, estimate, std.error, conf.low, conf.high)


slopes <- many_models %>% 
  unnest(coefs) %>% 
  filter(term == "I(year - yearMin)") %>% 
  arrange(estimate) %>% 
  mutate(country = fct_inorder(country)) %>% 
  select(country, continent, estimate, std.error, conf.low, conf.high)


# plot all intercepts

ggplot(data = intercepts, 
       aes(x = country, y = estimate, fill=continent))+
  geom_col()+
  coord_flip()+
  theme_minimal(6)+
  facet_wrap(~continent, scales="free")+
  labs(title = 'Life expectancy in 1952',
       caption = 'Source: Gapminder package',
       x = NULL,
       y = NULL) +
  theme(legend.position="none")

# plot all slopes
ggplot(data = slopes, 
       aes(x = country, y = estimate, fill = continent))+
  geom_col()+
  coord_flip()+
  theme_minimal(6)+
  facet_wrap(~continent, scales="free")+
  labs(title = 'Average yearly improvement in life expectancy, 1952-2007',
       caption = 'Source: Gapminder package', 
       x = NULL,
       y = NULL) +
  theme(legend.position="none")

# Plot CIs for slopes
ggplot(data = slopes, 
       aes(x = country, y = estimate, colour = continent))+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  coord_flip()+
  theme_minimal(6)+
  facet_wrap(~continent, scales="free")+
  labs(title = 'Average yearly improvement in life expectancy, 1952-2007',
       caption = 'Source: Gapminder package', 
       x = NULL,
       y = NULL) +
  theme(legend.position="none")

## Fit 500 CAPM regressions for each of the SP500 companies against the SP500 

library(rvest)
library(tictoc)
library(janitor)
library(tidyquant)

#let's define interval- from today to 5 years ago
ending_date <- Sys.Date()
starting_date <- Sys.Date() - 365*5 # go back 5 years from today; otherwise, give starting date as "2017-01-01"

# let us scrape the wikipedia page that has the SP500 constituents
sp500_url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

#get tables that exist on URL- we don't use CSS selctors, just  
#   html_nodes(css="table")
tables <- sp500_url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called sp500 
# Use purr::map() to create a list of all tables in URL
sp500 <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               clean_names())


# constituents
table1 <- sp500[[1]] # the first table on the page contains the ticker symbols

# we need a vector of strings with just the tickers
tickers <- table1 %>% 
  select(symbol) %>% 
  pull()  # pull() gets them as a sting of characters

sp500_stocks <- tickers %>% 
  tq_get(get  = "stock.prices",
         from = starting_date # go back 5 years from today; otherwise, give starting date as "2017-01-01"
  ) %>%
  group_by(symbol)

# get prices for SPY, the SP500 ETF
spy <- tq_get("SPY", get  = "stock.prices",
              from = starting_date
) 

#calculate monthly  returns
my_big_stocks_returns_monthly <- sp500_stocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_return",
               cols = c(nested.col)) 


#calculate SPY monthly  returns
spy_returns_monthly <- spy %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "SPY_return",
               cols = c(nested.col))

sp500_data <- left_join(my_big_stocks_returns_monthly, spy_returns_monthly, by="date")

# there was a lot of downloading data and joining individual stock returns with the SP500 returns.
# now we can run our 500 regressions

tic()
too_many_models <- sp500_data %>% 
  # create a list containing a separate dataframe for each stock
  group_by(symbol) %>% 
  nest()  %>% 
  
  # Run a simple regression model for every country in the dataframe
  mutate(simple_capm_model = data %>% 
           map(~lm(monthly_return ~  SPY_return, data = .))) %>% 
  
  # extract regression coefficients and model details with broom::tidy()
  mutate(coefs = simple_capm_model %>% 
           map(~ tidy(., conf.int = TRUE)),
         details = simple_capm_model %>% 
           map(glance)) %>% 
  ungroup()
toc()
# it takes about 1.2 seconds on my mac mini

# pull intercepts, or alphas of the stocks
intercepts <- 
  too_many_models %>% 
  unnest(coefs) %>% 
  filter(term == "(Intercept)") %>% 
  arrange(estimate) %>% 
  mutate(symbol = fct_inorder(symbol)) %>% 
  select(symbol, estimate, std.error, conf.low, conf.high)

# Too many to print... If you do want to print, use the following code
intercepts %>%
  rename(alpha = estimate) %>%
  kable()%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


# pull slopes, or betas 
slopes <- too_many_models %>% 
  unnest(coefs) %>% 
  filter(term == "SPY_return") %>% 
  arrange(estimate) %>% 
  mutate(symbol = fct_inorder(symbol)) %>% 
  select(symbol, estimate, std.error, conf.low, conf.high)

# Too many betas to print... If you do want to print, use the following code
slopes %>%
  rename(beta = estimate) %>%
  arrange(desc(beta)) %>%
  kable()%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


# pull R^2 and regression SE, or specific risk 
rsq_errors <- too_many_models %>% 
  unnest(details) %>% 
  select(symbol, r.squared, sigma) %>% 
  arrange(desc(r.squared)) 

# let us join everything together
capm_results <- inner_join(slopes, intercepts, by="symbol")

# Column `symbol` joining factors with different levels, coercing to character vector
capm_results <- inner_join(capm_results, rsq_errors, by="symbol")

capm_results <- inner_join(capm_results, table1, by="symbol")


# rename and keep beta, se(beta), and CI for beta, alpha, rsq, regression SE (specific risk)
# as well as name, sector, and industry
capm_results <- capm_results %>% 
  rename(
    beta = estimate.x,
    se_beta = std.error.x,
    beta_low = conf.low.x,
    beta_high = conf.high.x,
    alpha = estimate.y,
    residual_se = sigma,
    sector = gics_sector) %>% 
  select(symbol, security, beta, se_beta, beta_low, beta_high, alpha, r.squared, residual_se, sector, gics_sub_industry,headquarters_location) 


# plot 501 betas (1/2)
ggplot(data = capm_results, aes(x = beta, fill=sector))+
  geom_density(alpha = 0.3)+
  theme_bw(8)+
  labs(title = 'Estimating distribution of betas by sector',
       
       # zoo::as.yearmon() gives months as, e.g., "Sep 2023" 
       subtitle = paste0(zoo::as.yearmon(starting_date), " to ", zoo::as.yearmon(ending_date)),
       x = "") +
  facet_wrap(~sector)+
  theme(legend.position = "none")+
  NULL

## Plotting 500 betas (2/2)


# Using ggridges, a nice package to draw many density plots together
library(ggridges)

ggplot(capm_results, aes(x = beta, y = sector, fill=sector)) +
  geom_density_ridges(alpha= 0.6) + 
  theme_bw() + 
  labs(
    x = "Beta estimate", 
    y = "",
    title = "Estimating SP500 betas",
    subtitle = paste0(zoo::as.yearmon(starting_date), " to ", zoo::as.yearmon(ending_date)),
  ) + 
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )+
  geom_vline(xintercept = 1, 
             colour = "#001e62", #HEX code for the official LBS dark blue 
             size = 2)+
  NULL


