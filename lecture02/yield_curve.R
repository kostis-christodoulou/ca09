library(tidyverse)
library(tidyquant)

# Get a list of FRED codes for US rates and US yield curve; choose monthly frequency
tickers <- c('TB3MS', # 3-month Treasury bill (or T-bill)
             'TB6MS', # 6-month
             'GS1',   # 1-year
             'GS2',   # 2-year, etc....
             'GS3',
             'GS5',
             'GS7',
             'GS10',
             'GS20',
             'GS30')  #.... all the way to the 30-year rate

# Turn  FRED codes to human readable variables
myvars <- c('3-Month Treasury Bill',
            '6-Month Treasury Bill',
            '1-Year Treasury Rate',
            '2-Year Treasury Rate',
            '3-Year Treasury Rate',
            '5-Year Treasury Rate',
            '7-Year Treasury Rate',
            '10-Year Treasury Rate',
            '20-Year Treasury Rate',
            '30-Year Treasury Rate')

maturity <- c('3m', '6m', '1y', '2y','3y','5y','7y','10y','20y','30y')

# be default R will sort these maturities alphabetically; but since we want
# to keep them in that exact order, we recast maturity as a factor 
# or categorical variable, with the levels defined as we want
maturity <- factor(maturity, levels = maturity)

# Create a lookup dataset
mylookup<-data.frame(symbol = tickers,
                     var = myvars, 
                     maturity = maturity)
# Take a look:
mylookup %>% 
  knitr::kable()

df <- tickers %>% 
  tq_get(get="economic.data", 
         from="1960-01-01")   # start from January 1990


yield_curve <- left_join(df,mylookup,by="symbol") %>% 
  rename(value=price) %>%
  mutate(duration = factor(var, levels = myvars)) %>% 
  as_tibble()

#plot yield curve 1: Time series plots of all maturities

ggplot(data=yield_curve, aes(x=date,y=value, colour= duration))+
  geom_line()+
  facet_wrap(~duration, nrow=5)+
  theme_bw()+
  theme(legend.position="none") +
  labs(title="Yields on U.S. Treasury rates since 1960",
       x = NULL, y = "%",
       caption="Source: St. Louis Federal Reserve Economic Database (FRED)")

#plot yield curve 2

yield_curve  %>%
  mutate(year = factor(year(date))) %>%
  filter(date>'1999-01-01')  %>%
  ggplot(aes(x=maturity,y=value, group=date, colour = year))+
  geom_line()+
  theme_bw()+ 
  facet_wrap(~year, nrow=6)+
  theme(legend.position="none") +
  labs(title = "US Yield Curve", 
       x = "Maturity", 
       y = "Yield (%)", 
       caption="Source: St. Louis Federal Reserve Economic Database (FRED)")


#plot yield curve 3: Time series plots of 3months and 10 year maturities

yield_curve %>% 
  filter(symbol %in% c('TB3MS', 'GS10'))%>% 
  filter(date>'1999-01-01')%>% 
  ggplot(aes(x=date,y=value, colour= duration))+
  geom_line()+
  # facet_wrap(~duration)+
  theme_bw()+
  # Remove title for fill legend
  guides(colour=guide_legend(title=NULL))+
  labs(title="Yields on 3-month and 10-year US Treasury rates since 1999",
       x = NULL, y = "%",
       caption="Source: St. Louis Federal Reserve Economic Database (FRED)")


#plot yield curve 3 facetted: Time series plots of 3months and 10 year maturities

yield_curve %>% 
  filter(symbol %in% c('TB3MS', 'GS10'))%>% 
  filter(date>'1999-01-01')%>% 
  ggplot(aes(x=date,y=value, colour= duration))+
  geom_line()+
  facet_wrap(~duration)+
  theme_bw()+
  theme(legend.position="none") +
  labs(title="Yields on 3-month and 10-year US Treasury rates since 1999",
       x = NULL, y = "%",
       caption="Source: St. Louis Federal Reserve Economic Database (FRED)")


# A partial inversion occurs when only some of the short-term Treasuries 
# 5 years have higher yields than 30-year Treasuries

#plot yield curve 4: Time series plots of 2- and 5- year maturities

yield_curve %>% 
  filter(symbol %in% c('GS2', 'GS5'))%>% 
  filter(date>'1999-01-01')%>% 
  ggplot(aes(x=date,y=value, colour= duration))+
  geom_line()+
  # facet_wrap(~duration)+
  theme_bw()+
  # Remove title for fill legend
  guides(colour=guide_legend(title=NULL))+
  labs(title="Yields on 2- and 5-year US Treasury rates since 1999",
       x = NULL, y = "%",
       caption="Source: St. Louis Federal Reserve Economic Database (FRED)")


#plot yield curve 4 facetted: Time series plots of 2- and 5-year maturities

yield_curve %>% 
  filter(symbol %in% c('GS2', 'GS5'))%>% 
  filter(date>'1999-01-01')%>% 
  ggplot(aes(x=date,y=value, colour= duration))+
  geom_line()+
  facet_wrap(~duration)+
  theme_bw()+
  theme(legend.position="none") +
  labs(title="Yields on 3-month and 10-year US Treasury rates since 1999",
       x = NULL, y = "%",
       caption="Source: St. Louis Federal Reserve Economic Database (FRED)")


