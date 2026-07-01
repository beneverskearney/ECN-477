#prelims
setwd('C:/Users/bek2809/OneDrive - UNC-Wilmington/Senior Year/ECN 477 - 001')

library(fredr)
library(tidyverse)
library(dplyr)


#### 5.1 Time Series Data Prep ####

#api - Application Programming Interface

# Pull data from fred api
fred_key = "e422db82da87b3d24cb9709d59fb45ca"

fredr_set_key(fred_key)

# Define a start and end date
start_date = as.Date("1960-01-01")
end_date = as.Date("2019-12-31")



ngdp_raw = fredr(
  series_id = "GDP",
  observation_start = start_date,
  observation_end = end_date
)

ngdp = ngdp_raw %>%
  rename(ngdp=value) %>% # renames the column to ngdp
  select(date, ngdp)     # keep only date and ngdp

# plot GDP in levels
ggplot(data=ngdp) +
  geom_line(aes(x=date, y=ngdp), color="purple")+
  labs(title="US Nominal Gross Domestic Product",
     x="",
     y="Millions of US Dollars") +
  theme_classic()

# Pull RGDP in levels over the same period

rgdp_raw = fredr(
  series_id = "GDPC1",
  observation_start = start_date,
  observation_end = end_date
)

# Define a start and end date
start_date = as.Date("1960-01-01")
end_date = as.Date("2019-12-31")

# Rename the column through a pipe
rgdp = rgdp_raw %>%
  rename(rgdp=value) %>%
  select(date, rgdp)

#Plot rgdp
ggplot(data=ngdp) +
  geom_line(aes(x=date, y=ngdp), color="red")+
  labs(title="US Nominal Gross Domestic Product",
       x="",
       y="Millions of US Dollars") +
  theme_classic()

# Merge the two datasets
macro_data = full_join(ngdp, rgdp, by="date")
macro_wide = macro_data
macro_long = macro_wide %>%
  pivot_longer(cols=c(ngdp, rgdp), names_to="variable", values_to="values")

# Plot both NGDP and RGDP
ggplot(macro_long, aes(x=date, y=value)) +
  geom_line() +
  labs(title="US Gross Domestic Product",
       x="",
       y="Billions of US Dollars") +
  scale_color_manual(labels=c("Nominal", "Real"), values=c("purple","skyblue")) +
  theme_classic()


# Transformations
macro_wide2 = macro_wide %>%
  mutate(
    rgdp_tm1 = lag(rgdp),                                          # First lag of Y
    rgdp_diff1 = rgdp - rgdp_tm1,                                  # First difference of Y
    rgdp_diff4 = rgdp - lag(rgdp,4),                               # Fourth difference of Y
    rgdp_growth1 = 100 * ((rgdp - lag(rgdp)))/ lag(rgdp),          # Quarterly % change formula
    rgdp_growth1a = 100 * ((rgdp / lag(rgdp))^4 - 1),              # Quarterly % change (annualized) - CAGR
    rgdp_growth4 =  100 * ((rgdp - lag(rgdp,4))/ lag(rgdp,4)),     # Annual % change 
    rgdp_lgrowth1 = 4 * 100 * (log(rgpd) - log(lag(rgdp))          # Log first difference
    ))
# Plot to compare log-difference with %-change

# Autocorrelation functions (ACF)
acf(macro_wide2$rgdp)

# Estimate AR(1)
library(forecast)
ar1 = arima(rgdp, order=c(1,0,0)) # Order should always be order=c(p,0,0) p=AR terms (lags)
summary(ar1)

#### 5.5 Autoregressive Distributed Lag Models (ARDL) ####

# Obtain term spread measure from FRED
term_spread = fredr(
  series_id = "T10Y2Y",
  observation_start = start_date,
  observation_end = end_date
)








