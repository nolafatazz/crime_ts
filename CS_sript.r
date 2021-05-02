repo = "http://cran.us.r-project.org"

if(!require(tidyverse)) install.packages("tidyverse", repos = repo)
if(!require(ggplot2)) install.packages("ggplot2", repos = repo)
if(!require(trend)) install.packages("trend", repos = repo)
if(!require(timetk)) install.packages("timetk", repos = repo)
if(!require(lubridate)) install.packages("lubridate", repos = repo)
if(!require(astsa)) install.packages("astsa", repos = repo)
if(!require(forecast)) install.packages("forecast", repos = repo)
if(!require(tseries)) install.packages("tseries", repos = repo)
if(!require(TSA)) install.packages("TSA", repos = repo)
if(!require(rmarkdown)) install.packages("rmarkdown", repos = repo)
if(!require(fpp2)) install.packages("fpp2", repos = repo)
if(!require(readxl)) install.packages("readxl", repos = repo)
if(!require(knitr)) install.packages("knitr", repos = repo)
if(!require(kableExtra)) install.packages("kableExtra", repos = repo)
if(!require(gridExtra)) install.packages("gridExtra", repos = repo)
if(!require(nlme)) install.packages("xts", repos = repo)
if(!require(modelr)) install.packages("modelr", repos = repo)

covid_dummy <- read_xlsx('data/covid_dummy.xlsx',
                         sheet = 'month')

covid_dummy_trunc <- covid_dummy$covid_dummy[97:120]

theft_ts_trunc <- ts(theft_ts[97:120], start = c(2019, 1), frequency = 12)

# test with each crime...
# testing truncated series

sarima(theft_ts_trunc,0,1,1,1,0,0,12, 
       no.constant = TRUE, 
       xreg = covid_dummy_trunc, 
       details = FALSE)

# model
theft_control_model <- sarima(theft_control_ts,0,1,1,1,0,0,10, no.constant = TRUE)

# # calculate fits using actuals and resids since sarima doesn't store the fits
# fitted_theft_control <- theft_control_ts + theft_control_model$fit$residuals
# 
# # create df to use in ggplot
# theft_control_df <- theft_control_ts %>%
#   as.data.frame() %>%
#   mutate(t = seq(1, 96, 1),
#          actual = x,
#          fit = fitted_theft_control) %>%
#   select(t, actual, fit)
# 
# # rmse of fits vs actual
# rmse(theft_control_df$actual, theft_control_df$fit)*100
# 
# # create df of fits vs actuals for ggplot
# theft_control_df %>%
#   ggplot() +
#   geom_line(aes(x = t, y = actual), color = "blue") +
#   geom_line(aes(x = t, y = fit), color = "red")

#generate the forecast for the last 2 years to test forecasting accuracy
theft_forc <- sarima.for(theft_control_ts,24,0,1,1,1,0,0,10, no.constant = TRUE)

# create theft forecast df to use in ggplot
theft_fore_df <- theft_forc$pred %>% 
  as.data.frame() %>% 
  mutate(t = seq(97, 120, 1),
         actual = theft_ts[97:120],
         pred = x) %>% 
  select(t, actual, pred) %>% 
  pivot_longer(!t, names_to = "Type", values_to = "Value")

# rmse of our forecasted values
fore_rmse <- theft_fore_df <- theft_forc$pred %>% 
  as.data.frame() %>% 
  mutate(t = seq(97, 120, 1),
         actual = theft_ts[97:120],
         pred = x) %>% 
  select(t, actual, pred)

rmse(fore_rmse$actual, fore_rmse$pred)

# rmse as a proportion of the mean crime rate
rmse(fore_rmse$actual, fore_rmse$pred)/mean(theft_ts[97:120])*100

# plot actuals vs the 24 month forecast
theft_fore_df %>% 
  ggplot(aes(x = t, y = Value, color = Type)) +
  geom_line()+
  ggtitle('Predicted Theft Rates vs Actuals', subtitle = '2019 & 2020') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))






