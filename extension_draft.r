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
if(!require(Metrics)) install.packages("Metrics", repos = repo)

##read data into from excel
#Read data from excel sheet
crimes <- read_xlsx('data/crime.xlsx', 
                    sheet = 'crimes')

##Import COVID Case Data
covid <- read_xlsx('data/covid_cases.xlsx',
                   sheet = 'month')

covid_dummy <- read_xlsx('data/covid_dummy.xlsx')


#names columns by creating vector of names
colnames(crimes) <- c('date', 'crime', 'desc')

#read in population worksheet
pop <- read_xlsx('data/pop.xlsx', 
                 sheet = 'pop')
#name column names of population data frame
colnames(pop) <- c('month', 'year', 'pop')

#new data frame to combine with crime occurrence to make rate.
crimes_trans <- crimes %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(year < 2021) %>% 
  group_by(month, year, crime) %>% 
  summarize(count = n())

#merge dataframes to reflect crime rate per occurrence
crime_pop <- left_join(crimes_trans, 
                       pop, 
                       by = c('month', 'year')) %>% 
  mutate(rate = count/pop,
         date = date(paste0(year,'-',month,'-1'))) %>% 
  select(month, year, date, crime, rate) %>% 
  pivot_wider(names_from = 'crime', 
              values_from = c('rate'))

#Make TS objects of full ts each of the crime type.
theft_ts <- ts(crime_pop$THEFT, 
               start = c(2011, 1),
               frequency = 12)

homicide_ts<- ts(crime_pop$HOMICIDE, start = c(2011, 1),
                 frequency = 12)

narcotics_ts <- ts(crime_pop$NARCOTICS, start = c(2011, 1),
                   frequency = 12)

sexual_assault_ts <- ts(crime_pop$`SEXUAL ASSAULT`, start = c(2011, 1),
                        frequency = 12)

assault_battery_ts <- ts(crime_pop$ASSAULT +crime_pop$BATTERY, start = c(2011, 1),
                         frequency = 12)

covid_ts <- ts(covid, start = c(2011,1), frequency = 12)

covid_dummy_ts <- ts(covid_dummy$covid_dummy, start= c(2011,1),
                     frequency = 12)


##creating control ts
narcotics_control_ts <- ts(crime_pop$NARCOTICS[1:96],  start = c(2011, 1),
                           end = c(2018,12),
                           frequency = 12)

sass_control_ts <- ts(crime_pop$`SEXUAL ASSAULT`[1:96],  start = c(2011, 1),
                      end = c(2018,12),
                      frequency = 12)

theft_control_ts <- ts(crime_pop$THEFT[1:96],  start = c(2011, 1),
                       end = c(2018,12),
                       frequency = 12)

homicide_control_ts <- ts(crime_pop$HOMICIDE[1:96],  start = c(2011, 1),
                          end = c(2018,12),
                          frequency = 12)


bass_control_ts <- ts(crime_pop$ASSAULT[1:96] +crime_pop$BATTERY[1:96],
                      start = c(2011,1), end = c(2018,12), frequency = 12)


##truncated ts
covid_dummy_trunc <- covid_dummy$covid_dummy[97:120]
theft_ts_trunc <- ts(theft_ts[97:120], start = c(2019, 1), frequency = 12)
narcotics_ts_trunc <- ts(narcotics_ts[97:120], start = c(2019, 1), frequency = 12)
homicide_ts_trunc <- ts(homicide_ts[97:120], start = c(2019, 1), frequency = 12)
sass_ts_trunc <- ts(sexual_assault_ts[97:120], start = c(2019, 1), frequency = 12)
bass_ts_trunc <- ts(assault_battery_ts[97:120], start = c(2019, 1), frequency = 12)

##SARIMA MODELS
## correct order control model
theft_control_model <- sarima(theft_control_ts,0,1,1,1,0,0,10, no.constant = TRUE)
narcotics_control_model <- sarima(narcotics_control_ts, 0,1,1,1,0,1,10, no.constant = TRUE)
sass_control_model <- sarima(sass_control_ts, 0,1,1,1,0,1,10, no.constant = TRUE)
homicide_control_model <- sarima(homicide_control_ts, 0,1,0,1,0,1,10, no.constant = TRUE)
bass_control_model <- sarima(bass_control_ts, 0,1,1,1,0,0,10, no.constant = TRUE)

##forecasts of control
theft_control_for <- sarima.for(theft_control_ts,
                                24,0,1,1,1,0,0,10, no.constant = TRUE)
narcotics_control_for <- sarima.for(narcotics_control_ts,
                                    24, 0,1,1,1,0,1,10, no.constant = TRUE)
homicide_control_for <- sarima.for(homicide_control_ts, 
                                   24,0,1,0,1,0,1,10, no.constant = TRUE)
sass_control_for <-  sarima(sass_control_ts, 
                            24,0,1,1,1,0,1,10, no.constant = TRUE)
bass_control_for <- sarima.for(bass_control_ts,
                                24,0,1,1,1,0,0,10, no.constant = TRUE)


##forecast of whole ts
theft_forecast <- sarima.for(theft_ts,
                                24,0,1,1,1,0,0,10, no.constant = TRUE)

narcotics_forecast <- sarima.for(narcotics_ts,
                                 24, 0,1,1,1,0,1,10, no.constant = TRUE)
homicide_forecast <- sarima.for(homicide_ts, 
                                24,0,1,0,1,0,1,10, no.constant = TRUE)
sass_forecast <- sarima.for(sexual_assault_ts,
                            24,0,1,1,1,0,1,10, no.constant = TRUE)
bass_forecast <- sarima.for(assault_battery_ts,
                            24,0,1,1,1,0,0,10, no.constant = TRUE)


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
theft_fore_df <- theft_forecast$pred %>% 
  as.data.frame() %>% 
  mutate(t = seq(97, 120, 1),
         actual = theft_ts[97:120],
         pred = x) %>% 
  select(t, actual, pred) %>% 
  pivot_longer(!t, names_to = "Type", values_to = "Value")

# rmse of our forecasted values
theft_forcast_rmse <- theft_fore_df <- theft_forecast$pred %>% 
  as.data.frame() %>% 
  mutate(t = seq(97, 120, 1),
         actual = theft_ts[97:120],
         pred = x) %>% 
  select(t, actual, pred)

rmse(theft_forcast_rmse$actual, theft_forcast_rmse$pred)

# rmse as a proportion of the mean crime rate
rmse(fore_rmse$actual, fore_rmse$pred)/mean(theft_ts[97:120])*100

# plot actuals vs the 24 month forecast
theft_fore_df %>% 
  ggplot(aes(x = t, y = Value, color = Type)) +
  geom_line()+
  ggtitle('Predicted Theft Rates vs Actuals', subtitle = '2019 & 2020') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

##Narcotics Validation
# create naroc forecast df to use in ggplot
narcotics_forcast_df <- narcotics_forecast$pred %>% 
  as.data.frame() %>% 
  mutate(t = seq(97, 120, 1),
         actual = narcotics_ts[97:120],
         pred = x) %>% 
  select(t, actual, pred) %>% 
  pivot_longer(!t, names_to = "Type", values_to = "Value")

# rmse of our forecasted values
narcotics_forcast_rmse <- narcotics_forcast_df <- narcotics_forecast$pred %>% 
  as.data.frame() %>% 
  mutate(t = seq(97, 120, 1),
         actual = narcotics_ts[97:120],
         pred = x) %>% 
  select(t, actual, pred)

rmse(narcotics_forcast_rmse$actual, narcotics_forcast_rmse$pred)

# rmse as a proportion of the mean crime rate
rmse(narcotics_forcast_rmse$actual,
     narcotics_forcast_rmse$pred)/mean(narcotics_ts[97:120])*100

# plot actuals vs the 24 month forecast
narcotics_forcast_df %>% 
  ggplot(aes(x = t, y = Value, color = Type)) +
  geom_line()+
  ggtitle('Predicted Narcotics Rates vs Actuals', subtitle = '2019 & 2020') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

##Homicide Validaton
homicide_forcast_df <- homicide_forecast$pred %>% 
  as.data.frame() %>% 
  mutate(t = seq(97, 120, 1),
         actual = homicide_ts[97:120],
         pred = x) %>% 
  select(t, actual, pred) %>% 
  pivot_longer(!t, names_to = "Type", values_to = "Value")

# rmse of our forecasted values
homicide_forcast_rmse <- homicide_forcast_df <- homicide_forecast$pred %>% 
  as.data.frame() %>% 
  mutate(t = seq(97, 120, 1),
         actual = theft_ts[97:120],
         pred = x) %>% 
  select(t, actual, pred)

rmse(homicide_forcast_rmse$actual, homicide_forcast_rmse$pred)

# rmse as a proportion of the mean crime rate
rmse(homicide_forcast_rmse$actual, 
     homicide_forcast_rmse$pred)/mean(homicide_ts[97:120])*100

# plot actuals vs the 24 month forecast
homicide_forcast_df %>% 
  ggplot(aes(x = t, y = Value, color = Type)) +
  geom_line()+
  ggtitle('Predicted Homicide Rates vs Actuals', subtitle = '2019 & 2020') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

##Assault/Battery Validation
# create naroc forecast df to use in ggplot
bass_forcast_df <- bass_forecast$pred %>% 
  as.data.frame() %>% 
  mutate(t = seq(97, 120, 1),
         actual = assault_battery_ts[97:120],
         pred = x) %>% 
  select(t, actual, pred) %>% 
  pivot_longer(!t, names_to = "Type", values_to = "Value")

# rmse of our forecasted values
bass_forcast_rmse <- bass_forcast_df <- bass_forecast$pred %>% 
  as.data.frame() %>% 
  mutate(t = seq(97, 120, 1),
         actual = assault_battery_ts[97:120],
         pred = x) %>% 
  select(t, actual, pred)

rmse(bass_forcast_rmse$actual, bass_forcast_rmse$pred)

# rmse as a proportion of the mean crime rate
rmse(bass_forcast_rmse$actual, bass_forcast_rmse$pred)/mean(assault_battery_ts[97:120])*100

# plot actuals vs the 24 month forecast
bass_forcast_df %>% 
  ggplot(aes(x = t, y = Value, color = Type)) +
  geom_line()+
  ggtitle('Predicted Assault/Battery Rates vs Actuals', 
          subtitle = '2019 & 2020') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

##Sexual Assault Validation
sass_forcast_df <- sass_forecast$pred %>% 
  as.data.frame() %>% 
  mutate(t = seq(97, 120, 1),
         actual = sexual_assault_ts[97:120],
         pred = x) %>% 
  select(t, actual, pred) %>% 
  pivot_longer(!t, names_to = "Type", values_to = "Value")

# rmse of our forecasted values
sass_forcast_rmse <- sass_forcast_df <- sass_forecast$pred %>% 
  as.data.frame() %>% 
  mutate(t = seq(97, 120, 1),
         actual = sexual_assault_ts[97:120],
         pred = x) %>% 
  select(t, actual, pred)

rmse(sass_forcast_rmse$actual, sass_forcast_rmse$pred)

# rmse as a proportion of the mean crime rate
rmse(sass_forcast_rmse$actual, sass_forcast_rmse$pred)/mean(sexual_assault_ts[97:120])*100

# plot actuals vs the 24 month forecast
sass_forcast_df %>% 
  ggplot(aes(x = t, y = Value, color = Type)) +
  geom_line()+
  ggtitle('Predicted Sexual Assault Rates vs Actuals', subtitle = '2019 & 2020') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



