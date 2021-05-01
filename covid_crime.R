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


##Import COVID Case Data
covid <- read_xlsx('data/covid_cases.xlsx',
                   sheet = 'month')

##Covid TS object
covid_ts <- ts(covid, start = c(2011,1), frequency = 12)

sarima(theft_ts,0,1,1,1,0,0,10, no.constant = TRUE, xreg = covid_ts, error_actio)


##Set Crime Control TS

##
theft_control_ts <- ts(crime_pop$THEFT[1:96],  start = c(2011, 1),
                                   end = c(2018,12),
                                           frequency = 12)
auto.arima(theft_control_ts)

theft_control_model <- sarima(theft_control_ts,0,1,1,1,0,0,10, no.constant = TRUE)

rmse(theft_control_model, theft_control_for)

  sarima(theft_control_ts,2,0,2,0,0,1,10)

acf2(theft_control_ts)

theft_control_for <- sarima.for(theft_control_ts,24,0,1,1,1,0,0,10, no.constant = TRUE)

tsplot(theft_ts)

rmse
