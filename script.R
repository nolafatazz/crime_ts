library(tidyverse)
library(ggplot2)
library(trend)
library(timetk)
library(lubridate)
library(astsa)
library(forecast)
library(tseries)
library(TSA)
library(rmarkdown)
library(fpp2)

sar

install.packages("TSA")
install.packages("testcorr")
install.packages("rmarkdown")
install.packages('fpp2')

#Read data from excel sheet
crimes <- read_xlsx('data/crime.xlsx', 
                    sheet = 'crimes')

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
  
crime_name <- 'ASSAULT'

p <-  crime_pop %>% 
  ggplot() +
  geom_line(aes(x = date, y = get(crime_name))) +
  ggtitle(label = paste0('Crime Rate for ', crime_name),
          subtitle = 'From 2011 to 2020') +
  ylab(crime_name) +
  xlab('Date') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  
  geom_smooth(aes(x = date, y = get(crime_name)))

ggplotly(p)


#Make TS objects of each of the crime type.
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

#First difference objects of crimes.
first_theft <- ts(diff(theft_ts))
first_homicide <- ts(diff(homicide_ts))
first_sexual_assault <- ts(diff(sexual_assault_ts))
first_narcotics <- ts(diff(narcotics_ts))
first_battery_assault <- ts(diff(assault_battery_ts))

#Second difference objects of crimes.
second_theft <- ts(diff(theft_ts,differences = 2))
second_homicide <- ts(diff(homicide_ts,difference = 2))
second_sexual_assault <- ts(diff(sexual_assault_ts, differences = 2))
second_narcotics <- ts(diff(narcotics_ts, differences = 2))
second_battery_assault <- ts(diff(assault_battery_ts, differences = 2))





bartels.test(sexual_assault_ts)

 
#Plots made using GGPlot
theftplot <-  crime_pop %>% 
  ggplot() +
  geom_line(aes(x = date, y = theft_ts)) +
  ggtitle(label = paste0('Crime Rate for Theft'),
          subtitle = 'From 2011 to 2020') +
  ylab('Theft Rate per Capita') +
  xlab('Date') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  
  geom_smooth(aes(x = date, y = theft_ts))

ggplotly(theftplot)

homicideplot <-  crime_pop %>% 
  ggplot() +
  geom_line(aes(x = date, y = homicide_ts)) +
  ggtitle(label = paste0('Crime Rate for Homicide'),
          subtitle = 'From 2011 to 2020') +
  ylab('Homice Rate per Capita') +
  xlab('Date') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  
  geom_smooth(aes(x = date, y = homicide_ts))

sexualplot <-  crime_pop %>% 
  ggplot() +
  geom_line(aes(x = date, y = sexual_assault_ts)) +
  ggtitle(label = paste0('Crime Rate for Sexual Assault'),
          subtitle = 'From 2011 to 2020') +
  ylab('Sexual Assault Rate per Capita') +
  xlab('Date') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  
  geom_smooth(aes(x = date, y = sexual_assault_ts))

narcoticsplot <-  crime_pop %>% 
  ggplot() +
  geom_line(aes(x = date, y = narcotics_ts)) +
  ggtitle(label = paste0('Crime Rate for Narcotics'),
          subtitle = 'From 2011 to 2020') +
  ylab('Narcotics Rate per Capita') +
  xlab('Date') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  
  geom_smooth(aes(x = date, y = narcotics_ts))

batt_ass_plot <-  crime_pop %>% 
  ggplot() +
  geom_line(aes(x = date, y = assault_battery_ts)) +
  ggtitle(label = paste0('Crime Rate for Assault/Battery'),
          subtitle = 'From 2011 to 2020') +
  ylab('Assault/Battery Rate per Capita') +
  xlab('Date') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  
  geom_smooth(aes(x = date, y = assault_battery_ts))

ggplotly(sexualplot)
ggplotly(theftplot)
ggplotly(narcoticsplot)
ggplotly(batt_ass_plot)
ggplotly(homicideplot)


#Raw data plots
ts.plot(homicide_ts)
ts.plot(theft_ts)
ts.plot(narcotics_ts)
ts.plot(sexual_assault_ts)
ts.plot(assault_battery_ts)



diff_theft <-  crime_pop %>% 
  ggplot() +
  geom_line(aes(x = date, y = first_theft)) +
  ggtitle(label = paste0('Crime Rate for First Difference of Theft'),
          subtitle = 'From 2011 to 2020') +
  ylab('Theft Rate per Capita') +
  xlab('Date') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  
  geom_smooth(aes(x = date, y = first_theft))




ndiffs(sexual_assault_ts)

ndiffs(homicide_ts)
ndiffs(narcotics_ts)
ndiffs(assault_battery_ts)


#Narcotics EDA
ts.plot(narcotics_ts, main = 'Narcotics Raw Data Time Series', 
        ylab= 'Narcotics Rate per Capita')
ts.plot(diff(narcotics_ts), main = 'First Difference of Narcotics
        Time Series', ylab= 'Narcotics Rate per Capita')
ndiffs(narcotics_ts)
acf2(narcotics_ts, main = 'Narcotics Raw Data ACF/PACF Plots')
auto.arima(narcotics_ts)
acf2(first_narcotics, main = 'First Difference Narcotics 
     ACF/PACF Plots')
adf.test(narcotics_ts)
adf.test(first_narcotics)
plot(decompose(narcotics_ts))
plot(decompose(diff(narcotics_ts)))

#Theft EDA
ts.plot(theft_ts, main = 'Theft Raw Data Time Series', 
        ylab= 'Theft Rate per Capita')
ts.plot(diff(theft_ts), main = 'First Difference of Theft Time Series', 
        ylab= 'Theft Rate per Capita')
ndiffs(theft_ts)
acf2(theft_ts, main = 'Theft Raw Data ACF/PACF Plots')
auto.arima(theft_ts)
acf2(first_theft, main = 'First Difference Theft ACF/PACF Plots')
adf.test(theft_ts)
adf.test(first_theft)
plot(decompose(theft_ts))
plot(decompose(diff(theft_ts)))

#Homicide EDA
ts.plot(homicide_ts, main = 'Homicide 
        Raw Data Time Series', 
        ylab= 'Homicide Rate per Capita')
ts.plot(diff(homicide_ts), main = 'First Difference of
        Homicide Time Series', 
        ylab= 'Homicide Rate per Capita')
ndiffs(homicide_ts)
acf2(homicide_ts, main = 'Homicide
     Raw Data ACF/PACF Plots')
acf2(diff(homicide_ts), main = 'First Difference of 
     Homicide ACF/PACF Plots')
auto.arima(homicide_ts)
adf.test(homicide_ts)
adf.test(diff(homicide_ts))
plot(decompose(homicide_ts))
plot(decompose(diff(homicide_ts)))

McLeod

#Sexual Assault EDA
ts.plot(sexual_assault_ts, main = 'Sexual Assault 
        Raw Data Time Series', 
        ylab= 'Sexual Assault Rate per Capita')
ts.plot(diff(sexual_assault_ts), main = 'First Difference of
        Sexual Assault Time Series', 
        ylab= 'Sexual Assault Rate per Capita')
ndiffs(sexual_assault_ts)
acf2(sexual_assault_ts, main = 'Sexual Assault
     Raw Data ACF/PACF Plots')
acf2(diff(sexual_assault_ts), main = 'First Difference of 
     Sexual Assault ACF/PACF Plots')
auto.arima(sexual_assault_ts)
adf.test(sexual_assault_ts)
adf.test(diff(sexual_assault_ts))
plot(decompose(sexual_assault_ts))
plot(decompose(diff(sexual_assault_ts)))

#Assault/Battery EDA
ts.plot(assault_battery_ts, main = 'Assault/Battery 
        Raw Data Time Series', 
        ylab= 'Assault/Battery Rate per Capita')
ts.plot(diff(assault_battery_ts), main = 'First Difference of
        Assault/Battery Time Series', 
        ylab= 'Assault/Battery Rate per Capita')
ndiffs(assault_battery_ts)
acf2(assault_battery_ts, main = 'Assault/Battery
     Raw Data ACF/PACF Plots')
acf2(diff(assault_battery_ts), main = 'First Difference of 
     Assault/Battery ACF/PACF Plots')
auto.arima(assault_battery_ts)
adf.test(assault_battery_ts)
adf.test(diff(assault_battery_ts))
plot(decompose(assault_battery_ts))
plot(decompose(diff(assault_battery_ts)))

McLeod.Li.test(theft_ts)
?tr

ndiffs(theft_ts)
ndiffs(narcotics_ts)
ndiffs(sexual_assault_ts)
ndiffs(assault_battery_ts)

theft_narcotics_cc_plot<- plot(cc.test(theft_ts, narcotics_ts, max.lag = 20))
ndiffs(homicide_ts)
is.na(theft_ts)