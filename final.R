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

#Read data from excel sheet
crimes <- read_xlsx('data/crime.xlsx', 
                    sheet = 'crimes')

##Import COVID Case Data
covid <- read_xlsx('data/covid_cases.xlsx',
                   sheet = 'month')


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

#Make TS objects of each of the crime type.
theft_ts <- ts(crime_pop$THEFT, 
               start = c(2011, 1),
               frequency = 12)

##Generate Time Series Objects
homicide_ts<- ts(crime_pop$HOMICIDE, start = c(2011, 1),
                 frequency = 12)

narcotics_ts <- ts(crime_pop$NARCOTICS, start = c(2011, 1),
                   frequency = 12)

sexual_assault_ts <- ts(crime_pop$`SEXUAL ASSAULT`, start = c(2011, 1),
                        frequency = 12)

assault_battery_ts <- ts(crime_pop$ASSAULT +crime_pop$BATTERY, start = c(2011, 1),
                         frequency = 12)

covid_ts <- ts(covid, start = c(2011,1), frequency = 12)


#Narcotics EDA
narcotics_raw <- ts.plot(narcotics_ts, main = 'Narcotics Raw Data Time Series', 
                         ylab= 'Narcotics Rate per Capita')

narcotics_diff <- ts.plot(diff(narcotics_ts), main = 'First Difference of Narcotics
        Time Series', ylab= 'Narcotics Rate per Capita')


ndiffs(narcotics_ts)

adf.test(narcotics_ts)

plot(decompose(narcotics_ts))

plot(decompose(diff(narcotics_ts)))

acf2(narcotics_ts, main = 'Narcotics Raw Data ACF/PACF Plots')

acf2(first_narcotics, main = 'First Difference Narcotics 
     ACF/PACF Plots')

auto.arima(narcotics_ts)
##Indicates ARIMA(1,1,0)(2,0,1)[12] model. 

narcotics_model1 <- sarima(narcotics_ts,1,1,0,2,0,1,12)
##Model has a residual ACF/PACF and LB plot off. Also has an insignificant constant term.
##AIC - -14.40172
##BIC - -14.26159

##Narcotics Model 1 Regression Output Table
narcotics_model1$ttable %>% knitr::kable(caption = "Narcotics 
                                         SARIMA(1,1,0)(2,0,1)[12] Model", color = "black") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


narcotics_model2 <- sarima(narcotics_ts,1,1,0,2,0,1,12, no.constant = TRUE)
##Model still has same ACF/PACF and LB plot problems. Also, the ACF/PACF shows that there is a 10-month cycle. 
##AIC - -14.41701
##BIC - -14.30024

##Narcotics Model 2 Regression Output Table
narcotics_model2$ttable %>% knitr::kable(caption = "Narcotics 
                                         SARIMA(1,1,0)(2,0,1)[12] Model
                                         without Constant") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


narcotics_model3 <- sarima(narcotics_ts,1,1,0,2,0,1,10)
##ACF plot much better; however, LB plot still off. sar1n and constant has insignigicant pvalue.
##AIC - -15.05853
##BIC - -14.9184

##Narcotics Model 3 Regression Output Table
narcotics_model3$ttable %>% knitr::kable(caption = "Narcotics 
                                         SARIMA(1,1,0)(2,0,1)[10] Model") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


narcotics_model4 <- sarima(narcotics_ts,1,1,0,1,0,1,10, no.constant = TRUE)
##LB plot off and ACF still shows a problem. 
##AIC - -15.10449
##BIC - -15.01107

##Narcotics Model 4 Regression Output Table
narcotics_model4$ttable %>% knitr::kable(caption = "Narcotics 
                                         SARIMA(1,1,0)(1,0,1)[10] Model with
                                         No Constant") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


narcotics_model5 <- sarima(narcotics_ts,0,1,1,1,0,1,10, no.constant = TRUE)
##ACF plot looks good for the most part, At lag 18-20 there is some anomoly. 
## QQ plot shows one outlier. LB plot looks a little bit better; however the first couple of lags are under
##threshold.

##Narcotics Model 5 Regression Output Table
narcotics_model5$ttable %>% knitr::kable(caption = "Narcotics 
                                         SARIMA(0,1,1)(1,0,1)[10] Model with No Constant") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


round(ARMAtoAR(ar = c(0,0,0,0,0,0,0,0,.9528), ma = c(-.9783,0,0,0,0,0,0,0,0,-.5613,.5491),  
               lag.max =  50))
##Converges


round(ARMAtoMA(ar = c(0,0,0,0,0,0,0,0,.9528), ma = c(-.9783,0,0,0,0,0,0,0,0,-.5613,.5491),  
               lag.max =  500))

##converges.....

polyroot(.9528)
polyroot(c(-.9783, -.5613, .5491))

##good model....



narcotics_forecast <- sarima.for(narcotics_ts,24,0,1,1,1,0,1,10, no.constant = TRUE)

##Forecast Estimation Table
narcotics_forecast %>% knitr::kable(caption = "Narcotics Forecast Predicted Rates") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(2, color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")




#Theft EDA
ts.plot(theft_ts, main = 'Theft Raw Data Time Series', 
        ylab= 'Theft Rate per Capita')

ts.plot(diff(theft_ts), main = 'First Difference of Theft Time Series', 
        ylab= 'Theft Rate per Capita')

adf.test(theft_ts)

adf.test(diff(theft_ts))

plot(decompose(theft_ts))

plot(decompose(diff(theft_ts)))


acf2(theft_ts, main = 'Theft Raw Data ACF/PACF Plots')

acf2(first_theft, main = 'First Difference Theft ACF/PACF Plots')

auto.arima(theft_ts)
##Autoarima suggests an ARIMA(2,0,2)(0,0,1)[12] model 

theft_model1 <- sarima(theft_ts,2,0,2,0,0,1,12, no.constant = TRUE)
##ar1 and ma1 are both insignificant. LB, QQ, and Standardized residual plots look good
##ACF plot show incorrect seasonality component.
##AIC - -15.01811
##BIC - -14.87873

##Theft Model 1 Regression Output Table
theft_model1$ttable %>% knitr::kable(caption = "Theft
                                         SARIMA(2,0,2)(0,1,1)[12] Model") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")



theft_model2 <- sarima(theft_ts,2,0,2,0,0,1,10)
##LB plot is borderline. QQ plot shows a possible outlier, but still in CI.
##ACF shows an anomoly around lag 20. ar1 and ma1 are still insignificant. 
##AIC - -15.21844
##BIC - -15.05583

##Theft Model 2 Regression Output Table
theft_model2$ttable %>% knitr::kable(caption = "Theft SARIMA(2,0,2)(0,0,1)[10] Model") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


theft_model3 <- sarima(theft_ts,1,0,1,0,0,1,10)
##ACF plot still shows an anomoly at lag 20. LB plot still borderline. All pvalues are significant. \
##AIC - -15.18618
##BIC - -15.07003

##Theft Model 3 Regression Output Table
theft_model3$ttable %>% knitr::kable(caption = "Theft
                                         SARIMA(1,0,1)(0,0,1)[10] Model") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


theft_model4 <- sarima(theft_ts,1,0,1,1,0,0,10)
##Pvalues all significant. ACF problem corrected. LB plot good. QQ plot show possible outlier.
##AIC - -15.31437
##BIC - -15.19822

##Theft Model 4 Regression Output Table
theft_model4$ttable %>% knitr::kable(caption = "Theft
                                         SARIMA(1,0,1)(1,0,0)[10] Model") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")

round(ARMAtoAR(ar = c(.7943,0,0,0,0,0,0,0,.9916), ma = -.6165, 
               lag.max =  50))
##converges


round(ARMAtoMA(ar = c(.7943,0,0,0,0,0,0,0,.9916), ma = -.6165, 
               lag.max =  50))
##not causal - diverges.....


theft_model5 <- sarima(theft_ts,0,1,1,1,0,0,10, no.constant = TRUE)
##Plots look good. pvalues good. QQ plot does show some outliers outside CI. 

##Theft Model 5 Regression Output Table
theft_model5$ttable %>% knitr::kable(caption = "Theft
                                         SARIMA(0,1,1)(1,0,0)[10] Model
                                      without Constant") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")

round(ARMAtoAR(ar = c(0,0,0,0,0,0,0,0,0,.5730), ma = -.7841, 
               lag.max =  50))
##converges


round(ARMAtoMA(ar = c(0,0,0,0,0,0,0,0,0,.5730), ma = -.7841, 
               lag.max =  50))
##converges

polyroot(.5730)
polyroot(-.7841)
##good model!!!!!

theft_forecast <- sarima.for(theft_ts,24,0,1,1,1,0,0,10)




#Homicide EDA
ts.plot(homicide_ts, main = 'Homicide 
        Raw Data Time Series', 
        ylab= 'Homicide Rate per Capita')

ts.plot(diff(homicide_ts), main = 'First Difference of
        Homicide Time Series', 
        ylab= 'Homicide Rate per Capita')

adf.test(homicide_ts)
adf.test(diff(homicide_ts))

plot(decompose(homicide_ts))

plot(decompose(diff(homicide_ts)))

ndiffs(homicide_ts)

acf2(homicide_ts, main = 'Homicide
     Raw Data ACF/PACF Plots')

acf2(diff(homicide_ts), main = 'First Difference of 
     Homicide ACF/PACF Plots')

auto.arima(homicide_ts)
##Autoarima suggests an ARIMA(0,0,0)(1,0,0)[12] model.


homicide_model1 <- sarima(homicide_ts,0,0,0,1,0,0,12)
##ACF plot shows incorrect seasonality. All other plots are good.
##xmean is insignificant.
##AIC - -19.30235
##BIC - -19.23266

##Homicide Model 1 Regression Output Table
homicide_model1$ttable %>% knitr::kable(caption = "Homicide
                                         SARIMA(0,0,0)(1,0,0)[12] Model") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


homicide_model2 <- sarima(homicide_ts,0,0,0,1,0,0,10)
##ACF plot shows anomoly. xmean insignificant. 
##AIC - -19.08095
##BIC - -19.0345

##Homicide Model 2 Regression Output Table
homicide_model2$ttable %>% knitr::kable(caption = "Homicide
                                         SARIMA(0,0,0)(1,0,0)[10] Model") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


homicide_model3 <- sarima(homicide_ts,0,0,0,1,0,0,10, no.constant = TRUE)
##same plots and output. xmean dropped. 

##Homicide Model 3 Regression Output Table
homicide_model3$ttable %>% knitr::kable(caption = "Homicide
                                         SARIMA(0,0,0)(1,0,0)[10] Model
                                        without Constant") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


homicide_model4 <- sarima(homicide_ts,0,0,0,1,0,1,10)
#NA values. Plots look good.

##Homicide Model 4 Regression Output Table
homicide_model4$ttable %>% knitr::kable(caption = "Homicide
                                         SARIMA(0,0,0)(1,0,1)[10] Model") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")



homicide_model5 <- sarima(homicide_ts,0,0,0,1,0,1,10, no.constant = TRUE)
##pvalues all good. plots good. 
##AIC - -19.25005
##BIC - -19.18037

##Homicide Model 5 Regression Output Table
homicide_model5$ttable %>% knitr::kable(caption = "Homicide
                                         SARIMA(0,0,0)(1,0,1)[10] Model
                                        without Constant") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")

round(ARMAtoAR(ar = c(0,0,0,0,0,0,0,0,0,.9916), ma = c(0,0,0,0,0,0,0,0,0,0, -.5250), 
               lag.max =  50))
##converges

round(ARMAtoMA(ar = c(0,0,0,0,0,0,0,0,0,.9916), ma = c(0,0,0,0,0,0,0,0,0,0, -.5250), 
               lag.max = 1000))
##converges

polyroot(0.9916)
polyroot(-.5250)
##no parameter redundancy..... good model

homicide_forecast <- sarima.for(homicide_ts,24,0,0,0,1,0,1,10, no.constant = TRUE)


#Sexual Assault EDA
ts.plot(sexual_assault_ts, main = 'Sexual Assault 
        Raw Data Time Series', 
        ylab= 'Sexual Assault Rate per Capita')

ts.plot(diff(sexual_assault_ts), main = 'First Difference of
        Sexual Assault Time Series', 
        ylab= 'Sexual Assault Rate per Capita')

adf.test(sexual_assault_ts)

adf.test(diff(sexual_assault_ts))

plot(decompose(sexual_assault_ts))

plot(decompose(diff(sexual_assault_ts)))

ndiffs(sexual_assault_ts)

acf2(sexual_assault_ts, main = 'Sexual Assault
     Raw Data ACF/PACF Plots')

acf2(diff(sexual_assault_ts), main = 'First Difference of 
     Sexual Assault ACF/PACF Plots')

auto.arima(sexual_assault_ts)
##auto arima suggested ARIMA(1,1,1) model.


sex_assault_model1 <- sarima(sexual_assault_ts,1,1,1, no.constant = TRUE)
##Significant pvalues.
##ACF plot shows cyclical behavior.
##AIC - -20.13231
##BIC - -20.06225

##Sexual Assault Model 1 Regression Output Table
sex_assault_model1$ttable %>% knitr::kable(caption = "Sexual Assault
                                         SARIMA(1,1,1) Model without Constant") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


round(ARMAtoAR(ar = .3378, ma = -.9716, 
               lag.max =  50))
##converges

round(ARMAtoMA(ar = .3378, ma = -.9716, 
               lag.max =  50))
##converges

polyroot(.3378)
polyroot(-.9716)
##model is causal and invertible, no parameter redundancy.


sex_assault_model2 <- sarima(sexual_assault_ts,1,1,1,1,0,0,10, no.constant = TRUE)
##all significant pvalues. plots look good. 
##AIC - -20.21428
##BIC - -20.12086

##Sexual Assault Model 2 Regression Output Table
sex_assault_model2$ttable %>% knitr::kable(caption = "Sexual Assault
                                         SARIMA(1,1,1)(1,0,0)[10] Model without Constant") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")

round(ARMAtoAR(ar = c(.2543,0,0,0,0,0,0,0,0,.344,0.0875), ma = -.9775, 
               lag.max = 50))
##converges

round(ARMAtoMA(ar = c(.2543,0,0,0,0,0,0,0,0,.344,0.0875), ma = -.9775, 
               lag.max = 50))
##converges

polyroot(c(.2543, .344, .0875))
polyroot(-.9775)
##causal/invertable, no parameter redundancy.



sex_assault_model3 <- sarima(sexual_assault_ts,1,1,1,1,0,1,10, no.constant = TRUE)
##insignificant ar1 pvalue

##Sexual Assault Model 3 Regression Output Table
sex_assault_model3$ttable %>% knitr::kable(caption = "Sexual Assault
                                         SARIMA(1,1,1)(1,0,1)[10] Model without Constant") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


sex_assault_model4 <- sarima(sexual_assault_ts,0,1,1,1,0,1,10, no.constant = TRUE)
##LB model borderline. All other plots look good. 
##AIC - -20.28843
##BIC - -20.19501

##Sexual Assault Model 4 Regression Output Table
sex_assault_model4$ttable %>% knitr::kable(caption = "Sexual Assault
                                         SARIMA(0,1,1)(1,0,1)[10] Model without Constant") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")



round(ARMAtoAR(ar = c(0,0,0,0,0,0,0,0,0, .9568), ma = c(-.9609,0,0,0,0,0,0,0,0,-.7754,.7451), 
               lag.max = 50))
##converges

round(ARMAtoMA(ar = c(0,0,0,0,0,0,0,0,0, .9568), ma = c(-.9609,0,0,0,0,0,0,0,0,-.7754,.7451), 
               lag.max = 50))
##converges

polyroot(.9568)
polyroot(c(-.9609, -.7754, .7451))
##no parameter redundancy


sex_assault_forecast <- sarima.for(sexual_assault_ts,24,0,1,1,1,0,1,10, no.constant = TRUE)


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


adf.test(assault_battery_ts)

adf.test(diff(assault_battery_ts))

plot(decompose(assault_battery_ts))

plot(decompose(diff(assault_battery_ts)))

auto.arima(assault_battery_ts)
##Autoarima suggests ARIMA(1,0,0)

abatt_model1 <- sarima(assault_battery_ts,1,0,0)
#ACF anomoly at lag 10. Many outliers on QQ plot. LB plot not good. 
##AIC - -14.9037
##BIC - -14.83418

##Assault/Battery Model 1 Regression Output Table
abatt_model1$ttable %>% knitr::kable(caption = "Assault/Battery
                                         SARIMA(1,0,0) Model") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


abatt_model2 <- sarima(assault_battery_ts,1,0,0,0,0,1,10)
##ACF and QQ plot way off. 
##AIC  - -15.37531
##BIC - -15.28239

##Assault/Battery Model 2 Regression Output Table
abatt_model2$ttable %>% knitr::kable(caption = "Assault/Battery
                                         SARIMA(1,0,0)(0,0,1)[10] Model") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")

#alt 2
abatt_model3 <- sarima(assault_battery_ts,1,0,1,1,0,0,10)
##plots much better. ACF plot shows lag 18 outside ci. Outlier in QQ plot. 
##AIC - -15.81367
##BIC - -15.69752

##Assault/Battery Model 3 Regression Output Table
abatt_model3$ttable %>% knitr::kable(caption = "Assault/Battery
                                         SARIMA(1,0,1)(1,0,0)[10] Model") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")

round(ARMAtoAR(ar = c(.8051,0,0,0,0,0,0,0,0, .8009, .6448), ma = -.5807, 
               lag.max = 50))
##Converges

round(ARMAtoMA(ar = c(.8051,0,0,0,0,0,0,0,0, .8009, .6448), ma = -.5807, 
               lag.max = 50))

##not causal.....  diverges...


abatt_model4 <- sarima(assault_battery_ts,1,1,1,1,0,0,10)
##insignificant constant and ar1 value. 
##ACF plot shows anomoly at lag 18. 
##AIC - -15.75057
##BIC - -15.6338

##Assault/Battery Model 4 Regression Output Table
abatt_model4$ttable %>% knitr::kable(caption = "Assault/Battery
                                         SARIMA(1,1,1)(1,0,0)[10] Model") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")


abatt_model5 <- sarima(assault_battery_ts,0,1,1,1,0,0,10, no.constant = TRUE)
##pvalues significant. ACF plot still shows anomoly.
##AIC - -15.78042
##BIC - -15.71036

##Assault/Battery Model 5 Regression Output Table
abatt_model5$ttable %>% knitr::kable(caption = "Assault/Battery
                                         SARIMA(0,1,1)(1,0,0)[10] Model
                                     without Constant") %>% 
        kable_styling(latex_options=c("striped")) %>% 
        column_spec(1:ncol(narcotics_model1$ttable), color = "black") %>% 
        row_spec(0, color = "white", background = "#5a5cd6")

round(ARMAtoAR(ar = c(0,0,0,0,0,0,0,0,0, .8046), ma = -.7375, 
               lag.max = 50))
##converges

round(ARMAtoMA(ar = c(0,0,0,0,0,0,0,0,0, .8046), ma = -.7375, 
               lag.max = 50))
##converges

polyroot(-.7375)
polyroot(.8046)
##model causal/invertible, no parameter redundancy

abatt_forecast <- sarima.for(assault_battery_ts,24,0,1,1,1,0,0,10)

