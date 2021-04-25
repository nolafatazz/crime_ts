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



#Narcotics EDA
ts.plot(narcotics_ts, main = 'Narcotics Raw Data Time Series', 
        ylab= 'Narcotics Rate per Capita')

ts.plot(diff(narcotics_ts), main = 'First Difference of Narcotics
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

sarima(narcotics_ts,1,1,0,2,0,1,12)
##Model has a residual ACF/PACF and LB plot off. Also has an insignificant constant term.
##AIC - -14.40172
##BIC - -14.26159

#alt 1
sarima(narcotics_ts,1,1,0,2,0,1,12, no.constant = TRUE)
##Model still has same ACF/PACF and LB plot problems. Also, the ACF/PACF shows that there is a 10-month cycle. 
##AIC - -14.41701
##BIC - -14.30024

#alt 2
sarima(narcotics_ts,1,1,0,2,0,1,10)
##ACF plot much better; however, LB plot still off. sar1n and constant has insignigicant pvalue.
##AIC - -15.05853
##BIC - -14.9184


#alt3
sarima(narcotics_ts,1,1,0,1,0,1,10, no.constant = TRUE)
##LB plot off and ACF still shows a problem. 
##AIC - -15.10449
##BIC - -15.01107


#alt4
sarima(narcotics_ts,0,1,1,1,0,1,10, no.constant = TRUE)
##ACF plot looks good for the most part, At lag 18-20 there is some anomoly. 
## QQ plot shows one outlier. LB plot looks a little bit better; however the first couple of lags are under
##threshold.

round(ARMAtoAR(ar = c(0,0,0,0,0,0,0,0,.9528), ma = c(-.9783,0,0,0,0,0,0,0,0,-.5613,.5491),  
               lag.max =  50))
##Converges


round(ARMAtoMA(ar = c(0,0,0,0,0,0,0,0,.9528), ma = c(-.9783,0,0,0,0,0,0,0,0,-.5613,.5491),  
               lag.max =  500))

##converges.....

polyroot(.9528)
polyroot(c(-.9783, -.5613, .5491))

##good model....

sarima.for(narcotics_ts,24,0,1,1,1,0,1,10, no.constant = TRUE)

##alt5
sarima(narcotics_ts,0,1,1,2,0,1,10, no.constant = TRUE)
##Insignificant sar2 component, LB plot is off. Outlier present in QQ plot. 

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

sarima(theft_ts,2,0,2,0,0,1,12, no.constant = TRUE)
##ar1 and ma1 are both insignificant. LB, QQ, and Standardized residual plots look good
##ACF plot show incorrect seasonality component.
##AIC - -15.01811
##BIC - -14.87873

#alt model 1
sarima(theft_ts,2,0,2,0,0,1,10)
##LB plot is borderline. QQ plot shows a possible outlier, but still in CI.
##ACF shows an anomoly around lag 20. ar1 and ma1 are still insignificant. 
##AIC - -15.21844
##BIC - -15.05583

#alt model 2
sarima(theft_ts,1,0,1,0,0,1,10)
##ACF plot still shows an anomoly at lag 20. LB plot still borderline. All pvalues are significant. \
##AIC - -15.18618
##BIC - -15.07003

#alt model 3
sarima(theft_ts,1,0,1,1,0,0,10)
##Pvalues all significant. ACF problem corrected. LB plot good. QQ plot show possible outlier.
##AIC - -15.31437
##BIC - -15.19822

round(ARMAtoAR(ar = c(.7943,0,0,0,0,0,0,0,.9916), ma = -.6165, 
               lag.max =  50))
##converges


round(ARMAtoMA(ar = c(.7943,0,0,0,0,0,0,0,.9916), ma = -.6165, 
               lag.max =  50))
##not causal - diverges.....


#altmodel 4
sarima(theft_ts,0,1,1,1,0,0,10, no.constant = TRUE)
##Plots look good. pvalues good. QQ plot does show some outliers outside CI. 

round(ARMAtoAR(ar = c(0,0,0,0,0,0,0,0,0,.5730), ma = -.7841, 
               lag.max =  50))
##converges


round(ARMAtoMA(ar = c(0,0,0,0,0,0,0,0,0,.5730), ma = -.7841, 
               lag.max =  50))
##converges

polyroot(.5730)
polyroot(-.7841)
##good model!!!!!

sarima.for(theft_ts,24,0,1,1,1,0,0,10)




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


sarima(homicide_ts,0,0,0,1,0,0,12)
##ACF plot shows incorrect seasonality. All other plots are good.
##xmean is insignificant.
##AIC - -19.30235
##BIC - -19.23266

##alternate model 1
sarima(homicide_ts,0,0,0,1,0,0,10)
##ACF plot shows anomoly. xmean insignificant. 
##AIC - -19.08095
##BIC - -19.0345

##alt model 2
sarima(homicide_ts,0,0,0,1,0,0,10, no.constant = TRUE)
##same plots and output. xmean dropped. 

##alternative model 3
sarima(homicide_ts,0,0,0,1,0,1,10)
#NA values. Plots look good. 


##alt model 4
sarima(homicide_ts,0,0,0,1,0,1,10, no.constant = TRUE)
##pvalues all good. plots good. 
##AIC - -19.25005
##BIC - -19.18037

round(ARMAtoAR(ar = c(0,0,0,0,0,0,0,0,0,.9916), ma = c(0,0,0,0,0,0,0,0,0,0, -.5250), 
               lag.max =  50))
##converges

round(ARMAtoMA(ar = c(0,0,0,0,0,0,0,0,0,.9916), ma = c(0,0,0,0,0,0,0,0,0,0, -.5250), 
               lag.max = 1000))
##converges

polyroot(0.9916)
polyroot(-.5250)
##no parameter redundancy..... good model

sarima.for(homicide_ts,24,0,0,0,1,0,1,10, no.constant = TRUE)


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


sarima(sexual_assault_ts,1,1,1, no.constant = TRUE)
##Significant pvalues.
##ACF plot shows cyclical behavior.
##AIC - -20.13231
##BIC - -20.06225


round(ARMAtoAR(ar = .3378, ma = -.9716, 
               lag.max =  50))
##converges

round(ARMAtoMA(ar = .3378, ma = -.9716, 
               lag.max =  50))
##converges

polyroot(.3378)
polyroot(-.9716)
##model is causal and invertible, no parameter redundancy.


##alt. model 1
sarima(sexual_assault_ts,1,1,1,1,0,0,10, no.constant = TRUE)
##all significant pvalues. plots look good. 
##AIC - -20.21428
##BIC - -20.12086

round(ARMAtoAR(ar = c(.2543,0,0,0,0,0,0,0,0,.344,0.0875), ma = -.9775, 
               lag.max = 50))
##converges

round(ARMAtoMA(ar = c(.2543,0,0,0,0,0,0,0,0,.344,0.0875), ma = -.9775, 
               lag.max = 50))
##converges

polyroot(c(.2543, .344, .0875))
polyroot(-.9775)
##causal/invertable, no parameter redundancy.


##alt model 2
sarima(sexual_assault_ts,1,1,1,1,0,1,10, no.constant = TRUE)
##insignificant ar1 pvalue


##alt model 3
sarima(sexual_assault_ts,0,1,1,1,0,1,10, no.constant = TRUE)
##LB model borderline. All other plots look good. 
##AIC - -20.28843
##BIC - -20.19501

round(ARMAtoAR(ar = c(0,0,0,0,0,0,0,0,0, .9568), ma = c(-.9609,0,0,0,0,0,0,0,0,-.7754,.7451), 
               lag.max = 50))
##converges

round(ARMAtoMA(ar = c(0,0,0,0,0,0,0,0,0, .9568), ma = c(-.9609,0,0,0,0,0,0,0,0,-.7754,.7451), 
               lag.max = 50))
##converges

polyroot(.9568)
polyroot(c(-.9609, -.7754, .7451))
##no parameter redundancy

Box.test(sexual_assault_ts, type = "Ljung-Box")

##all good

sarima.for(sexual_assault_ts,24,0,1,1,1,0,1,10, no.constant = TRUE)





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

sarima(assault_battery_ts,1,0,0)
#ACF anomoly at lag 10. Many outliers on QQ plot. LB plot not good. 
##AIC - -14.9037
##BIC - -14.83418

##alternative 1
sarima(assault_battery_ts,1,0,0,0,0,1,10)
##ACF and QQ plot way off. 
##AIC  - -15.37531
##BIC - -15.28239

#alt 2
sarima(assault_battery_ts,1,0,1,1,0,0,10)
##plots much better. ACF plot shows lag 18 outside ci. Outlier in QQ plot. 
##AIC - -15.81367
##BIC - -15.69752

round(ARMAtoAR(ar = c(.8051,0,0,0,0,0,0,0,0, .8009, .6448), ma = -.5807, 
               lag.max = 50))
##Converges

round(ARMAtoMA(ar = c(.8051,0,0,0,0,0,0,0,0, .8009, .6448), ma = -.5807, 
               lag.max = 50))

##not causal.....  diverges...



#alt 3
sarima(assault_battery_ts,1,1,1,1,0,0,10)
##insignificant constant and ar1 value. 
##ACF plot shows anomoly at lag 18. 
##AIC - -15.75057
##BIC - -15.6338

#alt 4
sarima(assault_battery_ts,0,1,1,1,0,0,10, no.constant = TRUE)
##pvalues significant. ACF plot still shows anomoly.
##AIC - -15.78042
##BIC - -15.71036

round(ARMAtoAR(ar = c(0,0,0,0,0,0,0,0,0, .8046), ma = -.7375, 
               lag.max = 50))
##converges

round(ARMAtoMA(ar = c(0,0,0,0,0,0,0,0,0, .8046), ma = -.7375, 
               lag.max = 50))
##converges

polyroot(-.7375)
polyroot(.8046)
##model causal/invertible, no parameter redundancy

sarima.for(assault_battery_ts,24,0,1,1,1,0,0,10)

adf.test(narcotics_ts
         )
