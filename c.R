##creating control models
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


## correct order control model
theft_control_model <- sarima(theft_control_ts,0,1,1,1,0,0,10, no.constant = TRUE)

##correct
narcotics_control_model <- sarima(narcotics_control_ts, 0,1,1,1,0,1,10, no.constant = TRUE)


##correct
sass_control_model <- sarima(sass_control_ts, 0,1,1,1,0,1,10, no.constant = TRUE)

##correct
homicide_control_model <- sarima(homicide_control_ts, 0,1,0,1,0,1,10, no.constant = TRUE)

##correct
bass_control_model <- sarima(bass_control_ts, 0,1,1,1,0,0,10, no.constant = TRUE)

##forecasts of control
theft_control_for <- sarima.for(theft_control_ts,24,0,1,1,1,0,0,10, no.constant = TRUE)
narcotics_control_for <- sarima.for(narcotics_control_ts,24, 0,1,1,1,0,1,10, no.constant = TRUE)
homicide_control_for <- sarima.for(homicide_control_ts, 24,0,1,0,1,0,1,10, no.constant = TRUE)
?
theft_forecast <- sarima.for(theft_ts,24,0,1,1,1,0,0,10, no.constant = TRUE)
homicide_forecast <- sarima.for(homicide_ts,24, 0,1,0,1,0,1,10, no.constant = TRUE)
tsplot(homicide_ts)
