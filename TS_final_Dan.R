require(memisc)
require(dplyr)
require(reshape)
require(lubridate)
require(forecast)
require(stringr)
accident <- read.csv('/Users/DanLo1108/Downloads/osha_accident.csv')
    
clean <- accident %>% mutate(fatality = ifelse(fatality=='X',1,0)) %>% subset(year(event_date) < 2012 & year(event_date) > 1984) %>%
    mutate(sic=as.numeric(substr(sic_list,1,2)))

clean2 <- clean %>% mutate(industry = cases(
    "Agriculture" = sic < 8,
    "Forestry and Fishing" = sic < 10,
    "Metal Mining" = sic == 10,
    "Coal Mining" = sic == 12,
    "Oil and Gas Extraction" = sic == 13,
    "Other Mining" = sic == 14,
    "Building Construction" = sic == 15,
    "Heavy Construction" = sic == 16,
    "Construction Special Trace Contractors" = sic == 17,
    "Manufacturing" = sic < 40,
    "Transportation" = sic < 48,
    "Communications" = sic == 48,
    "Electric, Gas, and Sanitary Services" = sic == 49,
    "Wholesale Trade" = sic < 52,
    "Retail Trade" = sic < 60,
    "Finance, Insurance, and Real Estate" = sic < 68,
    "Lodging Services" = sic == 70,
    "Business Services" = sic %in% c(72,73,81),
    "Auto and Other Repairs" = sic %in% c(75,76),
    "Entertainment and Recreational Services" = sic %in% c(78, 79, 84),
    "Health Services" = sic == 80,
    "Social Services" = sic == 83,
    "Membership Organizations" = sic == 86,
    "Private Households" = sic == 88,
    "Misc Services" = sic == 89,
    "Public Administration" = sic >= 90,
    "Other" = T
    ))


fatal <- subset(clean2, fatality==1)
non_fatal <- subset(clean2, fatality==0)

fatal.setup <- fatal %>% group_by(year(event_date), month(event_date)) %>%
    summarize(n())
names(fatal.setup) <- c("year", "month","deaths")
fatal.setup$yearmonth <- paste(fatal.setup$year, fatal.setup$month, sep="-")
fatal.setup$date <- as.Date(as.yearmon(fatal.setup$yearmonth))
#fatal.setup$date <- as.Date(fatal.setup$event_date, format="%Y-%m-%d")
date_seq <- seq(from=min(fatal.setup$date),to=max(fatal.setup$date),by="1 month") %>% as.data.frame   
names(date_seq) <- 'date'
fatal.setup <- left_join(date_seq,fatal.setup,by='date') 
fatal.setup[is.na(fatal.setup$deaths),4] <- 0
fatal.ts <- ts(fatal.setup$deaths,frequency=1,start=min(fatal.setup$date))

non_fatal.setup <- non_fatal %>% group_by(year(event_date), month(event_date))  %>%
    summarize(n())
names(non_fatal.setup) <- c("year", "month","injuries")
#non_fatal.setup$date <- as.Date(non_fatal.setup$date, format="%Y-%m-%d")
non_fatal.setup$yearmonth <- paste(non_fatal.setup$year, non_fatal.setup$month, sep="-")
non_fatal.setup$date <- as.Date(as.yearmon(non_fatal.setup$yearmonth)) 
date_seq <- seq(from=min(non_fatal.setup$date),to=max(non_fatal.setup$date),by="1 month") %>% as.data.frame   
names(date_seq) <- 'date'
non_fatal.setup <- left_join(date_seq,non_fatal.setup,by='date') 
non_fatal.setup[is.na(non_fatal.setup$injuries),4] <- 0
non_fatal.ts <- ts(non_fatal.setup$injuries,frequency=1,start=min(non_fatal.setup$date))

total.ts <- fatal.ts + non_fatal.ts

all.ts <- list(fatal.ts,non_fatal.ts,total.ts)



write.table(data.frame(total.ts),'/Users/DanLo1108/Downloads/total.csv',sep=",")


###Fatal###
ts <- ts(fatal.ts)
plot(ts)
acf(ts)
#There is clear seasonality here (period 12)


#Create train/test set
ts_train <- ts(ts[1:288])
ts_test <- ts(ts[(length(ts_train)+1):length(ts)])

plot(ts_train)
plot(ts_test)

t <- seq(1,length(ts_train))
t2 <- t^2
t3 <- t^3
logt <- log(t)
p <- 12
sint <- sin(2*pi*t/p)
cost <- cos(2*pi*t/p)
sqrtt <- sqrt(t)
regression <- lm(ts_train~t+t2+t3+sint+cost)
summary(regression)

plot(ts_train)
lines(regression$fit,col=2,lwd=2)

resid <- regression$residuals

#Check for stationarity of residuals
adf.test(resid) #Stationary at alpha = 0.1
pp.test(resid)


#Check if residuals are plausibly white noise (Box-Pierce)
#library(stats)
Box.test(resid,type=c("Box-Pierce"))

#Check acf and pacf plots of residuals
acf(resid)
pacf(resid)


#Get time series of residuals
#library(forecast)
auto.arima(resid,max.p=5,max.q=5,trace='TRUE')


#Create sarima model based on results of auto.arima
model <- arima(resid,order=c(2,0,3))
ts_forecasts <- c(forecast(model,12)$mean)

#Function that forecasts time ahead in data
time_forecast <- function(time,regression){
  forecasts <- c()
  coeff <- regression$coefficients
  for(i in 1:length(time)){
    u_t <- coeff[[1]]+coeff[[2]]*time[i]+coeff[[3]]*time[i]^2+coeff[[4]]*time[i]^3+coeff[[5]]*sin(2*pi*time[i]/12)+coeff[[6]]*cos(2*pi*time[i]/12)
    forecasts <- append(forecasts, u_t)
  }
  forecasts 
}

#Define number of forecast steps
f_steps <- 12
t_start <- length(ts_train)
time <- seq(t_start+1,t_start+f_steps)

ut_forecasts <- time_forecast(time,regression)

forecasts <- ts_forecasts+ut_forecasts
test <- ts_test[1:12]

plot(ts)
lines(time,forecasts,col='red')

accuracy.gts(ts(forecasts),ts(test))





###NEXT 12###



#Create train/test set
ts_train <- ts(ts[1:300])
ts_test <- ts(ts[(length(ts_train)+1):length(ts)])

t <- seq(1,length(ts_train))
t2 <- t^2
t3 <- t^3
p <- 12
sint <- sin(2*pi*t/p)
cost <- cos(2*pi*t/p)
regression <- lm(ts_train~t+t2+t3+sint+cost)
summary(regression)

plot(ts_train)
lines(regression$fit,col=2,lwd=2)

resid <- regression$residuals

#Check for stationarity of residuals
adf.test(resid) #Stationary at alpha = 0.1
pp.test(resid)


#Check if residuals are plausibly white noise (Box-Pierce)
#library(stats)
Box.test(resid,type=c("Box-Pierce"))

#Check acf and pacf plots of residuals
acf(resid)
pacf(resid)


#Get time series of residuals
#library(forecast)
#auto.arima(resid,max.p=5,max.q=5,trace='TRUE')


#Create sarima model based on results of auto.arima
model <- arima(resid,order=c(2,0,3))
ts_forecasts <- c(forecast(model,12)$mean)


#Define number of forecast steps
f_steps <- 12
t_start <- length(ts_train)
time <- seq(t_start+1,t_start+f_steps)

ut_forecasts <- time_forecast(time,regression)

forecasts <- ts_forecasts+ut_forecasts
test <- ts_test[1:12]

plot(ts)
lines(time,forecasts,col='red')

accuracy.gts(ts(forecasts),ts(test))





###LAST 12###

#Create train/test set
ts_train <- ts(ts[1:312])
ts_test <- ts(ts[(length(ts_train)+1):length(ts)])

t <- seq(1,length(ts_train))
t2 <- t^2
t3 <- t^3
p <- 12
sint <- sin(2*pi*t/p)
cost <- cos(2*pi*t/p)
regression <- lm(ts_train~t+t2+t3+sint+cost)
summary(regression)

plot(ts_train)
lines(regression$fit,col=2,lwd=2)

resid <- regression$residuals

#Check for stationarity of residuals
adf.test(resid) #Stationary at alpha = 0.1
pp.test(resid)


#Check if residuals are plausibly white noise (Box-Pierce)
#library(stats)
Box.test(resid,type=c("Box-Pierce"))

#Check acf and pacf plots of residuals
acf(resid)
pacf(resid)


#Get time series of residuals
#library(forecast)
#auto.arima(resid,max.p=5,max.q=5,trace='TRUE')


#Create sarima model based on results of auto.arima
model <- arima(resid,order=c(2,0,3))
ts_forecasts <- c(forecast(model,12)$mean)


#Define number of forecast steps
f_steps <- 12
t_start <- length(ts_train)
time <- seq(t_start+1,t_start+f_steps)

ut_forecasts <- time_forecast(time,regression)

forecasts <- ts_forecasts+ut_forecasts
test <- ts_test[1:12]

plot(ts)
lines(time,forecasts,col='red')

accuracy.gts(ts(forecasts),ts(test))




