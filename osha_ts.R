require(memisc)
require(dplyr)
require(reshape)
require(lubridate)
require(forecast)
require(stringr)
accident <- read.csv('osha_accident.csv')

clean <- accident %>% mutate(fatality = ifelse(fatality=='X',1,0)) %>% subset(year(event_date) < 2012 & year(event_date) > 1984) %>%
  mutate(sic=as.numeric(substr(sic_list,1,2)))



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
fatalV <- fatal.setup$deaths

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
non_fatalAll <- non_fatal.setup$injuries
total.ts <- fatal.ts + non_fatal.ts
totalV <- fatalV + non_fatalV
all.ts <- list(fatal.ts,non_fatal.ts,total.ts)



findArima <- function(n1, n2, dset){
  dset <- dset[n1:n2]
  t <- 1:length(dset)
  tSQ <- t^2 
  tCB <- t^3 
  cosT = cos((2*pi*t)/12)
  sinT = sin((2*pi*t)/12) 
  Reg <- lm(dset ~ t+tSQ+tCB+cosT+sinT)
  Resid <- dset - Reg$fitted.values
  auto.arima(Resid, seasonal=TRUE, stationary=FALSE)
}



#FULL: ARIMA(1, 0, 3;)
#0-225: ARIMA(0, 0, 0)
#0-150: ARIMA(3, 0, 0)
#100-300: ARIMA(2, 0, 3)

fatal0225 <- findArima(0, 225, fatalAll)
fatal0150 <- findArima(0,150, fatalAll)
fatal100300 <- findArima(100, 300, fatalAll)


#FULL: ARIMA(1, 0, 1)
#0-225 ARIMA(1, 0, 1)
#0-150 ARIMA(1, 0, 1)
#100-300: ARIMA(0, 0, 0)
nonFatal0225 <- findArima(0, 225, non_fatalAll)
nonFatal0150 <- findArima(0, 150, non_fatalAll)
nonFatal100300 <- findArima(100, 300, non_fatalAll)

reproduceModelPredict <- function(autoArima, dset, start, end){
  DS <- dset[start, end]
  p <- autoArima$arma[1]
  q <- autoArima$arma[2]
  d <- autoArima$arma[6]
  newModel <- arima(DS, order=c(p, d, q))
  forecast(newModel, 1)$mean
  
}
hwFatal <- HoltWinters(ts(fatalAll, frequency=12))
hwNonFatal <- HoltWinters(ts(non_fatalAll, frequency=12))
hwNonFatal <- HoltWinters(ts(non_fatalAll, frequency=12))
plot(hwFatal)
plot(hwNonFatal)
  