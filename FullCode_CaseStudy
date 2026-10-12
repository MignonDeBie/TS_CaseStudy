load("C:/Users/mmyde/Downloads/citibike.RData")
Sys.setlocale("LC_TIME", "English")

# DATA EXPLORATION

#Compute the total demand 
data_citibike$total_demand <- data_citibike$`area 1` + data_citibike$`area 2` + data_citibike$`area 3` +
                              data_citibike$`area 4` + data_citibike$`area 5` + data_citibike$`area 6` 
#Dataset its called data_citibike

#Create the column date
data_citibike$date_hour <- as.POSIXct(paste(data_citibike$year, data_citibike$month, data_citibike$day, data_citibike$hour), 
                                 format="%Y %m %d %H", tz = "America/New_York")
data_citibike$date_day<- as.Date(paste(data_citibike$year, data_citibike$month, data_citibike$day), 
                                      format="%Y %m %d")
data_citibike$date_month<- as.Date(paste(data_citibike$year, data_citibike$month, "01"), 
                                 format="%Y %m %d")
#Day of the week 
data_citibike$week_day <- weekdays(data_citibike$date_day)
#to have the correct order of the week

data_citibike$week_day <- factor(data_citibike$week_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


head(data_citibike[c( "year","month", "day", "hour", "date_hour","date_day", "date_month")])

library(ggplot2)


#Plot demand in area 1: (we dont actually need this I think)

#By month:
ggplot(data_citibike, aes(x = month, y = `area 1`)) +
  # Get the demand per month 
  stat_summary(fun = mean, geom = "line", color = "darkblue", size = 1.2) +
  labs(title = "Demand zone 1 by month", x = "Month", y = "Demand") +
  theme_minimal()

#By day:
ggplot(data_citibike, aes(x = date_day, y = `area 1`)) +
  # Get the demand per day
  stat_summary(fun = sum, geom = "line", color = "darkblue", size = 0.6) +
  labs(title = "Demand zone 1 by day", x = "date", y = "Demand") +
  theme_minimal()

#By hour:
ggplot(data_citibike, aes(x = date_hour, y = `area 1`)) +
  # Get the demand per hour per day 
  stat_summary(fun = mean, geom = "line", color = "darkblue", size = 0.6) +
  labs(title = "Demand zone 1 by day", x = "date", y = "Demand") +
  theme_minimal()

#By hour:
ggplot(data_citibike, aes(x = hour, y = `area 1`)) +
  # Get the demand per hour
  stat_summary(fun = mean, geom = "line", color = "darkblue", size = 0.6) +
  labs(title = "Demand zone 1 by hour", x = "hour", y = "Demand") +
  theme_minimal()


#Plot total demand:

#By month total_demand: (mean instead of sum bc some months have more days which makes it unfair otherwise)
ggplot(data_citibike, aes(x = month, y = `total_demand`)) +
  # Get the demand per month 
  stat_summary(fun = mean, geom = "line", color = "deeppink3", size = 1.2) +
  labs(title = "Average total demand by month", x = "Month", y = "Average demand") +
  theme_minimal()

#By day total_demand: (each group is one day of exactly 24 hours, so all groups are the same size so use sum)
ggplot(data_citibike, aes(x = date_day, y = `total_demand`)) +
  # Get the demand per day
  stat_summary(fun = sum, geom = "line", color = "deeppink3", size = 1.2) +
  labs(title = "Total demand by day", x = "Day", y = "Demand") +
  theme_minimal()

#By Weekday total_demand: (bc not all days appear evenly in the data we use mean)
ggplot(data_citibike, aes(x = week_day, y = `total_demand`, group = 1)) +
  # Get the demand per day 
  stat_summary(fun = mean, geom = "line", color = "deeppink3", size = 1.2) +
  labs(title = "Average total demand by day of the week", x = "WeekDay", y = "Average demand") +
  theme_minimal()

#By hour total: (use mean to get an average, which is easier to interpret)
ggplot(data_citibike, aes(x = hour, y = `total_demand`, group = 1)) +
  # Get the demand per hour
  stat_summary(fun = mean, geom = "line", color = "deeppink3", size = 1.2) +
  labs(title = "Average total demand per hour ", x = "Hour", y = "Average demand") +
  theme_minimal()


#ZONE 6

#By month for zone 6:
ggplot(data_citibike, aes(x = month, y = `area 6`)) +
  # Get the demand per month 
  stat_summary(fun = mean, geom = "line", color = "springgreen2", size = 1.2) +
  labs(title = "Average demand by month area 6", x = "Month", y = "Average demand") +
  theme_minimal()

#By day zone 6:
ggplot(data_citibike, aes(x = date_day, y = `area 6`)) +
  # Get the demand per day
  stat_summary(fun = sum, geom = "line", color = "springgreen2", size = 1.2) +
  labs(title = "Total demand per day area 6", x = "Day", y = "Demand") +
  theme_minimal()

#By Weekday zone 6:
ggplot(data_citibike, aes(x = week_day, y = `area 6`, group = 1)) +
  # Get the demand per weekday 
  stat_summary(fun = mean, geom = "line", color = "springgreen2", size = 1.2) +
  labs(title = "Average demand by day of the week area 6", x = "WeekDay", y = "Average demand") +
  theme_minimal()

#By hour zone 6:
ggplot(data_citibike, aes(x = hour, y = `area 6`, group = 1)) +
  # Get the demand per hour
  stat_summary(fun = mean, geom = "line", color = "springgreen2", size = 1.2) +
  labs(title = "Average demand per hour area 6", x = "Hour", y = "Average demand") +
  theme_minimal()

#By hour and weekday zone 6: (shows that weekdays and weekends have a different daily pattern)
ggplot(data_citibike, aes(x = hour, y = `area 6`, color = week_day)) +
  # Average demand per hour of the day, one line per weekday
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(title = "Average demand by hour of the day and weekday, area 6",
       x = "Hour", y = "Average demand per hour", color = "Weekday") +
  theme_minimal()



#UNIT ROOT TESTS

train <- data_citibike$month <= 7 # only use april-july for training, save august for testing
log_demand6 <- log(data_citibike$`area 6`[train]) # log-transform the data series

plot(log_demand6, type = "l", main = "Log demand area 6 (April-July)",
     xlab = "Hour", ylab = "log(demand)") # plot log-transformed data

#Manual application of Pantula principle
library(bootUR)

# Step 1: H0 d=2, H1 d=1
diff_area6 <- diff(log_demand6)

# Since first differences remove a linear trend, only a constant  is needed here
test_d2 <- adf(diff_area6, deterministics = "intercept")  # adf instead of adf.test bc that's what bootUR used
print(test_d2)

# Step 2: Test d=1 vs d=0 (Data in levels)
# Since the series show a clear trend, we  include a trend component
test_d1 <- adf(log_demand6, deterministics = "trend")
print(test_d1)

# we reject both since p-value < 0.05 -> the series is I(0), trend-stationary, so shocks are temporary


## these are not necessary I think:
##By hour zone 6 (Log-Transformed):
##ggplot(data_citibike, aes(x = hour, y = log_demand6, group = 1)) +
  ## Get the demand per hour
  ##stat_summary(fun = sum, geom = "line", color = "springgreen2", size = 1.2) +
  ##labs(title = "Total log-demand per hour area 6", x = "Hour", y = "Log(Demand)") +
  ##theme_minimal()

##install.packages(c("colorspace", "forecast", "ggplot2", "tseries"), lib = "C:/R_packages_seguros")
##library(tseries)
##install.packages("bootUR")










#ARIMA MODELS
library(forecast)

# Route i: Correlograms to manually decide how many lags we need

# Correlogram ACF --> to get the MA  q
acf(log_demand6, main = "ACF - Log Demand Area 6", lag.max = 72) # 3 days
# not on differenced bc we've just seen that it's I(0)
# we see a wave shape in the ACF and thus conclude that MA does not fit here

# Correlogram PACF --> to get the AR p
pacf(log_demand6, main = "PACF - Log Demand Area 6", lag.max = 72)
# we see 2 big spikes at lags 1 and 2, so we try AR(1) and AR(2)

# Models chosen from correlograms
ar1 <- arima(log_demand6, order = c(1, 0, 0))  # AR(1)
ar2 <- arima(log_demand6, order = c(2, 0, 0))  # AR(2)

# Test information criteria for both
# the lower the values the better, since that's best balance between good fit 
# to data and not too many parameters
AIC(ar1)  # finds model that predicts best
BIC(ar1)  # finds simpler models

AIC(ar2)
BIC(ar2)

# we find that AR(2) is best here (which is intuitive as well)

# Trying simple ARMA (bc data is quite complex with a lot of significant lags)
# starting from AR(2) since we've found that to be better than AR(1) here
arma21 <- arima(log_demand6, order = c(2, 0, 1))  # ARMA(21)
arma22 <- arima(log_demand6, order = c(2, 0, 2))  # ARMA(22)

AIC(arma21)
BIC(arma21)

AIC(arma22)
BIC(arma22)

#Table with all information criteria for the selected models
ic_table <- data.frame(
  Model = c("AR(1)", "AR(2)", "ARMA(2,1)", "ARMA(2,2)"),
  AIC   = c(AIC(ar1), AIC(ar2), AIC(arma21), AIC(arma22)),
  BIC   = c(BIC(ar1), BIC(ar2), BIC(arma21), BIC(arma22))
)
print(ic_table)

#AR(22) gives best results, so let's try more complex ones
arma23 <- arima(log_demand6, order = c(2, 0, 3))  # ARMA(23)
arma24 <- arima(log_demand6, order = c(2, 0, 4))  # ARMA(24)
arma25 <- arima(log_demand6, order = c(2, 0, 5))  # ARMA(25)
#arma26 <- arima(log_demand6, order = c(2, 0, 6))  # ARMA(26) -> gives error since overparameterised

ic_table2 <- data.frame(
  Model = c("AR(1)", "AR(2)", "ARMA(2,1)", "ARMA(2,2)", "ARMA(2,3)", "ARMA(2,4)", "ARMA(2,5)"),
  AIC   = c(AIC(ar1), AIC(ar2), AIC(arma21), AIC(arma22), AIC(arma23), AIC(arma24), AIC(arma25)),
  BIC   = c(BIC(ar1), BIC(ar2), BIC(arma21), BIC(arma22), BIC(arma23), BIC(arma24), BIC(arma25))
)

print(ic_table2)

# so this suggests that ARMA(2,5) is best here, but let's try automatic selection


# Route ii: Automatic ARIMA selection

#Create the series for 24 hours
serie_demand <-ts(log_demand6, frequency = 24)  #not differenced yet, so auto.arima can choose for itself

auto_model <- auto.arima(serie_demand, seasonal = FALSE)
summary(auto_model)

# this finds ARIMA(5,1,0) to be best, but we can't compare its AIC and BIC to 
# the ones we found with manual selection since here they use differencing

# let's see what happens when we specify d=0 (consistent with our unit root test)
auto_model_d0 <- auto.arima(serie_demand, d = 0, seasonal = FALSE)
summary(auto_model_d0)

# This finds ARIMA(5,0,5) and gives the lowest AIC and BIC yet.
# However, q=5 and p=5 are the maximum possible values which suggests we're
# overparameterising and there's a need to capture seasonality
# (also, p>2 seems weird when looking at the PACF with 2 big peaks only at the beginning)



## Install required packages 
## create new file in disc C
##dir.create("C:/R_packages_seguros", showWarnings = FALSE)

# Install the packages
##install.packages(c("colorspace", "forecast", "ggplot2", "tseries"), lib = "C:/R_packages_seguros")
##library(forecast, lib.loc = "C:/R_packages_seguros")
##library(ggplot2) 
##library(tseries)




#SARIMA MODELS
auto_model_s <- auto.arima(serie_demand, seasonal = TRUE)
summary(auto_model_s)

# this gives a seasonal model with (5,0,1)(2,1,0)[24] since we put frequency=24.
# The AIC and BIC are negative (not a bad thing, the lower the better)

# let's try it manually to see what we get, starting from AR(2) just like we did for ARIMA

# The regular ACF shows large peaks at 24, 48, etc, so there's a strong daily pattern
# and we should use D=1 to compare each hour with the same hour the day before

# let's plot the ACF and PACF after differencing to see what P and Q to pick
sdiff_log_demand6 <- diff(log_demand6, lag = 24)
acf(sdiff_log_demand6,  main = "ACF - seasonally differenced log demand",  lag.max = 72)
pacf(sdiff_log_demand6, main = "PACF - seasonally differenced log demand", lag.max = 72)
# these don't tell us much so let's just try some combinations

sarima1 <- Arima(serie_demand, order = c(2, 0, 0), seasonal = c(1, 1, 0))
sarima2 <- Arima(serie_demand, order = c(2, 0, 0), seasonal = c(2, 1, 0))
sarima3 <- Arima(serie_demand, order = c(2, 0, 0), seasonal = c(0, 1, 1))
sarima4 <- Arima(serie_demand, order = c(2, 0, 0), seasonal = c(0, 1, 2))
sarima5 <- Arima(serie_demand, order = c(2, 0, 0), seasonal = c(1, 1, 1))
sarima6 <- Arima(serie_demand, order = c(2, 0, 0), seasonal = c(2, 1, 1))
sarima7 <- Arima(serie_demand, order = c(2, 0, 0), seasonal = c(1, 1, 2))
sarima8 <- Arima(serie_demand, order = c(2, 0, 0), seasonal = c(2, 1, 2))

sarima_table <- data.frame(
  Model = c("auto: SARIMA(5,0,1)(2,1,0)",
            "SARIMA(2,0,0)(1,1,0)", "SARIMA(2,0,0)(2,1,0)",
            "SARIMA(2,0,0)(0,1,1)", "SARIMA(2,0,0)(0,1,2)",
            "SARIMA(2,0,0)(1,1,1)", "SARIMA(2,0,0)(2,1,1)",
            "SARIMA(2,0,0)(1,1,2)", "SARIMA(2,0,0)(2,1,2)"),
  AIC   = c(AIC(auto_model_s), AIC(sarima1), AIC(sarima2), AIC(sarima3), AIC(sarima4),
            AIC(sarima5), AIC(sarima6), AIC(sarima7), AIC(sarima8)),
  BIC   = c(BIC(auto_model_s), BIC(sarima1), BIC(sarima2), BIC(sarima3), BIC(sarima4),
            BIC(sarima5), BIC(sarima6), BIC(sarima7), BIC(sarima8))
)
print(sarima_table)

# so SARIMA(2,0,0)(2,1,2) clearly gives the best AIC and BIC

# combine auto.arima's non-seasonal part with the best found seasonal part
sarima9 <- Arima(serie_demand, order = c(5, 0, 1), seasonal = c(2, 1, 2))
sarima10 <- Arima(serie_demand, order = c(5, 0, 1), seasonal = c(3, 1, 2))
sarima11 <- Arima(serie_demand, order = c(5, 0, 1), seasonal = c(2, 1, 3))
#sarima12 <- Arima(serie_demand, order = c(5, 0, 1), seasonal = c(3, 1, 3))
# sarima12 could not be estimated, again likely overparameterisation since 
# parameters partly cancel each other out, so the likelihood surface is too 
# flat/unstable to find a clear optimum

sarima_table2 <- data.frame(
  Model = c("auto: SARIMA(5,0,1)(2,1,0)",
            "SARIMA(2,0,0)(1,1,0)", "SARIMA(2,0,0)(2,1,0)",
            "SARIMA(2,0,0)(0,1,1)", "SARIMA(2,0,0)(0,1,2)",
            "SARIMA(2,0,0)(1,1,1)", "SARIMA(2,0,0)(2,1,1)",
            "SARIMA(2,0,0)(1,1,2)", "SARIMA(2,0,0)(2,1,2)",
            "SARIMA(5,0,1)(2,1,2)", "SARIMA(5,0,1)(3,1,2)",
            "SARIMA(5,0,1)(2,1,3)"),
  AIC   = c(AIC(auto_model_s), AIC(sarima1), AIC(sarima2), AIC(sarima3), AIC(sarima4),
            AIC(sarima5), AIC(sarima6), AIC(sarima7), AIC(sarima8),
            AIC(sarima9), AIC(sarima10), AIC(sarima11)),
  BIC   = c(BIC(auto_model_s), BIC(sarima1), BIC(sarima2), BIC(sarima3), BIC(sarima4),
            BIC(sarima5), BIC(sarima6), BIC(sarima7), BIC(sarima8),
            BIC(sarima9), BIC(sarima10), BIC(sarima11))
)
print(sarima_table2)

summary(sarima9)  #best model: lowest AIC/BIC, all coefficients significant
summary(sarima10) #unreliable: NaN standard errors
summary(sarima11) #unreliable: NaN standard errors

# so we have found that (5,0,1)(2,1,2) is the best model, auto.arima likely got
# stuck in a local optimum and never found the seasonal MA part

#https://www.geeksforgeeks.org/r-language/sarima-in-r/


# EXPONENTIAL SMOOTHING MODELS
library(forecast)

time_series <- ts(log_demand6, frequency = 24) 
model_hw <- hw(time_series, h = 168, seasonal = "additive")
summary(model_hw)

# 1) Additive errors
ets_ANN  <- ets(serie_demand, model = "ANN", damped = FALSE)                   # SES
ets_ANA  <- ets(serie_demand, model = "ANA", damped = FALSE)
#ets_ANM  <- ets(serie_demand, model = "ANM") #forbidden combo
ets_AAN  <- ets(serie_demand, model = "AAN", damped = FALSE)                   # Holt
ets_AAA  <- ets(serie_demand, model = "AAA", damped = FALSE)                   # additive Holt-Winters
#ets_AAM  <- ets(serie_demand, model = "AAM", damped = FALSE) #forbidden combo
ets_AAdN <- ets(serie_demand, model = "AAN", damped = TRUE)
ets_AAdA <- ets(serie_demand, model = "AAA", damped = TRUE)
#ets_AAdM <- ets(serie_demand, model = "AAM", damped = TRUE) #forbidden combo
#ets_AMN  <- ets(serie_demand, model = "AMN", damped = FALSE, allow.multiplicative.trend = TRUE) #forbidden combo           
#ets_AMA  <- ets(serie_demand, model = "AMA", damped = FALSE, allow.multiplicative.trend = TRUE) #forbidden combo             
#ets_AMM  <- ets(serie_demand, model = "AMM", damped = FALSE, allow.multiplicative.trend = TRUE) #forbidden combo              
#ets_AMdN <- ets(serie_demand, model = "AMN", damped = TRUE, allow.multiplicative.trend = TRUE) #forbidden combo
#ets_AMdA <- ets(serie_demand, model = "AMA", damped = TRUE, allow.multiplicative.trend = TRUE) #forbidden combo
#ets_AMdM <- ets(serie_demand, model = "AMM", damped = TRUE, allow.multiplicative.trend = TRUE) #forbidden combo 

# 2) Multiplicative errors
ets_MNN  <- ets(serie_demand, model = "MNN", damped = FALSE)
ets_MNA  <- ets(serie_demand, model = "MNA", damped = FALSE)
ets_MNM  <- ets(serie_demand, model = "MNM", damped = FALSE)
ets_MAN  <- ets(serie_demand, model = "MAN", damped = FALSE)
ets_MAA  <- ets(serie_demand, model = "MAA", damped = FALSE)
ets_MAM  <- ets(serie_demand, model = "MAM", damped = FALSE)                   # multiplicative Holt-Winters
ets_MAdN <- ets(serie_demand, model = "MAN", damped = TRUE)
ets_MAdA <- ets(serie_demand, model = "MAA", damped = TRUE)
ets_MAdM <- ets(serie_demand, model = "MAM", damped = TRUE)
ets_MMN  <- ets(serie_demand, model = "MMN", damped = FALSE, allow.multiplicative.trend = TRUE)
#ets_MMA  <- ets(serie_demand, model = "MMA", damped = FALSE, allow.multiplicative.trend = TRUE) #forbidden combo            
ets_MMM  <- ets(serie_demand, model = "MMM", damped = FALSE, allow.multiplicative.trend = TRUE)
ets_MMdN <- ets(serie_demand, model = "MMN", damped = TRUE, allow.multiplicative.trend = TRUE)
#ets_MMdA <- ets(serie_demand, model = "MMA", damped = TRUE, allow.multiplicative.trend = TRUE) #forbidden combo
ets_MMdM <- ets(serie_demand, model = "MMM", damped = TRUE, allow.multiplicative.trend = TRUE)

# Automatic selection
ets_auto <- ets(serie_demand, allow.multiplicative.trend = TRUE) #otherwise it doesn't consider M trend
summary(ets_auto)

##ets_auto2 <- ets(model_hw, allow.multiplicative.trend = TRUE) #otherwise it doesn't consider M trend
##summary(ets_auto2)
summary(ets_MAM)

ets_table <- data.frame(
  Model = c("ETS(A,N,N)", "ETS(A,N,A)", "ETS(A,A,N)", "ETS(A,A,A)", "ETS(A,Ad,N)", "ETS(A,Ad,A)",
            "ETS(M,N,N)", "ETS(M,N,A)", "ETS(M,N,M)", "ETS(M,A,N)", "ETS(M,A,A)", "ETS(M,A,M)",
            "ETS(M,Ad,N)", "ETS(M,Ad,A)", "ETS(M,Ad,M)", "ETS(M,M,N)", "ETS(M,M,M)",
            "ETS(M,Md,N)", "ETS(M,Md,M)", "auto"),
  AIC   = c(AIC(ets_ANN), AIC(ets_ANA), AIC(ets_AAN), AIC(ets_AAA), AIC(ets_AAdN), AIC(ets_AAdA),
            AIC(ets_MNN), AIC(ets_MNA), AIC(ets_MNM), AIC(ets_MAN), AIC(ets_MAA), AIC(ets_MAM),
            AIC(ets_MAdN), AIC(ets_MAdA), AIC(ets_MAdM), AIC(ets_MMN), AIC(ets_MMM),
            AIC(ets_MMdN), AIC(ets_MMdM), AIC(ets_auto)),
  BIC   = c(BIC(ets_ANN), BIC(ets_ANA), BIC(ets_AAN), BIC(ets_AAA), BIC(ets_AAdN), BIC(ets_AAdA),
            BIC(ets_MNN), BIC(ets_MNA), BIC(ets_MNM), BIC(ets_MAN), BIC(ets_MAA), BIC(ets_MAM),
            BIC(ets_MAdN), BIC(ets_MAdA), BIC(ets_MAdM), BIC(ets_MMN), BIC(ets_MMM),
            BIC(ets_MMdN), BIC(ets_MMdM), BIC(ets_auto))
)
ets_table[order(ets_table$AIC), ]   # sorted with best model on top

# so we get that additive Holt-Winters aka AAA (additive error, additive trend, 
# additive seasonality has the lowest AIC and is thus the best model here. 
# It was found best through both the automatic and the manual way.

summary(ets_auto)          # chosen model + estimated alpha, beta, gamma + AIC/BIC
plot(ets_auto)             # level, slope and seasonal components
checkresiduals(ets_auto)   # are the residuals white noise?



# IN-SAMPLE MODEL COMPARISON

# AIC/BIC: only comparable within a model class with the same differencing
# For in-sample comparison across classes we need accuracy()
# And it's just in-sample so we don't include august yet

insample_table <- rbind(
  "ARIMA: ARMA(2,5)"            = accuracy(arma25),
  "ARIMA auto (d=0): ARMA(5,5)" = accuracy(auto_model_d0),
  "SARIMA auto: (5,0,1)(2,1,0)" = accuracy(auto_model_s),
  "SARIMA: (5,0,1)(2,1,2)"      = accuracy(sarima9),
  "ETS(A,A,A)"                  = accuracy(ets_auto)
)
rownames(insample_table) <- c("ARIMA: ARMA(2,5)", "ARIMA auto (d=0): ARMA(5,5)",
                              "SARIMA auto: (5,0,1)(2,1,0)", "SARIMA: (5,0,1)(2,1,2)",
                              "ETS(A,A,A)")
round(insample_table, 4)




# QUICK TEST WITH 168H INSTEAD OF 24H

serie_demand_WEEK <-ts(log_demand6, frequency = 168)  #not differenced yet, so auto.arima can choose for itself

# let's plot the ACF and PACF after differencing to see what P and Q to pick
sdiff_log_demand6_WEEK <- diff(log_demand6, lag = 168)
acf(sdiff_log_demand6_WEEK,  main = "ACF - seasonally differenced log demand WEEK",  lag.max = 336)
# --> clear peak at lag 1 and lag 168 (so every week) --> seasonal MA term Q=1
pacf(sdiff_log_demand6_WEEK, main = "PACF - seasonally differenced log demand WEEK", lag.max = 336)
# --> clear peak at lag 1 and lag 168 (so every week) --> seasonal AR term P=1

auto_model_s_WEEK <- auto.arima(serie_demand_WEEK, seasonal = TRUE)
summary(auto_model_s_WEEK)
# --> gives ARIMA(2,0,0)(0,1,0)

sarima9_WEEK <- Arima(serie_demand_WEEK, order = c(5, 0, 1), seasonal = c(2, 1, 2))
# doesnt work here, can't even find initial value

#lets try others based on auto.arima giving ARIMA(2,0,0)(0,1,0)
sarima_WEEK2 <- Arima(serie_demand_WEEK, order = c(2, 0, 0), seasonal = c(1, 1, 0)) #error
sarima_WEEK3 <- Arima(serie_demand_WEEK, order = c(2, 0, 0), seasonal = c(0, 1, 1))
sarima_WEEK4 <- Arima(serie_demand_WEEK, order = c(2, 0, 0), seasonal = c(1, 1, 1)) #error

summary(sarima9)        #best sarima we found for 24H (5,0,1)(2,1,2)
summary(sarima_WEEK3)   #best sarima we found for 168H (2,0,0)(0,1,1)
# --> thus with a weekly season instead of daily we find a much simpler sarima,
# and the lower RMSE tells us that it fits better

checkresiduals(sarima9) # oh wait we need to change the frequency
ggAcf(residuals(sarima9), lag.max = 336) # clear peaks at 168 and 336
checkresiduals(sarima_WEEK3) 
ggAcf(residuals(sarima_WEEK3), lag.max = 336) # big peaks are gone
# so now the weekly pattern is gone from the residuals, yay!

# updating the in-sample table
insample_table2 <- rbind(
  "ARIMA: ARMA(2,5)"            = accuracy(arma25),
  "ARIMA auto (d=0): ARMA(5,5)" = accuracy(auto_model_d0),
  "SARIMA auto: (5,0,1)(2,1,0)[24]" = accuracy(auto_model_s),
  "SARIMA: (5,0,1)(2,1,2)[24]"      = accuracy(sarima9),
  "SARIMA: (2,0,0)(0,1,1)[168]"        = accuracy(sarima_WEEK3),
  "ETS(A,A,A)"                  = accuracy(ets_AAA)
)
# we keep ETS at 24 since it cannot handle higher values
rownames(insample_table2) <- c("ARIMA: ARMA(2,5)", "ARIMA auto (d=0): ARMA(5,5)",
                              "SARIMA auto: (5,0,1)(2,1,0)[24]", "SARIMA: (5,0,1)(2,1,2)[24]", 
                              "SARIMA: (2,0,0)(0,1,1)[168]", "ETS(A,A,A)")
round(insample_table2, 4)
# clearly our new found SARIMA: (2,0,0)(0,1,1)[168] fits the data best







# OUT-OF-SAMPLE FORECAST EXERCISE

# Data
y_all   <- log(data_citibike$`area 6`)        # log demand, all 3672 hours
n_train <- sum(data_citibike$month <= 7)      # 2928 hours (april-july)
n_test  <- length(y_all) - n_train            # 744 hours (august)
h       <- 24                                 # forecast horizon: the next day

origins <- seq(0, n_test - h, by = h)         # 0, 24, ..., 720: hours into august where each forecast starts
n_orig  <- length(origins)                    # 31 forecast days
# to speed up, just 1 forecast per 24 hours (but can still see for every hour so h=1,...,24)
#n_orig <- 2 #for quick test


# Matrices to store the forecasts and actual values
actuals <- matrix(NA, nrow = n_orig, ncol = h)   # rows = days, columns = hours

fc_roll_arma    <- matrix(NA, nrow = n_orig, ncol = h)
fc_roll_arma55  <- matrix(NA, nrow = n_orig, ncol = h)
fc_roll_sarauto <- matrix(NA, nrow = n_orig, ncol = h)
fc_roll_sar24   <- matrix(NA, nrow = n_orig, ncol = h)
fc_roll_sar168  <- matrix(NA, nrow = n_orig, ncol = h)
fc_roll_ets     <- matrix(NA, nrow = n_orig, ncol = h)
fc_roll_snaive  <- matrix(NA, nrow = n_orig, ncol = h)

fc_exp_arma     <- matrix(NA, nrow = n_orig, ncol = h)
fc_exp_arma55   <- matrix(NA, nrow = n_orig, ncol = h)
fc_exp_sarauto  <- matrix(NA, nrow = n_orig, ncol = h)
fc_exp_sar24    <- matrix(NA, nrow = n_orig, ncol = h)
fc_exp_sar168   <- matrix(NA, nrow = n_orig, ncol = h)
fc_exp_ets      <- matrix(NA, nrow = n_orig, ncol = h)
fc_exp_snaive   <- matrix(NA, nrow = n_orig, ncol = h)



### Rolling window
for (i in 1:n_orig) {
  o <- origins[i]
  
  # training window: the most recent 2928 hours before the forecast day
  train_window <- y_all[(1 + o):(n_train + o)]
  
  train_24  <- ts(train_window, frequency = 24)
  train_168 <- ts(train_window, frequency = 168)
  
  # re-estimating once a week to save computation time (and adding 1 day of data
  # to 2900 hours you already have shouldnt make the biggest difference on the parameters)
  reestimate <- (i %% 7 == 1)
  #reestimate <- TRUE
  
  #updating parameters to better fit the new data
  if (reestimate) {
    fit_arma   <- Arima(train_24,  order = c(2, 0, 5))
    fit_arma55 <- Arima(train_24,  order = c(5, 0, 5))                    
    fit_sarauto <- Arima(train_24, order = c(5, 0, 1), seasonal = c(2, 1, 0))
    fit_sar24  <- Arima(train_24,  order = c(5, 0, 1), seasonal = c(2, 1, 2))
    fit_sar168 <- Arima(train_168, order = c(2, 0, 0), seasonal = c(0, 1, 1))
    fit_ets    <- ets(train_24, model = "AAA", damped = FALSE)
  } else {
    fit_arma   <- Arima(train_24,  model = fit_arma)     # same parameters (as previous day), new data
    fit_arma55 <- Arima(train_24,  model = fit_arma55)
    fit_sarauto <- Arima(train_24, model = fit_sarauto)
    fit_sar24  <- Arima(train_24,  model = fit_sar24)
    fit_sar168 <- Arima(train_168, model = fit_sar168)
    fit_ets    <- ets(train_24, model = fit_ets)
  }
  
  # forecast the next 24 hours with each model
  fc_roll_arma[i, ]   <- forecast(fit_arma,   h = h)$mean   #only extract mean which gives point estimate we want
  fc_roll_arma55[i, ] <- forecast(fit_arma55, h = h)$mean
  fc_roll_sarauto[i, ] <- forecast(fit_sarauto, h = h)$mean
  fc_roll_sar24[i, ]  <- forecast(fit_sar24,  h = h)$mean
  fc_roll_sar168[i, ] <- forecast(fit_sar168, h = h)$mean
  fc_roll_ets[i, ]    <- forecast(fit_ets,    h = h)$mean
  fc_roll_snaive[i, ] <- snaive(train_168, h = h)$mean   # benchmark: same hour last week (nothing to estimate)
  
  # the actual values of those 24 hours
  actuals[i, ] <- y_all[(n_train + o + 1):(n_train + o + h)]
  
  cat("rolling: day", i, "of", n_orig, "\n")   # progress
}





### Expanding window
for (i in 1:n_orig) {
  o <- origins[i]
  
  # training window: everything from 1 April up to the forecast day
  train_window <- y_all[1:(n_train + o)]
  
  train_24  <- ts(train_window, frequency = 24)
  train_168 <- ts(train_window, frequency = 168)
  
  # re-estimate once a week
  reestimate <- (i %% 7 == 1)
  
  #re-estimate everyday
  #reestimate <- TRUE
  
  if (reestimate) {
    fit_arma   <- Arima(train_24,  order = c(2, 0, 5))
    fit_arma55 <- Arima(train_24,  order = c(5, 0, 5))                      
    fit_sarauto <- Arima(train_24, order = c(5, 0, 1), seasonal = c(2, 1, 0)) 
    fit_sar24  <- Arima(train_24,  order = c(5, 0, 1), seasonal = c(2, 1, 2))
    fit_sar168 <- Arima(train_168, order = c(2, 0, 0), seasonal = c(0, 1, 1))
    fit_ets    <- ets(train_24, model = "AAA", damped = FALSE)
  } else {
    fit_arma   <- Arima(train_24,  model = fit_arma)
    fit_arma55 <- Arima(train_24,  model = fit_arma55)
    fit_sarauto <- Arima(train_24, model = fit_sarauto)
    fit_sar24  <- Arima(train_24,  model = fit_sar24)
    fit_sar168 <- Arima(train_168, model = fit_sar168)
    fit_ets    <- ets(train_24, model = fit_ets)
  }
  
  fc_exp_arma[i, ]   <- forecast(fit_arma,   h = h)$mean
  fc_exp_arma55[i, ] <- forecast(fit_arma55, h = h)$mean
  fc_exp_sarauto[i, ] <- forecast(fit_sarauto, h = h)$mean
  fc_exp_sar24[i, ]  <- forecast(fit_sar24,  h = h)$mean
  fc_exp_sar168[i, ] <- forecast(fit_sar168, h = h)$mean
  fc_exp_ets[i, ]    <- forecast(fit_ets,    h = h)$mean
  fc_exp_snaive[i, ] <- snaive(train_168, h = h)$mean   # benchmark: same hour last week
  
  cat("expanding: day", i, "of", n_orig, "\n")
}

# save everything so we don;t have to run this again
#save(actuals, list = ls(pattern = "^fc_"), file = "oos_forecasts_1h.RData")




### Forecast error metrics
error_metrics <- function(fc) {
  c(RMSE = sqrt(mean((actuals - fc)^2)),
    MAE  = mean(abs(actuals - fc)),
    MAPE = mean(abs(exp(actuals) - exp(fc)) / exp(actuals)) * 100)
}

oos_table <- rbind(
  "ARMA(2,5) - rolling"                   = error_metrics(fc_roll_arma),
  "ARMA(5,5) auto - rolling"              = error_metrics(fc_roll_arma55),
  "SARIMA auto (5,0,1)(2,1,0)[24] - rolling" = error_metrics(fc_roll_sarauto),
  "SARIMA(5,0,1)(2,1,2)[24] - rolling"    = error_metrics(fc_roll_sar24),
  "SARIMA(2,0,0)(0,1,1)[168] - rolling"   = error_metrics(fc_roll_sar168),
  "ETS(A,A,A) - rolling"                  = error_metrics(fc_roll_ets),
  "Seasonal naive (week) - rolling"       = error_metrics(fc_roll_snaive),
  "ARMA(2,5) - expanding"                 = error_metrics(fc_exp_arma),
  "ARMA(5,5) auto - expanding"            = error_metrics(fc_exp_arma55),
  "SARIMA auto (5,0,1)(2,1,0)[24] - expanding" = error_metrics(fc_exp_sarauto),
  "SARIMA(5,0,1)(2,1,2)[24] - expanding"  = error_metrics(fc_exp_sar24),
  "SARIMA(2,0,0)(0,1,1)[168] - expanding" = error_metrics(fc_exp_sar168),
  "ETS(A,A,A) - expanding"                = error_metrics(fc_exp_ets),
  "Seasonal naive (week) - expanding"     = error_metrics(fc_exp_snaive)
)
round(oos_table, 4)


#quick test to make sure weekly updating of paramters is really okay
fit_d1 <- Arima(ts(y_all[1:n_train], frequency = 168),
                order = c(2,0,0), seasonal = c(0,1,1))
fit_d8 <- Arima(ts(y_all[(1 + 168):(n_train + 168)], frequency = 168),
                order = c(2,0,0), seasonal = c(0,1,1))
round(rbind(day1 = coef(fit_d1), day8 = coef(fit_d8)), 3)
# --> 1 week of new data changes the SARIMA[168] parameters by less than their
# standard errors so daily re-estimation would barely change the forecasts
# we expect other models to give similar results


#rerun after restart including new RMSE and MAE
load("~/Econometrics/TimeSeries/code/oos_forecasts_1h.RData")
error_metrics <- function(fc) {
  c(RMSE       = sqrt(mean((actuals - fc)^2)), #log
    MAE        = mean(abs(actuals - fc)), #log
    MAPE       = mean(abs((exp(actuals) - exp(fc))) / exp(actuals)) * 100, #percentage error in bikes
    RMSE_bikes = sqrt(mean((exp(actuals) - exp(fc))^2)),  #in bikes (exp to go from log to bikes)
    MAE_bikes  = mean(abs(exp(actuals) - exp(fc)))) # in bikes
}



oos_table2 <- rbind(
  "ARMA(2,5) - rolling"                   = error_metrics(fc_roll_arma),
  "ARMA(5,5) auto - rolling"              = error_metrics(fc_roll_arma55),
  "SARIMA auto (5,0,1)(2,1,0)[24] - rolling" = error_metrics(fc_roll_sarauto),
  "SARIMA(5,0,1)(2,1,2)[24] - rolling"    = error_metrics(fc_roll_sar24),
  "SARIMA(2,0,0)(0,1,1)[168] - rolling"   = error_metrics(fc_roll_sar168),
  "ETS(A,A,A) - rolling"                  = error_metrics(fc_roll_ets),
  "Seasonal naive (week) - rolling"       = error_metrics(fc_roll_snaive),
  "ARMA(2,5) - expanding"                 = error_metrics(fc_exp_arma),
  "ARMA(5,5) auto - expanding"            = error_metrics(fc_exp_arma55),
  "SARIMA auto (5,0,1)(2,1,0)[24] - expanding" = error_metrics(fc_exp_sarauto),
  "SARIMA(5,0,1)(2,1,2)[24] - expanding"  = error_metrics(fc_exp_sar24),
  "SARIMA(2,0,0)(0,1,1)[168] - expanding" = error_metrics(fc_exp_sar168),
  "ETS(A,A,A) - expanding"                = error_metrics(fc_exp_ets),
  "Seasonal naive (week) - expanding"     = error_metrics(fc_exp_snaive)
)
round(oos_table2, 4)

save(actuals, oos_table2, list = ls(pattern = "^fc_"),
     file = "~/Econometrics/TimeSeries/code/oos_results2.RData")




# DIEBOLD-MARIANO TEST AND MODEL CONFIDENCE SET

# get one long  hourly error vector per model
#rolling
e_sar168  <- as.vector(t(exp(actuals) - exp(fc_roll_sar168)))
e_sar24   <- as.vector(t(exp(actuals) - exp(fc_roll_sar24)))
e_sarauto <- as.vector(t(exp(actuals) - exp(fc_roll_sarauto)))
e_ets     <- as.vector(t(exp(actuals) - exp(fc_roll_ets)))
e_arma    <- as.vector(t(exp(actuals) - exp(fc_roll_arma)))
e_arma55  <- as.vector(t(exp(actuals) - exp(fc_roll_arma55)))
e_snaive  <- as.vector(t(exp(actuals) - exp(fc_roll_snaive)))

#expanding
x_sar168  <- as.vector(t(exp(actuals) - exp(fc_exp_sar168)))
x_sar24   <- as.vector(t(exp(actuals) - exp(fc_exp_sar24)))
x_sarauto <- as.vector(t(exp(actuals) - exp(fc_exp_sarauto)))
x_ets     <- as.vector(t(exp(actuals) - exp(fc_exp_ets)))
x_arma    <- as.vector(t(exp(actuals) - exp(fc_exp_arma)))
x_arma55  <- as.vector(t(exp(actuals) - exp(fc_exp_arma55)))
x_snaive  <- as.vector(t(exp(actuals) - exp(fc_exp_snaive)))


#dm test (best model against all others)
#rolling RMSE
dm.test(e_sar168, e_sar24,   h = 24, power = 2) 
#power=2 since squared for RMSE (1 would have been absolute for MAE) and we decided RMSE is the most logical one here
dm.test(e_sar168, e_sarauto, h = 24, power = 2)
dm.test(e_sar168, e_ets,     h = 24, power = 2)
dm.test(e_sar168, e_arma,    h = 24, power = 2)
dm.test(e_sar168, e_arma55,  h = 24, power = 2)
dm.test(e_sar168, e_snaive,  h = 24, power = 2)
#dm.test(e_sar168, e_snaive, h = 24, power = 1)   #since RMSE and MAE disagreed on these

#expanding RMSE
dm.test(x_sar168, x_sar24,   h = 24, power = 2)
dm.test(x_sar168, x_sarauto, h = 24, power = 2)
dm.test(x_sar168, x_ets,     h = 24, power = 2)
dm.test(x_sar168, x_arma,    h = 24, power = 2)
dm.test(x_sar168, x_arma55,  h = 24, power = 2)
dm.test(x_sar168, x_snaive,  h = 24, power = 2)

#rolling MAE
dm.test(e_sar168, e_sar24,   h = 24, power = 1)
dm.test(e_sar168, e_sarauto, h = 24, power = 1)
dm.test(e_sar168, e_ets,     h = 24, power = 1)
dm.test(e_sar168, e_arma,    h = 24, power = 1)
dm.test(e_sar168, e_arma55,  h = 24, power = 1)
dm.test(e_sar168, e_snaive,  h = 24, power = 1)

#expanding MAE
dm.test(x_sar168, x_sar24,   h = 24, power = 1)
dm.test(x_sar168, x_sarauto, h = 24, power = 1)
dm.test(x_sar168, x_ets,     h = 24, power = 1)
dm.test(x_sar168, x_arma,    h = 24, power = 1)
dm.test(x_sar168, x_arma55,  h = 24, power = 1)
dm.test(x_sar168, x_snaive,  h = 24, power = 1)

#mcs
#install.packages("MCS")
library(MCS)

#loss matrix (rows=hours, columns=models)
loss_roll_rmse <- cbind(SARIMA168  = e_sar168^2,
                      SARIMA24   = e_sar24^2,
                      SARIMAauto = e_sarauto^2,
                      ETS        = e_ets^2,
                      ARMA25     = e_arma^2,
                      ARMA55     = e_arma55^2,
                      Naive_week = e_snaive^2)

loss_exp_rmse <- cbind(SARIMA168  = x_sar168^2,
                     SARIMA24   = x_sar24^2,
                     SARIMAauto = x_sarauto^2,
                     ETS        = x_ets^2,
                     ARMA25     = x_arma^2,
                     ARMA55     = x_arma55^2,
                     Naive_week = x_snaive^2)

loss_roll_mae <- cbind(SARIMA168  = abs(e_sar168),
                       SARIMA24   = abs(e_sar24),
                       SARIMAauto = abs(e_sarauto),
                       ETS        = abs(e_ets),
                       ARMA25     = abs(e_arma),
                       ARMA55     = abs(e_arma55),
                       Naive_week = abs(e_snaive))

loss_exp_mae <- cbind(SARIMA168  = abs(x_sar168),
                      SARIMA24   = abs(x_sar24),
                      SARIMAauto = abs(x_sarauto),
                      ETS        = abs(x_ets),
                      ARMA25     = abs(x_arma),
                      ARMA55     = abs(x_arma55),
                      Naive_week = abs(x_snaive))


set.seed(1110);MCS_roll_rmse_90  <- MCSprocedure(Loss = loss_roll_rmse,  alpha = 0.10, B = 5000, statistic = "Tmax")
set.seed(1110);MCS_roll_rmse_75  <- MCSprocedure(Loss = loss_roll_rmse,  alpha = 0.25, B = 5000, statistic = "Tmax")
set.seed(1110);MCS_exp_rmse_90   <- MCSprocedure(Loss = loss_exp_rmse,   alpha = 0.10, B = 5000, statistic = "Tmax")
set.seed(1110);MCS_exp_rmse_75   <- MCSprocedure(Loss = loss_exp_rmse,   alpha = 0.25, B = 5000, statistic = "Tmax")
set.seed(1110);MCS_roll_mae_90 <- MCSprocedure(Loss = loss_roll_mae, alpha = 0.10, B = 5000, statistic = "Tmax")
set.seed(1110);MCS_roll_mae_75 <- MCSprocedure(Loss = loss_roll_mae, alpha = 0.25, B = 5000, statistic = "Tmax")
set.seed(1110);MCS_exp_mae_90  <- MCSprocedure(Loss = loss_exp_mae,  alpha = 0.10, B = 5000, statistic = "Tmax")
set.seed(1110);MCS_exp_mae_75  <- MCSprocedure(Loss = loss_exp_mae,  alpha = 0.25, B = 5000, statistic = "Tmax")
#alpha values based on the paper

MCS_roll_rmse_90
MCS_roll_rmse_75
MCS_exp_rmse_90
MCS_exp_rmse_75
MCS_roll_mae_90
MCS_roll_mae_75
MCS_exp_mae_90
MCS_exp_mae_75




### TIME SERIES PART OF CROSS-SECTIONAL HIERARCHY

# putting all zones in a table
Y_original <- data.frame(
  y1 = data_citibike$`area 1`,
  y2 = data_citibike$`area 2`,
  y3 = data_citibike$`area 3`,
  y4 = data_citibike$`area 4`,
  y5 = data_citibike$`area 5`,
  y6 = data_citibike$`area 6`
)

Y_original$y7 <- rowSums(Y_original[, c("y1", "y2", "y3", "y4")])
Y_original$y8 <- rowSums(Y_original[, c("y5", "y6")])
Y_original$y9 <- Y_original$y7 + Y_original$y8

head(Y_original)


#training and testing data
train   <- data_citibike$month <= 7
test    <- data_citibike$month == 8
n_train <- sum(train)                      #april-july
n_test  <- sum(test)                       #august
h       <- 24                              #forecast the next day
origins <- seq(0, n_test - h, by = h)      # 0, 24, ..., 720: 31 forecast days
n_orig  <- length(origins)
#n_orig <- 2   # quick test


# Store forecasts and actual values on the original scale (rows = hours of August, columns = series)
forecasts_h <- matrix(
  NA_real_, nrow = n_test, ncol = 9,
  dimnames = list(NULL, names(Y_original))
)

actuals_h <- as.matrix(Y_original[test, ])

# in-sample residuals in bikes (rows = training hours, columns = series), needed for WLS and MinT
res_h <- matrix(NA_real_, nrow = n_train, ncol = 9, dimnames = list(NULL, names(Y_original)))


# Use rolling window approach since computationally cheaper and we've seen that
# rolling and expanding give very similar results

for (i in seq_along(Y_original)) {
  
  series_name <- names(Y_original)[i]
  
  # Log-transform the series
  # log1p = log(1 + y): areas 2 and 3 have hours with 0 bikes, and log(0) = -Inf
  log_series <- log1p(Y_original[[series_name]])
  
  for (d in 1:n_orig) {
    o <- origins[d]
    
    # rolling window: the last 2928 hours before the forecast day, weekly season
    train_window <- ts(log_series[(1 + o):(n_train + o)], frequency = 168)
    
    # re-estimate once a week (day 1, 8, 15, 22, 29), otherwise reuse the parameters
    #if (d %% 7 == 1) {
    if (d == 1) {   # estimate once (on April-July), then only update the data each day
      fit <- Arima(train_window, order = c(2, 0, 0), seasonal = c(0, 1, 1))
      # residuals on the training data, in bikes: actual - fitted
      res_h[, i] <- Y_original[train, series_name] - pmax(0, expm1(fitted(fit)))
    } else {
      fit <- Arima(train_window, model = fit)
    }
    
    fc <- forecast(fit, h = h)
    
    # Transform back to the original scale: z = log(1 + y)  ->  y = e^z - 1
    # pmax to avoid negative demand
    forecasts_h[(o + 1):(o + h), i] <- pmax(0, expm1(as.numeric(fc$mean)))
  }
  
  print(paste("Completed:", series_name))
}

# save so the loop never has to run again
save(forecasts_h, actuals_h, res_h, file = "~/Econometrics/TimeSeries/code/base_forecasts.RData")

load("~/Econometrics/TimeSeries/code/base_forecasts.RData")

# Check the results
head(forecasts_h)
head(actuals_h)


# Check hierarchical coherence of the independent forecasts
coherence_check <- data.frame(
  Zone7_difference =
    forecasts_h[, "y7"] -
    rowSums(forecasts_h[, c("y1", "y2", "y3", "y4")]),
  
  Zone8_difference =
    forecasts_h[, "y8"] -
    rowSums(forecasts_h[, c("y5", "y6")]),
  
  Total_difference =
    forecasts_h[, "y9"] -
    forecasts_h[, "y7"] -
    forecasts_h[, "y8"]
)

head(coherence_check)
summary(coherence_check)

# Check coherence of actual demand (should be exactly 0 since the data add up by construction)
actual_coherence <- data.frame(
  Zone7_difference =
    actuals_h[, "y7"] -
    rowSums(actuals_h[, c("y1", "y2", "y3", "y4")]),
  
  Zone8_difference =
    actuals_h[, "y8"] -
    rowSums(actuals_h[, c("y5", "y6")]),
  
  Total_difference =
    actuals_h[, "y9"] -
    actuals_h[, "y7"] -
    actuals_h[, "y8"]
)

head(actual_coherence)    #yes perfect, it's all 0s
summary(actual_coherence)





### RECONCILIATION
#install.packages("FoReco")
library(FoReco)

# Aggregation matrix: how the upper series are built from the six areas
agg_mat <- rbind(
  y7 = c(1, 1, 1, 1, 0, 0),
  y8 = c(0, 0, 0, 0, 1, 1),
  y9 = c(1, 1, 1, 1, 1, 1)
)
colnames(agg_mat) <- paste0("y", 1:6) #to make the names y1, y2, etc.

# Function to check the coherence
check_coherence <- function(fc) {
  data.frame(
    Zone7_difference = fc[, "y7"] - rowSums(fc[, c("y1", "y2", "y3", "y4")]),
    Zone8_difference = fc[, "y8"] - rowSums(fc[, c("y5", "y6")]),
    Total_difference = fc[, "y9"] - fc[, "y7"] - fc[, "y8"]
  )
}


### Classical 1: bottom-up

# forecast only the six areas, and add them up for the zones and NYC
forecasts_bu <- csbu(
  base = forecasts_h[, paste0("y", 1:6)],
  agg_mat = agg_mat
)
head(forecasts_bu)
summary(check_coherence(forecasts_bu))


### Classical 2: top-down

# forecast only NYC, and split it over the six areas
# using each area's average share of total demand in the training data (april-july)
shares <- colSums(Y_original[train, paste0("y", 1:6)]) / sum(Y_original[train, "y9"])
shares

forecasts_td <- cstd(
  base = forecasts_h[, "y9"],
  agg_mat = agg_mat,
  weights = shares
)
head(forecasts_td)
summary(check_coherence(forecasts_td))





### Regression-based 1: OLS

# Reorder the base forecasts: upper-level series first, then bottom-level series
forecasts_ordered <- forecasts_h[, c("y7", "y8", "y9", "y1", "y2", "y3", "y4", "y5", "y6")]

# OLS: adjust all 9 base forecasts as little as possible (equal weights) so that they add up
forecasts_ols <- csrec(
  base = forecasts_ordered,
  agg_mat = agg_mat,
  comb = "ols"
)
summary(check_coherence(forecasts_ols))
head(forecasts_ols)


### Regression-based 2: WLS (structural scaling) 
# (weights each series by how many areas it contains)
forecasts_str <- csrec(
  base = forecasts_ordered,
  agg_mat = agg_mat,
  comb = "str"
)
summary(check_coherence(forecasts_str))


### Regression-based 3: WLS (variance scaling)
# weights each series by the variance of its in-sample forecast errors
# residuals in the same order as the base forecasts (upper levels first)
res_ordered <- res_h[, c("y7", "y8", "y9", "y1", "y2", "y3", "y4", "y5", "y6")]

forecasts_wls <- csrec(
  base = forecasts_ordered,
  agg_mat = agg_mat,
  comb = "wls",
  res = res_ordered
)
summary(check_coherence(forecasts_wls))


### Regression-based 4: MinT (shrinkage)
# uses the variances AND the correlations between the series' forecast errors
# (shrinkage makes the estimated covariance matrix more stable)
forecasts_shr <- csrec(
  base = forecasts_ordered,
  agg_mat = agg_mat,
  comb = "shr",
  res = res_ordered
)
summary(check_coherence(forecasts_shr))


### forecast accuracy of base vs reconciled

# put every method in the same column order as actuals_h (y1, ..., y9)
cols <- colnames(actuals_h)

rmse_by_series <- function(fc) sqrt(colMeans((actuals_h - fc[, cols])^2))

accuracy_table <- rbind(
  Base        = rmse_by_series(forecasts_h),
  Bottom_up   = rmse_by_series(forecasts_bu),
  Top_down    = rmse_by_series(forecasts_td),
  OLS         = rmse_by_series(forecasts_ols),
  Structural  = rmse_by_series(forecasts_str),
  WLS_var     = rmse_by_series(forecasts_wls),
  MinT_shr    = rmse_by_series(forecasts_shr)
)
round(accuracy_table, 1)   # RMSE in bikes, per series (columns) and method (rows)


