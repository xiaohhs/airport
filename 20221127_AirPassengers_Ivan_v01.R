# Analoog aan:
# http://rstudio-pubs-static.s3.amazonaws.com/311446_08b00d63cc794e158b1f4763eb70d43a.html
# en Beginning TSA (Pluralsight)

#==========================================================
# Installatie van packages                             ####
#==========================================================
#
#install.packages("tidyverse")
#install.packages("forecast")
#install.packages("tseries")
#==========================================================



#==========================================================
# Laden van libraries                                  ####
#==========================================================

library(tidyverse)    # Zwitsers zakmes
library(forecast)     # Bevat functies tbv time series analysis
library(tseries)      # Bevat functies tbv time series analysis
library(ggplot2)      # O.m. voor autoplot


#==========================================================
# Inlezen dataset                                      ####
#==========================================================

# Data afkomstig uit standaard R-base

data("AirPassengers")
ap <- AirPassengers


#==========================================================
# Exploring and preparing the data                     ####
#==========================================================

head(ap)
str (ap)    # betreft TS-data/class
glimpse (ap)
summary (ap)

any(is.na(ap))   # geen NA's

ap

frequency(ap) #betreft jaarlijkse data



#==========================================================
# Visualise the data                                   ####
#==========================================================

plot (ap, ylab = "Aantal passagiers (per duizend)",
      main = "AirPassenger van 1949 t/m 1960")
# Te zien is:
# Stijgende trend (stijgend gemiddelde)
# Seasonality effect, met steeds groter wordende amplitude
# Pieken in de zomermaanden  
# Pieken worden steeds groter (variantie groeit)
# Stationarity: constant mean and variance
# Het lijkt erop dat er sprake is van: 
#   - Lineaire trend
#   - Steeds groter wordende variantie, dus multiplicative model voor decomposition
#   - Geen stationary dataset
# Transformatie nodig voordat verder gewerkt kan worden met de dataset 

## If the seasonal variation is constant, you should use an additive model. 
## And if the time series increases in magnitude, you should use an multiplicative model.


# Seasonplot; beter om verschil per jaar te zien
ggseasonplot(ap, year.labels = TRUE, 
             ylab = "Aantal passagiers (per duizend)",
             main = "Seasonplot AirPassenger van 1949 t/m 1960")



#==========================================================
# Decomposition of the data                            ####
#==========================================================

#plot(log(ap)) 
decompose_ap <- decompose(ap,"multiplicative") 
plot(decompose_ap)
autoplot (decompose_ap) # om de verschillen te zien ;-) geeft beter beeld van R


## Eerdere beeld wordt bevestigd door decompositie



#==========================================================
# Testing for stationarity                             ####
#==========================================================

# Data is stationary als:
#   - Constant mean
#   - Constant variance
#   - No autocorrelation
#
# Als niet stationary, dan:
#   - Transformation (bijv log transformation)
#   - Differencing (same statistics throughout dataset)
#   - De-trending (Time series decomposition)


#### Stationary test (ADF, Augmented Dickey-Fuller)

# Unit root test 'Augmented Dickey-Fuller' test (par 2.3)
#   removes autocorrelation and tests for stationarity
#   Als p < 0,05, dan sprake van stationarity

adf.test (ap)   # nu p-value =0.01, dus stationarity(!), doordat ADF autocorrelation verwijdert


#### Stationary test (Autocorrelation)

# acf toont autocorrelation 
#     acf -> correlation coefficient between lags of time series
# pacf toont partial autocorrelation
#     pacf -> correlation coeff adjusted for shorter lags


# ETH pag 70
acf(ap, lag.max = 48, main = "Autocorrelation AirPassenger van 1949 t/m 1960")
# Waarnemingen buiten de 95% betrouwbaarheidsintervallen, 
#     dus er is sprake van Autocorrelation bij deze waarnemingen
# "Again, this is an indication for a non-stationary series. 
# It needs to be decomposed, before the serial correlation in 
# the stationary remainder term can be studied."


# Bepalen van autocorrelatie van restwaarden R
# NA's worden omzeild door selectie van 7 tm 138 
decompose_ap$random
acf(decompose_ap$random[7:138], main = "Autocorrelation decomposed AirPassenger van 1949 t/m 1960")
# Hier is dus minder sprake van waarnemingen buiten 95% betrouwbaarheidsinterval
#   en dus minder sprake van autocorrelatie




#==========================================================
# Fitten van time series model                         ####
#==========================================================

# 1. Lineair model
#Trendlijn wordt in blauw geplot

autoplot(ap) + 
    geom_smooth(method="lm") + 
    labs(x ="Jaar", y = "Aantal passagiers (per duizend)", 
         title="AirPassenger van 1949 t/m 1960") 

# 2. ARIMA-model
my_arima <- auto.arima(ap)
my_arima

# ARIMA-fitted model is:
#   Y (hat) = 0.5960Y (t−20) + 0.2143Y (t−12) − 0.9819e (t−1) + E

## Ljung-Box test 
## Tests whether there is any significant autocorrelation in a series; EHT pag 68
Box.test(ap, lag=20, type="Ljung-Box")  #pag 68
# X-squared = 1434.1, df = 20, p-value < 2.2e-16
# x^2 waarschijnlijk veel groter dan 95% betr interv
# p-waarde ligt dichtbij nul, dus nul-hypothese verwerpen? OPZOEKEN!



#==========================================================
# Forecastinf                                          ####
#==========================================================
#RNGversion("3.5.2")
#set.seed(12345)
# http://rstudio-pubs-static.s3.amazonaws.com/311446_08b00d63cc794e158b1f4763eb70d43a.html
# 95% confidence interval where h is the forecast horizon periods in months

forecast_ap <- forecast(my_arima, level = c(95), h = 36)
autoplot(forecast_ap)

summary (forecast_ap)
# in Dec 1963: 
#   voorspelling 534.3262
#   min_95%: 451.1089 
#   max_95%: 617.5435


