## SPRINT 2 - AIR PASSENGERS DATA ##


# Packages installeren en inladen
install.packages("ggfortify") # onderdeel van ggplot2; werkt met timeseries i.p.v. dataframes.
install.packages("tseries")
install.packages("forecast")
install.packages("vars")
install.packages("fpp2")      # gevonden via R.J. Hyndman: Time Series Analysis.

library("ggfortify")
library("tseries")
library("forecast")
library("vars")
library("fpp2")
library("dplyr")
library("ggplot2")
library("imputeTS")


# Documentatie van de nieuwe libraries
?ggfortify
?forecast
?vars


# Data inladen
data("AirPassengers")
TS <- AirPassengers   # wordt direct ingeladen als type 'timeseries'


# EDA
TS

TS %>% frequency( )   # geeft 12 terug, m.a.w. 12x per jaar = timeseries in maanden.
TS %>% cycle()
TS %>% deltat() * 365
           
TS %>% mean()
TS %>% summary()

TS %>% ts.eps()

# Visuals
TS %>% autoplot()

boxplot(TS~cycle(TS),
        xlab = "Maand",
        ylab = "Spreiding van passagiers",
        main = "Spreiding van aantal passagiers per maand",
        col = "darkred")

# Data ontleden via de decompose function

#### Check: wanneer additive, wanneer multiplicative? ####

# Vanuit Google: "We can usually identify an additive or multiplicative time series 
#                 from its variation. If the magnitude of the seasonal component changes 
#                 with time, then the series is multiplicative. Otherwise, the series is additive"

# De vraag is dan: hoe lezen we uit de data af of de "magnitude of the season component changes"?

DecomposeAdd <- decompose(TS, type = c("additive"))
DecomposeMulti <- decompose(TS, type = c("multiplicative"))

autoplot(DecomposeAdd)
autoplot(DecomposeMulti)

autoplot(DecomposeAdd$seasonal)
autoplot(DecomposeMulti$seasonal)


# Stationarity tests
#### Wie kan deze uitkomsten goed duiden? Heb hier nog moeite mee! ####
adf.test(TS)

autoplot(acf(TS, plot = FALSE)) +
  labs(title = "Correlogram van aantal maandelijkse passagiers tussen 1949 en 1961")

DecomposeMulti$random

autoplot(acf(na.remove(DecomposeMulti$random), plot = FALSE)) +
  labs(title = "Correlogram van aantal maandelijkse passagiers tussen 1949 en 1961")

# Passagiers per jaar met trendlijn
autoplot(TS) + geom_smooth(method = "lm") + labs(
  x = "Jaar",
  y = "Aantal passagiers",
  title = "Aantal passagiers per jaar",
  subtitle = "Inclusief trendlijn") +

  
# Auto Arima
ArimaTS <- auto.arima(TS, approximation = FALSE, trace = TRUE)
ArimaTS

autoplot(ArimaTS)
ggtsdiag(ArimaTS)


# Auto Forecasting
ForecastTS <- forecast(ArimaTS, level = 95)

autoplot(ForecastTS)  + geom_smooth(method = "lm") + labs(
  x = "Jaar",
  y = "Aantal passagiers",
  title = "Aantal passagiers per jaar",
  subtitle = "Met forecast voor opvolgende jaren (95% confidence)")

# Forecast o.b.v. laatste 5 jaar (60 maanden)
autoplot(ForecastTS, include = 60, level = 95) + geom_smooth(method = "lm") + 
  labs(
  x = "Jaar",
  y = "Aantal passagiers",
  title = "Aantal passagiers per jaar",
  subtitle = "Met forecast voor opvolgende jaren (95% confidence)")    

# Forecast o.b.v. laatste 2 jaar (24 maanden)
autoplot(ForecastTS, include = 24, level = 95) + geom_smooth(method = "lm") + 
  labs(
  x = "Jaar",
  y = "Aantal passagiers",
  title = "Aantal passagiers per jaar",
  subtitle = "Met forecast voor opvolgende jaren (95% confidence)")

