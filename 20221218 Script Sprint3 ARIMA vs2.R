# INLADEN PACKAGES
library(readr)
library(tidyverse)
library(lubridate)
library(forecast)

# INLADEN DATABESTAND
setwd("C:/Users/jcklo/Documents/2021/Studie/Modules/Applied Big data")
# setwd("C:/Users/Ivan/Documents/SynologyDrive/202108-THGS-BigData/202209-Applied BigData/_Repo-Airlines/airport")

Datasetruw  <- read.csv2("DataFiles/cleandataR.csv", stringsAsFactors = TRUE,
                         na.strings=c("", "NA"))


#==========================================================
# Passagiers per Dag                                   ####
#==========================================================
#### EDA 
Datasetruw
summary(Datasetruw)
glimpse(Datasetruw) # 679,582 rijen, 26 kolommen


# Selecteren twee belangrijkste kolommen: datum en passagies
Dagpax <- Datasetruw [c('Actuele.datum.tijd','Pay.pax')]
Dagpax
summary(Dagpax)  #352892 NA's


# NA's verwijderen uit Pay.pax
Dagpax <- Dagpax %>% filter(!is.na(Dagpax$Pay.pax))
Dagpax
summary(Dagpax) # Geen NA's meer


# Omzetten datum naar datumtype (Lubridate)
Dagpax$Actuele.datum.tijd <- mdy_hm(Dagpax$Actuele.datum.tijd)
summary(Dagpax)
head(Dagpax)


# Selecteer alleen datum
Dagpax$Actuele.datum.tijd <- date(Dagpax$Actuele.datum.tijd)
head(Dagpax)


# Tel aantal passagiers per dag
Dagpax <- Dagpax %>% group_by(Actuele.datum.tijd) %>% 
  summarize(PassagiersPerDag = sum(Pay.pax))
Dagpax
summary(Dagpax$PassagiersPerDag)

plot(Dagpax$PassagiersPerDag) # Dipje aan eind van plot, enkele outliers
boxplot(Dagpax$PassagiersPerDag) # Outliers

### Visual ts totaledataset
Dagpax.ts <- ts(Dagpax$PassagiersPerDag, start = c(2003,212), frequency = 365)
plot(Dagpax.ts, xlab="Jaren", ylab="Aantal passagiers", main="Aantal passagiers 30-7-03 t/m 31-8-21")
###

#####################################################
#### Sprint 3                                    ####
#####################################################

### Omzetten naar Time Series
# Selecie data: van 1-8-2003 tm 31-8-2019
Dagpax1 <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01')
Dagpax2 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2019-08-31')
Dagpax3 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2019-06-30')
# Dagpax3 wordt de input; we gaan juli en aug 2019 voorspellen

summary (Dagpax1) #min 1-8-2003, max 31-08-2021
summary (Dagpax2) #min 1-8-2003, max 31-08-2019
summary (Dagpax3) #min 1-8-2003, max 30-06-2019

#### Passagiers per dag als Time Serie (Dagpax3)
dp3 <- ts (Dagpax3$PassagiersPerDag, start = c(2003,212), frequency = 365)
# Deze dag moet ook goed staat, heeft invloed op forecast begindatum
# als je werkt met 212 dan begint forecast op 2019, 179 en eind 238 (dp3)

autoplot (dp3) 
dc3 <- decompose(dp3)
autoplot(dc3) # Warning message: Removed 728 rows containing missing values (`geom_line()`), maar geen gevolgen voor nu

## opmerkingen: stijgende trend, seasonality, remainder is geen white noise,
# maar wordt langzaam steeds groter, dus dataset is niet random en dusnog niet geschikt
# om obv hiervan te voorspellen

# Testing stationarity middels acf en pacf
autoplot(acf(dp3,plot=FALSE))+ labs(title="Correlogram Passagiers") 
autoplot(pacf(dp3,plot=FALSE))+ labs(title="Correlogram Passagiers")

#==========================================================
# Fitten van time series model                         ####
#==========================================================

# ARIMA-model
my_arima <- auto.arima(dp3)
my_arima

# Output:
# Series: dp3 
# ARIMA(5,1,2)(0,1,0)[365] 
# Arima p,d,q maal seasonal Arima p,d,q [frequency]

# Coefficients:
#   ar1      ar2     ar3     ar4     ar5      ma1      ma2
# -0.9737  -0.2164  0.1890  0.3908  0.4775  -0.1070  -0.8231
# s.e.   0.0145   0.0216  0.0224  0.0209  0.0137   0.0084   0.0074

# sigma^2 = 564255:  log likelihood = -43750.89
# AIC=87517.79   AICc=87517.81   BIC=87570.6

# ARIMA-fitted model is: NOG DOEN; INVULLEN EN VERKLAREN
#   Y (hat) = xxx0.5960Y (t−20) + xxx0.2143Y (t−12) − xxx0.9819e (t−1) + E

# NOG DOEN; VERKLARING LJUNG-BOX TEST
## Ljung-Box test 
## Tests whether there is any significant autocorrelation in a series; EHT pag 68
Box.test(dp3, lag=20, type="Ljung-Box")  #pag 68
# Output:
# X-squared = 75523, df = 20, p-value < 2.2e-16

# x^2 waarschijnlijk veel groter dan 95% betr interv
# p-waarde ligt dichtbij nul, dus nul-hypothese verwerpen? 
# NOG DOEN; UITZOEKEN/VERKLAREN

#==========================================================
# Forecastinf                                          ####
#==========================================================
#RNGversion("3.5.2")
#set.seed(12345)
# http://rstudio-pubs-static.s3.amazonaws.com/311446_08b00d63cc794e158b1f4763eb70d43a.html
# 95% confidence interval where h is the forecast horizon periods in months

forecast_dp3 <- forecast(my_arima, level = c(95), h = 60) # dit geeft voorspelling 60 dagen in toekomst
autoplot(forecast_dp3)
summary (forecast_dp3)

# Output:
#Forecast method: ARIMA(5,1,2)(0,1,0)[365]

# Model Information:
# Series: dp3 
# ARIMA(5,1,2)(0,1,0)[365] 

##Coefficients:
##  ar1      ar2     ar3     ar4     ar5      ma1             ma2
##-0.9737  -0.2164  0.1890  0.3908  0.4775  -0.1070          -0.8231    
##s.e.   0.0145   0.0216  0.0224  0.0209  0.0137   0.0084    0.0074

##sigma^2 = 564255:  log likelihood = -43750.89
##AIC=87517.79   AICc=87517.81   BIC=87570.6

##Error measures:
##                  ME     RMSE     MAE       MPE     MAPE      MASE      ACF1
##Training set -2.994689 726.6437 526.398 -40.26478 54.62597  0.6407867 0.1963871


forecast_dp3$mean    
# Voorspelling volgende 60 dagen (h).
#Time Series: Start = c(2019, 179)  End = c(238); begin is dag 28-6-19 , eind is dag 26-8-19
# Lijkt erop 1-1-19 plus 179 dat het 29-6-19 is,
# maar als je 1-1-19  plus 10 doet is dat 11-1-19, terwijl de 10e dag 10-1-19 is dus 1 minder 
# Idem voor eind: lijkt erop 1-1-19 plus 238 dat het 27-8-19
# # maar als je 1-1-19  plus 10 doet is dat 11-1-19, terwijl de 10e dag 10-1-19 is dus 1 minder 

voorsp_60 <- as.data.frame(forecast_dp3$mean)  #voorspellingen van TS naar DF
head(voorsp_60)
# Afronden
voorsp_60round <- round(voorsp_60) %>%
  rename(Forecast = x)
head(voorsp_60round)

# DF werkelijk en voorspellingen samenvoegen en nieuwe kolom om verschil te laten zien
Dagpax_werk <- Dagpax2 %>%
  filter(Actuele.datum.tijd >= "2019-06-28" & Actuele.datum.tijd <= "2019-08-26")

Dagpax_forecast <- cbind(Dagpax_werk, voorsp_60round) %>%
  mutate(verschil = (PassagiersPerDag - Forecast))
head(Dagpax_forecast)
summary(Dagpax_forecast$verschil)
Dagpax_forecast

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-2318.0  -289.2   376.0   302.8  1012.8  2129.0 

# Gemiddelde (absolute) aantallen per dag
mean(abs(Dagpax_forecast$verschil)) #832

#==========================================================
# Einde Sprint3                                        ####
#==========================================================