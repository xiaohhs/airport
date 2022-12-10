# INLADEN PACKAGES
library(readr)
library(tidyverse)
library(lubridate)
library(forecast)


# INLADEN DATABESTAND
#setwd("C:/Users/jcklo/Documents/2021/Studie/Modules/Applied Big data")
setwd("C:/Users/Ivan/Documents/SynologyDrive/202108-THGS-BigData/202209-Applied BigData/_Repo-Airlines/airport")
  
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


#####################################################
#### Sprint 3                                    ####
#####################################################

### Omzetten naar Time Series
# Selecie data: van 1-8-2003 tm 31-8-2019
Dagpax1 <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01')
Dagpax2 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2019-08-31')
Dagpax3 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2019-06-30') # dit wordt input; we gaan juli en aug 2019 voorspellen

summary (Dagpax1) #min 1-8-2003, max 31-08-2021
summary (Dagpax2) #min 1-8-2003, max 31-08-2019
summary (Dagpax3) #min 1-8-2003, max 30-06-2019

#### Passagiers per dag als Time Serie (Dagpax3)
dp3 <- ts (Dagpax3$PassagiersPerDag, start = c(2003,8), frequency = 365)
autoplot (dp3)
dc3 <- decompose(dp3)
autoplot(dc3) # Decompose van Dagpax3
## opmerkingen: stijgende trend, seasonality, remainder is geen white noise, maar wordt langzaam steeds groter, dus ...?


#==========================================================
# Fitten van time series model                         ####
#==========================================================

# 1. Lineair model
#Trendlijn wordt in blauw geplot

autoplot(dp3) + 
  geom_smooth(method="lm") + 
  labs(x ="Jaar", y = "Aantal passagiers", 
       title="Trendlijn Passagiers RTH-Airport") 

# 2. ARIMA-model
my_arima <- auto.arima(dp3)
my_arima

# ARIMA-fitted model is: *** nog in te vullen!!! ***
#   Y (hat) = xxx0.5960Y (t−20) + xxx0.2143Y (t−12) − xxx0.9819e (t−1) + E

## Ljung-Box test 
## Tests whether there is any significant autocorrelation in a series; EHT pag 68
Box.test(dp3, lag=20, type="Ljung-Box")  #pag 68
# X-squared = 75523, df = 20, p-value < 2.2e-16
# x^2 waarschijnlijk veel groter dan 95% betr interv
# p-waarde ligt dichtbij nul, dus nul-hypothese verwerpen? OPZOEKEN!



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

#### RESULTAAT
##Forecast method: ARIMA(5,1,2)(0,1,0)[365]

##Model Information:
##  Series: dp3 
##ARIMA(5,1,2)(0,1,0)[365] 

##Coefficients:
##  ar1      ar2     ar3     ar4     ar5      ma1
##-0.9737  -0.2164  0.1890  0.3908  0.4775  -0.1070
##s.e.   0.0145   0.0216  0.0224  0.0209  0.0137   0.0084
##ma2
##-0.8231
##s.e.   0.0074

##sigma^2 = 564255:  log likelihood = -43750.89
##AIC=87517.79   AICc=87517.81   BIC=87570.6

##Error measures:
##  ME     RMSE     MAE       MPE     MAPE
##Training set -2.994689 726.6437 526.398 -40.26478 54.62597
##MASE      ACF1
##Training set 0.6407867 0.1963871

# Voorspelling volgende 60 dagen (h).
#Time Series: Start = c(2018, 340)  End = c(2019, 34);  
# (lijkt vanaf de 340e dag in 2018 = 7 dec 2018 ipv 1/7/2019!(?)
forecast_dp3$mean    


voorsp_60 <- as.data.frame(forecast_dp3$mean)  #voorspellingen van TS naar DF
head(voorsp_60)

schat_1jul19 <- round(voorsp_60[1,1])  # 1-7-2019 (7-12-2018!!) = 9031 passagiers
schat_2jul19 <- round(voorsp_60[2,1])  # 2-7-2019 (8-12-2018!!) = 8411 passagiers
schat_15jul19 <- round(voorsp_60[15,1])  # 15-7-2019 (21-12-2018!!) = 7893 passagiers
schat_1aug19 <- round(voorsp_60[32,1])  # 1-8-2019 (7-1-2019!!) = 8056 passagiers
schat_29aug19 <- round(voorsp_60[60,1])  # 29-8-2019 (4-2-2019!!) = 7271 passagiers


#### Berekening van verschil tussen werkelijke waarde en schatting
act_1jul19 <- as.numeric(Dagpax2  %>% 
                           filter (Actuele.datum.tijd == "2019-07-01") %>% 
                           select (PassagiersPerDag))
print (act_1jul19 - schat_1jul19)

act_2jul19 <- as.numeric(Dagpax2  %>% 
                           filter (Actuele.datum.tijd == "2019-07-02") %>% 
                           select (PassagiersPerDag))
print (act_2jul19 - schat_29aug19)

act_15jul19 <- as.numeric(Dagpax2  %>% 
                            filter (Actuele.datum.tijd == "2019-07-15") %>% 
                            select (PassagiersPerDag))
print (act_15jul19 - schat_15jul19)

act_1aug19 <- as.numeric(Dagpax2  %>% 
                           filter (Actuele.datum.tijd == "2019-08-01") %>% 
                           select (PassagiersPerDag))
print (act_1aug19 - schat_1aug19)

act_29aug19 <- as.numeric(Dagpax2  %>% 
                            filter (Actuele.datum.tijd == "2019-08-29") %>% 
                            select (PassagiersPerDag))
print (act_29aug19 - schat_29aug19)


#==========================================================
# Einde Sprint3                                        ####
#==========================================================



#### Hieronder experimenten
