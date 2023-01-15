# INLADEN PACKAGES
library(readr)
library(tidyverse)
library(lubridate)
library(forecast)
library(nnet)

# INLADEN DATABESTAND
#setwd("C:/Users/jcklo/Documents/2021/Studie/Modules/Applied Big data")
#setwd("C:/Users/xiao.peng/OneDrive - Stichting Hogeschool Utrecht/backup/xiao peng hu/MBA 2/Applied Big Data/airport")
#setwd("C:/Users/Ivan/Documents/SynologyDrive/202108-THGS-BigData/202209-Applied BigData/_Repo-Airlines/airport")
setwd("C:/Users/xiao.peng/OneDrive - Stichting Hogeschool Utrecht/backup/xiao peng hu/MBA 2/Applied Big Data/airport")

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
#Dagpax.ts <- ts(Dagpax$PassagiersPerDag, start = c(2003,212), frequency = 365.25)
#plot(Dagpax.ts, xlab="Jaren", ylab="Aantal passagiers", main="Aantal passagiers 30-7-03 t/m 31-8-21")
###


#### Selectie datumbereik

### Omzetten naar Time Series
# Selecie data: van 1-8-2003 tm 31-8-2019
Dagpax1 <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01')
Dagpax2 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2019-08-31')
Dagpax3 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2019-06-30')
Dagpax4 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2019-04-30')
Dagpax5 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2019-02-28')
Dagpax6 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2018-12-31')
Dagpax7 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2018-10-31')
Dagpax8 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2018-08-31')
# Dagpax3 wordt de input; we gaan juli en aug 2019 voorspellen
# Waarom: in eerste instantie verschoven de dagen, maar dat kwam doordat
# we dachten dat het getal na de komma de maanden waren, maar blijkt dagen te zijn
# dus start 1-8-03 is niet start = c(2003,8) maar start = c(2003, 212)
# dus dat 'probleem' is getackeld.
# Toch gebleven bij voorspellen jul aug 19, omdat we sowieso voor corona wilde gaan zitten
# En begin is 1-8-03, dus eindigen dan ook in augustus in 2019

#summary (Dagpax1) #min 1-8-2003, max 31-08-2021
#summary (Dagpax2) #min 1-8-2003, max 31-08-2019
#summary (Dagpax3) #min 1-8-2003, max 30-06-2019


#### Passagiers per dag als Time Serie (Dagpax3)
#### AANGEPAST SPRINT5: freq = 365.25 (was 365)
#### Uitkomst blijft ongewijzigd
dp3 <- ts (Dagpax3$PassagiersPerDag, start = c(2003,211), frequency = 365.25)

autoplot (dp3) 
dc3 <- decompose(dp3)
autoplot(dc3) # Warning message: Removed 728 rows containing missing values (`geom_line()`), maar geen gevolgen voor nu

## opmerkingen: stijgende trend, seasonality, remainder is geen white noise,
# maar wordt langzaam steeds groter, dus dataset is niet random en dusnog niet geschikt
# om obv hiervan te voorspellen

# Testing stationarity middels acf en pacf
#autoplot(acf(dp3,plot=FALSE))+ labs(title="Correlogram Passagiers") 
#autoplot(pacf(dp3,plot=FALSE))+ labs(title="Correlogram Passagiers")






#####################################################
#### Sprint 6 NeuralNet with dataset 1-8-03 tm 31-8-19                       
#####################################################


### Visual ts totaledataset
Dagpax3.ts <- ts(Dagpax3$PassagiersPerDag, start = c(2003,213), frequency = 365.25)
plot(Dagpax.ts, xlab="Jaren", ylab="Aantal passagiers", main="Aantal passagiers 1-8-2003 t/m 31-8-2019")
dc <- decompose(Dagpax.ts)
autoplot(dc) 
## opmerkingen: stijgende trend, seasonality, remainder is geen white noise,
# maar wordt langzaam steeds groter, dus dataset is niet random en dusnog niet geschikt
# om obv hiervan te voorspellen
### OPM Ivan: decomposition oogt 'bijzonder'
Dagpax4.ts <- ts(Dagpax4$PassagiersPerDag, start = c(2003,213), frequency = 365.25)
Dagpax5.ts <- ts(Dagpax5$PassagiersPerDag, start = c(2003,213), frequency = 365.25)
Dagpax6.ts <- ts(Dagpax6$PassagiersPerDag, start = c(2003,213), frequency = 365.25)
Dagpax7.ts <- ts(Dagpax7$PassagiersPerDag, start = c(2003,213), frequency = 365.25)
Dagpax8.ts <- ts(Dagpax8$PassagiersPerDag, start = c(2003,213), frequency = 365.25)



# Testing stationarity middels acf en pacf
autoplot(acf(Dagpax.ts,plot=FALSE))+ labs(title="Correlogram Passagiers") 
autoplot(pacf(Dagpax.ts,plot=FALSE))+ labs(title="Correlogram Passagiers")



#####################################################
#### NeuralNet  scenario different two months
#####################################################
# NNAR(p,P,k)
#   Lagged values (p) used as inputs
#   Number of nodes (k) in the hidden layer
#   Seasonal lag (P) [vaak P=1]


# Creeeren Neural network model in functie calc_nnet
# ?nnetar
# Feed-forward neural networks with a single hidden layer and 
#   lagged inputs for forecasting univariate time series.


#########
## Functie om nnetar aan te roepen met argumenten
## aantal repeats, aantal hidden layers, time-serie 
## Output:  parameters NNAR, gemiddelde afwijking, grafiek met forecast
#########


calc_nnet <- function(reps = 1, hlayer = 1, tms) {
  RNGversion("3.5.2") 
  set.seed(12345)
  
  nnet <- nnetar (tms, repeats = reps, P = hlayer) 
  nnetforecast <- forecast(nnet, h = 62, PI = T)  # PI = Prediction intervals
  
  #summary(nnetforecast)
  plot(nnetforecast)  # autoplot werkt niet binnen functie, plot wel
  
  # Vergelijking van voorspelling Neuralnet met werkelijke waarde
  jul_aug_nnet <- round(as.data.frame(nnetforecast$mean))  #voorspellingen van nnet naar DF
  head(jul_aug_nnet)
  
  jul_aug_pax <- as.data.frame(tail(Dagpax2, 62)[,2])   #Werkelijke waarde jul_aug_pax
  tmp <- cbind(jul_aug_pax, jul_aug_nnet, abs(jul_aug_pax - jul_aug_nnet))
  res <- round (mean (tmp[,3]))
  
  output <- paste(nnetforecast$method, "-> gemiddelde afwijking", res)
  return(output) # Toont gebruikte parameters NNAR(p,P,k)[f]
}


# Resultaat scenario met data 1-8-2003 t/m 30-06-2019
calc_nnet(2,1,Dagpax3.ts)  # 506 (!)
calc_nnet(5,1,Dagpax3.ts) # 649
calc_nnet(10,1,Dagpax3.ts) # 839
calc_nnet(2,2, Dagpax3.ts) # 583
calc_nnet(5,2,Dagpax3.ts) # 545
calc_nnet(10,2,Dagpax3.ts) # 599
calc_nnet(2,3,Dagpax3.ts) # 631
calc_nnet(10,3,Dagpax3.ts) # 589

calc4_nnet <- function(reps = 1, hlayer = 1, tms) {
  RNGversion("3.5.2") 
  set.seed(12345)
  
  nnet <- nnetar (tms, repeats = reps, P = hlayer) 
  nnetforecast <- forecast(nnet, h = 61, PI = T)  # PI = Prediction intervals
  
  #summary(nnetforecast)
  plot(nnetforecast)  # autoplot werkt niet binnen functie, plot wel
  
  # Vergelijking van voorspelling Neuralnet met werkelijke waarde
last_two_months_nnet <- round(as.data.frame(nnetforecast$mean))  #voorspellingen van nnet naar DF
  head(last_two_months_nnet)
  
  last_two_months_pax <- as.data.frame(tail(Dagpax3, 61)[,2])   #Werkelijke waarde mei_jun 2019
  tmp <- cbind(last_two_months_pax, last_two_months_nnet, abs(last_two_months_pax - last_two_months_nnet))
  res <- round (mean (tmp[,3]))
  
  output <- paste(nnetforecast$method, "-> gemiddelde afwijking", res)
  return(output) # Toont gebruikte parameters NNAR(p,P,k)[f]
}

calc4_nnet(1,1,Dagpax4.ts)  #486 
calc4_nnet(2,1,Dagpax4.ts)  # 438 
calc4_nnet(5,1,Dagpax4.ts) # 497
calc4_nnet(10,1,Dagpax4.ts) # 714
calc4_nnet(2,2, Dagpax4.ts) # 671
calc4_nnet(5,2,Dagpax4.ts) # 676
calc4_nnet(10,2,Dagpax4.ts) # 710
calc4_nnet(2,3,Dagpax4.ts) # 592
calc4_nnet(10,3,Dagpax4.ts) # 428 (!)


calc5_nnet <- function(reps = 1, hlayer = 1, tms) {
  RNGversion("3.5.2") 
  set.seed(12345)
  
  nnet <- nnetar (tms, repeats = reps, P = hlayer) 
  nnetforecast <- forecast(nnet, h = 61, PI = T)  # PI = Prediction intervals
  
  #summary(nnetforecast)
  plot(nnetforecast)  # autoplot werkt niet binnen functie, plot wel
  
  # Vergelijking van voorspelling Neuralnet met werkelijke waarde
  last_two_months_nnet <- round(as.data.frame(nnetforecast$mean))  #voorspellingen van nnet naar DF
  head(last_two_months_nnet)
  
  last_two_months_pax <- as.data.frame(tail(Dagpax4, 61)[,2])   #Werkelijke waarde mar_apr 2019
  tmp <- cbind(last_two_months_pax, last_two_months_nnet, abs(last_two_months_pax - last_two_months_nnet))
  res <- round (mean (tmp[,3]))
  
  output <- paste(nnetforecast$method, "-> gemiddelde afwijking", res)
  return(output) # Toont gebruikte parameters NNAR(p,P,k)[f]
}

calc5_nnet(1,1,Dagpax5.ts)  #1353
calc5_nnet(2,1,Dagpax5.ts)  # 1294
calc5_nnet(5,1,Dagpax5.ts) # 1443
calc5_nnet(10,1,Dagpax5.ts) # 1062
calc5_nnet(2,2, Dagpax5.ts) # 877 (!)
calc5_nnet(5,2,Dagpax5.ts) # 960
calc5_nnet(10,2,Dagpax5.ts) # 995
calc5_nnet(2,3,Dagpax5.ts) # 1630
calc5_nnet(10,3,Dagpax5.ts) # 1003

calc6_nnet <- function(reps = 1, hlayer = 1, tms) {
  RNGversion("3.5.2") 
  set.seed(12345)
  
  nnet <- nnetar (tms, repeats = reps, P = hlayer) 
  nnetforecast <- forecast(nnet, h = 59, PI = T)  # PI = Prediction intervals
  
  #summary(nnetforecast)
  plot(nnetforecast)  # autoplot werkt niet binnen functie, plot wel
  
  # Vergelijking van voorspelling Neuralnet met werkelijke waarde
  last_two_months_nnet <- round(as.data.frame(nnetforecast$mean))  #voorspellingen van nnet naar DF
  head(last_two_months_nnet)
  
  last_two_months_pax <- as.data.frame(tail(Dagpax5, 59)[,2])   #Werkelijke waarde jan_feb 2019
  tmp <- cbind(last_two_months_pax, last_two_months_nnet, abs(last_two_months_pax - last_two_months_nnet))
  res <- round (mean (tmp[,3]))
  
  output <- paste(nnetforecast$method, "-> gemiddelde afwijking", res)
  return(output) # Toont gebruikte parameters NNAR(p,P,k)[f]
}

calc6_nnet(1,1,Dagpax6.ts)  #774
calc6_nnet(2,1,Dagpax6.ts)  # 581
calc6_nnet(5,1,Dagpax6.ts) # 603
calc6_nnet(10,1,Dagpax6.ts) # 619
calc6_nnet(2,2, Dagpax6.ts) # 701
calc6_nnet(5,2,Dagpax6.ts) # 628
calc6_nnet(10,2,Dagpax6.ts) # 634
calc6_nnet(2,3,Dagpax6.ts) # 558
calc6_nnet(10,3,Dagpax6.ts) # 552 (!)

calc7_nnet <- function(reps = 1, hlayer = 1, tms) {
  RNGversion("3.5.2") 
  set.seed(12345)
  
  nnet <- nnetar (tms, repeats = reps, P = hlayer) 
  nnetforecast <- forecast(nnet, h = 61, PI = T)  # PI = Prediction intervals
  
  #summary(nnetforecast)
  plot(nnetforecast)  # autoplot werkt niet binnen functie, plot wel
  
  # Vergelijking van voorspelling Neuralnet met werkelijke waarde
  last_two_months_nnet <- round(as.data.frame(nnetforecast$mean))  #voorspellingen van nnet naar DF
  head(last_two_months_nnet)
  
  last_two_months_pax <- as.data.frame(tail(Dagpax6, 61)[,2])   #Werkelijke waarde nov_dec 2018
  tmp <- cbind(last_two_months_pax, last_two_months_nnet, abs(last_two_months_pax - last_two_months_nnet))
  res <- round (mean (tmp[,3]))
  
  output <- paste(nnetforecast$method, "-> gemiddelde afwijking", res)
  return(output) # Toont gebruikte parameters NNAR(p,P,k)[f]
}

calc7_nnet(1,1,Dagpax7.ts)  #1324
calc7_nnet(2,1,Dagpax7.ts)  # 1168
calc7_nnet(5,1,Dagpax7.ts) # 1047
calc7_nnet(10,1,Dagpax7.ts) # 1025
calc7_nnet(2,2, Dagpax7.ts) # 1106
calc7_nnet(5,2,Dagpax7.ts) # 1175
calc7_nnet(10,2,Dagpax7.ts) # 1029
calc7_nnet(2,3,Dagpax7.ts) # 1338
calc7_nnet(10,3,Dagpax7.ts) # 874 (!)


calc8_nnet <- function(reps = 1, hlayer = 1, tms) {
  RNGversion("3.5.2") 
  set.seed(12345)
  
  nnet <- nnetar (tms, repeats = reps, P = hlayer) 
  nnetforecast <- forecast(nnet, h = 61, PI = T)  # PI = Prediction intervals
  
  #summary(nnetforecast)
  plot(nnetforecast)  # autoplot werkt niet binnen functie, plot wel
  
  # Vergelijking van voorspelling Neuralnet met werkelijke waarde
  last_two_months_nnet <- round(as.data.frame(nnetforecast$mean))  #voorspellingen van nnet naar DF
  head(last_two_months_nnet)
  
  last_two_months_pax <- as.data.frame(tail(Dagpax7, 61)[,2])   #Werkelijke waarde nov_dec 2018
  tmp <- cbind(last_two_months_pax, last_two_months_nnet, abs(last_two_months_pax - last_two_months_nnet))
  res <- round (mean (tmp[,3]))
  
  output <- paste(nnetforecast$method, "-> gemiddelde afwijking", res)
  return(output) # Toont gebruikte parameters NNAR(p,P,k)[f]
}

calc8_nnet(1,1,Dagpax8.ts)  #825
calc8_nnet(2,1,Dagpax8.ts)  # 912
calc8_nnet(5,1,Dagpax8.ts) # 1031
calc8_nnet(10,1,Dagpax8.ts) # 1010
calc8_nnet(2,2, Dagpax8.ts) # 750
calc8_nnet(5,2,Dagpax8.ts) # 825
calc8_nnet(10,2,Dagpax8.ts) # 828
calc8_nnet(2,3,Dagpax8.ts) # 746 (!)
calc8_nnet(10,3,Dagpax8.ts) # 769 


### Data, Fitted en Forecast in 1 afbeelding; warnings genegeerd
nnet <- nnetar (Dagpax.ts, 2, P = 1) 
nnetforecast <- forecast(nnet, h = 62, PI = T)  # PI = Prediction intervals
autoplot(Dagpax.ts, series = 'Data', 
         xlab="Jaren",
         ylab="Aantal passagiers", 
         main="Aantal passagiers 1-8-2003 t/m 31-8-2019 (werkelijk, model, voorspelling)",
         color = 'blue') + 
  autolayer(fitted(nnetforecast), series = 'Fitted', color = 'purple') +
  autolayer (nnetforecast, series = 'Forecast', color = 'darkgray') + xlim(2018, 2020)







####################
# dygraphs
####################

library(xts)
library(dygraphs)
#data <- nnetforecast$x              # Raw data van forecast
#lower <- nnetforecast$lower[,2]     # 95% betrouwbaarheidsinterval
#upper <- nnetforecast$upper [,2]    # 95% betrouwbaarheidsinterval
#pforecast <- nnetforecast$mean      # 60 dagen forecastperiode 

#Zie ook https://rstudio.github.io/dygraphs/


# Dygraph van 1 jan 2017 tm 30 jun 2019
#data_xts<- xts(x= Dagpax3$PassagiersPerDag, frequency = 365.25)
data_xts <- xts(x=Dagpax3$PassagiersPerDag, order.by=as.Date(Dagpax$Actuele.datum.tijd, "%Y-%d-%m"))

dygraph (data_xts, main = "RTH Airport - passagiers per dag",
         ylab="Aantal passagiers")  %>% 
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)


# Dygraph van 1 jan 2017 tm 30 jun 2019
# Creeren data 1 jul 2019 tm 31 aug 2019 tbv voorspelling
data62 <- seq(as.Date("2019-07-01"), as.Date("2019-08-31"), by="days")

forecast_xts <- xts(x= forecast, frequency = 365,
                    order.by = data62)

dygraph (forecast_xts, main = "RTH Airport - passagiers per dag",
         ylab="Aantal passagiers")  %>% 
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20) 

# NIET GELUKT om model + voorspelling te plotten
# NIET GELUKT om betrouwbaarheidsintervallen te plotten