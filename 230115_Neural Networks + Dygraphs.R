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
setwd("C:/Users/stefa/OneDrive/Bureaublad/Studies/Big Data Analytics (MBA)/9. Applied Big Data")

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
dp3 <- ts (Dagpax3$PassagiersPerDag, start = c(2003,214), frequency = 365.25)

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
#### Sprint 5 NeuralNet                          ####
#####################################################

# ----->  Dataset train periode vanaf 1-1-17 tm 30-6-19
Dagpax_17train <- Dagpax %>% filter(Actuele.datum.tijd >= '2017-01-01' & Actuele.datum.tijd <='2019-06-30') #909 obs
summary(Dagpax_17train)


### 2017 Visual ts totaledataset
Dagpax.ts17 <- ts(Dagpax_17train$PassagiersPerDag, start = c(2017,1), frequency = 365.25)
plot(Dagpax.ts17, xlab="Jaren", ylab="Aantal passagiers", main="Aantal passagiers 1-1-2017 t/m 31-8-2019")
dc17 <- decompose(Dagpax.ts17)
autoplot(dc17) 
## opmerkingen: stijgende trend, seasonality, remainder is geen white noise,
# maar wordt langzaam steeds groter, dus dataset is niet random en dusnog niet geschikt
# om obv hiervan te voorspellen
### OPM Ivan: decomposition oogt 'bijzonder'


# Testing stationarity middels acf en pacf
autoplot(acf(Dagpax.ts17,plot=FALSE))+ labs(title="Correlogram Passagiers") 
autoplot(pacf(Dagpax.ts17,plot=FALSE))+ labs(title="Correlogram Passagiers")






###############################################
# ----->  Dataset train periode vanaf 1-1-14 tm 30-6-19
Dagpax_14train <- Dagpax %>% filter(Actuele.datum.tijd >= '2014-01-01' & Actuele.datum.tijd <='2019-06-30') #2005 obs
#summary(Dagpax_14train)
str(Dagpax_14train)


### 2014 Visual ts totaledataset
Dagpax.ts14 <- ts(Dagpax_14train$PassagiersPerDag, start = c(2014,1), frequency = 365.25)
plot(Dagpax.ts14, xlab="Jaren", ylab="Aantal passagiers", main="Aantal passagiers 1-1-2014 t/m 31-8-2019")
dc14 <- decompose(Dagpax.ts14)
autoplot(dc14) 
## opmerkingen: stijgende trend, seasonality, remainder is geen white noise (diff toepassen?)
# dus dataset is niet stationair  


# Testing stationarity middels acf en pacf
autoplot(acf(Dagpax.ts14,plot=FALSE))+ labs(title="Correlogram Passagiers") 
autoplot(pacf(Dagpax.ts14,plot=FALSE))+ labs(title="Correlogram Passagiers")



#####################################################
#### NeuralNet Sprint5, scenario startdata 1-1-2014 en 1-1-2017
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


# Resultaat scenario met data 1-1-2014 t/m 30-06-2019
calc_nnet(2,1,Dagpax.ts14)  # NNAR(28,1,15)[365] -> 843
calc_nnet(5,1,Dagpax.ts14) # NNAR(28,1,15)[365] -> 925(!)
calc_nnet(10,1,Dagpax.ts14) # NNAR(15,1,8)[365] -> 844 (!)
calc_nnet(2,2, Dagpax.ts14) # NNAR(28,2,16)[365] -> 738
calc_nnet(5,2,Dagpax.ts14) # NNAR(15,2,9)[365] -> 510 <--
calc_nnet(10,2,Dagpax.ts14) # NNAR(15,2,9)[365] -> 594 (!)
calc_nnet(2,3,Dagpax.ts14) # NNAR(15,8) -> 719
calc_nnet(10,3,Dagpax.ts14) # NNAR(15,8) -> 673

# Resultaat scenario met data 1-1-2017 t/m 30-06-2019
calc_nnet(2,1,Dagpax.ts17)  # NNAR(15,1,8)[365] -> 800
calc_nnet(5,1,Dagpax.ts17) # NNAR(15,1,8)[365] -> 792
calc_nnet(10,1,Dagpax.ts17) # NNAR(15,1,8)[365] -> 778
calc_nnet(2,2,Dagpax.ts17) # NNAR(15,2,9)[365] -> 894
calc_nnet(5,2,Dagpax.ts17) # NNAR(15,2,9)[365] -> 706 <--
calc_nnet(10,2,Dagpax.ts17) # NNAR(15,2,9)[365] -> 796(!)
calc_nnet(2,3,Dagpax.ts17) # NNAR(15,8) -> 1056
calc_nnet(10,3,Dagpax.ts17) # NNAR(15,8) -> 787


### Data, Fitted en Forecast in 1 afbeelding; warnings genegeerd
nnet <- nnetar (Dagpax.ts17, 2, P = 1) 
nnetforecast <- forecast(nnet, h = 62, PI = T)  # PI = Prediction intervals
autoplot(Dagpax.ts17, series = 'Data', 
         xlab="Jaren",
         ylab="Aantal passagiers", 
         main="Aantal passagiers 1-1-2017 t/m 31-8-2019 (werkelijk, model, voorspelling)",
         color = 'blue') + 
  autolayer(fitted(nnetforecast), series = 'Fitted', color = 'purple') +
  autolayer (nnetforecast, series = 'Forecast', color = 'darkgray') #+ xlim(2018, 2020)



# Resultaat Sprint 4, met data 1-8-2003 t/m 30-06-2019
calc_nnet(2,1,dp3) # NNAR(29,1,16)[365] -> 506 <--
#calc_nnet(2,2,dp3) # NNAR(29,2,16)[365] -> 583(!)


#######################################################################
### Overzicht
###   Gemiddelde afwijking aantal passagiers per dag 
###   Sprint 4: 
###     Selectie 2003-01-01 t/m 2019-06-30 -> 506 
###
###   Sprint 5: 
###     Selectie 2014-01-01 t/m 2019-06-30 -> 510
###     Selectie 2017-01-01 t/m 2019-06-30 -> 706
### 
###   Conclusie:
###     Meer historie meenemen, levert kleinere afwijking en betere voorspelling op.
### 
########################################################################


####################
# EINDE SPRINT 5
####################




####################
# SPRINT 6
####################

library(xts)
library(dygraphs)
library(forecast)
#data <- nnetforecast$x              # Raw data van forecast
#lower <- nnetforecast$lower[,2]     # 95% betrouwbaarheidsinterval
#upper <- nnetforecast$upper [,2]    # 95% betrouwbaarheidsinterval
pforecast <- nnetforecast$mean      # 60 dagen forecastperiode 

#Zie ook https://rstudio.github.io/dygraphs/


# Dygraph van 1 jan 2017 tm 30 jun 2019
data_xts<- xts(x= Dagpax_17train$PassagiersPerDag, frequency = 365,
               order.by = Dagpax_17train$Actuele.datum.tijd)

dygraph (data_xts, main = "RTH Airport - passagiers per dag",
         ylab="Aantal passagiers")  %>% 
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)


# Dygraph van 1 jan 2017 tm 30 jun 2019
# Creeren data 1 jul 2019 tm 31 aug 2019 tbv voorspelling
data62 <- seq(as.Date("2019-07-01"), as.Date("2019-08-31"), by="days")

forecast_xts <- xts(x= pforecast, frequency = 365,
                    order.by = data62)

dygraph (forecast_xts, main = "RTH Airport - passagiers per dag",
         ylab="Aantal passagiers")  %>% 
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20) 

# NIET GELUKT om model + voorspelling te plotten
# NIET GELUKT om betrouwbaarheidsintervallen te plotten


# Csv-files aanmaken voor Power BI
write.table(data_xts, "DataFiles/DataXTS.csv", sep = ",", row.names = FALSE)
write.table(forecast_xts, "DataFiles/ForecastXTS.csv", sep = ",", row.names = FALSE)
