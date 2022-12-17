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

### Visual ts totaledataset
Dagpax.ts <- ts(Dagpax$PassagiersPerDag, start = c(2003,212), frequency = 365)
plot(Dagpax.ts, xlab="Jaren", ylab="Aantal passagiers", main="Aantal passagiers 30-7-03 t/m 31-8-21")
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

summary (Dagpax1) #min 1-8-2003, max 31-08-2021
summary (Dagpax2) #min 1-8-2003, max 31-08-2019
summary (Dagpax3) #min 1-8-2003, max 30-06-2019


#### Passagiers per dag als Time Serie (Dagpax3)
dp3 <- ts (Dagpax3$PassagiersPerDag, start = c(2003,214), frequency = 365)

autoplot (dp3) 
dc3 <- decompose(dp3)
autoplot(dc3) # Warning message: Removed 728 rows containing missing values (`geom_line()`), maar geen gevolgen voor nu

## opmerkingen: stijgende trend, seasonality, remainder is geen white noise,
# maar wordt langzaam steeds groter, dus dataset is niet random en dusnog niet geschikt
# om obv hiervan te voorspellen

# Testing stationarity middels acf en pacf
autoplot(acf(dp3,plot=FALSE))+ labs(title="Correlogram Passagiers") 
autoplot(pacf(dp3,plot=FALSE))+ labs(title="Correlogram Passagiers")



#####################################################
#### Sprint 4 NeuralNet                          ####
#####################################################
RNGversion("3.5.2")
set.seed(12345)

# NNAR(p,P,k)
#   Lagged values (p) used as inputs
#   Number of nodes (k) in the hidden layer
#   Seasonal lag (P) [vaak P=1]

#### Passagiers per dag als Time Series (Dagpax3)
#dp3 <- ts (Dagpax3$PassagiersPerDag, start = c(2003,214), frequency = 365)
#autoplot (dp3) 


# Creeeren Neural network model
# ?nnetar
# Feed-forward neural networks with a single hidden layer and 
#   lagged inputs for forecasting univariate time series.
nnet <- nnetar(dp3, repeats = 2)
 


# Forecast 62 dagen 
nnetforecast <- forecast(nnet, h = 62, PI = T)  # PI = Prediction intervals
autoplot(nnetforecast)

# NNAR(29,1,16)[365] -> Neuralnet AutoRegression model
#   Gebruikt de laatste 29 waarden als input, en 1 observatie van seasonal cycle
#   Seasonal cycle is yearly met 365 dagen; 16 nodes in hidden layer


#### Vergelijking van voorspelling Neuralnet met werkelijke waarde
# Omzetten voorspellingen van Nnet naar DF
jul_aug_nnet <- round(as.data.frame(nnetforecast$mean))  #voorspellingen van nnet naar DF
head(jul_aug_nnet)

jul_aug_pax <- as.data.frame(tail(Dagpax2, 62)[,2])
#jul_aug_pax
tmp <- cbind(jul_aug_pax, jul_aug_nnet, abs(jul_aug_pax - jul_aug_nnet))
#tmp
round (mean (tmp[,3]))




########################################################################
### Conclusie: 
###   Gemiddelde afwijking aantal passagiers per dag 
###   obv Neural network is 2x rep -> 506; 5x rep -> 649;  20x rep -> 816
### Voorspelling obv ETS-model is beter/slechter dan SARIMA model??
########################################################################


####################
# EINDE SPRINT 4
####################
