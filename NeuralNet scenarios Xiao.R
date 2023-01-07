# Scenario NN 

# INLADEN PACKAGES
library(readr)
library(tidyverse)
library(lubridate)
library(forecast)

# INLADEN DATABESTAND
#setwd("C:/Users/jcklo/Documents/2021/Studie/Modules/Applied Big data")
#setwd("C:/Users/Ivan/Documents/SynologyDrive/202108-THGS-BigData/202209-Applied BigData/_Repo-Airlines/airport")
setwd("C:/Users/xiao.peng/OneDrive - Stichting Hogeschool Utrecht/backup/xiao peng hu/MBA 2/Applied Big Data/airport")
Dagpax  <- read.csv("DataFiles/Dagpax.csv") # 2067 obs


# Datasets maken
#Dagpax<- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2019-08-31')
#Dagpax_train <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2019-06-30') #2005 obs
#Dagpax<- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2019-06-30')
#Dagpax_train <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2019-04-30')
#Dagpax<- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2019-04-30')
#Dagpax_train <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2019-02-28')
#Dagpax<- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2019-02-28')
#Dagpax_train <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2018-12-31')
#Dagpax<- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2018-12-31')
#Dagpax_train <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2018-10-31')
Dagpax<- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2018-10-31')
Dagpax_train <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2018-08-31')
summary(Dagpax_train)

### Visual ts totaledataset

Dagpax.ts <- ts(Dagpax_train$PassagiersPerDag, start = c(2003,213), frequency = 365.25)
plot(Dagpax.ts, xlab="Jaren", ylab="Aantal passagiers", main="Aantal passagiers")

dc <- decompose(Dagpax.ts)
autoplot(dc) 
## opmerkingen: stijgende trend, seasonality, remainder is geen white noise,
# maar wordt langzaam steeds groter, dus dataset is niet random en dusnog niet geschikt
# om obv hiervan te voorspellen

# Testing stationarity middels acf en pacf
autoplot(acf(Dagpax.ts,plot=FALSE))+ labs(title="Correlogram Passagiers") 
autoplot(pacf(Dagpax.ts,plot=FALSE))+ labs(title="Correlogram Passagiers")



#####################################################
#### NeuralNet                          ####
#####################################################
RNGversion("3.5.2")
library(nnet)
set.seed(12345)





# NNAR(p,P,k)
#   Lagged values (p) used as inputs
#   Number of nodes (k) in the hidden layer
#   Seasonal lag (P) [vaak P=1]



# Creeeren Neural network model
#?nnetar
# Feed-forward neural networks with a single hidden layer and 
#   lagged inputs for forecasting univariate time series.
nnet <- nnetar(Dagpax.ts, repeats = 2)



# Forecast 62 dagen NOTE: last two months, can be 61 days etc...
nnetforecast <- forecast(nnet, h = 61, PI = T)  # PI = Prediction intervals
autoplot(nnetforecast)

# NNAR(29,1,16)[365] -> Neuralnet AutoRegression model
#   Gebruikt de laatste 29 waarden als input, en 1 observatie van seasonal cycle
#   Seasonal cycle is yearly met 365 dagen; 16 nodes in hidden layer


#### Vergelijking van voorspelling Neuralnet met werkelijke waarde
# Omzetten voorspellingen van Nnet naar DF
jul_aug_nnet <- round(as.data.frame(nnetforecast$mean))  #voorspellingen van nnet naar DF
head(jul_aug_nnet)

jul_aug_pax <- as.data.frame(tail(Dagpax, 61)[,2])
#jul_aug_pax
tmp <- cbind(jul_aug_pax, jul_aug_nnet, abs(jul_aug_pax - jul_aug_nnet))
#tmp
round (mean (tmp[,3]))




########################################################################
### Old### Conclusie: 
###   Gemiddelde afwijking aantal passagiers per dag 
###   obv Neural network is 2x rep -> 506; 5x rep -> 649;  20x rep -> 816
### Voorspelling obv ETS-model is beter/slechter dan SARIMA model??
########################################################################


####################
# EINDE SPRINT 4
####################