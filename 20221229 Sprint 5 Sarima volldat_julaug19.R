# Volledige dataset: 1-8-03 tm 31-8-19
# periode in model: 1-8-03 tm 30-6-19
# Voorspelling: 1-7-19 tm 31-8-19

#================================================#
# STAP 1 - INLADEN PACKAGES

library(readr)
library(tidyverse)
library(lubridate)
library(forecast)
#================================================#
# STAP 2 - INLADEN DATABESTAND

setwd("C:/Users/jcklo/Documents/2021/Studie/Modules/Applied Big data")
# setwd("C:/Users/Ivan/Documents/SynologyDrive/202108-THGS-BigData/202209-Applied BigData/_Repo-Airlines/airport")

Datasetruw  <- read.csv2("DataFiles/cleandataR.csv", stringsAsFactors = TRUE,
                         na.strings=c("", "NA"))

#================================================#
# STAP 3A - EDA

Datasetruw
summary(Datasetruw) # Pay.pax 352892 NA's 
glimpse(Datasetruw) # 679,582 rijen, 26 kolommen

# Datawrangling:Selecteren twee belangrijkste kolommen datum en passagiers, NA's verwijderen
Dagpax <- Datasetruw [c('Actuele.datum.tijd','Pay.pax')] %>%
  filter(!is.na(Datasetruw$Pay.pax))

# Omzetten datum naar datumtype (Lubridate)
Dagpax$Actuele.datum.tijd <- mdy_hm(Dagpax$Actuele.datum.tijd)

# Selecteer alleen datum
Dagpax$Actuele.datum.tijd <- date(Dagpax$Actuele.datum.tijd)

# Dataset periode vanaf 1-8-03 tm 31-8-19
Dagpax <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2019-08-31')
summary(Dagpax)

# Dataset train periode vanaf 1-8-03 tm 30-6-19
Dagpax_train <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01' & Actuele.datum.tijd <='2019-06-30')
summary(Dagpax_train)

# Tel aantal passagiers per dag (volledige dataset)
Dagpax <- Dagpax %>% group_by(Actuele.datum.tijd) %>% 
  summarize(PassagiersPerDag = sum(Pay.pax))
head(Dagpax)
tail(Dagpax)
summary(Dagpax$PassagiersPerDag)
str(Dagpax) # datum is date class en passagiers is integer class

# Tel aantal passagiers per dag (train dataset)
Dagpax_train <- Dagpax_train %>% group_by(Actuele.datum.tijd) %>% 
  summarize(PassagiersPerDag = sum(Pay.pax))
head(Dagpax_train)
tail(Dagpax_train)
summary(Dagpax_train$PassagiersPerDag)
str(Dagpax_train) # datum is date class en passagiers is integer class

# Toevoegen van datum gerelateerde kolommen volledige dataset (ipv alleen datumkolom)
Dagpax_EDA <- Dagpax %>%
  mutate(Year = year(Actuele.datum.tijd)) %>%
  mutate(Month = month(Actuele.datum.tijd)) %>%
  mutate(Day = day(Actuele.datum.tijd)) %>%
  mutate(Weekday = wday(Actuele.datum.tijd))

#================================================#
# STAP 3B - EDA VISUALS

# passagiers per weekdag
ggplot(Dagpax_EDA, aes(x = factor(Weekday), y = PassagiersPerDag)) +
  geom_boxplot()
# NOG DOEN: toevoevoegen van titel en wijzigen naam x en y as en kleurtjes boxplots

# passagiers per maand
ggplot(Dagpax_EDA, aes(x = factor(Month), y = PassagiersPerDag)) +
  geom_boxplot()
# NOG DOEN: toevoevoegen van titel en wijzigen naam x en y as en kleurtjes boxplots

# Passagiers per maand voor ieder jaar
pivot1 <- Dagpax_EDA %>%
  group_by(Year, Month) %>%
  summarise(PassagiersPerDag = sum(PassagiersPerDag, na.rm = TRUE))

values <- pivot1[,3] 
ts <- ts(values,start=c(2003,1),end=c(2019,8),frequency=12)
ggseasonplot(ts, year.labels = TRUE, 
             ylab = "Passagiers",
             main = "Passagiers per maand")
# NOG DOEN: leesbare aantallen op y as en year labels buiten de grafiek ipv erin

# passagiers per weekdag (voor ieder jaar)
pivot2 <- Dagpax_EDA %>%
  group_by(Year, Weekday) %>%
  summarise(PassagiersPerDag = sum(PassagiersPerDag, na.rm = TRUE))

values2 <- pivot2[,3] 
ts2 <- ts(values2, start=c(2003,1),end=c(2019,7),frequency=7)
ggseasonplot(ts2, year.labels = TRUE, 
             ylab = "Passagiers",
             main = "Passagiers per dag")
# NOG DOEN: leesbare aantallen op y as en year labels buiten de grafiek ipv erin

#================================================#
# STAP 4 - OMZETTEN DATASET NAAR TS

# Betreft eerder gemaakte selectie: Dagpax_dip (1-8-03 tm 31-8-19)
summary(Dagpax)

# ts van maken
Dagpax.ts <- ts(Dagpax$PassagiersPerDag, start = c(2003,213), frequency = 365.25)
start(Dagpax.ts) # 2003.58
end(Dagpax.ts) # 2019.646
frequency(Dagpax.ts) # 365.25
length(Dagpax.ts) #5869
head(Dagpax.ts) # 2795 2259 1961 1535 2675 2197 
tail(Dagpax.ts) #  8416 7104 7878 8534 8924 8044 

# visual ts
plot(Dagpax.ts, xlab="Jaren", ylab="Aantal passagiers", main="Aantal passagiers 1-8-03 t/m 31-8-19")
# Hoort wellicht bij visual EDA, maw wat houdt dataset in

autoplot (Dagpax.ts) 
dc <- decompose(Dagpax.ts)
autoplot(dc) 


#================================================#
# STAP 5 - STATIONARIZATION

# Testing stationarity middels acf en pacf
autoplot(acf(Dagpax.ts,plot=FALSE))+ labs(title="Correlogram Passagiers") 
autoplot(pacf(Dagpax.ts,plot=FALSE))+ labs(title="Correlogram Passagiers")

#================================================#
# STAP 6 - MODEL BOUWEN

# train data is 1-8-03 tm 30-6-19
Dagpax.ts_train <- ts(Dagpax_train$PassagiersPerDag, start = c(2003,213), frequency = 365.25)
start(Dagpax.ts_train) # 2003.58
end(Dagpax.ts_train) # 2019.476 
frequency(Dagpax.ts_train) # 365.25
length(Dagpax.ts_train) #5807 
head(Dagpax.ts_train) # 2795 2259 1961 1535 2675 2197 
tail(Dagpax.ts_train) #  6326 7060 8529 8057 7615 8005 

# Model obv traindata
my_arima <- auto.arima(Dagpax.ts_train)
my_arima

# Output:
# Series: Dagpax.ts_dip 
# ARIMA(5,1,2)(0,1,0)[365] 

#Coefficients:
#     ar1      ar2     ar3     ar4     ar5      ma1      ma2
# -0.9737  -0.2164  0.1890  0.3908  0.4775  -0.1070  -0.8231
#s.e.   0.0145   0.0216  0.0224  0.0209  0.0137   0.0084   0.0074

#sigma^2 = 564281:  log likelihood = -43750.89
# AIC=87517.79   AICc=87517.81   BIC=87570.6

#================================================#
# STAP 7 - ERROR/MODEL EVALUATIE

summary(my_arima)
#Training set error measures:
#                ME     RMSE     MAE       MPE     MAPE      MASE      ACF1
#Training set -2.994689 726.6437 526.398 -40.26478 54.62597 0.6407867 0.1963871

autoplot(my_arima) 
checkresiduals(my_arima)

#================================================#
# STAP 8 - FORECASTING

forecast_dagpax <- forecast::forecast(my_arima, level = c(95), h = 62) # dit geeft voorspelling 62 dagen in toekomst, zijnde jul-aug
autoplot(forecast_dagpax) # NOG DOEN: betrouwbaarheidsinterval duidelijker zichtbaar maken
summary (forecast_dagpax)
# begin is 2019.4791 (eind trainset is (end = 2019.4763), verschil is .0027 en dat klopt als je volgende punt in forecast bekijkt)
# End forecast is 2019.646

forecast_dagpax$mean    

# Forecast visual alleen laatste 5 jaar tonen (beter leesbaar)
autoplot(forecast_dagpax) + xlim(2015, 2020)

#================================================#
# STAP 9 - VERSCHILLEN PER DAG

#voorspellingen van TS naar DF
voorsp_julaug<- as.data.frame(forecast_dagpax$mean)  

# Afronden voorspellingen
voorsp_julauground <- round(voorsp_julaug) %>%
  rename(Forecast = x)

# DF werkelijk en voorspellingen samenvoegen en nieuwe kolom om verschil te laten zien
Dagpax_werk <- Dagpax %>%
  filter(Actuele.datum.tijd >= "2019-07-01" & Actuele.datum.tijd <= "2019-08-31")

Dagpax_forecast <- cbind(Dagpax_werk, voorsp_julauground) %>%
  mutate(verschil = (PassagiersPerDag - Forecast))
summary(Dagpax_forecast$verschil)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2095.00   -87.75   400.50   286.52   772.00  1598.00 

# Gemiddelde (absolute) aantallen per dag
mean(abs(Dagpax_forecast$verschil)) #656 

# Visual foutmarges 
# eerst kolom datum en verschil selecteren en weekdag toevoegen
Foutmarge <- Dagpax_forecast %>%
  mutate(Weekday = wday(Dagpax_forecast$Actuele.datum.tijd, label = TRUE)) %>%
  select(Actuele.datum.tijd, Weekday, verschil)

# histogram foutmarge per weekdag
ggplot(Foutmarge, aes(x = Actuele.datum.tijd, y = verschil, fill = Weekday)) +
  geom_bar(stat = "identity") +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "verschil") +
  theme_bw() +
  ggtitle("Gem.foutmarge pd")
# Conclusie: dinsdagen en maandagen lijkt het model slecht te voorspellen ivm hoge foutmarge
# kan niet zeggen of het begin van de voorspelling beter voorspeld dan later in de voorspelling

#####
# Deze stap als laatste, want als je eerder library itsmr draait dan probelemen
# met forecast functies.
# Hoef je niet te draaien want geen effect op voorspelling.
# Wellicht alleen interessant om mee te nemen in presentatie
# Deze stap hoort bij STAP 8 - ERROR/EVALUATIE MODEL
library(itsmr)
test(resid(my_arima)) 

