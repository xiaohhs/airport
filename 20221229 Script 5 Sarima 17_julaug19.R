# Volledige dataset: 1-1-17 tm 31-8-19
# periode in model: 1-1-17 tm 30-6-19
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

# Volledige dataset periode vanaf 1-1-17 tm 31-8-19
Dagpax_17 <- Dagpax %>% filter(Actuele.datum.tijd >= '2017-01-01' & Actuele.datum.tijd <='2019-08-31')
summary(Dagpax_17)

# Dataset train periode vanaf 1-1-17 tm 30-6-19
Dagpax_17_train <- Dagpax %>% filter(Actuele.datum.tijd >= '2017-01-01' & Actuele.datum.tijd <='2019-06-30')
summary(Dagpax_17_train)

# Tel aantal passagiers per dag (volledige dataset)
Dagpax_17 <- Dagpax_17 %>% group_by(Actuele.datum.tijd) %>% 
  summarize(PassagiersPerDag = sum(Pay.pax))
head(Dagpax_17)
tail(Dagpax_17)
summary(Dagpax_17$PassagiersPerDag)
str(Dagpax_17) # datum is date class en passagiers is integer class

# Tel aantal passagiers per dag (train dataset)
Dagpax_17_train <- Dagpax_17_train %>% group_by(Actuele.datum.tijd) %>% 
  summarize(PassagiersPerDag = sum(Pay.pax))
head(Dagpax_17_train)
tail(Dagpax_17_train)
summary(Dagpax_17_train$PassagiersPerDag)
str(Dagpax_17_train) # datum is date class en passagiers is integer class

# Toevoegen van datum gerelateerde kolommen volledige dataset (ipv alleen datumkolom)
Dagpax_17_EDA <- Dagpax_17 %>%
  mutate(Year = year(Actuele.datum.tijd)) %>%
  mutate(Month = month(Actuele.datum.tijd)) %>%
  mutate(Day = day(Actuele.datum.tijd)) %>%
  mutate(Weekday = wday(Actuele.datum.tijd))

#================================================#
# STAP 3B - EDA VISUALS

# passagiers per weekdag
ggplot(Dagpax_17_EDA, aes(x = factor(Weekday), y = PassagiersPerDag)) +
  geom_boxplot()
# NOG DOEN: toevoevoegen van titel en wijzigen naam x en y as en kleurtjes boxplots

# passagiers per maand
ggplot(Dagpax_17_EDA, aes(x = factor(Month), y = PassagiersPerDag)) +
  geom_boxplot()
# NOG DOEN: toevoevoegen van titel en wijzigen naam x en y as en kleurtjes boxplots

# Passagiers per maand voor ieder jaar
pivot1_17 <- Dagpax_17_EDA %>%
  group_by(Year, Month) %>%
  summarise(PassagiersPerDag = sum(PassagiersPerDag, na.rm = TRUE))

values_17 <- pivot1_17[,3] 
ts_17 <- ts(values_17,start=c(2017,1),end=c(2019,8),frequency=12)
ggseasonplot(ts_17, year.labels = TRUE, 
             ylab = "Passagiers",
             main = "Passagiers per maand")
# NOG DOEN: leesbare aantallen op y as en year labels buiten de grafiek ipv erin

# passagiers per weekdag (voor ieder jaar)
pivot2_17 <- Dagpax_17_EDA %>%
  group_by(Year, Weekday) %>%
  summarise(PassagiersPerDag = sum(PassagiersPerDag, na.rm = TRUE))

values2_17 <- pivot2_17[,3] 
ts2_17 <- ts(values2_17, start=c(2017,1),end=c(2019,7),frequency=7)
ggseasonplot(ts2_17, year.labels = TRUE, 
             ylab = "Passagiers",
             main = "Passagiers per dag")
# NOG DOEN: leesbare aantallen op y as en year labels buiten de grafiek ipv erin

#================================================#
# STAP 4 - OMZETTEN DATASET NAAR TS

# Betreft eerder gemaakte selectie: Dagpax_17 (1-1-17 tm 31-8-19)
summary(Dagpax_17)

# ts van maken
Dagpax_17.ts <- ts(Dagpax_17$PassagiersPerDag, start = c(2017,1), frequency = 365.25)
start(Dagpax_17.ts) # 2017
end(Dagpax_17.ts) # 2019.656 
frequency(Dagpax_17.ts) # 365.25
length(Dagpax_17.ts) #971 
head(Dagpax_17.ts) # 3359 4365 3877 3730 3483 3719
tail(Dagpax_17.ts) # 8416 7104 7878 8534 8924 8044

# visual ts
plot(Dagpax_17.ts, xlab="Jaren", ylab="Aantal passagiers", main="Aantal passagiers 1-1-17 t/m 31-8-19")
# Hoort wellicht bij visual EDA, maw wat houdt dataset in

autoplot (Dagpax_17.ts) 
dc_17 <- decompose(Dagpax_17.ts)
autoplot(dc_17) 

#================================================#
# STAP 5 - STATIONARIZATION

# Testing stationarity middels acf en pacf
autoplot(acf(Dagpax_17.ts,plot=FALSE))+ labs(title="Correlogram Passagiers") 
autoplot(pacf(Dagpax_17.ts,plot=FALSE))+ labs(title="Correlogram Passagiers")

#================================================#
# STAP 6 - MODEL BOUWEN

# train data is 1-1-17 tm 30-6-19
Dagpax_17_train.ts <- ts(Dagpax_17_train$PassagiersPerDag, start = c(2017,1), frequency = 365.25)
start(Dagpax_17_train.ts) # 2017
end(Dagpax_17_train.ts) # 2019.486
frequency(Dagpax_17_train.ts) # 365.25
length(Dagpax_17_train.ts) #909 
head(Dagpax_17_train.ts) #  3359 4365 3877 3730 3483 3719
tail(Dagpax_17_train.ts) #  6326 7060 8529 8057 7615 8005
#End = 2019.48596851472 

# Model obv traindata
my_arima_17 <- auto.arima(Dagpax_17_train.ts)
my_arima_17

# Coefficients:
#   ar1      ar2      ar3      ma1     ma2      ma3      ma4      ma5
#-0.7284  -1.1004  -0.2827  -0.0750  0.7374  -0.7867  -0.0478  -0.4668
#s.e.   0.0844   0.0400   0.0837   0.0773  0.0441   0.0537   0.0595   0.0407

#sigma^2 = 552750:  log likelihood = -4362.01
#AIC=8742.02   AICc=8742.36   BIC=8780.7

#================================================#
# STAP 7 - ERROR/MODEL EVALUATIE

summary(my_arima_17)
# Training set error measures:
#                 ME     RMSE     MAE        MPE     MAPE      MASE        ACF1
#Training set 9.035519 570.2401 348.893 -0.9880858 8.508085 0.3904879 -0.01280065

autoplot(my_arima_17) 
checkresiduals(my_arima_17)

#================================================#
# STAP 8 - FORECASTING

forecast_dagpax_17 <- forecast::forecast(my_arima_17, level = c(95), h = 62)
# dit geeft voorspelling 62 dagen in toekomst, zijnde jul-aug

autoplot(forecast_dagpax_17) # NOG DOEN: betrouwbaarheidsinterval duidelijker zichtbaar maken
summary (forecast_dagpax_17) # begin is 2019.4887 (eind trainset is (end = 2019.4859),
# verschil is .0027 en dat klopt als je volgende punt in forecast bekijkt)
# End forecast is 2019.655

forecast_dagpax_17$mean    

#================================================#
# STAP 9 - VERSCHILLEN PER DAG

#voorspellingen van TS naar DF
voorsp_17_julaug<- as.data.frame(forecast_dagpax_17$mean)  

# Afronden voorspellingen
voorsp_17_julauground <- round(voorsp_17_julaug) %>%
  rename(Forecast = x)

# DF werkelijk en voorspellingen samenvoegen en nieuwe kolom om verschil te laten zien
Dagpax_17_werk <- Dagpax_17 %>%
  filter(Actuele.datum.tijd >= "2019-07-01" & Actuele.datum.tijd <= "2019-08-31")

Dagpax_17_forecast <- cbind(Dagpax_17_werk, voorsp_17_julauground) %>%
  mutate(verschil = (PassagiersPerDag - Forecast))
summary(Dagpax_17_forecast$verschil)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-934.0   -76.5   310.0   327.7   762.5  1572.0 

# Gemiddelde (absolute) aantallen per dag
mean(abs(Dagpax_17_forecast$verschil)) #532 

# Visual foutmarges 
# eerst kolom datum en verschil selecteren en weekdag toevoegen
Foutmarge_17 <- Dagpax_17_forecast %>%
  mutate(Weekday = wday(Dagpax_17_forecast$Actuele.datum.tijd, label = TRUE)) %>%
  select(Actuele.datum.tijd, Weekday, verschil)

# histogram foutmarge per weekdag
ggplot(Foutmarge_17, aes(x = Actuele.datum.tijd, y = verschil, fill = Weekday)) +
  geom_bar(stat = "identity") +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "verschil") +
  theme_bw() +
  ggtitle("Gem.foutmarge pd")
# Conclusie: Eerste 3 weken vd voorspelling beter voorspeld dan later in de voorspelling

#####
# Deze stap als laatste, want als je eerder library itsmr draait dan probelemen
# met forecast functies.
# Geen meerwaarde voor werking model. Wellicht alleen interessant om als visual
# te laten zien in presentatie
# Deze stap hoort bij STAP 7 - ERROR/EVALUATIE MODEL
library(itsmr)
test(resid(my_arima_17)) 

