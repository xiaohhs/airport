# Dataset: 1-1-14 tm 31-8-19
# periode in model: 1-1-14 tm 30-6-19
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

# Dataset periode vanaf 1-1-14 tm 31-8-19
Dagpax_14 <- Dagpax %>% filter(Actuele.datum.tijd >= '2014-01-01' & Actuele.datum.tijd <='2019-08-31')
summary(Dagpax_14)

# Dataset train periode vanaf 1-1-14 tm 30-6-19
Dagpax_14train <- Dagpax %>% filter(Actuele.datum.tijd >= '2014-01-01' & Actuele.datum.tijd <='2019-06-30')
summary(Dagpax_14train)

# Tel aantal passagiers per dag volledige dataset
Dagpax_14 <- Dagpax_14 %>% group_by(Actuele.datum.tijd) %>% 
  summarize(PassagiersPerDag = sum(Pay.pax))
head(Dagpax_14)
tail(Dagpax_14)
summary(Dagpax_14$PassagiersPerDag)
str(Dagpax_14) # datum is date class en passagiers is integer class

# Tel aantal passagiers per dag train dataset
Dagpax_14train <- Dagpax_14train %>% group_by(Actuele.datum.tijd) %>% 
  summarize(PassagiersPerDag = sum(Pay.pax))
head(Dagpax_14train)
tail(Dagpax_14train)
summary(Dagpax_14train$PassagiersPerDag)
str(Dagpax_14train) # datum is date class en passagiers is integer class

# Toevoegen van datum gerelateerde kolommen volledige dataset (ipv alleen datumkolom)
Dagpax_14_EDA <- Dagpax_14 %>%
  mutate(Year = year(Actuele.datum.tijd)) %>%
  mutate(Month = month(Actuele.datum.tijd)) %>%
  mutate(Day = day(Actuele.datum.tijd)) %>%
  mutate(Weekday = wday(Actuele.datum.tijd))

#================================================#
# STAP 3B - EDA VISUALS

# passagiers per weekdag
ggplot(Dagpax_14_EDA, aes(x = factor(Weekday), y = PassagiersPerDag)) +
  geom_boxplot()
# NOG DOEN: toevoevoegen van titel en wijzigen naam x en y as en kleurtjes boxplots

# passagiers per maand
ggplot(Dagpax_14_EDA, aes(x = factor(Month), y = PassagiersPerDag)) +
  geom_boxplot()
# NOG DOEN: toevoevoegen van titel en wijzigen naam x en y as en kleurtjes boxplots

# Passagiers per maand voor ieder jaar
pivot1_14 <- Dagpax_14_EDA %>%
  group_by(Year, Month) %>%
  summarise(PassagiersPerDag = sum(PassagiersPerDag, na.rm = TRUE))

values_14 <- pivot1_14[,3] 
ts_14 <- ts(values_14,start=c(2014,1),end=c(2019,8),frequency=12)
ggseasonplot(ts_14, year.labels = TRUE, 
             ylab = "Passagiers",
             main = "Passagiers per maand")
# NOG DOEN: leesbare aantallen op y as en year labels buiten de grafiek ipv erin

# passagiers per weekdag (voor ieder jaar)
pivot2_14 <- Dagpax_14_EDA %>%
  group_by(Year, Weekday) %>%
  summarise(PassagiersPerDag = sum(PassagiersPerDag, na.rm = TRUE))

values2_14 <- pivot2_14[,3] 
ts2_14 <- ts(values2_14,start=c(2010,1),end=c(2019,7),frequency=7)
ggseasonplot(ts2_14, year.labels = TRUE, 
             ylab = "Passagiers",
             main = "Passagiers per dag")
# NOG DOEN: leesbare aantallen op y as en year labels buiten de grafiek ipv erin

#================================================#
# STAP 4 - OMZETTEN DATASET NAAR TS

# Betreft eerder gemaakte selectie: Dagpax (1-1-14 tm 31-8-19)
summary(Dagpax_14)

# ts van maken
Dagpax.ts_14 <- ts(Dagpax_14$PassagiersPerDag, start = c(2014,1), frequency = 365.25)
start(Dagpax.ts_14) # 2014
end(Dagpax.ts_14) # 2019.656 
frequency(Dagpax.ts_14) # 365.25
length(Dagpax.ts_14) #2067 
head(Dagpax.ts_14) # 3167 3361 4828 3023 4821 3757
tail(Dagpax.ts_14) #  8416 7104 7878 8534 8924 8044 

# visual ts
plot(Dagpax.ts_14, xlab="Jaren", ylab="Aantal passagiers", main="Aantal passagiers 1-1-14 t/m 31-8-19")
# Hoort wellicht bij visual EDA, maw wat houdt dataset in

autoplot (Dagpax.ts_14) 
dc_14 <- decompose(Dagpax.ts_14)
autoplot(dc_14) 

#================================================#
# STAP 6 - STATIONARIZATION

# Testing stationarity middels acf en pacf

autoplot(acf(Dagpax.ts_14,plot=FALSE))+ labs(title="Correlogram Passagiers") 
autoplot(pacf(Dagpax.ts_14,plot=FALSE))+ labs(title="Correlogram Passagiers")

#================================================#
# STAP 6 - MODEL BOUWEN

# train data is 1-1-14 tm 30-6-19
Dagpax.ts_14train <- ts(Dagpax_14train$PassagiersPerDag, start = c(2014,1), frequency = 365.25)
start(Dagpax.ts_14train) # 2014
end(Dagpax.ts_14train) # 2019.487
frequency(Dagpax.ts_14train) # 365.25
length(Dagpax.ts_14train) #2005 
head(Dagpax.ts_14train) # 3167 3361 4828 3023 4821 3757
tail(Dagpax.ts_14train) #  6326 7060 8529 8057 7615 8005

# Model obv traindata
my_arima_14 <- auto.arima(Dagpax.ts_14train)
my_arima_14

# Output:
# Series: Dagpax.ts_dip 
# ARIMA(5,1,2)(0,1,0)[365] 

# Coefficients:
#  ar1      ar2     ar3     ar4     ar5      ma1      ma2
#-1.0186  -0.3076  0.1667  0.4513  0.5008  -0.1029  -0.8193
#s.e.   0.0266   0.0412  0.0438  0.0400  0.0251   0.0160   0.0138

#sigma^2 = 808012:  log likelihood = -13474.77
#AIC=26965.53   AICc=26965.62   BIC=27008.75

#================================================#
# STAP 7 - ERROR/MODEL EVALUATIE

summary(my_arima_14)
#Training set error measures:
#                ME    RMSE      MAE       MPE    MAPE      MASE      ACF1
#Training set 5.098008 810.921 572.4933 -2.334691 14.5524 0.5829665 0.2076768

autoplot(my_arima_14) 
checkresiduals(my_arima_14)

#================================================#
# STAP 9 - FORECASTING

forecast_dagpax_14 <- forecast::forecast(my_arima_14, level = c(95), h = 62)
# dit geeft voorspelling 62 dagen in toekomst, zijnde jul-aug
autoplot(forecast_dagpax_14) # NOG DOEN: betrouwbaarheidsinterval duidelijker zichtbaar maken
summary (forecast_dagpax_14) # begin is 2019.4894 (eind trainset is (end = 2019.4867,
#verschil is .0027 en klopt met verschil tussen volgende punt)
# End forecast is 2019.6564
forecast_dagpax_14$mean    

#================================================#
# STAP 9 - VERSCHILLEN PER DAG

#voorspellingen van TS naar DF
voorsp_julaug_14<- as.data.frame(forecast_dagpax_14$mean)  

# Afronden voorspellingen
voorsp_julauground_14 <- round(voorsp_julaug_14) %>%
  rename(Forecast = x)

# DF werkelijk en voorspellingen samenvoegen en nieuwe kolom om verschil te laten zien
Dagpax_werk_14 <- Dagpax_14 %>%
  filter(Actuele.datum.tijd >= "2019-07-01" & Actuele.datum.tijd <= "2019-08-31")

Dagpax_forecast_14 <- cbind(Dagpax_werk_14, voorsp_julauground_14) %>%
  mutate(verschil = (PassagiersPerDag - Forecast))
summary(Dagpax_forecast_14$verschil)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2041.0   -97.5   386.0   277.0   757.8  1593.0 

# Gemiddelde (absolute) aantallen per dag
mean(abs(Dagpax_forecast_14$verschil)) #653

# Visual foutmarges 
# eerst kolom datum en verschil selecteren en weekdag toevoegen
Foutmarge_14 <- Dagpax_forecast_14 %>%
  mutate(Weekday = wday(Dagpax_forecast_14$Actuele.datum.tijd, label = TRUE)) %>%
  select(Actuele.datum.tijd, Weekday, verschil)

# histogram foutmarge per weekdag
ggplot(Foutmarge_14, aes(x = Actuele.datum.tijd, y = verschil, fill = Weekday)) +
  geom_bar(stat = "identity") +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "verschil") +
  theme_bw() +
  ggtitle("Gem.foutmarge pd")

#####
# Deze stap als laatste, want als je eerder library itsmr draait dan probelemen
# met forecast functies.
# Geen effect op voorspelling, dus hoef je niet draaien
# wellicht alleen zinvol voor onderbouwing in bv presentatie
# Deze stap hoort bij STAP 8 - ERROR/EVALUATIE MODEL
library(itsmr)
test(resid(my_arima_14)) 
