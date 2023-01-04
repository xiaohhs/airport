# Volledige dataset: 1-1-10 tm 31-8-19
# periode in model: 1-1-10 tm 30-6-19
# Voorspelling: 1-7-19 tm 31-8-19

#================================================#
# STAP 1 - INLADEN PACKAGES

library(readr)
library(tidyverse)
library(lubridate)
library(forecast)
#================================================#
# STAP 2 - INLADEN DATABESTAND

#setwd("C:/Users/jcklo/Documents/2021/Studie/Modules/Applied Big data")
setwd("C:/Users/Ivan/Documents/SynologyDrive/202108-THGS-BigData/202209-Applied BigData/_Repo-Airlines/airport")


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

# Dataset periode vanaf 1-1-10 tm 31-8-19
Dagpax_dip <- Dagpax %>% filter(Actuele.datum.tijd >= '2010-01-01' & Actuele.datum.tijd <='2019-08-31')
summary(Dagpax_dip)

# Dataset train periode vanaf 1-1-10 tm 30-6-19
Dagpax_diptrain <- Dagpax %>% filter(Actuele.datum.tijd >= '2010-01-01' & Actuele.datum.tijd <='2019-06-30')
summary(Dagpax_diptrain)

# Tel aantal passagiers per dag volledige dataset
Dagpax_dip <- Dagpax_dip %>% group_by(Actuele.datum.tijd) %>% 
  summarize(PassagiersPerDag = sum(Pay.pax))
head(Dagpax_dip)
tail(Dagpax_dip)
summary(Dagpax_dip$PassagiersPerDag)
str(Dagpax_dip) # datum is date class en passagiers is integer class

# Tel aantal passagiers per dag train dataset
Dagpax_diptrain <- Dagpax_diptrain %>% group_by(Actuele.datum.tijd) %>% 
  summarize(PassagiersPerDag = sum(Pay.pax))
head(Dagpax_diptrain)
tail(Dagpax_diptrain)
summary(Dagpax_diptrain$PassagiersPerDag)
str(Dagpax_diptrain) # datum is date class en passagiers is integer class

# Toevoegen van datum gerelateerde kolommen volledige dataset (ipv alleen datumkolom)
Dagpax_dip_EDA <- Dagpax_dip %>%
  mutate(Year = year(Actuele.datum.tijd)) %>%
  mutate(Month = month(Actuele.datum.tijd)) %>%
  mutate(Day = day(Actuele.datum.tijd)) %>%
  mutate(Weekday = wday(Actuele.datum.tijd))

# alles klopt qua schrikkeljaar en weekdagen tov datum
Dagpax_dip_EDA
#================================================#
# STAP 3B - EDA VISUALS

# passagiers per weekdag
ggplot(Dagpax_dip_EDA, aes(x = factor(Weekday), y = PassagiersPerDag)) +
  geom_boxplot()
# NOG DOEN: toevoevoegen van titel en wijzigen naam x en y as en kleurtjes boxplots

# passagiers per maand
ggplot(Dagpax_dip_EDA, aes(x = factor(Month), y = PassagiersPerDag)) +
  geom_boxplot()
# NOG DOEN: toevoevoegen van titel en wijzigen naam x en y as en kleurtjes boxplots

# Passagiers per maand voor ieder jaar
pivot1_dip <- Dagpax_dip_EDA %>%
  group_by(Year, Month) %>%
  summarise(PassagiersPerDag = sum(PassagiersPerDag, na.rm = TRUE))

values_dip <- pivot1_dip[,3] 
ts_dip <- ts(values_dip,start=c(2010,1),frequency=12)
ggseasonplot(ts_dip, year.labels = TRUE, 
             ylab = "Passagiers",
             main = "Passagiers per maand")
# NOG DOEN: leesbare aantallen op y as en year labels buiten de grafiek ipv erin

# passagiers per weekdag (voor ieder jaar)
pivot2_dip <- Dagpax_dip_EDA %>%
  group_by(Year, Weekday) %>%
  summarise(PassagiersPerDag = sum(PassagiersPerDag, na.rm = TRUE))

values2_dip <- pivot2_dip[,3] 
ts2_dip <- ts(values2_dip,start=c(2010,1),end=c(2019,7),frequency=7)
ggseasonplot(ts2_dip, year.labels = TRUE, 
             ylab = "Passagiers",
             main = "Passagiers per dag")
# NOG DOEN: leesbare aantallen op y as en year labels buiten de grafiek ipv erin

#================================================#
# STAP 4 - OMZETTEN DATASET NAAR TS

# Betreft eerder gemaakte selectie: Dagpax_dip (1-1-10 tm 31-8-19)
summary(Dagpax_dip)

# ts van maken
Dagpax.ts_dip1 <- ts(Dagpax_dip$PassagiersPerDag, start = c(2010,1), frequency = 365.25)
start(Dagpax.ts_dip1) # 2010 1
end(Dagpax.ts_dip1) # 2019.648 
frequency(Dagpax.ts_dip1) # 365.25
length(Dagpax.ts_dip1) #3525 
head(Dagpax.ts_dip1) # 1580 2864 2452 2181 1776 1839
tail(Dagpax.ts_dip1) #  8416 7104 7878 8534 8924 8044 

# visual ts
plot(Dagpax.ts_dip1, xlab="Jaren", ylab="Aantal passagiers", main="Aantal passagiers 1-1-10 t/m 31-8-19")
# Hoort wellicht bij visual EDA, maw wat houdt dataset in

autoplot (Dagpax.ts_dip1) 
dc_dip <- decompose(Dagpax.ts_dip1)
autoplot(dc_dip) 

#================================================#
# STAP 5 - STATIONARIZATION

# Testing stationarity middels acf en pacf

autoplot(acf(Dagpax.ts_dip1,plot=FALSE))+ labs(title="Correlogram Passagiers") 
autoplot(pacf(Dagpax.ts_dip1,plot=FALSE))+ labs(title="Correlogram Passagiers")

#================================================#
# STAP 6 - MODEL BOUWEN

# train data is 1-1-10 tm 30-6-19
Dagpax.ts_diptrain <- ts(Dagpax_diptrain$PassagiersPerDag, start = c(2010,1), frequency = 365.25)
start(Dagpax.ts_diptrain) # 2010 
end(Dagpax.ts_diptrain) # 2019.478 
frequency(Dagpax.ts_diptrain) # 365.25
length(Dagpax.ts_diptrain) #3463 
head(Dagpax.ts_diptrain) # 1580 2864 2452 2181 1776 1839
tail(Dagpax.ts_diptrain) #  6326 7060 8529 8057 7615 8005 

# Model obv traindata
my_arima_dip <- auto.arima(Dagpax.ts_diptrain)
my_arima_dip

# Output:
# Series: Dagpax.ts_dip 
# ARIMA(5,1,0)(0,1,0)[365] 

# Coefficients:
#       ar1      ar2      ar3     ar4    ar5
#     -1.1848  -0.8727  -0.3457  0.1347  0.319
#s.e.   0.0170   0.0272   0.0308  0.0272  0.017

# sigma^2 = 907929:  log likelihood = -25640.37
# AIC=51292.73   AICc=51292.76   BIC=51328.96

#================================================#
# STAP 7 - ERROR/MODEL EVALUATIE

summary(my_arima_dip)
#Training set error measures:
#                ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set 0.9679894 900.3668 652.2233 -3.624671 19.36542 0.6936216 0.2415086

autoplot(my_arima_dip) 

checkresiduals(my_arima_dip)

#================================================#
# STAP 8 - FORECASTING

forecast_dagpax_dip <- forecast::forecast(my_arima_dip, level = c(95), h = 62) # dit geeft voorspelling 62 dagen in toekomst, zijnde jul-aug
autoplot(forecast_dagpax_dip) # NOG DOEN: betrouwbaarheidsinterval duidelijker zichtbaar maken
summary (forecast_dagpax_dip) # begin is 2019.4812 (eind trainset is (end = 2019.4784),
# verschil is .0027 en dat klopt als je volgende punt in forecast bekijkt)
forecast_dagpax_dip$mean    

# Forecast visual alleen laatste 5 jaar tonen (beter leesbaar)
autoplot(forecast_dagpax_dip) + xlim(2015, 2020)

#### Toevoeging Ivan: Data, Fitted en Forecast in 1 afbeelding
autoplot(Dagpax.ts_diptrain, series = 'Data', xlab="Jaren", ylab="Aantal passagiers", main="Aantal passagiers 1-7-2010 t/m 31-8-21") + 
  autolayer(fitted(forecast_dagpax_dip), series = 'Fitted') +
  autolayer(forecast_dagpax_dip, series = 'Forecast') + xlim(2015, 2020)


#================================================#
# STAP 9 - VERSCHILLEN PER DAG

#voorspellingen van TS naar DF
voorsp_julaug_dip<- as.data.frame(forecast_dagpax_dip$mean)  

# Afronden voorspellingen
voorsp_julauground_dip <- round(voorsp_julaug_dip) %>%
  rename(Forecast = x)

# DF werkelijk en voorspellingen samenvoegen en nieuwe kolom om verschil te laten zien
Dagpax_werk_dip <- Dagpax_dip %>%
  filter(Actuele.datum.tijd >= "2019-07-01" & Actuele.datum.tijd <= "2019-08-31")

Dagpax_forecast_dip <- cbind(Dagpax_werk_dip, voorsp_julauground_dip) %>%
  mutate(verschil = (PassagiersPerDag - Forecast))
summary(Dagpax_forecast_dip$verschil)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1976.0  -189.8   313.0   199.3   681.5  1549.0

# Gemiddelde (absolute) aantallen per dag
mean(abs(Dagpax_forecast_dip$verschil)) #636

# Visual foutmarges 
# eerst kolom datum en verschil selecteren en weekdag toevoegen
Foutmarge_dip <- Dagpax_forecast_dip %>%
  mutate(Weekday = wday(Dagpax_forecast_dip$Actuele.datum.tijd, label = TRUE)) %>%
  select(Actuele.datum.tijd, Weekday, verschil)

# histogram foutmarge per weekdag
ggplot(Foutmarge_dip, aes(x = Actuele.datum.tijd, y = verschil, fill = Weekday)) +
  geom_bar(stat = "identity") +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "verschil") +
  theme_bw() +
  ggtitle("Gem.foutmarge pd") +
  geom_hline(yintercept=600)

#####
# Deze stap als laatste, want als je eerder library itsmr draait dan probelemen
# met forecast functies.
# HOef je niet te draaien want geen effect op voorspelling. Alleen interessant in bv presentatie onderbouwing
# Deze stap hoort bij STAP 7 - ERROR/EVALUATIE MODEL
library(itsmr)
test(resid(my_arima_dip)) 
