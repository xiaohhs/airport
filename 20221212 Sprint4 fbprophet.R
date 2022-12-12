# Sprint 4
# model fb 

# INLADEN PACKAGES
library(readr)
library(tidyverse)
library(lubridate)
library(forecast)
library(prophet)

# INLADEN DATABESTAND
setwd("C:/Users/jcklo/Documents/2021/Studie/Modules/Applied Big data")

Datasetruw  <- read.csv2("DataFiles/cleandataR.csv", stringsAsFactors = TRUE,
                         na.strings=c("", "NA"))

############## dit is hetzelfde als in sprint 3
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

############

# Vanaf hier is NIEUW tbv sprint 4

# Basis forecast dataset gelijk aan ARIMA maken, dus 1-8-03 tot 1-7-19
Dagpax_prophet <- Dagpax %>%
  filter(Actuele.datum.tijd >= "2003-08-01" & Actuele.datum.tijd <= "2019-06-30") %>%
  rename(ds = Actuele.datum.tijd, y = PassagiersPerDag)

m <- prophet(Dagpax_prophet, seasonality.mode = 'multiplicative')
future <- make_future_dataframe(m, 60, freq = 'd') # voorspelt 60 dagen
forecast <- predict(m, future)
plot(m, forecast)
dyplot.prophet(m, forecast)

# DF maken om verschil inzichtelijk te maken
df_werkelijk <- Dagpax %>%
  filter(Actuele.datum.tijd >= "2019-07-01" & Actuele.datum.tijd <= "2019-08-29")

df_forecast <- forecast %>%
  filter(ds >= "2019-07-01" & ds <= "2019-08-30") %>%
  select(yhat)

df_verschil <- cbind(df_werkelijk, df_forecast) %>%
  mutate(verschil = (PassagiersPerDag - yhat)) 

# Verschil: Gemiddelde (absolute) aantallen per dag
mean(abs(df_verschil$verschil)) #1354
# Conclusie; voorspelling is veel minder accuraat dan ARIMA

#The components figure will now show the seasonality as a percent of the trend
prophet_plot_components(m, forecast) # NAGAAN OF DIT KLOPT, WANT ZATERDAG IS LAAGST!!!
# Denk een soort decomposition
# Kan op zich wel kloppen dat zaterdag laag is, want paar handmatig bekeken
# en idd zaterdag is sowieso niet hoogste in de week
