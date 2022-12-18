library(ggfortify)
library(tseries)
library(forecast)
library(tidyverse)
library(readr)
library(lubridate)

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

### Omzetten naar Time Series
# Selecie data: van 1-8-2003 tm 31-8-2019
Dagpax1 <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01')
Dagpax2 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2019-08-31')
Dagpax3 <- Dagpax1 %>% filter(Actuele.datum.tijd <= '2019-06-30')
Dagpax_julaug19 <- Dagpax %>%
  filter(Actuele.datum.tijd >= "2019-07-01" & Actuele.datum.tijd <= "2019-08-29")
# Dagpax3 wordt de input; we gaan juli en aug 2019 voorspellen (ARIMA/SARIMA)

summary (Dagpax1) #min 1-8-2003, max 31-08-2021
summary (Dagpax2) #min 1-8-2003, max 31-08-2019
summary (Dagpax3) #min 1-8-2003, max 30-06-2019

#### Sprint 4 TBATS ######

#### Passagiers per dag als Time Serie (Dagpax3)
dp3 <- ts (Dagpax3$PassagiersPerDag, start = c(2003,213), frequency = 365)
# start 2003, 213 is 1-8-13
# De 213e dag is niet (1-1-03 plus 213 is 2-8-03), maar 1 dag minder dus 1-8-03
# want de 10e dag is niet (1-1-03 plus 10 is 11-1-19), maar 1 dag minder dus 10-1-19

#fit TBATS model
fit <- tbats(dp3) # TBATS voorspelt vanaf 1 juli 2019, dus vanaf input dagpax 3

#use model to make predictions
predict <- predict(fit)
predict

#plot the predicted values
predict_val<- forecast(fit, level=c(95), h = 60) # forecast for 60 days
plot(predict_val)
summary(predict_val)
predict_val$mean  
#ime Series:
# Start = c(2019, 180) # De 180e dag is niet (1-1-19 plus 180 is 30-6-19, maar
# 1 dag minder dus 29-6-19.
# # want de 10e dag is niet (1-1-03 plus 10 is 11-1-19), maar 1 dag minder dus 10-1-19
# End = c(2019, 239)  # De 239e dag is niet (1-1-19 plus 239 is 28-8-19), maar
# 1 dag minder dus 27-8-19)
# want de 10e dag is niet (1-1-03 plus 10 is 11-1-19), maar 1 dag minder dus 10-1-19 
# Dus voorspelling is van 29-6-19 tm 27-8-19

voorsp_60 <- as.data.frame(predict_val$mean)  #voorspellingen van TS naar DF
head(voorsp_60)
# slechts eerste 60 regels selecteren

# Afronden
voorsp_60round <- round(voorsp_60) %>%
  rename(Forecast = x)
head(voorsp_60round)

# DF werkelijk en voorspellingen samenvoegen en nieuwe kolom om verschil te laten zien
Dagpax_werk <- Dagpax %>%
  filter(Actuele.datum.tijd >= "2019-06-29" & Actuele.datum.tijd <= "2019-08-27")

Dagpax_forecast <- cbind(Dagpax_werk, voorsp_60round) %>%
  mutate(verschil = (PassagiersPerDag - Forecast))
head(Dagpax_forecast)
summary(Dagpax_forecast$verschil)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1517.0    97.5   761.5   684.8  1331.8  2478.0 

# Gemiddelde (absolute) aantallen per dag
mean(abs(Dagpax_forecast$verschil)) #941



