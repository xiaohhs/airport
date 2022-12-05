# INLADEN PACKAGES
library(readr)
library(tidyverse)
library(lubridate)


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


#####################################################
#### De onderstaande code is nog in bewerking!!! ####
#####################################################

### Omzetten naar Time Series
# tijdlijn is nog niet op datum
plot.ts(Dagpax$PassagiersPerDag)


Dagpax1 <- Dagpax %>% filter(Actuele.datum.tijd >= '2003-08-01')
plot(Dagpax1$PassagiersPerDag) # Dipje aan eind van plot, enkele outliers
boxplot(Dagpax1$PassagiersPerDag) # Outliers
aaa <- ts(Dagpax1,start=c(2003,8), end = c(2021,8), frequency = 365)
plot(aaa)
bbb <- decompose(aaa)
plot(bbb)

Dagpax.ts <- ts(Dagpax$PassagiersPerDag, start = c(2003,7), frequency = 365)
plot (Dagpax.ts)


### https://stackoverflow.com/questions/31491157/how-do-i-create-daily-time-series-starting-from-a-specific-date
library (xts)
data_xts <- xts(x=Dagpax$PassagiersPerDag, order.by=as.Date(Dagpax$Actuele.datum.tijd, "%Y-%d-%m"))
#head(Dagpax)
head(data_xts)
# controle klopt

#plot(Dagpax$PassagiersPerDag)
plot(data_xts)

plot(decompose(Dagpax))


library(forecast)
arima(Dagpax.ts)
arima (data_xts)
# zelfde uitkomst :-)

# checken...
forecast::Arima(data_xts)


## Dip in aantallen passagiers tijdens corona (ca 2020-03 en 2020-06)
Dagpax$PassagiersPerDag [Dagpax$Actuele.datum.tijd == '2020-06-30']

