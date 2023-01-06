#### Code Data Cleaning & EDA ####

# INLADEN PACKAGES
library(readr)
library(tidyverse)
library(lubridate)


# INLADEN DATABESTAND
#setwd("C:/Users/jcklo/Documents/2021/Studie/Modules/Applied Big data")
#setwd("C:/Users/Ivan/Documents/SynologyDrive/202108-THGS-BigData/202209-Applied BigData/_Repo-Airlines/airport")
setwd("C:/Users/xiao.peng/OneDrive - Stichting Hogeschool Utrecht/backup/xiao peng hu/MBA 2/Applied Big Data/airport")
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
write.table(Dagpax, "DataFiles/Dagpax.csv", sep = ",", row.names = FALSE)

plot(Dagpax$PassagiersPerDag) # Dipje aan eind van plot, enkele outliers
boxplot(Dagpax$PassagiersPerDag) # Outliers

Dagpax_1419 <- Dagpax %>% filter(Actuele.datum.tijd >= '2014-01-01' & Actuele.datum.tijd <='2019-08-31')
summary(Dagpax_1419)
write.table(Dagpax_1419, "DataFiles/Dagpax_1419.csv", sep = ",", row.names = FALSE) #2067 obs


Dagpax_1719 <- Dagpax %>% filter(Actuele.datum.tijd >= '2017-01-01' & Actuele.datum.tijd <='2019-08-31')
summary(Dagpax_1719)
write.table(Dagpax_1719, "DataFiles/Dagpax_1719.csv", sep = ",", row.names = FALSE)#971 obs

