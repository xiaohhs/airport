# INLADEN PACKAGES
library(readr)

# INLADEN DATABESTAND
setwd("C:/Users/jcklo/Documents/2021/Studie/Modules/Applied Big data")
Datasetruw  <- read.csv2("DataFiles/cleandataR.csv", stringsAsFactors = TRUE,
                         na.strings=c("", "NA"))



