library(timetk)
library(tsibble)
library(zoo)
library(xts)
library(TSstudio)
library(forcats)
library(readxl)
setwd("C:/Users/fabia/OneDrive/Documentos/UNAL/Series de tiempo/Pasajeros")
Pasajeros_metro <- read_excel("Pasajeros metro.xlsx")
View(Pasajeros_metro)
ts_metro <- ts(Pasajeros_metro$Usuarios, start = c(2021,01), frequency = 365)
plot(ts_metro, main = "Pasajeros metro de Medellin", xlab = "Tiempo",ylab="Pasajeros")


####ACF y PACF####
acf(ts_metro,main="",sub=" Función de Autocorrelación Simple", ci.type ="ma")
pacf(ts_metro,main="",sub=" Función de Autocorrelación Parcial")


####Estatabilizar varianza marginal#####


lambda=forecast::BoxCox.lambda(ts_metro, method = "guerrero", lower = -1, upper = 3)
lambda

Metro_One=forecast::BoxCox(ts_metro,lambda=lambda)
plot(forecast::BoxCox(Metro_One,lambda=lambda))



l_ts_metro=log(Metro_One)
plot(l_ts_metro)

###Eliminación tendencia serie transformada###


ndiffs(l_ts_metro)
diffmetro = diff(l_ts_metro)
plot(diffmetro)
ndiffs(diffmetro)
###ACF###

par(mfrow=c(2,2), mar=c(4,4,4,1)+.1)
plot(ts_metro, main= "Pasajeros metro Medellin")
acf(ts_metro)
plot(diffmetro, main="Serie diferenciada de Pasajeros metro Medellin")
acf(diffmetro)


###Detección de ciclos####

par(mfrow=c(1,1))
tbl_ts_metro<- as_tsibble(diffmetro)
tbl_ts_metro<-as_tibble(tbl_ts_metro)
tbl_ts_metro$index<-as.Date(tbl_ts_metro$index)
tbl_ts_metro

library(timetk)
tbl_ts_metro%>%plot_seasonal_diagnostics(.date_var = index,.value = value,.feature_set = c("month.lbl"),.geom="boxplot") 



######Periodograma#####
par(mfrow=c(1,2))
plot.ts(diffmetro)
spectrum(diffmetro, log='no')
abline(v=365/7, lty=2,col="blue")
abline(v=365/3.5, lty=2,col="red")

###Suavizamiento exponencial######

