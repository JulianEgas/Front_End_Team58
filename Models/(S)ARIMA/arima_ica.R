library(readr)
library(readxl)
library(tidyverse)
library(urca)
library(MASS)
library(forecast)
library(TSA)
library(uroot)


ica_2000_2020 <- tibble()

for (i in 1:21){
  route <- paste("PAGOS"," ",2000+i-1,".xls",sep="")
  database_per_year <- read_excel(route)
  
  database_per_year$year <- 2000+i-1
  database_per_year<- database_per_year[,c("year","PERIODO LIQUIDADO","V/R FACTURA")]
  
  
  ica_2000_2020 <- bind_rows(
    ica_2000_2020,
    database_per_year
  )
}



complete_ica <- ica_2000_2020 %>% dplyr::select(fecha='PERIODO LIQUIDADO',valor_factura="V/R FACTURA") %>% 
  group_by(fecha) %>% 
  
  summarise(valor_factura=sum(valor_factura,na.rm=T),
            freq=n()) %>% 
  filter(!(is.na(fecha)|fecha=="0"))


write.csv(complete_ica,"complete_ica.csv")



# Full table --------------------------------------------------------------


ica_2000_2020_full <- tibble()

for (i in 1:21){
  route <- paste("PAGOS"," ",2000+i-1,".xls",sep="")
  database_per_year <- read_excel(route)
  
  database_per_year$year <- 2000+i-1

  
  ica_2000_2020_full <- bind_rows(
    ica_2000_2020_full,
    database_per_year
  )
}

write.csv(ica_2000_2020_full,"ica_pagos_2000_2020.csv",row.names = F)




# Time series analysis ----------------------------------------------------



unique(ica_2000_2020$`PERIODO LIQUIDADO`) %>% sort()


time_series_ica <- ts(complete_ica[24:263,2],start=c(2001,1),frequency=12)
plot(time_series_ica)

Acf(diff(time_series_ica,1))
Pacf(diff(time_series_ica,1))


summary(hegy.test(time_series_ica,deterministic=c(1,0,1),lag.method="fixed",maxlag=1))
#Reject every null hypothesis

summary(ur.df(time_series_ica,type="trend",lags=1))
#Reject null of unit root

eacf(diff(time_series_ica,1))



complete_ica[24:263,2]

write.csv(complete_ica[24:263,1:2],"ica_ts_2000_2020.csv",row.names = F)

# Arima -------------------------------------------------------------------

train=1:180

time_series_ica




#best fit
ar_12_ma_2 <- arima(time_series_ica[train],order=c(p=11,d=1,q=2),method="ML")
plot(residuals(ar_1_ma_2))
Acf(residuals(ar_1_ma_2))
Pacf(residuals(ar_1_ma_2))



ar_2_ma_1 <- arima(time_series_ica[train],order=c(p=2,d=1,q=1),method="ML")
plot(residuals(ar_2_ma_1))
Acf(residuals(ar_2_ma_1))
Pacf(residuals(ar_2_ma_1))


ar_11_ma_2 <- arima(time_series_ica[train],order=c(p=11,d=1,q=2),method="ML")#doesnt make the model better
plot(residuals(ar_0_ma_2))
Acf(residuals(ar_0_ma_2))
Pacf(residuals(ar_0_ma_2))


ar_0_ma_2 <- arima(time_series_ica[train],order=c(p=12,d=1,q=2),method="ML")
plot(residuals(ar_0_ma_2))
Acf(residuals(ar_0_ma_2))
Pacf(residuals(ar_0_ma_2))


#prediction plot


arma_12_2_pred <- predict(ar_12_ma_2,60)
plot(ar_12_ma_2,n.ahead=60,col="red",ylab="Pago ICA (COP)")

sqrt(mean((time_series_ica[-train] - arma_12_2_pred$pred)^2))


#3367203510/1000000
