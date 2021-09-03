library(readr)
library(readxl)
library(tidyverse)
library(urca)
library(MASS)
library(forecast)
library(TSA)
library(uroot)

predial_2016_2020 <- read_delim("clean_predial_payment_v3_(correct_date).csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)


predial_2000_2015 <- tibble()

for (i in 1:16){
  route <- paste("PAGOS"," ",2000+i-1,".xlsx",sep="")
  database_per_year <- read_excel(route, col_types = c("numeric", "numeric", "text", 
                                                            "text", "numeric", "numeric", "date", 
                                                            "text", "text", "text", "text", "text", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "text"))
  
  database_per_year$year <- 2000+i-1
  database_per_year<- database_per_year[,c("year","periodo_liq","vlr_fac")]
  
  
  predial_2000_2015 <- bind_rows(
    predial_2000_2015,
    database_per_year
  )
}







predial_2016_2020_v2 <- predial_2016_2020 %>% dplyr::select(valor_factura,fecha=fecha_pago_actual) %>% 
  filter(!is.na(fecha)) %>% 
  mutate(fecha=paste(str_extract(fecha,"\\d{1,4}"),
                    0,
                     str_extract(fecha,"\\d{1}$"),sep="") %>% as.numeric()
         
         )






complete_predial <- predial_2000_2015 %>% dplyr::select(valor_factura=vlr_fac,fecha=periodo_liq) %>%
  
  bind_rows(predial_2016_2020_v2) %>% 
  group_by(fecha) %>% 
  
  summarise(valor_factura=sum(valor_factura,na.rm=T)) %>% 
  filter(!(is.na(fecha)|fecha=="0")) %>% 
  ungroup()


complete_predial


write.csv(complete_predial,"complete_predial.csv")


# Full dataset ------------------------------------------------------------



predial_2000_2015_full <- tibble()


for (i in 1:16){
  route <- paste("PAGOS"," ",2000+i-1,".xlsx",sep="")
  database_per_year <- read_excel(route, col_types = c("numeric", "numeric", "text", 
                                                       "text", "numeric", "numeric", "date", 
                                                       "text", "text", "text", "text", "text", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "text"))
  
  database_per_year$year <- 2000+i-1
  predial_2000_2015_full <- bind_rows(
    predial_2000_2015_full,
    database_per_year
  )
  
}

predial_2000_2015_full <- predial_2000_2015_full %>% mutate(
  cedula=as.numeric(cedula),
  codigo_catastral=as.numeric(codigo_catastral)
)

predial_2016_2020_v3 <- predial_2016_2020 %>%
  mutate(periodo_liq=paste(str_extract(fecha_pago_actual,"\\d{1,4}"),0,str_extract(fecha_pago_actual,"\\d{1}$"),sep="") %>% as.numeric(.),
         
         cedula=as.numeric(nit_cedula),
         vlr_cptotro=as.numeric(valor_conceptos)
         
  ) %>% 
  
  dplyr::select(vlr_fac=valor_factura,
         cedula,
         nombre=nombre_pago,
         codigo_catastral=codigo_catrastral,
         proindiviso,
         ult_pag=fecha_ultimo_pago,
         vlr_cptotro,
         ano_fac=year,
         year,
         periodo_liq)



final_predial_2000_2020 <- predial_2000_2015_full %>%
  
  bind_rows(predial_2016_2020_v3) %>% dplyr::select(-year)



write.csv(final_predial_2000_2020,"predial_pagos_2000_2020.csv",row.names = F)



# Time series analysis -----------------------------------------------------


library(uroot)

time_series_predial <- ts(complete_predial[6:119,3],start=c(1993,1),frequency=4)


plot(time_series_predial)

Acf(time_series_predial)
Pacf(time_series_predial)


#Hegy test

summary(hegy.test(time_series_predial,deterministic=c(1,0,1),lag.method="fixed",maxlag=4))
#Dummies are not significant

summary(hegy.test(time_series_predial,deterministic=c(1,0,0),lag.method="fixed",maxlag=4))
#No se rechaza raiz unitaria no estacional.
#No se rechaza raiz unitaria semestral.
#se rechaza otros tipos de raíz.



summary(hegy.test(time_series_predial_2,deterministic=c(1,0,1),lag.method="fixed",maxlag=4))
#Se rechazan todas las hipótesis nulas de raíz unitaria, hay estacionariedad.


time_series_predial_2 <- time_series_predial-stats::lag(time_series_predial,-2)
plot(time_series_predial_2)
#shifting the time series
shifted_time_series_predial_2 <- time_series_predial_2+1000000000000
plot(time_series_predial_2+1000000)
summary(hegy.test(shifted_time_series_predial_2,deterministic=c(1,0,1),lag.method="fixed",maxlag=4))





boxcox(shifted_time_series_predial_2~1,seq(-3,3,0.01))
plot(shifted_time_series_predial_2)

summary(ur.df(shifted_time_series_predial_2,type="trend",lags=7))#no unit root

Acf(shifted_time_series_predial_2)
Pacf(shifted_time_series_predial_2)

eacf(shifted_time_series_predial_2)



# Dummies deseason --------------------------------------------------------



library(vctrs)


d1=vec_c(rep(c(1,0,0,0),28),1,0)
d2=vec_c(rep(c(0,1,0,0),28),0,1)
d3=vec_c(rep(c(0,0,1,0),28),0,0)
d4=vec_c(rep(c(0,0,0,1),28),0,0)

predial_dummies <- lm(time_series_predial~-1+d1+d2+d3+d4)

Acf(resid(predial_dummies))
Pacf(resid(predial_dummies))





# Arima specification -----------------------------------------------------


time_series_predial <- time_series_predial[33:112]

#write.csv(time_series_predial,"ts_predial_2001_2020.csv")


train <- 1:60


ar_5 <- arima(time_series_predial[train],order=c(p=5,d=0,q=0),seasonal=list(order=c(p=0,d=2,q=1),period=4),method="ML")
plot(residuals(ar_5))
Acf(residuals(ar_5))      
Pacf(residuals(ar_5))
#Se escoge este

ar_2_ma_5 <- arima(time_series_predial[train],order=c(p=4,d=0,q=5),seasonal=list(order=c(p=0,d=2,q=1),period=4),method="ML")
plot(residuals(ar_2_ma_5))
Acf(residuals(ar_2_ma_5))      
Pacf(residuals(ar_2_ma_5))


ar_2_ma_12 <- arima(time_series_predial[train],order=c(p=1,d=0,q=12),seasonal=list(order=c(p=0,d=2,q=1),period=4),method="ML")
plot(residuals(ar_2_ma_12))
Acf(residuals(ar_2_ma_12))#white noise residuals      
Pacf(residuals(ar_2_ma_12))#white noise residuals


ar5_pred <- predict(ar_2_ma_5,n.ahead = 20)
#plot(ar_5,n.ahead=21,col="red")


write.csv(ar5_pred$pred,"arima_predial_pred_2015_2020.csv")





sqrt(mean((time_series_predial[-train] - ar5_pred$pred)^2))
#30727552479



# Out of sample prediction ------------------------------------------------



full_sample_ar_5 <- arima(time_series_predial,order=c(p=5,d=0,q=0),seasonal=list(order=c(p=0,d=2,q=1),period=4),method="ML")
plot(residuals(full_sample_ar_5))
Acf(residuals(full_sample_ar_5))      
Pacf(residuals(full_sample_ar_5))
#Se escoge este

full_sample_ar_2_ma_5 <- arima(time_series_predial,order=c(p=4,d=0,q=5),seasonal=list(order=c(p=0,d=2,q=1),period=4),method="ML")
plot(residuals(full_sample_ar_2_ma_5))
Acf(residuals(full_sample_ar_2_ma_5))      
Pacf(residuals(full_sample_ar_2_ma_5))


full_sample_ar_2_ma_12 <- arima(time_series_predial,order=c(p=1,d=0,q=12),seasonal=list(order=c(p=0,d=2,q=1),period=4),method="ML")
plot(residuals(full_sample_ar_2_ma_12))
Acf(residuals(full_sample_ar_2_ma_12))#white noise residuals      
Pacf(residuals(full_sample_ar_2_ma_12))#white noise residuals


full_sample_ar5_pred <- predict(full_sample_ar_5,n.ahead = 32)

full_sample_ar5_pred$pred
plot(full_sample_ar5_pred$pred)


arima_predial_fullsample_pred <- tibble(arima_predial_2021_2028=full_sample_ar5_pred$pred,
       lower_ci=full_sample_ar5_pred$pred-1.96*full_sample_ar5_pred$se,
       upper_ci=full_sample_ar5_pred$pred+1.96*full_sample_ar5_pred$se)


write.csv(arima_predial_fullsample_pred,"arima_predial_pred_2020_2028.csv",row.names = F)
