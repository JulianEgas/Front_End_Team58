library(tidyverse)
library(forecast)
library(urca)
library(readxl)
library(zoo)
library(tempdisagg)


# Desempleo ---------------------------------------------------------------



desempleo_medellin <- read_excel("Desempleo Medellín AM.xlsx")


desempleo_medellin_trimestre <- desempleo_medellin %>% mutate(
  trimestre=case_when(
    
    str_detect(`TrimestreMóvil`,"(?i)ene-mar")~"01",
    str_detect(`TrimestreMóvil`,"(?i)abr-jun")~"02",
    str_detect(`TrimestreMóvil`,"(?i)jul-sep")~"03",
    str_detect(`TrimestreMóvil`,"(?i)oct-dic")~"04")
) %>% filter(!is.na(trimestre)) %>% 
  mutate(fecha=paste(`Año (Mes comienzo)`,trimestre,sep=""))



write.csv(desempleo_medellin_trimestre,"desempleo_medellin_trimestre.csv")



time_series_desempleo <- ts(desempleo_medellin_trimestre$tasa_desempleo,frequency = 4,start=c(2001,1))
plot(time_series_desempleo)
Acf(time_series_desempleo)
Pacf(time_series_desempleo)

summary(ur.df(time_series_desempleo,lags = 2))
#la estadística de prueba está a la derecha por tanto no se rechaza la hipótesis nula de raiz unitaria

summary(ur.df(diff(time_series_desempleo,1),lags = 2))
#la estadística de prueba está a la izquierda por tanto se rechaza la hipótesis nula de raíz unitaria
Acf(diff(time_series_desempleo,1))
Pacf(diff(time_series_desempleo,1))

#CONTIENE UNA RAÍZ UNITARIA



#Serie mensual


time_series_desempleo_mensual <- td(time_series_desempleo~1,to=12,method="denton-cholette")
time_series_desempleo_mensual <- ts(predict(time_series_desempleo_mensual),frequency = 12,start=c(2001,1))
plot(time_series_desempleo_mensual)
Acf(time_series_desempleo_mensual)
Pacf(time_series_desempleo_mensual)


summary(ur.df(time_series_desempleo_mensual,lags = 8))
#Hay raiz unitaria
summary(ur.df(diff(time_series_desempleo_mensual,1),lags = 8))



# Inflación ---------------------------------------------------------------


inflacion_medellin <- read_excel("ipc_med_2000_2021.xlsx")

time_series_inflacion <- ts(inflacion_medellin[,3],start=c(2000,1),freq=12)
plot(time_series_inflacion)
Acf(time_series_inflacion)
Pacf(time_series_inflacion)


summary(ur.df(time_series_inflacion,lags=1))
#Se rechaza la hipótesis nula de raíz unitaria


#a serie trimestral

time_series_inflacion_tri <- aggregate(time_series_inflacion,nfrequency=4)
plot(time_series_inflacion_tri)
Acf(time_series_inflacion_tri)
Pacf(time_series_inflacion_tri)

summary(ur.df(time_series_inflacion_tri,lags=4))# no estacionaria
summary(ur.df(diff(time_series_inflacion_tri,1),lags=3))# estacionaria

# Aeropuerto --------------------------------------------------------------



llegadas_aeropuerto <- read_excel("Passengers_in_MDE_00_21.xlsx")

time_series_aeropuerto <- ts(as.double(llegadas_aeropuerto[["pasajeros"]]),start=c(2000,1),freq=12)
plot(time_series_aeropuerto)
Acf(time_series_aeropuerto)
Pacf(time_series_aeropuerto)


summary(ur.df(time_series_aeropuerto,lags=1))
#No se rechaza hipótesis nula de raíz unitaria

summary(ur.df(diff(time_series_aeropuerto,1),lags=1))
#Se rechaza hipótesis nula de raíz unitaria

time_series_aeropuerto_tri <-aggregate(time_series_aeropuerto,nfrequency=4)
plot(time_series_aeropuerto_tri)
Acf(time_series_aeropuerto_tri)
Pacf(time_series_aeropuerto_tri)

summary(ur.df(time_series_aeropuerto_tri,lags=3))# no estacionaria
summary(ur.df(diff(time_series_aeropuerto_tri,1),lags=1))# estacionaria


#Avaluo predial -----------------------------------------------------




predial_factura_2000_2020 <- tibble()

for (i in 1:21){
  route <- paste(2000+i-1,".xlsx",sep="")
  database_per_year <- read_excel(route)
  
  database_per_year$year <- 2000+i-1
  database_per_year<- database_per_year[,c("year","vlr_tot_avaluo")]
  
  
  predial_factura_2000_2020 <- bind_rows(
    predial_factura_2000_2020,
    database_per_year
  )
}


yearly_predial_factura_2001_2020 <- predial_factura_2000_2020 %>% group_by(year) %>% summarise(avaluo=sum(vlr_tot_avaluo,na.rm=T)) %>% filter(year!=2001)

yearly_predial_factura_2001_2020 <- ts(yearly_predial_factura_2001_2020,start=c(2001,1),frequency=1)


#qrtr_predial_factura_2000_2020 <- 


predial_ts_2001_2020 <- ts(complete_predial[1:80],start=c(2001,1),frequency=4)


td(yearly_predial_factura_2001_2020~predial_ts_2001_2020, to=4,method="denton-cholette")



  # ADL MODELS --------------------------------------------------------------


#--------------------------------------------------------------Predial-------------------------------------

complete_predial <- read_csv("C:/Users/josej/OneDrive - Universidad de Antioquia/Escritorio/DS4A/Proyecto/PROYECTO/Predial nuevos datos/complete_predial.csv")
complete_predial <- ts(complete_predial[38:119,3],start=c(2001,1),frequency=4)

train=1:60#minus the one used for the lag



time_series_desempleo
time_series_inflacion_tri_2 <- time_series_inflacion_tri[5:86]
time_series_aeropuerto_tri_2 <- time_series_aeropuerto_tri[5:86]

dy_complete_predial <- diff(complete_predial[1:80],2)
dy_time_series_desempleo <- diff(time_series_desempleo,1)
dy_time_series_inflacion_tri <- diff(time_series_inflacion_tri_2,1)
dy_time_series_aeropuerto_tri <- diff(time_series_aeropuerto_tri_2,1)

#library(vctrs)
d1=vec_c(rep(c(1,0,0,0),20))
d2=vec_c(rep(c(0,1,0,0),20))
d3=vec_c(rep(c(0,0,1,0),20))
d4=vec_c(rep(c(0,0,0,1),20))
#

data_predial=tibble(
  d_pago_predial=dy_complete_predial,
  d_desempleo=dy_time_series_desempleo[2:79],
  d_inflacion=dy_time_series_inflacion_tri[2:79],
  d_llegadas_aero=dy_time_series_aeropuerto_tri[2:79]
)


data_predial=tibble(
  "complete_predial1"=complete_predial[1:80],
  "aeropuerto1"=time_series_aeropuerto_tri_2[1:80]
  
)

#write.csv(data_adl,"adl_trainning.csv")

#adl_predial <- lm(d_pago_predial~-1+d_desempleo+d_inflacion+d_llegadas_aero+d1+d2+d3+d4,
#                  data=data_predial[train,])




adl_predial <- lm(complete_predial1~1+stats::lag(complete_predial1,2)+aeropuerto1+stats::lag(aeropuerto1,1),
                  data=data_predial[train,])


summary(adl_predial)

plot(resid(adl_predial),type="l")
Acf(resid(adl_predial))
Pacf(resid(adl_predial))
summary(ur.df(resid(adl_predial)))#estacionarios

#prediction

new_data=tibble(
  "complete_predial1"=as.numeric(complete_predial)[1:80][-train],
  "aeropuerto1"=time_series_aeropuerto_tri_2[1:80][-train])


new_data=tibble(
  d_desempleo=time_series_desempleo[2:79][-train],
  d_inflacion=time_series_inflacion_tri[2:79][-train],
  d_llegadas_aero=time_series_aeropuerto_tri[2:79][-train],
  d1=d1[2:79][-train],
  d2=d2[2:79][-train],
  d3=d3[2:79][-train],
  d4=d4[2:79][-train]
  
)

  
adl_predial_pred <- predict(adl_predial,newdata=new_data,se.fit=T)


write.csv(adl_predial_pred,"adl_predial_prediction_2016_2020.csv")



predicted_predial <- ts(adl_predial_pred$fit,start=c(2016,1),freq=4)
plot(complete_predial,type="l")
lines(predicted_predial,col="red")



sqrt(mean((data$pago_predial[-train] - adl_predial_pred$fit)^2))
#405557804551

#error
#405557804551/1000000
#30727552479/1000000




#-------------------------------------------------------------ICA---------------------

complete_ica <- read_csv("C:/Users/josej/OneDrive - Universidad de Antioquia/Escritorio/DS4A/Proyecto/PROYECTO/ICA nuevos datos/complete_ica.csv", 
                         col_types = cols(...1 = col_skip(), freq = col_skip()))



complete_ica <- ts(complete_ica[24:263,2],start=c(2001,1),frequency=12)

train=1:179



tm_desempleo_m_2001_2020 <- time_series_desempleo_mensual[1:240]
tm_inflacion_m_2001_2020 <- time_series_inflacion[13:252]
tm_aeropuerto_m_2001_2020 <- time_series_aeropuerto[13:252]


dy_complete_ica <- diff(complete_ica,1)
d_tm_m_desempleo <- diff(tm_desempleo_m_2001_2020,1)
d_tm_m_inflacion <- diff(tm_inflacion_m_2001_2020,1)
d_tm_m_aeropuerto <- diff(tm_aeropuerto_m_2001_2020,1)



data_ica=tibble(
  pago_ica=complete_ica[2:240],
  
  desempleo=tm_desempleo_m_2001_2020[2:240],
  inflacion=tm_inflacion_m_2001_2020[2:240],
  llegadas=tm_aeropuerto_m_2001_2020[2:240],
  
  
  dy_complete_ica,
  d_tm_m_desempleo,
  d_tm_m_inflacion,
  d_tm_m_aeropuerto
  
)


adl_ica <- lm(dy_complete_ica~1+
                d_tm_m_aeropuerto,data=data_ica[train,])
summary(adl_ica)

plot(resid(adl_ica),type="l")
summary(ur.df(resid(adl_ica)))#estacionarios

#prediction


new_data=tibble(
  d_tm_m_desempleo=tm_desempleo_m_2001_2020[2:240][-train],
  d_tm_m_inflacion=tm_inflacion_m_2001_2020[2:240][-train],
  d_tm_m_aeropuerto=tm_aeropuerto_m_2001_2020[2:240][-train])




adl_ica_pred <- predict(adl_ica,newdata=new_data,se.fit=T)

write.csv(adl_ica_pred,"adl_ica_prediction_2015_2020.csv")


sqrt(mean((data_ica$pago_ica[-train] - adl_ica_pred$fit)^2))

#3910402126
