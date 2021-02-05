# ANÁLSIS DE SERIES DE TIEMPO #

library(dplyr)
library(TSA)
library(lubridate) #Paquete para manipular fechas
library(naniar) #libreria quita -99 por NA

#Carpeta de trabajo
getwd()


### Función de agrupación de promedios mensuales ###
groupmeses <- function(dataframe){
  data_mes <- mutate(dataframe, Fecha = as.Date(Fecha, "%Y-%m-%d"))
  data_mes <- mutate(data_mes, mes= floor_date(data_mes$Fecha, "month"))
  result <- data_mes %>%  group_by(mes) %>%
    summarise(NO=mean(NO, na.rm = TRUE), NE=mean(NE, na.rm = TRUE),
              CE=mean(CE, na.rm = TRUE),SO=mean(SO, na.rm = TRUE),
              SE=mean(SE, na.rm = TRUE))
  return(result)
}


### LECTURA ARCHIVOS ###
CO <- read.csv("datos_IMECA/datos_limpios/CO.csv")
NO2 <- read.csv("datos_IMECA/datos_limpios/NO2.csv")
SO2 <- read.csv("datos_IMECA/datos_limpios/SO2.csv")
O3 <- read.csv("datos_IMECA/datos_limpios/O3.csv")
PM10 <- read.csv("datos_IMECA/datos_limpios/PM10.csv")

#Reasignación de nombres en columnas
tags<-c("Fecha","NO","NE","CE","SO","SE")
names(CO)<-tags
names(NO2)<-tags
names(SO2)<-tags
names(O3)<-tags
names(PM10)<-tags

#Agrupación de Data por mes
CO <- groupmeses(CO)
NO2 <- groupmeses(NO2)
SO2 <- groupmeses(SO2)
O3 <- groupmeses(O3)
PM10 <- groupmeses(PM10)

#Eliminando última fila (contiene NA)
CO <- CO[1:192,]
NO2 <- NO2[1:192,]
SO2 <- SO2[1:192,]
O3 <- O3[1:192,]
PM10 <- PM10[1:192,]


### SERIES DE TIEMPO ###
#Zona Noroeste
NO.CO.ts<-ts(CO$NO, start = 2005, frequency = 12)
NO.NO2.ts<-ts(NO2$NO, start = 2005, frequency = 12)
NO.SO2.ts<-ts(SO2$NO,start = 2005,  frequency = 12)
NO.03.ts<-ts(O3$NO,start = 2005,  frequency = 12)
NO.PM10.ts<-ts(PM10$NO, start = 2005, frequency = 12)

#ZONA NORESTE
NE.CO.ts<-ts(CO$NE, start = 2005, frequency = 12)
NE.NO2.ts<-ts(NO2$NE, start = 2005, frequency = 12)
NE.SO2.ts<-ts(SO2$NE,start = 2005,  frequency = 12)
NE.03.ts<-ts(O3$NE, start = 2005, frequency = 12)
NE.PM10.ts<-ts(PM10$NE,start = 2005,  frequency = 12)

#ZONA CENTRO
CE.CO.ts<-ts(CO$CE, start = 2005, frequency = 12)
CE.NO2.ts<-ts(NO2$CE,start = 2005,  frequency = 12)
CE.SO2.ts<-ts(SO2$CE,start = 2005,  frequency = 12)
CE.03.ts<-ts(O3$CE,start = 2005,  frequency = 12)
CE.PM10.ts<-ts(PM10$CE,start = 2005,  frequency = 12)

#ZONA SUROESTE
SO.CO.ts<-ts(CO$SO,start = 2005,  frequency = 12)
SO.NO2.ts<-ts(NO2$SO,start = 2005,  frequency = 12)
SO.SO2.ts<-ts(SO2$SO,start = 2005,  frequency = 12)
SO.03.ts<-ts(O3$SO, start = 2005, frequency = 12)
SO.PM10.ts<-ts(PM10$SO,start = 2005,  frequency = 12)

#ZONA SURESTE
SE.CO.ts<-  ts(CO$SE, start = 2005, frequency = 12)
SE.NO2.ts<- ts(NO2$SE,start = 2005,  frequency = 12)
SE.SO2.ts<- ts(SO2$SE,start = 2005,  frequency = 12)
SE.03.ts<-  ts(O3$SE, start = 2005, frequency = 12)
SE.PM10.ts<-ts(PM10$SE,start = 2005,  frequency = 12)

