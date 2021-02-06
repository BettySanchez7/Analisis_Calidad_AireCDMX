

library(dplyr)
library(ggplot2)
library(egg)
library(lubridate)

getwd()
#Aqui va la ruta donde estÃ¡n alojados archivos
#setwd("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/Grafica1")

fecha <- function(dataframe){
  dataframe$Fecha <- as.Date(dataframe$Fecha) %>% format("%b") %>%
    factor(levels = c("ene.", "feb.", "mar.", "abr.", "may.", "jun.",
                      "jul.", "ago.", "sep.", "oct.", "nov.", "dic."), ordered = TRUE)
  return(dataframe$Fecha)
}

Media_CO <- read.csv("Media_CO.csv")
Media_NO2<-read.csv("Media_NO2.csv")
Media_O3<-read.csv("Media_O3.csv")
Media_PM10<-read.csv("Media_PM10.csv")
Media_SO2<-read.csv("Media_SO2.csv")
tags<-c("Fecha","NO","NE","CE","SO","SE")
names(Media_CO)<-tags
names(Media_NO2)<-tags
names(Media_SO2)<-tags
names(Media_O3)<-tags
names(Media_PM10)<-tags

###DATA FRAMES DE AÑOS 2019 Y 2020
CO_2019 <- Media_CO[169:180,]
CO_2020 <- Media_CO[181:192,]
NO2_2019 <- Media_NO2[169:180,]
NO2_2020 <- Media_NO2[181:192,]
O3_2019 <- Media_O3[169:180,]
O3_2020 <- Media_O3[181:192,]
PM10_2019 <- Media_PM10[169:180,]
PM10_2020 <- Media_PM10[181:192,]
SO2_2019 <- Media_SO2[169:180,]
SO2_2020 <- Media_SO2[181:192,]

###Cambio de formato a fechas a letra de meses

CO_2019$Fecha<- fecha(CO_2019)
CO_2020$Fecha <- fecha(CO_2020)
NO2_2019$Fecha <- fecha(NO2_2019)
NO2_2020$Fecha <- fecha(NO2_2020)
O3_2019$Fecha <- fecha(O3_2019)
O3_2020$Fecha <- fecha(O3_2020)
PM10_2019$Fecha <- fecha(PM10_2019)
PM10_2020$Fecha <- fecha(PM10_2020)
SO2_2019$Fecha <- fecha(SO2_2019)
SO2_2020$Fecha <- fecha(SO2_2020)

write.csv(CO_2019,"data/CO_2019.csv",row.names = FALSE)
write.csv(CO_2020,"data/CO_2020.csv",row.names = FALSE)
write.csv(NO2_2019,"data/NO2_2019.csv",row.names = FALSE)
write.csv(NO2_2020,"data/NO2_2020.csv",row.names = FALSE)
write.csv(O3_2019,"data/O3_2019.csv",row.names = FALSE)
write.csv(O3_2020,"data/O3_2020.csv",row.names = FALSE)
write.csv(PM10_2019,"data/PM10_2019.csv",row.names = FALSE)
write.csv(PM10_2020,"data/PM10_2020.csv",row.names = FALSE)
write.csv(SO2_2019,"data/SO2_2019.csv",row.names = FALSE)
write.csv(SO2_2020,"data/SO2_202O.csv",row.names = FALSE)
