library(dplyr)
library(ggplot2)
install.packages("egg")
library(egg)
library(lubridate)
getwd()
#Aqui va la ruta donde estÃ¡n alojados archivos
setwd("C:/Users/bety-/Documents/Repositorios/Analisis_Calidad_AireCDMX/datos_IMECA/datos_limpios/promedios")

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

###Concentración de parámetros vs meses 2019

##Revisar como agrupar para mejor visualización (o escoger las zonas 
#más representativas, un dashboard para cambiar entre año y `parametro estaría bien`)
##-----------------NOROESTE 2019-------------------

ggplot(CO_2019, aes(x=Fecha, y=NO))+
         geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de CO",
       title = "Concentración de CO-Noroeste-2019")+
 theme_test() 
  
ggplot(NO2_2019, aes(x=Fecha, y=NO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de NO2",
       title = "Concentración de NO2-Noroeste-2019")+
  theme_test()

ggplot(O3_2019, aes(x=Fecha, y=NO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de O3",
       title = "Concentración de O3-Noroeste-2019")+
  theme_test()

ggplot(PM10_2019, aes(x=Fecha, y=NO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de PM10",
       title = "Concentración de PM10-Noroeste-2019")+
  theme_test()

ggplot(SO2_2019, aes(x=Fecha, y=NO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de SO2",
       title = "Concentración de SO2-Noroeste-2019")+
  theme_test()


##-----------------NORESTE 2019-------------------
ggplot(CO_2019, aes(x=Fecha, y=NE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de CO",
       title = "Concentración de CO-Noreste-2019")+
  theme_test() 

ggplot(NO2_2019, aes(x=Fecha, y=NE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de NO2",
       title = "Concentración de NO2-Noreste-2019")+
  theme_test()

ggplot(O3_2019, aes(x=Fecha, y=NE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de O3",
       title = "Concentración de O3-Noreste-2019")+
  theme_test()

ggplot(PM10_2019, aes(x=Fecha, y=NE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de PM10",
       title = "Concentración de PM10-Noreste-2019")+
  theme_test()

ggplot(SO2_2019, aes(x=Fecha, y=NE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de SO2",
       title = "Concentración de SO2-Noreste-2019")+
  theme_test()

##-----------------CENTRO 2019-------------------
ggplot(CO_2019, aes(x=Fecha, y=CE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de CO",
       title = "Concentración de CO-Centro-2019")+
  theme_test() 

ggplot(NO2_2019, aes(x=Fecha, y=CE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de NO2",
       title = "Concentración de NO2-Centro-2019")+
  theme_test()

ggplot(O3_2019, aes(x=Fecha, y=CE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de O3",
       title = "Concentración de O3-Centro-2019")+
  theme_test()

ggplot(PM10_2019, aes(x=Fecha, y=CE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de PM10",
       title = "Concentración de PM10-Centro-2019")+
  theme_test()

ggplot(SO2_2019, aes(x=Fecha, y=CE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de SO2",
       title = "Concentración de SO2-Centro-2019")+
  theme_test()

##-----------------SUROESTE 2019-------------------
ggplot(CO_2019, aes(x=Fecha, y=SO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de CO",
       title = "Concentración de CO-Suroeste-2019")+
  theme_test() 

ggplot(NO2_2019, aes(x=Fecha, y=SO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de NO2",
       title = "Concentración de NO2-Suroeste-2019")+
  theme_test()

ggplot(O3_2019, aes(x=Fecha, y=SO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de O3",
       title = "Concentración de O3-Suroeste-2019")+
  theme_test()

ggplot(PM10_2019, aes(x=Fecha, y=SO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de PM10",
       title = "Concentración de PM10-Suroeste-2019")+
  theme_test()

ggplot(SO2_2019, aes(x=Fecha, y=SO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de SO2",
       title = "Concentración de SO2-Suroeste-2019")+
  theme_test()

##-----------------SURESTE 2019-------------------
ggplot(CO_2019, aes(x=Fecha, y=SE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de CO",
       title = "Concentración de CO-Sureste-2019")+
  theme_test() 

ggplot(NO2_2019, aes(x=Fecha, y=SE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de NO2",
       title = "Concentración de NO2-Sureste-2019")+
  theme_test()

ggplot(O3_2019, aes(x=Fecha, y=SE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de O3",
       title = "Concentración de O3-Sureste-2019")+
  theme_test()

ggplot(PM10_2019, aes(x=Fecha, y=SE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de PM10",
       title = "Concentración de PM10-Sureste-2019")+
  theme_test()

ggplot(SO2_2019, aes(x=Fecha, y=SE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de SO2",
       title = "Concentración de SO2-Sureste-2019")+
  theme_test()


##-----------------NORESTE 2020-------------------
ggplot(CO_2020, aes(x=Fecha, y=NE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de CO",
       title = "Concentración de CO-Noreste-2020")+
  theme_test() 

ggplot(NO2_2020, aes(x=Fecha, y=NE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de NO2",
       title = "Concentración de NO2-Noreste-2020")+
  theme_test()

ggplot(O3_2020, aes(x=Fecha, y=NE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de O3",
       title = "Concentración de O3-Noreste-2020")+
  theme_test()

ggplot(PM10_2020, aes(x=Fecha, y=NE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de PM10",
       title = "Concentración de PM10-Noreste-2020")+
  theme_test()

ggplot(SO2_2020, aes(x=Fecha, y=NE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de SO2",
       title = "Concentración de SO2-Noreste-2020")+
  theme_test()

##-----------------CENTRO 2020-------------------
ggplot(CO_2020, aes(x=Fecha, y=CE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de CO",
       title = "Concentración de CO-Centro-2020")+
  theme_test() 

ggplot(NO2_2020, aes(x=Fecha, y=CE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de NO2",
       title = "Concentración de NO2-Centro-2020")+
  theme_test()

ggplot(O3_2020, aes(x=Fecha, y=CE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de O3",
       title = "Concentración de O3-Centro-2020")+
  theme_test()

ggplot(PM10_2020, aes(x=Fecha, y=CE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de PM10",
       title = "Concentración de PM10-Centro-2020")+
  theme_test()

ggplot(SO2_2020, aes(x=Fecha, y=CE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de SO2",
       title = "Concentración de SO2-Centro-2020")+
  theme_test()

##-----------------SUROESTE 2020-------------------
ggplot(CO_2020, aes(x=Fecha, y=SO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de CO",
       title = "Concentración de CO-Suroeste-2020")+
  theme_test() 

ggplot(NO2_2020, aes(x=Fecha, y=SO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de NO2",
       title = "Concentración de NO2-Suroeste-2020")+
  theme_test()

ggplot(O3_2020, aes(x=Fecha, y=SO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de O3",
       title = "Concentración de O3-Suroeste-2020")+
  theme_test()

ggplot(PM10_2020, aes(x=Fecha, y=SO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de PM10",
       title = "Concentración de PM10-Suroeste-2020")+
  theme_test()

ggplot(SO2_2020, aes(x=Fecha, y=SO))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de SO2",
       title = "Concentración de SO2-Suroeste-2020")+
  theme_test()

##-----------------SURESTE 2020-------------------
ggplot(CO_2020, aes(x=Fecha, y=SE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de CO",
       title = "Concentración de CO-Sureste-2020")+
  theme_test() 

ggplot(NO2_2020, aes(x=Fecha, y=SE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de NO2",
       title = "Concentración de NO2-Sureste-2020")+
  theme_test()

ggplot(O3_2020, aes(x=Fecha, y=SE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de O3",
       title = "Concentración de O3-Sureste-2020")+
  theme_test()

ggplot(PM10_2020, aes(x=Fecha, y=SE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de PM10",
       title = "Concentración de PM10-Sureste-2020")+
  theme_test()

ggplot(SO2_2020, aes(x=Fecha, y=SE))+
  geom_bar(stat="identity") + 
  labs(x = "Mes", y = "Concentración de SO2",
       title = "Concentración de SO2-Sureste-2020")+
  theme_test()
