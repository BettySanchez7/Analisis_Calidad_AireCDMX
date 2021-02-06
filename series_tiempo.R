# ANÁLSIS DE SERIES DE TIEMPO #

library(dplyr)
library(ggplot2)
library(TSA)
library(lubridate) #Paquete para manipular fechas
library(reshape2)

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

############################### LECTURA ARCHIVOS ###########################################
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

################################## SERIES DE TIEMPO ###########################################
#Zona Noroeste
NO.CO.ts<-ts(CO$NO, start = 2005, frequency = 12)
NO.NO2.ts<-ts(NO2$NO, start = 2005, frequency = 12)
NO.SO2.ts<-ts(SO2$NO,start = 2005,  frequency = 12)
NO.O3.ts<-ts(O3$NO,start = 2005,  frequency = 12)
NO.PM10.ts<-ts(PM10$NO, start = 2005, frequency = 12)

#ZONA NORESTE
NE.CO.ts<-ts(CO$NE, start = 2005, frequency = 12)
NE.NO2.ts<-ts(NO2$NE, start = 2005, frequency = 12)
NE.SO2.ts<-ts(SO2$NE,start = 2005,  frequency = 12)
NE.O3.ts<-ts(O3$NE, start = 2005, frequency = 12)
NE.PM10.ts<-ts(PM10$NE,start = 2005,  frequency = 12)

#ZONA CENTRO
CE.CO.ts<-ts(CO$CE, start = 2005, frequency = 12)
CE.NO2.ts<-ts(NO2$CE,start = 2005,  frequency = 12)
CE.SO2.ts<-ts(SO2$CE,start = 2005,  frequency = 12)
CE.O3.ts<-ts(O3$CE,start = 2005,  frequency = 12)
CE.PM10.ts<-ts(PM10$CE,start = 2005,  frequency = 12)

#ZONA SUROESTE
SO.CO.ts<-ts(CO$SO,start = 2005,  frequency = 12)
SO.NO2.ts<-ts(NO2$SO,start = 2005,  frequency = 12)
SO.SO2.ts<-ts(SO2$SO,start = 2005,  frequency = 12)
SO.O3.ts<-ts(O3$SO, start = 2005, frequency = 12)
SO.PM10.ts<-ts(PM10$SO,start = 2005,  frequency = 12)

#ZONA SURESTE
SE.CO.ts<-  ts(CO$SE, start = 2005, frequency = 12)
SE.NO2.ts<- ts(NO2$SE,start = 2005,  frequency = 12)
SE.SO2.ts<- ts(SO2$SE,start = 2005,  frequency = 12)
SE.O3.ts<-  ts(O3$SE, start = 2005, frequency = 12)
SE.PM10.ts<-ts(PM10$SE,start = 2005,  frequency = 12)


############################ GRAFICANDO SERIES DE TIEMPO ######################################
#Para la graficación de multiples series de tiempo en un mismo recuadro
#Los juntamos todos en una sola tabla primero
TS.CO <- melt(CO, id.vars = "mes")
TS.NO2 <- melt(NO2, id.vars = "mes")
TS.O3 <- melt(O3, id.vars = "mes")
TS.PM10 <- melt(PM10, id.vars = "mes")
TS.SO2 <- melt(SO2, id.vars = "mes")

#Gráfica total CO
ggplot(TS.CO, aes(x = mes, y = value, color = variable)) +
  geom_line(size = 0.5)+
  scale_color_manual(values = c("green", "blue", "red","yellow","pink"))

##Gŕaficas individuales (solo para mirar cuáles nos interesan)
#CO
layout(1:3)
ts.plot(NO.CO.ts)
ts.plot(NE.CO.ts)
ts.plot(CE.CO.ts)
ts.plot(SO.CO.ts)
ts.plot(SE.CO.ts)
dev.off()

#NO2
layout(1:3)
ts.plot(NO.NO2.ts)
ts.plot(NE.NO2.ts)
ts.plot(CE.NO2.ts)
ts.plot(SO.NO2.ts)
ts.plot(SE.NO2.ts)
dev.off()

#SO2
layout(1:3)
ts.plot(NO.SO2.ts)
ts.plot(NE.SO2.ts)
ts.plot(CE.SO2.ts)
ts.plot(SO.SO2.ts)
ts.plot(SE.SO2.ts)
dev.off()

#O3
layout(1:3)
ts.plot(NO.O3.ts)
ts.plot(NE.O3.ts)
ts.plot(CE.O3.ts)
ts.plot(SO.O3.ts)
ts.plot(SE.O3.ts)
dev.off()

#PM10
layout(1:3)
ts.plot(NO.PM10.ts)
ts.plot(NE.PM10.ts)
ts.plot(CE.PM10.ts)
ts.plot(SO.PM10.ts)
ts.plot(SE.PM10.ts)
dev.off()

#################################### GRÁFICAS CON TENDENCIA ####################################

################## Centro (CE)
#CO
CE.CO.comp <-decompose(CE.CO.ts,type = "mult")
#plot(CE.CO.comp)
trendCO <- as.numeric(CE.CO.comp$trend)

ggplot(CO,aes(x=mes)) +
  geom_line(aes(y=CE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendCO,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black","#d35400")) +
  labs(title = "Concentración de CO - Zona Centro", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(0,30)+
  theme_classic()

#NO2
CE.NO2.comp <-decompose(CE.NO2.ts,type = "mult")
#plot(CE.CO.comp)
trendNO2 <- as.numeric(CE.NO2.comp$trend)

ggplot(NO2,aes(x=mes)) +
  geom_line(aes(y=CE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendNO2,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#2471a3")) +
  labs(title = "Concentración de NO2 - Zona Centro", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(5,35)+
  theme_classic()

#SO2
CE.SO2.comp <-decompose(CE.SO2.ts,type = "mult")
#plot(CE.CO.comp)
trendSO2 <- as.numeric(CE.SO2.comp$trend)

ggplot(SO2,aes(x=mes)) +
  geom_line(aes(y=CE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendSO2,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#1e8449")) +
  labs(title = "Concentración de SO2 - Zona Centro", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(0,30)+
  theme_classic()

#O3
CE.O3.comp <-decompose(CE.O3.ts,type = "mult")
#plot(CE.CO.comp)
trendO3 <- as.numeric(CE.O3.comp$trend)

ggplot(O3,aes(x=mes)) +
  geom_line(aes(y=CE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendO3,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#e74c3c")) +
  labs(title = "Concentración de O3 - Zona Centro", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(15,60)+
  theme_classic()

#PM10
CE.PM10.comp <-decompose(CE.PM10.ts,type = "mult")
#plot(CE.CO.comp)
trendPM10 <- as.numeric(CE.PM10.comp$trend)

ggplot(PM10,aes(x=mes)) +
  geom_line(aes(y=CE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendPM10,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#7b241c")) +
  labs(title = "Concentración de PM10 - Zona Centro", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(15,105)+
  theme_classic()

################## Noroeste (NO)
#CO
NO.CO.comp <-decompose(NO.CO.ts,type = "mult")
#plot(NO.CO.comp)
trendCO <- as.numeric(NO.CO.comp$trend)

ggplot(CO,aes(x=mes)) +
  geom_line(aes(y=NO,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendCO,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black","#d35400")) +
  labs(title = "Concentración de CO - Zona Noroeste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(0,30)+
  theme_classic()


#NO2
NO.NO2.comp <-decompose(NO.NO2.ts,type = "mult")
#plot(NO.CO.comp)
trendNO2 <- as.numeric(NO.NO2.comp$trend)

ggplot(NO2,aes(x=mes)) +
  geom_line(aes(y=NO,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendNO2,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#2471a3")) +
  labs(title = "Concentración de NO2 - Zona Noroeste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(5,35)+
  theme_classic()

#SO2
NO.SO2.comp <-decompose(NO.SO2.ts,type = "mult")
#plot(NO.CO.comp)
trendSO2 <- as.numeric(NO.SO2.comp$trend)

ggplot(SO2,aes(x=mes)) +
  geom_line(aes(y=NO,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendSO2,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#1e8449")) +
  labs(title = "Concentración de SO2 - Zona Noroeste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(0,30)+
  theme_classic()

#O3
NO.O3.comp <-decompose(NO.O3.ts,type = "mult")
#plot(NO.CO.comp)
trendO3 <- as.numeric(NO.O3.comp$trend)

ggplot(O3,aes(x=mes)) +
  geom_line(aes(y=NO,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendO3,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#e74c3c")) +
  labs(title = "Concentración de O3 - Zona Noroeste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(15,60)+
  theme_classic()

#PM10
NO.PM10.comp <-decompose(NO.PM10.ts,type = "mult")
#plot(NO.CO.comp)
trendPM10 <- as.numeric(NO.PM10.comp$trend)

ggplot(PM10,aes(x=mes)) +
  geom_line(aes(y=NO,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendPM10,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#7b241c")) +
  labs(title = "Concentración de PM10 - Zona Noroeste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(15,105)+
  theme_classic()

################## Noreste (NE)
#CO
NE.CO.comp <-decompose(NE.CO.ts,type = "mult")
#plot(NE.CO.comp)
trendCO <- as.numeric(NE.CO.comp$trend)

ggplot(CO,aes(x=mes)) +
  geom_line(aes(y=NE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendCO,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black","#d35400")) +
  labs(title = "Concentración de CO - Zona Noreste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(0,30)+
  theme_classic()


#NO2
NE.NO2.comp <-decompose(NE.NO2.ts,type = "mult")
#plot(NE.CO.comp)
trendNO2 <- as.numeric(NE.NO2.comp$trend)

ggplot(NO2,aes(x=mes)) +
  geom_line(aes(y=NE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendNO2,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#2471a3")) +
  labs(title = "Concentración de NO2 - Zona Noreste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(5,35)+
  theme_classic()

#SO2
NE.SO2.comp <-decompose(NE.SO2.ts,type = "mult")
#plot(NE.CO.comp)
trendSO2 <- as.numeric(NE.SO2.comp$trend)

ggplot(SO2,aes(x=mes)) +
  geom_line(aes(y=NE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendSO2,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#1e8449")) +
  labs(title = "Concentración de SO2 - Zona Noreste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(0,30)+
  theme_classic()

#O3
NE.O3.comp <-decompose(NE.O3.ts,type = "mult")
#plot(NE.CO.comp)
trendO3 <- as.numeric(NE.O3.comp$trend)

ggplot(O3,aes(x=mes)) +
  geom_line(aes(y=NE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendO3,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#e74c3c")) +
  labs(title = "Concentración de O3 - Zona Noreste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(15,60)+
  theme_classic()

#PM10
NE.PM10.comp <-decompose(NE.PM10.ts,type = "mult")
#plot(NE.CO.comp)
trendPM10 <- as.numeric(NE.PM10.comp$trend)

ggplot(PM10,aes(x=mes)) +
  geom_line(aes(y=NE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendPM10,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#7b241c")) +
  labs(title = "Concentración de PM10 - Zona Noreste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(15,120)+
  theme_classic()

################## Suroeste (SO)
#CO
SO.CO.comp <-decompose(SO.CO.ts,type = "mult")
#plot(SO.CO.comp)
trendCO <- as.numeric(SO.CO.comp$trend)

ggplot(CO,aes(x=mes)) +
  geom_line(aes(y=SO,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendCO,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black","#d35400")) +
  labs(title = "Concentración de CO - Zona Suroeste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(0,30)+
  theme_classic()


#NO2
SO.NO2.comp <-decompose(SO.NO2.ts,type = "mult")
#plot(SO.CO.comp)
trendNO2 <- as.numeric(SO.NO2.comp$trend)

ggplot(NO2,aes(x=mes)) +
  geom_line(aes(y=SO,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendNO2,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#2471a3")) +
  labs(title = "Concentración de NO2 - Zona Suroeste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(5,35)+
  theme_classic()

#SO2
SO.SO2.comp <-decompose(SO.SO2.ts,type = "mult")
#plot(SO.CO.comp)
trendSO2 <- as.numeric(SO.SO2.comp$trend)

ggplot(SO2,aes(x=mes)) +
  geom_line(aes(y=SO,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendSO2,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#1e8449")) +
  labs(title = "Concentración de SO2 - Zona Suroeste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(0,30)+
  theme_classic()

#O3
SO.O3.comp <-decompose(SO.O3.ts,type = "mult")
#plot(SO.CO.comp)
trendO3 <- as.numeric(SO.O3.comp$trend)

ggplot(O3,aes(x=mes)) +
  geom_line(aes(y=SO,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendO3,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#e74c3c")) +
  labs(title = "Concentración de O3 - Zona Suroeste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(15,60)+
  theme_classic()

#PM10
SO.PM10.comp <-decompose(SO.PM10.ts,type = "mult")
#plot(SO.CO.comp)
trendPM10 <- as.numeric(SO.PM10.comp$trend)

ggplot(PM10,aes(x=mes)) +
  geom_line(aes(y=SO,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendPM10,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#7b241c")) +
  labs(title = "Concentración de PM10 - Zona Suroeste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(15,105)+
  theme_classic()

################## Sureste (SE)
#CO
SE.CO.comp <-decompose(SE.CO.ts,type = "mult")
#plot(SE.CO.comp)
trendCO <- as.numeric(SE.CO.comp$trend)

ggplot(CO,aes(x=mes)) +
  geom_line(aes(y=SE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendCO,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black","#d35400")) +
  labs(title = "Concentración de CO - Zona Sureste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(0,30)+
  theme_classic()

#NO2
SE.NO2.comp <-decompose(SE.NO2.ts,type = "mult")
#plot(SE.CO.comp)
trendNO2 <- as.numeric(SE.NO2.comp$trend)

ggplot(NO2,aes(x=mes)) +
  geom_line(aes(y=SE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendNO2,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#2471a3")) +
  labs(title = "Concentración de NO2 - Zona Sureste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(5,35)+
  theme_classic()

#SO2
SE.SO2.comp <-decompose(SE.SO2.ts,type = "mult")
#plot(SE.CO.comp)
trendSO2 <- as.numeric(SE.SO2.comp$trend)

ggplot(SO2,aes(x=mes)) +
  geom_line(aes(y=SE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendSO2,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#1e8449")) +
  labs(title = "Concentración de SO2 - Zona Sureste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(0,30)+
  theme_classic()

#O3
SE.O3.comp <-decompose(SE.O3.ts,type = "mult")
#plot(SE.CO.comp)
trendO3 <- as.numeric(SE.O3.comp$trend)

ggplot(O3,aes(x=mes)) +
  geom_line(aes(y=SE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendO3,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#e74c3c")) +
  labs(title = "Concentración de O3 - Zona Sureste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(15,60)+
  theme_classic()

#PM10
SE.PM10.comp <-decompose(SE.PM10.ts,type = "mult")
#plot(SE.CO.comp)
trendPM10 <- as.numeric(SE.PM10.comp$trend)

ggplot(PM10,aes(x=mes)) +
  geom_line(aes(y=SE,colour = "Serie de tiempo"))+
  geom_line(aes(y=trendPM10,colour = "Tendencia"), lwd = 0.9)+
  scale_colour_manual("", breaks = c("Serie de tiempo", "Tendencia"),
                      values = c("black", "#7b241c")) +
  labs(title = "Concentración de PM10 - Zona Sureste", y= "Concentración [unidades IMECA]",
       x = "Tiempo",subtitle = "Serie de tiempo de Enero 2005 - Diciembre 2020")+
  ylim(15,105)+
  theme_classic()