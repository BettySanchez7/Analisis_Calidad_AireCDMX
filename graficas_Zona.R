library(ggplot2)
getwd()
setwd("C:/Users/ramze/Desktop/Analisis_Calidad_AireCDMX/datos_IMECA/datos_limpios/promedios")
#Obteniendo los datos
CO <- read.csv("Media_CO.csv")
NO2 <- read.csv("Media_NO2.csv")
O3 <- read.csv("Media_O3.csv")
PM10 <- read.csv("Media_PM10.csv")
SO2 <- read.csv("Media_SO2.csv")

#Seleccionando años 2019 y 2020
CO_2019 <- CO[169:180,]
CO_2020 <- CO[181:192,]
NO2_2019 <- NO2[169:180,]
NO2_2020 <- NO2[181:192,]
O3_2019 <- O3[169:180,]
O3_2020 <- O3[181:192,]
PM10_2019 <- PM10[169:180,]
PM10_2020 <- PM10[181:192,]
SO2_2019 <- SO2[169:180,]
SO2_2020 <- SO2[181:192,]

#Obteniendo promedios anuales por Zona
#CO 2019
PromedioCO2019 <- apply(CO_2019[,2:6], 2,mean)
PromedioCO2019 <- as.data.frame(PromedioCO2019)
zonas<-c("NO","NE","CE","SO","SE")
prom_anualCO2019 <- as.data.frame(cbind(PromedioCO2019,zonas))
prom_anualCO2019 <- mutate(prom_anualCO2019, Promedio = as.numeric(PromedioCO2019))

#CO 2020
PromedioCO2020 <- apply(CO_2020[,2:6], 2, mean)
PromedioCO2020 <- as.data.frame(PromedioCO2020)
prom_anualCO2020 <- as.data.frame(cbind(PromedioCO2020,zonas))
prom_anualCO2020 <- mutate(prom_anualCO2020, Promedio = as.numeric(PromedioCO2020))

#NO2 2019
PromedioNO2_2019 <- apply(NO2_2019[,2:6], 2, mean)
PromedioNO2_2019 <- as.data.frame(PromedioNO2_2019)
prom_anualNO2_2019 <- as.data.frame(cbind(PromedioNO2_2019,zonas))
prom_anualNO2_2019 <- mutate(prom_anualNO2_2019, Promedio = as.numeric(PromedioNO2_2019))

#NO2 2020
PromedioNO2_2020 <- apply(NO2_2020[,2:6], 2, mean)
PromedioNO2_2020 <- as.data.frame(PromedioNO2_2020)
prom_anualNO2_2020 <- as.data.frame(cbind(PromedioNO2_2020,zonas))
prom_anualNO2_2020 <- mutate(prom_anualNO2_2020, Promedio = as.numeric(PromedioNO2_2020))

#O3 2019
PromedioO3_2019 <- apply(O3_2019[,2:6], 2, mean)
PromedioO3_2019 <- as.data.frame(PromedioO3_2019)
prom_anualO3_2019 <- as.data.frame(cbind(PromedioO3_2019,zonas))
prom_anualO3_2019 <- mutate(prom_anualO3_2019, Promedio = as.numeric(PromedioO3_2019))

#03 2020
PromedioO3_2020 <- apply(O3_2020[,2:6], 2, mean)
PromedioO3_2020 <- as.data.frame(PromedioO3_2020)
prom_anualO3_2020 <- as.data.frame(cbind(PromedioO3_2020,zonas))
prom_anualO3_2020 <- mutate(prom_anualO3_2020, Promedio = as.numeric(PromedioO3_2020))

#PM10 2019
PromedioPM10_2019 <- apply(PM10_2019[,2:6], 2, mean)
PromedioPM10_2019 <- as.data.frame(PromedioPM10_2019)
prom_anualPM10_2019 <- as.data.frame(cbind(PromedioPM10_2019,zonas))
prom_anualPM10_2019 <- mutate(prom_anualPM10_2019, Promedio = as.numeric(PromedioPM10_2019))

#PM10 2020
PromedioPM10_2020 <- apply(PM10_2020[,2:6], 2, mean)
PromedioPM10_2020 <- as.data.frame(PromedioPM10_2020)
prom_anualPM10_2020 <- as.data.frame(cbind(PromedioPM10_2020,zonas))
prom_anualPM10_2020 <- mutate(prom_anualPM10_2020, Promedio = as.numeric(PromedioPM10_2020))

#SO2 2019
PromedioSO2_2019 <- apply(SO2_2019[,2:6], 2, mean)
PromedioSO2_2019 <- as.data.frame(PromedioSO2_2019)
prom_anualSO2_2019 <- as.data.frame(cbind(PromedioSO2_2019,zonas))
prom_anualSO2_2019 <- mutate(prom_anualSO2_2019, Promedio = as.numeric(PromedioSO2_2019))

#SO2 2020
PromedioSO2_2020 <- apply(SO2_2020[,2:6], 2, mean)
PromedioSO2_2020 <- as.data.frame(PromedioSO2_2020)
prom_anualSO2_2020 <- as.data.frame(cbind(PromedioSO2_2020,zonas))
prom_anualSO2_2020 <- mutate(prom_anualSO2_2020, Promedio = as.numeric(PromedioSO2_2020))

#Graficando promedio anual de zonas por parametro
ggplot(prom_anualCO2019,aes(x=zonas, y=Promedio)) + 
  geom_col(fill = "gray") +
  labs(x = "Zona", y = "Promedio Anual",
       title = "Promedio Anual de Concentración de CO por zona - 2019 ")

ggplot(prom_anualCO2020,aes(x=zonas, y=Promedio)) + 
  geom_col(fill = "blue") +
  labs(x = "Zona", y = "Promedio Anual",
       title = "Promedio Anual de Concentración de CO por zona - 2020 ")

ggplot(prom_anualNO2_2019,aes(x=zonas, y=Promedio)) + 
  geom_col(fill = "gray") +
  labs(x = "Zona", y = "Promedio Anual",
       title = "Promedio Anual de Concentración de NO2 por zona - 2019 ")

ggplot(prom_anualNO2_2020,aes(x=zonas, y=Promedio)) + 
  geom_col(fill = "blue") +
  labs(x = "Zona", y = "Promedio Anual",
       title = "Promedio Anual de Concentración de NO2 por zona - 2020 ")


ggplot(prom_anualO3_2019,aes(x=zonas, y=Promedio)) + 
  geom_col(fill = "gray") +
  labs(x = "Zona", y = "Promedio Anual",
       title = "Promedio Anual de Concentración de NO2 por zona - 2019")

ggplot(prom_anualO3_2020,aes(x=zonas, y=Promedio)) + 
  geom_col(fill = "blue") +
  labs(x = "Zona", y = "Promedio Anual",
       title = "Promedio Anual de Concentración de NO2 por zona - 2020")

ggplot(prom_anualPM10_2019,aes(x=zonas, y=Promedio)) + 
  geom_col(fill = "gray") +
  labs(x = "Zona", y = "Promedio Anual",
       title = "Promedio Anual de Concentración de PM10 por zona - 2019")

ggplot(prom_anualPM10_2020,aes(x=zonas, y=Promedio)) + 
  geom_col(fill = "blue") +
  labs(x = "Zona", y = "Promedio Anual",
       title = "Promedio Anual de Concentración de PM10 por zona - 2020")

ggplot(prom_anualSO2_2019,aes(x=zonas, y=Promedio)) + 
  geom_col(fill = "gray") +
  labs(x = "Zona", y = "Promedio Anual",
       title = "Promedio Anual de Concentración de SO2 por zona - 2019")

ggplot(prom_anualSO2_2020,aes(x=zonas, y=Promedio)) + 
  geom_col(fill = "red") +
  labs(x = "Zona", y = "Promedio Anual",
       title = "Promedio Anual de Concentración de SO2 por zona - 2020")
