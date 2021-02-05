# Obtiene Promedios por parametro#

library(dplyr)
library(lubridate)
getwd()
#Aqui va la ruta donde estÃ¡n alojados archivos
setwd("C:/Users/bety-/Documents/Repositorios/Analisis_Calidad_AireCDMX/datos_IMECA/datos_limpios")


##Agrupando por aÃ±os###
groupmeses <- function(dataframe){
  dataframe$Fecha<-as.Date(dataframe$Fecha)
  data_mes <- mutate(dataframe, Fecha= floor_date(dataframe$Fecha, "month"))
  result <- data_mes%>%group_by(Fecha)%>%
    summarise_all(mean, na.rm = TRUE)
  return(result)
}

delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}

CO <- read.csv("CO.csv")
NO2<-read.csv("NO2.csv")
O3<-read.csv("O3.csv")
PM10<-read.csv("PM10.csv")
SO2<-read.csv("SO2.csv")

CO[CO =="-99"] <- NA
PM10[PM10 =="-99"] <- NA
NO2[NO2 =="-99"] <- NA
O3[O3 =="-99"] <- NA
SO2[SO2 =="-99"] <- NA

Media_CO <- groupmeses(CO)
Media_NO2<- groupmeses(NO2)
Media_O3 <- groupmeses(O3)
Media_PM10 <- groupmeses(PM10)
Media_SO2 <- groupmeses(SO2)
Media_CO <- delete.na(Media_CO)
Media_NO2 <- delete.na(Media_NO2)
Media_O3  <- delete.na(Media_O3 )
Media_PM10 <- delete.na(Media_PM10)
Media_SO2 <- delete.na(Media_SO2)

write.csv(Media_O3,"promedios/Media_O3.csv",row.names = FALSE)
write.csv(Media_SO2,"promedios/Media_SO2.csv",row.names = FALSE)
write.csv(Media_NO2,"promedios/Media_NO2.csv",row.names = FALSE)
write.csv(Media_CO,"promedios/Media_CO.csv",row.names = FALSE)
write.csv(Media_PM10,"promedios/Media_PM10.csv",row.names = FALSE)

