library(dplyr)
library(ggplot2)
library(e1071)

#Seleccionando carpeta de trabajo
setwd("Documentos/BEDU_DataScience/Proyecto/Analisis_Calidad_AireCDMX")

##########################FUNCIONES PARA LA LECTURA Y LIMPIEZA DE DATOS####################3
##Indicar separador entre comillas dobles
LecturaCSV <- function(url,sep){
  nombre <- lapply(dir(path = url,full.names = T), read.csv, sep = sep)
}

##Unión de datasets  
##Se unen los datasets y se renombra columnas para que coincidad
##Se seleccionan las columnas necesarias para trabajar 
##UIZ, PED no están en todos los datasets (P10 Y PM25)
uniondf <- function(data1,data2=NULL){
  DATA1 <- lapply(c(data1,data2), select, c(contains("FECHA"), SAG, TLA, XAL, MER, PED, UIZ))
  n = length(DATA1)
  for (i in 1:n){
    names(DATA1[[i]]) = c("FECHA", "SAG", "TLA", "XAL", "MER", "PED", "UIZ") 
  }
  DATA <- do.call(rbind, DATA1)
  
  return(DATA)
}

##ORDENAR datasets por fecha
orden <- function(datos){
  datos[order(as.Date(datos$FECHA, format="%d/%m/%Y")),]
}

##Agrupando por años, se obtiene el promedio de las columnas por estación, se agrupa por mes###
groupanios <- function(dataframe){
  data_anio <- mutate(dataframe, FECHA= as.Date(FECHA, "%d/%m/%Y"))
  data_anio <- mutate(data_anio, anio= format(FECHA,"%Y"))
  result <- data_anio %>%  group_by(anio) %>%
    summarise(mean.TLA=mean(TLA, na.rm = TRUE), mean.MER=mean(MER, na.rm = TRUE),
              mean.UIZ=mean(UIZ, na.rm = TRUE),mean.PED=mean(PED, na.rm = TRUE), cont=n())
  return(result)
}

################LECTURA DE DATOS########################33

##NO2
NO2 <- LecturaCSV("datasets/datasets_contaminantes/NO2/",",")
NO2 <- uniondf(NO2)

###CO
CO1 <- LecturaCSV("datasets/datasets_contaminantes/CO/sinpunto",",")
CO2 <- LecturaCSV("datasets/datasets_contaminantes/CO/punto",";")
CO <- uniondf(CO1,CO2)

###O3
O3 <- LecturaCSV("datasets/datasets_contaminantes/O3",",")
O3 <- uniondf(O3)

###SO2
SO2 <- LecturaCSV("datasets/datasets_contaminantes/SO2/",",")
SO2 <- uniondf(SO2)

##################################ORDENANDO DATOS POR EL AÑO########################
##ORDENANDO DATOS
CO <- orden(CO)
SO2 <- orden(SO2)
NO2 <- orden(NO2)
O3 <- orden(O3)

##########################Sustituir -99 por NA#############################3
NO2[NO2 =="-99"] <- NA
O3[O3 =="-99"] <- NA
SO2[SO2 =="-99"] <- NA

## En CO hay valores no numericos
CO$TLA <- gsub(",",".",CO$TLA)
CO$UIZ <- gsub(",",".",CO$UIZ)
CO$PED <- gsub(",",".",CO$PED)
CO$MER <- gsub(",",".",CO$MER)
CO <- mutate(CO, TLA= as.numeric(TLA), MER= as.numeric(MER), UIZ= as.numeric(UIZ), PED= as.numeric(PED))
CO[CO =="-99"] <- NA

#Leyendo número de datos faltantes
sum(is.na(NO2))
sum(is.na(NO2))

##Agrupando por años

NO2 <- groupanios(NO2)
O3 <- groupanios(O3)
CO <- groupanios(CO)
SO2 <- groupanios(SO2)

##############################################################################################3