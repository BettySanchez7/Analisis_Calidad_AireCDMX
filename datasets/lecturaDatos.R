library(dplyr)
library(ggplot2)
library(e1071)

setwd("Documents/Repositorios/Analisis_Calidad_AireCDMX")

##ALGUNOS DATASETS NO SE CARGABAN SEPARADOS POR COMAS POR LO QUE HICE UNA FUNCIÓN PARA CARGAR LOS QUE 
##SE SEPARARAN POR "," Y OTRA PARA LOS QUE SE SEPARABAN POR ";"

##########################FUNCIONES PARA LA LECTURA Y LIMPIEZA DE DATOS####################3
##Separada por comas
LecturaCSV <- function(url){
  nombre <- lapply(dir(path = url,full.names = T), read.csv, sep = ",")
}

##Unión de datasets  
##Se unen los datasets y se renombra columnas para que coincidad
##Se seleccionan las columnas necesarias para trabajar 
##UIZ, PED no están en todos los datasets (P10 Y PM25)
uniondf <- function(lista){
  DATA1 <- lapply(lista, select, c(contains("FECHA"), TLA, MER, UIZ, PED))
  n = length(DATA1)
  for (i in 1:n){
    names(DATA1[[i]]) = c("FECHA", "TLA", "MER", "UIZ", "PED") 
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
NO21 <- LecturaCSV("datasets/datasets_contaminantes/NO2/")
NO2 <- uniondf(NO21)

###CO
CO1 <- LecturaCSV("datasets/datasets_contaminantes/CO/")
CO <- uniondf(CO1)

###O3
O31 <- LecturaCSV("datasets/datasets_contaminantes/O3/")
O3 <- uniondf(O31)

###SO2
SO21 <- LecturaCSV("datasets/datasets_contaminantes/SO2/")
SO2 <- uniondf(SO21)


##################################ORDENANDO DATOS POR EL A�O########################
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

##Agrupando por años

NO2 <- groupanios(NO2)
O3 <- groupanios(O3)
CO <- groupanios(CO)
SO2 <- groupanios(SO2)

##############################################################################################3
