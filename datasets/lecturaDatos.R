library(dplyr)
library(ggplot2)
library(e1071)

#Seleccionando carpeta de trabajo
setwd("Documents/Repositorios/Analisis_Calidad_AireCDMX")

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
groupmeses <- function(dataframe){
  data_mes <- mutate(dataframe, FECHA= as.Date(FECHA, "%d/%m/%Y"))
  data_mes <- mutate(data_mes, mes= format(FECHA,"%m/%Y"))
  result <- data_mes %>%  group_by(mes) %>%
    summarise(mean.TLA=mean(TLA, na.rm = TRUE), mean.MER=mean(MER, na.rm = TRUE),
              mean.UIZ=mean(UIZ, na.rm = TRUE),mean.PED=mean(PED, na.rm = TRUE),
              mean.SAG=mean(SAG, na.rm = TRUE),mean.XAL=mean(XAL, na.rm = TRUE), cont=n())
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
CO$SAG <- gsub(",",".",CO$SAG)
CO$XAL <- gsub(",",".",CO$XAL)
CO$MER <- gsub(",",".",CO$MER)
CO <- mutate(CO, TLA= as.numeric(TLA), MER= as.numeric(MER), 
                 UIZ= as.numeric(UIZ), PED= as.numeric(PED),
                 SAG=as.numeric(SAG), XAL= as.numeric(XAL)
            )
CO[CO =="-99"] <- NA

#Leyendo número de datos faltantes
sum(is.na(NO2))
sum(is.na(NO2))

##Agrupando por años

NO2 <- groupmeses(NO2)
O3 <- groupmeses(O3)
CO <- groupmeses(CO)
SO2 <- groupmeses(SO2)

##Creando csv

write.csv(NO2, file="Documents/Repositorios/Analisis_Calidad_AireCDMX/datasets/NO2.csv", row.names = FALSE)
write.csv(O3, file="Documents/Repositorios/Analisis_Calidad_AireCDMX/datasets/O3.csv", row.names = FALSE)
write.csv(CO, file="Documents/Repositorios/Analisis_Calidad_AireCDMX/datasets/CO.csv", row.names = FALSE)
write.csv(SO2, file="Documents/Repositorios/Analisis_Calidad_AireCDMX/datasets/SO2.csv", row.names = FALSE)



##############################################################################################3