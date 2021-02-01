library(dplyr)
library(ggplot2)
library(e1071)

##ALGUNOS DATASETS NO SE CARGABAN SEPARADOS POR COMAS POR LO QUE HICE UNA FUNCIÓN PARA CARGAR LOS QUE 
##SE SEPARARAN POR "," Y OTRA PARA LOS QUE SE SEPARABAN POR ";"

##########################FUNCIONES PARA LA LECTURA Y LIMPIEZA DE DATOS####################3
##Separada por comas
LecturaCSV <- function(url){
  setwd(url)
  nombre <- lapply(dir(), read.csv, sep = ",")
}

##Separada por punto y coma
LecturaCSV2 <- function(url){
  setwd(url)
  nombre <- lapply(dir(), read.csv, sep = ";")
}

##Unión de datasets  
##Se unen los datasets y se renombra columnas para que coincidad
##Se seleccionan las columnas necesarias para trabajar 
##UIZ, PED no están en todos los datasets (P10 Y PM25)
uniondf <- function(data1, data2=NULL){
  DATA1 <- lapply(c(data1,data2), select, c(contains("FECHA"), TLA, MER, UIZ, PED))
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
NO21 <- LecturaCSV("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/contaminantes/NO2/sinpunto")
NO22 <- LecturaCSV2("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/contaminantes/NO2/punto")

NO2 <- uniondf(NO21, NO22)

###CO
CO1 <- LecturaCSV("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/contaminantes/CO/sinpunto")
CO2 <- LecturaCSV2("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/contaminantes/CO/punto")

CO <- uniondf(CO1, CO2)

###O3
O31 <- LecturaCSV("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/contaminantes/O3/sinpunto")
O32 <- LecturaCSV2("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/contaminantes/O3/punto")
O3 <- uniondf(O31, O32)

###SO2
SO21 <- LecturaCSV("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/contaminantes/SO2/sinpunto")
SO22 <- LecturaCSV2("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/contaminantes/SO2/punto")

SO2 <- uniondf(SO21, SO22)

###P10 UIZ NO EXISTE (CAMBIAR FUNCIÓN uniondfd)
#P101 <- LecturaCSV("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/contaminantes/P10/sinpunto")
#P102 <- LecturaCSV2("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/contaminantes/P10/conpunto")
#P10 <- uniondf(P101, P102)

###PM25 NO EXISTE PED (CAMBIAR FUNCIÓN uniondfd)
#PM251 <- LecturaCSV("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/contaminantes/PM25/sinpunto")
#PM252 <- LecturaCSV2("C:/Users/bety-/Documents/CURSOS/DATA SCIENCE/FundamentosR/Proyecto/contaminantes/PM25/punto")
#PM25 <- uniondf(PM251, PM252)


##################################ORDENANDO DATOS POR EL AÑO########################
##ORDENANDO DATOS
CO <- orden(CO)
SO2 <- orden(SO2)
NO2 <- orden(NO2)
O3 <- orden(O3)
#P10 <- orden(P10)
#PM25 <- orden(PM25)

##########################Sustituir -99 por NA#############################3
NO2[NO2 =="-99"] <- NA
O3[O3 =="-99"] <- NA
SO2[SO2 =="-99"] <- NA
#P10[P10 =="-99"] <- NA
#PM25[PM25 =="-99"] <-NA

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
#P10 <- groupanios(P10)
#PM25 <- groupanios(PM25)

##############################################################################################3
