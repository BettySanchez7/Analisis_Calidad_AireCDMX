# LECTURA Y LIMPIEZA ACHIVOS IMECA #

library(dplyr)

getwd()
#Aqui va la ruta donde están alojados archivos
#setwd("NewDataSets/csv/")

#LECTURA DE DATOS
DF.2005<-read.csv("NewDataSets/csv/imeca2005.csv")
DF.2006<-read.csv("NewDataSets/csv/imeca2006.csv")
DF.2007<-read.csv("NewDataSets/csv/imeca2007.csv")
DF.2008<-read.csv("NewDataSets/csv/imeca2008.csv")
DF.2009<-read.csv("NewDataSets/csv/imeca2009.csv")
DF.2010<-read.csv("NewDataSets/csv/imeca2010.csv")
DF.2011<-read.csv("NewDataSets/csv/imeca2011.csv")
DF.2012<-read.csv("NewDataSets/csv/imeca2012.csv")
DF.2013<-read.csv("NewDataSets/csv/imeca2013.csv")
DF.2014<-read.csv("NewDataSets/csv/imeca2014.csv")
DF.2015<-read.csv("NewDataSets/csv/imeca2015.csv")
DF.2016<-read.csv("NewDataSets/csv/imeca2016.csv")
DF.2017<-read.csv("NewDataSets/csv/imeca2017.csv")
DF.2018<-read.csv("NewDataSets/csv/imeca2018.csv")
DF.2019<-read.csv("NewDataSets/csv/imeca2019.csv")
DF.2020<-read.csv("NewDataSets/csv/imeca2020.csv")


#Eliminando columnas inecesarias (PM2.5 se agregó a partir de 2019)
DF.2010<-DF.2010[,-c(28:32)]
DF.2011<-DF.2011[,-c(28:31)]
DF.2012<-DF.2012[,-c(28:31)]
DF.2013<-DF.2013[,-c(28:31)]
DF.2014<-DF.2014[,-c(28:29)]
DF.2015<-DF.2015[,-c(28:29)]
DF.2016<-DF.2016[,-c(28:29)]
DF.2017<-DF.2017[,-c(28:29)]
DF.2018<-DF.2018[,-c(28:29)]
DF.2019<-DF.2019[,-c(8,14,20,26,32)]
DF.2020<-DF.2020[,-c(8,14,20,26,32)]

tags<-c( "Fecha"              ,        "Hora"                ,          "Noroeste.Ozono"               ,
 "Noroeste.dioxido.de.azufre"   , "Noroeste.dioxido.de.nitrogeno", "Noroeste.monoxido.de.carbono" ,
 "Noroeste.PM10"                , "Noreste.Ozono"                , "Noreste.dioxido.de.azufre"    ,
 "Noreste.dioxido.de.nitrogeno" , "Noreste.monoxido.de.carbono"  , "Noreste.PM10"                 ,
 "Centro.Ozono"                 , "Centro.dioxido.de.azufre"     , "Centro.dioxido.de.nitrogeno"  ,
 "Centro.monoxido.de.carbono"   , "Centro.PM10"                  , "Suroeste.Ozono"               ,
 "Suroeste.dioxido.de.azufre"   , "Suroeste.dioxido.de.nitrogeno", "Suroeste.monoxido.de.carbono" ,
 "Suroeste.PM10"                , "Sureste.Ozono"                , "Sureste.dioxido.de.azufre"    ,
 "Sureste.dioxido.de.nitrogeno" , "Sureste.monoxido.de.carbono"  , "Sureste.PM10" )

#Renombramos las columnas del DF
names(DF.2005)<-tags
names(DF.2006)<-tags
names(DF.2007)<-tags
names(DF.2008)<-tags
names(DF.2009)<-tags
names(DF.2010)<-tags
names(DF.2011)<-tags
names(DF.2012)<-tags
names(DF.2013)<-tags
names(DF.2014)<-tags
names(DF.2015)<-tags
names(DF.2016)<-tags
names(DF.2017)<-tags
names(DF.2018)<-tags
names(DF.2019)<-tags
names(DF.2020)<-tags

#Uniendo todo en un mismo DF
DF.ALL<-rbind(DF.2005,DF.2006,DF.2007,DF.2008,DF.2009,DF.2010,DF.2011,DF.2012,DF.2013,
              DF.2014,DF.2015,DF.2016,DF.2017,DF.2018,DF.2019,DF.2020)
str(DF.ALL)

#Transformando columna a tipo fecha
DF.ALL <- mutate(DF.ALL, Fecha = as.Date(DF.ALL$Fecha, format="%d/%m/%Y"))
str(DF.ALL)


#Separando el Data Frame por parámetros
O3 <- select(DF.ALL,Fecha,contains("Ozono"))
SO2 <- select(DF.ALL,Fecha,contains("dioxido.de.azufre"))
NO2 <- select(DF.ALL,Fecha,contains("dioxido.de.nitrogeno"))
CO <- select(DF.ALL,Fecha,contains("monoxido.de.carbono"))
PM10 <- select(DF.ALL,Fecha,contains("PM10"))

#Escribiendo archivos de trabajo
write.csv(O3,"NewDataSets/datos_limpios/O3.csv",row.names = FALSE)
write.csv(SO2,"NewDataSets/datos_limpios/SO2.csv",row.names = FALSE)
write.csv(NO2,"NewDataSets/datos_limpios/NO2.csv",row.names = FALSE)
write.csv(CO,"NewDataSets/datos_limpios/CO.csv",row.names = FALSE)
write.csv(PM10,"NewDataSets/datos_limpios/PM10.csv",row.names = FALSE)
