library(ggplot2)
library(dplyr)

getwd()
#Aqui va la ruta donde están alojados archivos
setwd("C:/Users/leona/Desktop/Nueva carpeta (2)/NewDataSets/csv")

#LECTURA DE DATOS
DF.2005<-read.csv("imeca2005.csv")
DF.2006<-read.csv("imeca2006.csv")
DF.2007<-read.csv("imeca2007.csv")
DF.2008<-read.csv("imeca2008.csv")
DF.2009<-read.csv("imeca2009.csv")
DF.2010<-read.csv("imeca2010.csv")
DF.2011<-read.csv("imeca2011.csv")
DF.2012<-read.csv("imeca2012.csv")
DF.2013<-read.csv("imeca2013.csv")
DF.2014<-read.csv("imeca2014.csv")
DF.2015<-read.csv("imeca2015.csv")
DF.2016<-read.csv("imeca2016.csv")
DF.2017<-read.csv("imeca2017.csv")
DF.2018<-read.csv("imeca2018.csv")
DF.2019<-read.csv("imeca2019.csv")
DF.2020<-read.csv("imeca2020.csv")


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

tags<-c( "ï..Fecha"              ,        "Hora"                ,          "Noroeste.Ozono"               ,
 "Noroeste.dioxido.de.azufre"   , "Noroeste.dioxido.de.nitrogeno", "Noroeste.monoxido.de.carbono" ,
 "Noroeste.PM10"                , "Noreste.Ozono"                , "Noreste.dioxido.de.azufre"    ,
 "Noreste.dioxido.de.nitrogeno" , "Noreste.monoxido.de.carbono"  , "Noreste.PM10"                 ,
 "Centro.Ozono"                 , "Centro.dioxido.de.azufre"     , "Centro.dioxido.de.nitrogeno"  ,
 "Centro.monoxido.de.carbono"   , "Centro.PM10"                  , "Suroeste.Ozono"               ,
 "Suroeste.dioxido.de.azufre"   , "Suroeste.dioxido.de.nitrogeno", "Suroeste.monoxido.de.carbono" ,
 "Suroeste.PM10"                , "Sureste.Ozono"                , "Sureste.dioxido.de.azufre"    ,
 "Sureste.dioxido.de.nitrogeno" , "Sureste.monoxido.de.carbono"  , "Sureste.PM10" )

#Renombramos algunas de las DF
names(DF.2019)<-tags
names(DF.2020)<-tags

#Nota, las mediciones de PM25 comienzan en el database a partir del año de 2019

DF.ALL<-rbind(DF.2005,DF.2006,DF.2007,DF.2008,DF.2009,DF.2010,DF.2011,DF.2012,DF.2013,
              DF.2014,DF.2015,DF.2016,DF.2017,DF.2018,DF.2019,DF.2020)

#Separamos los DATAFRAME en DF más pequeños.
#Noroeste
DF.NO.O3<-select(DF.ALL,c(1,3))
DF.NO.OS<-select(DF.ALL,c(1,4))
DF.NO.ON<-select(DF.ALL,c(1,5))
DF.NO.OC<-select(DF.ALL,c(1,6))
DF.NO.P10<-select(DF.ALL,c(1,7))
#Noreste
DF.NE.O3<-select(DF.ALL,c(1,8))
DF.NE.OS<-select(DF.ALL,c(1,9))
DF.NE.ON<-select(DF.ALL,c(1,10))
DF.NE.OC<-select(DF.ALL,c(1,11))
DF.NE.P10<-select(DF.ALL,c(1,12))

#Centro
DF.CE.O3<-select(DF.ALL,c(1,13))
DF.CE.OS<-select(DF.ALL,c(1,14))
DF.CE.ON<-select(DF.ALL,c(1,15))
DF.CE.OC<-select(DF.ALL,c(1,16))
DF.CE.P10<-select(DF.ALL,c(1,17))

#Suroeste
DF.SO.O3<-select(DF.ALL,c(1,18))
DF.SO.OS<-select(DF.ALL,c(1,19))
DF.SO.ON<-select(DF.ALL,c(1,20))
DF.SO.OC<-select(DF.ALL,c(1,21))
DF.SO.P10<-select(DF.ALL,c(1,22))

#Sureste
DF.SE.O3<-select(DF.ALL,c(1,23))
DF.SE.OS<-select(DF.ALL,c(1,24))
DF.SE.ON<-select(DF.ALL,c(1,25))
DF.SE.OC<-select(DF.ALL,c(1,26))
DF.SE.P10<-select(DF.ALL,c(1,27))

#Salida de información
setwd("C:/Users/leona/Desktop/Nueva carpeta (2)/NewDataSets/DataPocess")
write.csv(DF.NO.O3,"Noroeste_O3.csv",row.names = FALSE)
write.csv(DF.NO.OS,"Noroeste_0S.csv",row.names = FALSE)
write.csv(DF.NO.ON,"Noroeste_ON.csv",row.names = FALSE)
write.csv(DF.NO.OC,"Noroeste_OC.csv",row.names = FALSE)
write.csv(DF.NO.P10,"Noroeste_P10.csv",row.names = FALSE)

write.csv(DF.NE.O3,"Noreste_O3.csv",row.names = FALSE)
write.csv(DF.NE.OS,"Noreste_0S.csv",row.names = FALSE)
write.csv(DF.NE.ON,"Noreste_ON.csv",row.names = FALSE)
write.csv(DF.NE.OC,"Noreste_OC.csv",row.names = FALSE)
write.csv(DF.NE.P10,"Noreste_P10.csv",row.names = FALSE)

write.csv(DF.CE.O3,"Centro_O3.csv",row.names = FALSE)
write.csv(DF.CE.OS,"Centro_0S.csv",row.names = FALSE)
write.csv(DF.CE.ON,"Centro_ON.csv",row.names = FALSE)
write.csv(DF.CE.OC,"Centro_OC.csv",row.names = FALSE)
write.csv(DF.CE.P10,"Centro_P10.csv",row.names = FALSE)

write.csv(DF.SO.O3,"Suroeste_O3.csv",row.names = FALSE)
write.csv(DF.SO.OS,"Suroeste_0S.csv",row.names = FALSE)
write.csv(DF.SO.ON,"Suroeste_ON.csv",row.names = FALSE)
write.csv(DF.SO.OC,"Suroeste_OC.csv",row.names = FALSE)
write.csv(DF.SO.P10,"Suroeste_P10.csv",row.names = FALSE)

write.csv(DF.SE.O3,"Sureste_O3.csv",row.names = FALSE)
write.csv(DF.SE.OS,"Sureste_0S.csv",row.names = FALSE)
write.csv(DF.SE.ON,"Sureste_ON.csv",row.names = FALSE)
write.csv(DF.SE.OC,"Sureste_OC.csv",row.names = FALSE)
write.csv(DF.SE.P10,"Sureste_P10.csv",row.names = FALSE)
