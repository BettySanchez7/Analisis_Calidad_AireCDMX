# ANÁLISIS DE SERIES DE TIEMPO #

getwd()
library(TSA)
CO <- read.csv("contaminantes_mensual/CO.csv",header = T)
NO2 <- read.csv("contaminantes_mensual/NO2.csv", header = T)
O3 <- read.csv("contaminantes_mensual/O3.csv", header = T)
SO2 <- read.csv("contaminantes_mensual/SO2.csv", header = T)

#Creando las series de tiempo
CO.TLA <- ts(data = CO$mean.TLA, start = 2005, frequency = 12)
CO.MER <- ts(data = CO$mean.MER, start = 2005, frequency = 12)
CO.UIZ <- ts(data = CO$mean.UIZ, start = 2005, frequency = 12)
CO.PED <- ts(data = CO$mean.PED, start = 2005, frequency = 12)
CO.SAG <- ts(data = CO$mean.SAG, start = 2005, frequency = 12)
CO.XAL <- ts(data = CO$mean.XAL, start = 2005, frequency = 12)

NO2.TLA <- ts(data = NO2$mean.TLA, start = 2005, frequency = 12)
NO2.MER <- ts(data = NO2$mean.MER, start = 2005, frequency = 12)
NO2.UIZ <- ts(data = NO2$mean.UIZ, start = 2005, frequency = 12)
NO2.PED <- ts(data = NO2$mean.PED, start = 2005, frequency = 12)
NO2.SAG <- ts(data = NO2$mean.SAG, start = 2005, frequency = 12)
NO2.XAL <- ts(data = NO2$mean.XAL, start = 2005, frequency = 12)

O3.TLA <- ts(data = O3$mean.TLA, start = 2005, frequency = 12)
O3.MER <- ts(data = O3$mean.MER, start = 2005, frequency = 12)
O3.UIZ <- ts(data = O3$mean.UIZ, start = 2005, frequency = 12)
O3.PED <- ts(data = O3$mean.PED, start = 2005, frequency = 12)
O3.SAG <- ts(data = O3$mean.SAG, start = 2005, frequency = 12)
O3.XAL <- ts(data = O3$mean.XAL, start = 2005, frequency = 12)

SO2.TLA <- ts(data = SO2$mean.TLA, start = 2005, frequency = 12)
SO2.MER <- ts(data = SO2$mean.MER, start = 2005, frequency = 12)
SO2.UIZ <- ts(data = SO2$mean.UIZ, start = 2005, frequency = 12)
SO2.PED <- ts(data = SO2$mean.PED, start = 2005, frequency = 12)
SO2.SAG <- ts(data = SO2$mean.SAG, start = 2005, frequency = 12)
SO2.XAL <- ts(data = SO2$mean.XAL, start = 2005, frequency = 12)

#Graficas simples (Hay datos incompletos)
##CO
layout(1:3)
ts.plot(CO.TLA )
ts.plot(CO.MER )
ts.plot(CO.UIZ)

ts.plot(CO.PED)
ts.plot(CO.SAG)
ts.plot(CO.XAL)

##NO2
ts.plot(NO2.TLA )
ts.plot(NO2.MER )
ts.plot(NO2.UIZ)
 
ts.plot(NO2.PED)
ts.plot(NO2.SAG)
ts.plot(NO2.XAL)

##O3
ts.plot(O3.TLA )
ts.plot(O3.MER )
ts.plot(O3.UIZ)

ts.plot(O3.PED)
ts.plot(O3.SAG)
ts.plot(O3.XAL)

##SO2
ts.plot(SO2.TLA )
ts.plot(SO2.MER )
ts.plot(SO2.UIZ)

ts.plot(SO2.PED)
ts.plot(SO2.SAG)
ts.plot(SO2.XAL)
