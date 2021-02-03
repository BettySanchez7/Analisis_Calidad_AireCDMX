getwd()
library(TSA)
CO <- read.csv("contaminantes_mensual/CO.csv",header = T)
NO2 <- read.csv("contaminantes_mensual/NO2.csv", header = T)
O3 <- read.csv("contaminantes_mensual/O3.csv", header = T)
SO2 <- read.csv("contaminantes_mensual/SO2.csv", header = T)

#Creando las series de tiempo
CO1.ts <- ts(data = CO$mean.TLA, start = 2005, frequency = 12)
CO2.ts <- ts(data = CO$mean.MER, start = 2005, frequency = 12)
CO3.ts <- ts(data = CO$mean.UIZ, start = 2005, frequency = 12)
CO4.ts <- ts(data = CO$mean.PED, start = 2005, frequency = 12)
CO5.ts <- ts(data = CO$mean.SAG, start = 2005, frequency = 12)
CO6.ts <- ts(data = CO$mean.XAL, start = 2005, frequency = 12)

NO21.ts <- ts(data = NO2$mean.TLA, start = 2005, frequency = 12)
NO22.ts <- ts(data = NO2$mean.MER, start = 2005, frequency = 12)
NO23.ts <- ts(data = NO2$mean.UIZ, start = 2005, frequency = 12)
NO24.ts <- ts(data = NO2$mean.PED, start = 2005, frequency = 12)
NO25.ts <- ts(data = NO2$mean.SAG, start = 2005, frequency = 12)
NO26.ts <- ts(data = NO2$mean.XAL, start = 2005, frequency = 12)

O31.ts <- ts(data = O3$mean.TLA, start = 2005, frequency = 12)
O32.ts <- ts(data = O3$mean.MER, start = 2005, frequency = 12)
O33.ts <- ts(data = O3$mean.UIZ, start = 2005, frequency = 12)
O34.ts <- ts(data = O3$mean.PED, start = 2005, frequency = 12)
O35.ts <- ts(data = O3$mean.SAG, start = 2005, frequency = 12)
O36.ts <- ts(data = O3$mean.XAL, start = 2005, frequency = 12)

#Datos incompletos
layout(1:3)
ts.plot(CO1.ts)
ts.plot(CO2.ts)
ts.plot(CO3.ts)

ts.plot(CO4.ts)
ts.plot(CO5.ts)
ts.plot(CO6.ts)


