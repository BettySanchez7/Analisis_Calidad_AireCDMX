#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(egg)
library(lubridate)

getwd()
#Aqui va la ruta donde estÃ¡n alojados archivos
setwd("C:/Users/bety-/Documents/Repositorios/Analisis_Calidad_AireCDMX/datos_IMECA/datos_limpios/promedios")

fecha <- function(dataframe){
    dataframe$Fecha <- as.Date(dataframe$Fecha) %>% format("%b") %>%
        factor(levels = c("ene.", "feb.", "mar.", "abr.", "may.", "jun.",
                          "jul.", "ago.", "sep.", "oct.", "nov.", "dic."), ordered = TRUE)
    return(dataframe$Fecha)
}

Media_CO <- read.csv("Media_CO.csv")
Media_NO2<-read.csv("Media_NO2.csv")
Media_O3<-read.csv("Media_O3.csv")
Media_PM10<-read.csv("Media_PM10.csv")
Media_SO2<-read.csv("Media_SO2.csv")
tags<-c("Fecha","NO","NE","CE","SO","SE")
names(Media_CO)<-tags
names(Media_NO2)<-tags
names(Media_SO2)<-tags
names(Media_O3)<-tags
names(Media_PM10)<-tags

###DATA FRAMES DE AÑOS 2019 Y 2020
CO_2019 <- Media_CO[169:180,]
CO_2020 <- Media_CO[181:192,]
NO2_2019 <- Media_NO2[169:180,]
NO2_2020 <- Media_NO2[181:192,]
O3_2019 <- Media_O3[169:180,]
O3_2020 <- Media_O3[181:192,]
PM10_2019 <- Media_PM10[169:180,]
PM10_2020 <- Media_PM10[181:192,]
SO2_2019 <- Media_SO2[169:180,]
SO2_2020 <- Media_SO2[181:192,]

###Cambio de formato a fechas a letra de meses

CO_2019$Fecha<- fecha(CO_2019)
CO_2020$Fecha <- fecha(CO_2020)
NO2_2019$Fecha <- fecha(NO2_2019)
NO2_2020$Fecha <- fecha(NO2_2020)
O3_2019$Fecha <- fecha(O3_2019)
O3_2020$Fecha <- fecha(O3_2020)
PM10_2019$Fecha <- fecha(PM10_2019)
PM10_2020$Fecha <- fecha(PM10_2020)
SO2_2019$Fecha <- fecha(SO2_2019)
SO2_2020$Fecha <- fecha(SO2_2020)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    datasetImput <- reactive(
        switch(input$dataset, 
               "CO_2019" = CO_2019, 
               "NO2_2019" = NO2_2019, 
               "O3_2019" = O3_2019,
               "PM10_2019" = PM10_2019,
               "SO2_2019" = SO2_2019,
               "CO_2020" = CO_2020, 
               "NO2_2020" = NO2_2020, 
               "O3_2020" = O3_2020,
               "PM10_2020" = PM10_2020,
               "SO2_2020" = SO2_2020
               )
    )
    
    ejey <-reactive(
        switch(input$y)
               )
        
    output$output_text <- renderText(paste("Graficas"))
    output$IMECA <- renderText(paste("Los datos trabajados en este proyecto se obtuvieron de la base 
                                     de datos de la IMECA, este es el índice metropolitano de la calidad del aire funciona en
                                     México como valor de referencia para que la población de 
                                     grandes ciudades como la Ciudad de México y área metropolitana
                                     comprenda los niveles de contaminación del aire que prevalecen
                                     en su zona de transporte de residencia o trabajo.
                                     La finalidad fue conocer la concentración de parámetros de contaminación
                                     en la Ciudad de México, la escala que se expresa en nuestros resultados 
                                     va de un valor de 0-200, los cuales indican la condición de la calidad del aire
                                     de acuerdo a la siguiente tabla.
                                     "))

    output$plot <- renderPlot({
            y <- datasetImput()[ ,input$y]
            ggplot(datasetImput(), aes(x=Fecha, y=y))+
                geom_bar(stat="identity", aes(fill=y)) + 
                labs(x = "Mes", y = "Concentración [Unidades IMECA]",
                     title = "Concentración de parametro")+
                theme_test() +
                scale_fill_gradient (low = "green", high = "red" )
    
    })
    
})
