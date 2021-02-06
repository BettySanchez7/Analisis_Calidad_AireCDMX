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
#setwd("C:/Users/bety-/Documents/Repositorios/Analisis_Calidad_AireCDMX/datos_IMECA/datos_limpios/promedios")

CO_2019 <- read.csv("data/CO_2019.csv")
CO_2020 <- read.csv("data/CO_2020.csv")
NO2_2019 <- read.csv("data/NO2_2019.csv")
NO2_2020 <- read.csv("data/NO2_2020.csv")
O3_2019 <- read.csv("data/O3_2019.csv")
O3_2020 <- read.csv("data/O3_2020.csv")
PM10_2019 <- read.csv("data/PM10_2019.csv")
PM10_2020 <- read.csv("data/PM10_2020.csv")
SO2_2019 <- read.csv("data/SO2_2019.csv")
SO2_2020 <- read.csv("data/SO2_202O.csv")

fecha <- function(dataframe){
    dataframe$Fecha <- factor(dataframe$Fecha, levels = c("ene.", "feb.", "mar.", "abr.", "may.", "jun.",
                          "jul.", "ago.", "sep.", "oct.", "nov.", "dic."), ordered = TRUE)
    return(dataframe$Fecha)
}

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
