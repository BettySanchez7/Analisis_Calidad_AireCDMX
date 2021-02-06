#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    pageWithSidebar(
        headerPanel("Visualización de concentración de parámetros en el año 2019 y 2020"),
        sidebarPanel(
            selectInput("dataset", "Elige el parametro y año", 
                        c("CO_2019", "NO2_2019", "O3_2019", "PM10_2019", "SO2_2019",
                          "CO_2020", "NO2_2020", "O3_2020", "PM10_2020", "SO2_2020")),
            
            selectInput("y", "Elige la zona", 
                        choices = names(CO_2019),
                        selected = "NO")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
            tabPanel("Promedio mensual",
                     h3(textOutput("output_text")),
                     plotOutput("plot")
                     ),
            tabPanel("Promedio anual",                #Pestaña de imágenes  <---------
                     img( src = "imagen.png", 
                          height = 450, width = 450)
            )
            )
        )
    )
))
