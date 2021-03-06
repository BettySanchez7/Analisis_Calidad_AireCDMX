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
        headerPanel("Concentración de contaminantes en el aire de la Ciudad de México"),
        sidebarPanel(
            selectInput("dataset", "Elige el parametro y año", 
                        c("CO_2019", "NO2_2019", "O3_2019", "PM10_2019", "SO2_2019",
                          "CO_2020", "NO2_2020", "O3_2020", "PM10_2020", "SO2_2020")),
            
            selectInput("y", "Elige la zona", 
                        choices = c("NO","NE","CE","SO","SE"),
                        selected = "NO")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Promedio mensual",             #Promedio mensual
                         h3(textOutput("output_text")),
                         plotOutput("plot"),
                         #textOutput("interpretacion")
                         img( src = "inter.png", 
                              height = 200, width = 700)
                ),
                tabPanel("Promedio anual",                #Promedio anual
                         img( src = "anios.png", 
                              height = 1560, width = 864)
                ),
                tabPanel("Series de tiempo",                #Series de tiempo
                         img( src = "tiempo.png", 
                              height = 900, width = 1350)
                ),
                tabPanel("Interpretacion datos IMECA",                #Interpretación IMECA
                         textOutput("IMECA"),
                         img( src = "imeca.png", 
                              height = 250, width = 500)
                         
                )
            )
        )
    )
))
