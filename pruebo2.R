library(shiny)
library(tidyverse)
library(gapminder)
datos=iris
datos$Species=toupper(datos$Species)

ui <- fluidPage(
  #primera fila - titulo
  fluidRow(
    column(12,
           br(),
           em(h1(strong("Título de la aplicación"))),
           br(),
           align="center")
  ),
  hr(),
  #segunda fila con 2 columna
  fluidRow(
    column(3,
           h2("Filtro"),
           selectInput(
             inputId = "selectSpecie",
             label = "Seleccionar especie",
             choices = unique(datos$Species),
             selected = "setosa"),
           align="center"),
    column(9,
           h2("Tabla"),
           plotOutput("graf"),
           align="center")
  )
  
)


server <- function(input, output, session) {
  output$graf <- renderPlot({
    ggplot(data  = datos, aes(Sepal.Length, Petal.Width, col=Species)) +
      geom_point()
    
  })
}
  shinyApp(ui, server)