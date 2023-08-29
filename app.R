library(shiny)
library(DT)
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
           DT::DTOutput("tabla"),
           align="center")
  )
  
)

server <- function(input, output, session) {
  
  output$tabla= DT::renderDataTable({
    
    especieSeleccionada=input$selectSpecie
    
    datos = datos[datos$Species == especieSeleccionada,]
    
    DT::datatable(datos, caption = paste0("Especie seleccionada: ",especieSeleccionada))  })
  
}

shinyApp(ui, server)