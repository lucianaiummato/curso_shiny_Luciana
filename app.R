library(shiny)
library(DT)
library(tidyr)
library(glue)
library(highcharter)
library(shinyjs)
library(clipr)
library(bslib)
library(dplyr)
library(readxl)
library(rclipboard)

#archivos

datos <- read_excel("TABLERO_MORTALIDAD.xlsx")
datosSexoEdad <- read_excel("TABLERO_MORTALIDAD.xlsx", sheet = "sexo_edad")



ui <- fluidPage(
  # titulo
  useShinyjs(),
  rclipboardSetup(),
  fluidRow(
    column(width = 12,
           h1(strong("Título de la aplicación")),
           align = "center"
    )
  ),
  
  hr(),
  
  # primera fila de la ui
  fluidRow(
    column(width = 3,
           
           h2("Filtros"),
           selectInput(
             inputId = "selectCAUSA",
             label = "Seleccionar causas:",
             choices = unique(datos$CAUSA),
           #  multiple = TRUE,
             selected = "ECNT"
           ),
           actionButton(
             "boton",
             "Ver filas"
           ),
           uiOutput(
             "clip"),
           checkboxInput(
             inputId =  "mostrar_grafico",
             label =   "Mostrar/ocultar gráfico",
             value = TRUE
           ),
           align = "center"
    ),
    column(width = 9,
           h2("Título 2 de la col 2"),
           hr(),
           DT::DTOutput("tabla"),
           br(),
           highchartOutput("grafico"),
           textOutput("muestra_mensaje"),
           align = "center"
    )
    
  )
)

server <- function(input, output, session) {
  observeEvent(input$mostrar_grafico, {
    if (input$mostrar_grafico == T) {
      show("grafico")
    } else {
      hide("grafico")
    }
  })
  
  # observeEvent(input$mostrar_grafico,{
  #   clipr::write_clip(datosProcesados(), allow_non_interactive = TRUE)
  # })
  #CAUSASeleccionada = input$selectCAUSA
  # datos = datos[datos$CAUSA == CAUSASeleccionada,]
  # datos
  
  output$clip = renderUI({
    copiar = datosProcesados() %>% as.data.frame()
    paste(input$clipbtn)
    tagList(
      rclipButton(
        inputId="clipbtn",
        label="Copiar", 
        clipText= copiar, 
        icon=icon("clipboard"))
    )
    
  })
  
  datosProcesados = reactive({
    
    filter(datos,CAUSA %in% input$selectCAUSA)
  })
  
  
  mensaje = eventReactive(input$boton,{
    filas = nrow(datosProcesados())
    glue("cantidad de filas {filas} ")
  })
  
  output$muestra_mensaje = renderText({
    mensaje()
  })
  
  output$tabla = DT::renderDataTable({
    
    datos = datosProcesados()
    DT::datatable(datos, caption = paste0("Causa seleccionada: ", input$selectCAUSA))
    
  })
 # browser()
  #reactive para filtrar la base
  datosSexoEdad_r <- reactive({
    filter(datosSexoEdad,CAUSA %in% input$selectCAUSA)
    
  })
  output$grafico <- renderHighchart({
  #  browser()
    # armo el grafico con highchart
    hc <- highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = "xxxx") %>%
      hc_xAxis(title = list(text = "Grupo de edad")) %>%
      hc_yAxis(title = list(text = "Tasa ")) %>% 
      hc_credits(
        enabled = TRUE, text = "xxx", href = "xxx", style = list(fontSize = "12px")
      ) %>% 
      hc_exporting(enabled = TRUE) # enable exporting option
    datosSexoEdad <- datosSexoEdad_r() 
    opciones_causas <- unique(datosSexoEdad$CAUSA) 
    # Agrega una serie de datos para cada nivel de "prov"
    for (i in opciones_causas) {
      data_serie <- datosSexoEdad[datosSexoEdad$CAUSA == i, ]  
      hc <- hc %>%
        hc_add_series(data_serie, "line", hcaes(x = GRUPO_EDAD, y = Tasa_varones), name = i,
                      marker = list(radius = 4))
    }
    print(hc)
    
  })
  
  }


shinyApp(ui, server)
