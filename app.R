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
datos <-rename(datos, "TBM VARONES" = TBM_VARONES, "TAE VARONES" = TAE_VARONES, "TBM MUJERES" = TBM_MUJERES, "TAE MUJERES" = TAE_MUJERES )
datos <-datos %>% mutate_if(is.numeric, round, digits=2) 
datosSexoEdad <-datosSexoEdad %>% mutate_if(is.numeric, round, digits=2) 
tema_untref = bs_theme(bootswatch = "cerulean")

ui <- fluidPage(
  # titulo
  useShinyjs(),
  theme = tema_untref,
  rclipboardSetup(),
  fluidRow(
    column(width = 12,
           h1(strong("Mortalidad por ECNT")),
           h2(strong("Datos ficticios/borrador")),
           align = "center"
    )
  ),
  
  hr(),
  
  # primera fila de la ui
  fluidRow(
    column(width = 3,
           
           h3("Filtro"),
           selectInput(
             inputId = "selectCAUSA",
             label = "Seleccionar causas de muerte:",
             choices = unique(datos$CAUSA),
           #  multiple = TRUE,
             selected = "ECNT"
           ),
           
           uiOutput(
             "clip"),
           br(),
           downloadButton(
             "descargar",
             "Descargar"
           ),
           
           checkboxInput(
             inputId =  "mostrar_grafico",
             label =   "Mostrar/ocultar gráfico",
             value = TRUE
           ),
           align = "center"
    ),
    column(width = 9,
           h4("Tabla. Tasas de mortalidad cada 100.000 hab. Argentina, año 2021"),
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
  
  observeEvent(input$mostrar_grafico, {
    
    if (input$mostrar_grafico == T) {
      show("grafico", anim = T, animType = "fade")
    } else {
      hide("grafico", anim = T, animType = "flip")
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
        label="Copiar datos", 
        clipText= copiar, 
        icon=icon("clipboard"))
    )
    
  })
  #reactive para filtrar la base1
  datosProcesados = reactive({
    
    filter(datos,CAUSA %in% input$selectCAUSA)
  })
  
  
  # mensaje = eventReactive(input$boton,{
  #   filas = nrow(datosProcesados())
  #   glue("cantidad de filas {filas} ")
  # })
  
  # output$muestra_mensaje = renderText({
  #   mensaje()
  # })
  
  output$tabla = DT::renderDataTable({
    
    datos = datosProcesados()
    DT::datatable(datos, caption = paste0("Causa seleccionada: ", input$selectCAUSA))
    
  })
 # browser()
  #reactive para filtrar la base2
  datosSexoEdad_r <- reactive({
    filter(datosSexoEdad,CAUSA %in% input$selectCAUSA)
    
  })
  output$grafico <- renderHighchart({
  #  browser()
    # armo el grafico con highchart
    hc <- highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = "Tasa de mortalidad c/ 100.000 hab. según grupos de edad. Argentina, 2021") %>%
      hc_xAxis(title = list(text = "Grupo de edad")) %>%
      hc_yAxis(title = list(text = "Tasa ")) %>% 
      hc_xAxis(categories = datosSexoEdad$GRUPO_EDAD)%>% 
      hc_credits(
        enabled = TRUE, text = "Fuente: DEIS. Ministerio de Salud de la Nación", href = "DEIS Ministerio de Salud de la Nación", style = list(fontSize = "12px")
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
  
  output$descargar = downloadHandler(
    
    # nombre del archivo a descargar
    
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      
      # procesamiento de los datos para descargar
      datosParaDescargar = datosProcesados()
      datosParaDescargar = datosParaDescargar 
      
      # descarga de los datos
      write.csv(datosParaDescargar, file, row.names = F)
    }
  )
  
  }


shinyApp(ui, server)
