library(shiny)
library(DT)
library(tidyr)
library(glue)
library(highcharter)
library(shinyjs)
library(clipr)
library(bslib)
library(dplyr)

datos = iris
datos$Species = toupper(datos$Species)

tema_untref = bs_theme(bootswatch = "cerulean")
tema_untref = bs_theme_update(tema_untref, bg = "#fff", base_font = font_google("Montserrat"),
                              fg = "#000")

ui <- fluidPage(
  # titulo
  useShinyjs(),
  theme = tema_untref,
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
             inputId = "selectSpecie",
             label = "Seleccioar especie:",
             choices = unique(datos$Species),
             selected = "VERSICOLOR"
           ),
           actionButton(
             "boton",
             "Ver filas #"
           ),
           actionButton(
             "clip",
             "Copiar datos"
           ),
           disabled(
             checkboxInput(
               inputId = "mostrar_boxplot",
               label = "Mostrar / ocultar boxplot",
               value = TRUE
             )  
           ),
           
           br(),
           actionButton(
             "procesar",
             "Procesar"
           ),
           
           disabled(
             actionButton(
               "limpiar",
               "Limpiar filtro"
             )  
           ),
           br(),
           br(),
           hidden(
             downloadButton(
               "descargar",
               "Descargar"
             )
           ),
           
           align = "center"
    ),
    column(width = 9,
           h2("Tabla"),
           hr(),
           DT::DTOutput("tabla"),
           br(),
           highchartOutput("boxplot"),
           textOutput("muestra_mensaje"),
           align = "center"
    )
    
  )
)

server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observeEvent(input$mostrar_boxplot, {
    
    if (input$mostrar_boxplot == T) {
      show("boxplot", anim = T, animType = "fade")
    } else {
      hide("boxplot", anim = T, animType = "flip")
    }
    
  })
  
  observeEvent(input$clip, {
    clipr::write_clip(datosProcesados())
    showNotification("Copiado!", type = "message")
  })
  
  datosProcesados = reactive({
    especieSeleccionada = input$selectSpecie
    datos = datos[datos$Species == especieSeleccionada,]
    datos
  })
  
  mensaje = eventReactive(input$boton,{
    filas = nrow(datosProcesados())
    glue("cantidad de filas {filas} ")
  })
  
  output$muestra_mensaje = renderText({
    mensaje()
  })
  
  
  observeEvent(input$procesar, {
    
    # inicia la barra de progreso
    withProgress(message = "Procesando datos...",{
      enable("mostrar_boxplot")
      show("tabla", anim = T, animType = "fade")
      show("boxplot", anim = T, animType = "fade")
      
      # progreso 10%
      
      incProgress(0.1)
      
      # progreso 50%
      incProgress(0.5)
      Sys.sleep(2)
      output$tabla = DT::renderDataTable({
        
        datos = datosProcesados()
        DT::datatable(datos, caption = paste0("Especie seleccionada: ", input$selectSpecie))
      })
      
      Sys.sleep(2)
      output$boxplot = renderHighchart({
        #browser()
        
        # bloque repetido
        datos = datosProcesados()
        
        datos = datos %>% pivot_longer(cols = 1:4, values_to = "Valores", names_to = "Medidas")
        datos$Species = NULL
        
        datos$Medidas = factor(datos$Medidas)
        datos = datos %>% as_tibble()
        
        # progreso 40%
        incProgress(0.4)
        
        # grafico
        data_boxplot=data_to_boxplot(
          data = datos,
          variable = Valores,
          group_var = Medidas,
          group_var2 = Medidas,
          add_outliers = F,
          fillColor = c('#a6611a','#dfc27d','#80cdc1','#018571'),
          color="black"
        )
        
        highchart()%>%
          hc_xAxis(type ="category")%>%
          hc_add_series_list(data_boxplot)%>%
          hc_xAxis(title = list(text = "Medida"))%>%
          hc_yAxis(title = list(text = "Centímetros"))%>%
          hc_title(text= glue("Boxplot para medidas de <em>{input$selectSpecie}</em>")) %>%
          hc_subtitle(text= "Medidas de pétalo y sépalo") %>%
          hc_legend(enabled= FALSE)
      })
      
      disable("selectSpecie")
      disable("procesar")
      enable("limpiar")
      show("descargar", anim = T, animType="fade")  
    })
    
    
  })
  
  observeEvent(input$limpiar, {
    hide("tabla", anim = T, animType = "fade")
    hide("boxplot", anim = T, animType = "fade")
    enable("selectSpecie")
    enable("procesar")
    disable("limpiar")
    disable("mostrar_boxplot")
    hide("descargar", anim = T, animType="fade")
    
  })
  
  output$descargar = downloadHandler(
    
    # nombre del archivo a descargar
    
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      
      # procesamiento de los datos para descargar
      datosParaDescargar = datosProcesados()
      datosParaDescargar = datosParaDescargar %>% arrange(Petal.Length)
      
      # descarga de los datos
      write.csv(datosParaDescargar, file, row.names = F)
    }
  )
  
}

shinyApp(ui, server)
