library(shiny)
library(tidyverse)
library(tidyr)
library(DT)
library(lubridate)
library(shinyWidgets)
library(highcharter)

#descargo el dataset 

options(timeout=1000000) # incrementamos el timeout debido a que la descarga es lenta

url = "http://datos.salud.gob.ar/dataset/2eff770c-1c2b-4a22-9281-c3b5e9412086/resource/c1253897-d507-41f7-a3e1-6ed756e7243b/download/tasa-mortalidad-infantil-deis-1990-2021.csv"

download.file(url, destfile = "TMI.csv")

data = read.csv("TMI.csv")
unlink("TMI.csv")

DT::datatable(data)

###proceso los datos para graficar
bd <- data %>% pivot_longer(
  cols = !indice_tiempo,
  names_to = "prov",
  values_to = "TMI"
) %>%
  mutate(
    prov = str_sub(prov, 21, nchar(prov)),
    ano = year(ymd(indice_tiempo)),
    indice_tiempo = ymd(indice_tiempo),
    prov=case_when(prov == "cordoba" ~ str_to_title(prov),
                   prov == "caba"  ~ "CABA",
                   prov == "argentina" ~ str_to_title(prov),
                   prov == "corientes" ~ str_to_title(prov),
                   prov == "chaco" ~ str_to_title(prov),
                   prov == "chubut" ~ str_to_title(prov),
                   prov == "neuquen" ~ str_to_title(prov),
                   prov == "misiones" ~ str_to_title(prov),
                   prov == "jujuy" ~ str_to_title(prov),
                   prov == "catamarca" ~ str_to_title(prov),
                   prov == "corrientes" ~ str_to_title(prov),
                   prov == "formosa" ~ str_to_title(prov),
                   prov == "salta" ~ str_to_title(prov),
                   prov == "buenosaires" ~ "Buenos Aires",
                   prov == "santiagodelestero" ~ "Santiago del Estero",
                   prov == "santafe" ~ "Santa Fe",
                   prov == "tierradelfuego" ~ "Tierra del Fuego",
                   prov == "santacruz" ~ "Santa Cruz",
                   prov == "sanjuan" ~ "San Juan",
                   prov == "sanluis" ~ "San Luis",
                   prov == "lapampa" ~ "La Pampa",
                   prov == "larioja" ~ "La Rioja",
                   prov == "entrerios" ~ "Entre Rios",
                   prov == "rionegro" ~ "Rio Negro",
                   TRUE ~ prov  # Mantén el valor original para otros casos
    )
  ) %>% 
  select(-indice_tiempo)

DT::datatable(bd)


# Defino UI para mi aplicación
ui <- fluidPage(
  fluidRow(
    column(12,
           h1(strong("Mortalidad infantil en Argentinan")),
           align="center")
  ),
  fluidRow(
    column(12,
           h2("Seleccione una jurisdicción"),
           selectInput(
             inputId = "provinciaSeleccionada",
             label = "Seleccionar provincia",
             choices = unique(bd$prov),
             selected = "Buenos Aires"),
           align="center")
  ),
  fluidRow(
    column(12,
           h2("Serie de tiempo de TMI por 1000 nacidos vivos, Argentina, periodo 1990-2021"),
           plotOutput("grafico"),
           align="center")
  )
)

# Defino server
# Defino server
server <- function(input, output) {
  
  output$grafico= renderPlot({ escoger <- bd %>% 
      filter(prov == input$provinciaSeleccionada)
      ggplot(data  = escoger, aes(ano, TMI, group=prov, color=prov)) +
      geom_line(size=1) + 
        ylab("TMI")+
        xlab("Años")+
        theme(axis.title=element_text(size=10,face="bold"),
              legend.title = element_blank(),
              panel.background = element_rect(fill = "#FFFFFF"),
              axis.line = element_line(color = "grey"))
   
  })
  
  
}

# Corro la application
shinyApp(ui = ui, server = server)
