instalar = function (libreria) {
  if (libreria %in% installed.packages()[,"Package"]) {
    eval(parse(text=paste0("library(",libreria,")")))} else {
      install.packages(libreria)    
      eval(parse(text=paste0("library(",libreria,")")))
      library(libreria)
    }
}


instalar("glue")
instalar("foreign")
instalar("stringr")
instalar("readxl")
instalar("tidyverse")
instalar("dplyr")
instalar("ISOweek")
instalar("tidyr")
instalar("highcharter")
instalar("tsibble")
instalar("lubridate")
instalar("sf")
instalar("tmap")
instalar("leaflet")
instalar("DT")
instalar("classInt")
instalar("ggspatial")

# Define la lista de URLs a descargar
urls <- c(
  #1
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/f4096f8b-1692-4d5f-a6d8-09cae47931a4/download/vigilancia-respiratorias-agudas-2018-hasta-20200106.csv",
  #2
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/9a62f016-ba4e-4366-a883-22e9dc785d39/download/informacion-publica-respiratorias-nacional-hasta-20180626.csv",
  #3
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/0c29cc75-d81e-4f87-9257-4c30c24bfe55/download/vigilancia-de-infecciones-respiratorias-agudas-20181228.csv",
  #4
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/f2998314-9087-4641-aec7-f2f67c9ba865/download/informacion-publica-respiratorias-nacional-hasta-20220905.xls",
  #5
  "http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/37feeed4-dfcd-4f39-a403-93a6b4bd90d2/download/informacion-publica-respiratorias-nacional-hasta-20230706.xlsx"
  # Agrega más URLs según sea necesario
)

# Define los nombres de los archivos (pueden ser diferentes a los nombres originales)
nombres_archivos <- c(
  "vigilancia-respiratorias-agudas-2018-hasta-20200106.csv",
  "informacion-publica-respiratorias-nacional-hasta-20180626.csv",
  "vigilancia-de-infecciones-respiratorias-agudas-20181228.csv",
  "informacion-publica-respiratorias-nacional-hasta-20220905.xlsx",
  "informacion-publica-respiratorias-nacional-hasta-20230706.xlsx"
  # Agrega más nombres de archivos según sea necesario
)

# Carpeta de destino
carpeta_destino <- "C:/Users/Luciana/Desktop/Curso shiny/curso_shiny_Luciana/datos"

# Verifica y descarga los archivos desde las URLs
for (i in 1:length(urls)) {
  # Ruta completa del archivo en la carpeta /datos
  ruta_completa <- file.path(carpeta_destino, nombres_archivos[i])
  
  # Verifica si el archivo ya existe en la carpeta
  if (!file.exists(ruta_completa)) {
    # Si el archivo no existe, descárgalo desde la URL
    download.file(urls[i], destfile = ruta_completa, mode = "wb")
    cat("Archivo descargado y guardado en:", ruta_completa, "\n")
  } else {
    cat("El archivo ya existe en la carpeta:", ruta_completa, "\n")
  }
}

# Leer los archivos y asignar a objetos en R
datos <- list()  # Lista para almacenar los datos

for (i in 1:length(nombres_archivos)) {
  ruta_completa <- file.path(carpeta_destino, nombres_archivos[i])
  extension <- tools::file_ext(nombres_archivos[i])
  
  if (extension == "csv") {
    datos[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx") {
    datos[[i]] <- read_excel(ruta_completa)
  }
}

# Asignar a objetos con nombres específicos
for (i in 1:length(nombres_archivos)) {
  assign(paste0("datos", i), datos[[i]])
  cat("Datos", i, "cargados en objeto:", paste0("datos", i), "\n")
}
nombres_dataframes <- c("datos1", "datos2", "datos3", "datos4", "datos5")

# Loop para mostrar los nombres de las columnas de cada dataframe
for (nombre_df in nombres_dataframes) {
  if (exists(nombre_df)) {
    cat("Nombres de columnas en", nombre_df, ":\n")
    print(colnames(get(nombre_df)))
    cat("\n")
  } else {
    cat("El dataframe", nombre_df, "no existe.\n")
  }
}

#la renombro
# Loop para reemplazar el nombre de la columna 5  por "ano" en cada dataframe
for (nombre_df in nombres_dataframes) {
  if (exists(nombre_df)) {
    df <- get(nombre_df)  # Obtener el dataframe
    if (ncol(df) == 10) {
      colnames(df)[5] <- "ano"  # Reemplazar el nombre de la columna 5 por "ano"
      assign(nombre_df, df)  # Actualizar el dataframe en el entorno
      cat("Nombre de la columna 5 reemplazado por 'ano' en", nombre_df, "\n")
    } else {
      cat("El dataframe", nombre_df, "tiene un numero diferente de columnas, probablemente tenga otra estructura\n")
    }
  } else {
    cat("El dataframe", nombre_df, "no existe.\n")
  }
}

# Lista de dataframes (datos1, datos2, datos3, datos4, datos5)
lista<- list(datos1, datos2, datos3, datos4, datos5)

# Función para corregir el nombre de provincia
#algunas prvincias lo tienen mal escrito
#table(combined_df$provincia_nombre)

corregir_nombre <- function(df) {
  df %>% 
    mutate(provincia_nombre = stringr::str_replace(provincia_nombre, "Cordoba", "Córdoba")) %>%
    mutate(provincia_nombre = stringr::str_replace(provincia_nombre, "C¢rdoba","Córdoba")) %>%
    mutate(provincia_nombre = stringr::str_replace(provincia_nombre, "Tucuman", "Tucumán")) %>%
    mutate(provincia_nombre = stringr::str_replace(provincia_nombre, "Entre Rios", "Entre Ríos")) %>%
    mutate(provincia_nombre = stringr::str_replace(provincia_nombre, "Rio Negro", "Río Negro")) %>%
    mutate(provincia_nombre = stringr::str_replace(provincia_nombre, "Neuquen", "Neuquén"))
}

# Aplicar la función a cada dataframe en la lista
dataframes_list_corregidos <- map(lista, corregir_nombre)

# Unir los dataframes corregidos utilizando bind_rows
combined_df <- bind_rows(dataframes_list_corregidos)

table(combined_df$provincia_nombre)# verificar que esten bien los nombres

# Unir los dataframes después de corregir los nombres
combined_df <- bind_rows(dataframes_list_corregidos)

##aplico match basado en conicidencia exacta en variables
#"departamento_id", "provincia_nombre", "provincia_id", "ano", "semanas_epidemiologicas", "grupo_edad_id"
# y el merge con valor maximo.

summarized_df <- combined_df %>%
  filter(evento_nombre=="Enfermedad tipo influenza (ETI)")%>%##aca seleciono el evento
  group_by(departamento_id,provincia_nombre, provincia_id, ano, semanas_epidemiologicas,grupo_edad_id) %>%# match
  summarize(max_casos = max(cantidad_casos)) # criterio de merge (selecciono el mayr valor)

# Verificar duplicados en las columnas especificadas
duplicados <- summarized_df[duplicated(summarized_df[, c("departamento_id", "provincia_nombre", "provincia_id", "ano", "semanas_epidemiologicas", "grupo_edad_id")]), ]
nrow(duplicados)

## agrupo a nivel de provincias y semanas ya que es con lo que voy a cotruir mis sries de tiempo. 
datos_eti <- summarized_df %>%
  ungroup() %>% 
  group_by(provincia_nombre,ano, semanas_epidemiologicas) %>%
  summarize(casos = sum(max_casos))

DT::datatable(head(datos_eti))

###esta fn sirve para generar una variable fecha a aprtir de un vector de año y otro de semana EPI.
convert_epiweek <- function(year, week) {
  epiweek_date <- ifelse(year == 2020 & week == 53,
                         "2020-W53",
                         ifelse(year == 2020, ISOweek(ymd(
                           as.Date(paste(year, week, 1, sep = "-"), "%Y-%U-%u")
                         ) - weeks(1)),
                         ISOweek(as.Date(
                           paste(year, week, 1, sep = "-"), "%Y-%U-%u"
                         ))))
  return(epiweek_date)
}

datos_eti <- datos_eti %>% ungroup() %>%
  mutate(semana = 
           yearweek(convert_epiweek(ano, semanas_epidemiologicas)))
datos_eti$semana2 <- as.Date(datos_eti$semana)
datos_eti$semana3 <- as.POSIXct(datos_eti$semana)
DT::datatable(head(datos_eti))

ts_base = datos_eti %>%
  group_by(provincia_nombre, semana) %>% 
  summarise(casos = sum(casos))

ts_base$semana = as.character(ts_base$semana)
DT::datatable(ts_base)

grid = list(
  provincia_nombre = unique(ts_base$provincia_nombre),
  semana = unique(as.character(ts_base$semana))
)

grid = expand.grid(grid)
data_grafico = left_join(grid, ts_base %>% as.data.frame)
data_grafico$casos[is.na(data_grafico$casos)] = 0

DT::datatable(grid)

grafico =
  highchart() %>%
  hc_chart(type = "line",
           zoomType = 'xy') %>% #aca defino el tipo de grafico y si quiero la funcion para hacer zoom a algun eje
  hc_title(text = "Notificaciones de ETI por SEPI") %>% #titulo
  hc_xAxis(categories = unique(ts_base$semana))%>% ## ejes
  hc_yAxis(title = list(text = "Notificaciones")) #titulos de ejes

provincias_seleccionadas = c("Buenos Aires", "Córdoba", "CABA", "Santa Fe", "Mendoza")

#aca se hace un loop donde se itera por las provincias seleccionadas usando la fn de hc_add_series del paquete highcharter.

for (i in provincias_seleccionadas) {
  casos = data_grafico$casos[data_grafico$provincia_nombre == i]
  grafico = grafico %>% hc_add_series(name = i, data = casos)
}

grafico

##https://jkunst.com/highcharter/articles/themes.html#themes-19
grafico %>% 
  hc_add_theme(hc_theme_google())
#armo tabla de porcentajes para grafico de torta
torta <- summarized_df %>%
  filter(ano == 2022) %>%
  group_by(grupo_edad_id) %>%
  summarise(casos = sum(max_casos)) %>%
  mutate(porcent = round(casos / sum(casos) * 100, 1))

#ordeno las categorias de grupos de edad para que esten de menor a mayor
torta <- torta %>%
  mutate(
    grupo_edad = case_when(
      grupo_edad_id == 1 ~ "1. Menor de 4 años",
      grupo_edad_id == 2 ~ "1. Menor de 4 años",
      grupo_edad_id == 6 ~ "3. 10 a 14",
      grupo_edad_id == 3 ~ "1. Menor de 4 años",
      grupo_edad_id == 7 ~ "4. 15 a 19",
      grupo_edad_id == 4 ~ "1. Menor de 4 años",
      grupo_edad_id == 8 ~ "5. 20 a 64",
      grupo_edad_id == 9~ "5. 20 a 64",
      grupo_edad_id == 10 ~ "5. 20 a 64",
      grupo_edad_id == 11 ~ "5. 20 a 64",
      grupo_edad_id == 5~ "2. 5 a 9",
      grupo_edad_id == 12 ~ "6. más de 65",
      grupo_edad_id == 13 ~ "6. más de 65",
      grupo_edad_id == 17~ "7. Edad Sin Esp.",
      TRUE ~ as.character(grupo_edad_id)
    )
  ) %>% group_by(grupo_edad) %>% 
  summarise(porcent=sum(porcent)) %>% 
  arrange(as.numeric(substring(grupo_edad, 1, 2)))

# armo tabla de n para el gráfico de barras
barras <- summarized_df %>%
  filter(ano == 2022) %>%
  group_by(semanas_epidemiologicas) %>%
  summarise(casos = sum(max_casos)) %>%
  mutate(porcent = round(casos / sum(casos) * 100, 1))

# asi quedaron ambas tablas
DT::datatable(torta)

chart1 <- highchart() %>%
  hc_add_series(# agrego serie para barras
    barras,
    "column", hcaes(
      x = semanas_epidemiologicas, y = casos
    ),
    name = "Casos de ETI"
  ) %>%
  hc_add_series(#Agrego serie para torta
    torta, "pie", hcaes(
      name = grupo_edad, y = porcent
    ),
    name = "Casos de ETI por grupo de edad (%)"
  ) %>%
  ## en ociones puedo definir como quiero ver los labels,y la ubicacion y tamaño
  hc_plotOptions(
    series = list(
      showInLegend = FALSE,
      pointFormat = "{point.y}%",
      colorByPoint = FALSE
    ), 
    pie = list(## caracteristicas del pies
      center = c("65%", "10%"),
      size = 120,
      dataLabels = list(enabled = FALSE),
      colorByPoint = TRUE
    ),
    column = list(groupPadding = 0,#características de las barras
                  pointPadding = 0,
                  borderWidth = 0.3,
                  borderColor = "white",# color de los bordes
                  color= "#377eb8" #color de la barrra
    )
  )%>%
  ## Axis
  hc_yAxis(
    title = list(text = "Número de casos"),
    labels = list(format = "{value}"),
    max = 30000
  ) %>%
  hc_xAxis(title = list(text = "Semana EPI"),
           categories = barras$semanas_epidemiologicas
  ) %>%
  ## Titles, subtitle, caption and credits
  hc_title(
    text = "Grafico de barras combinado con piechart: Notificaciones de ETI, 2022"
  ) %>%
  hc_subtitle(
    text = "Ejemplo de grafico combinado para notificaciones de eti por semana y gráfico de torta por grupo de edad"
  ) %>%
  hc_caption(
    text = "Se representatan casos notificados de ETI al SNVS 2.0"
  ) %>%
  hc_credits(
    enabled = TRUE, text = "Fuente: Datos abiertos/ SNVS", href = "http://datos.salud.gob.ar/", style = list(fontSize = "12px")
  ) %>% 
  hc_exporting(enabled = TRUE) # enable exporting option
chart1

#armo tabla de porcentajes para grafico de torta
barras_apiladas <- summarized_df %>%
  filter(ano == 2022) %>%
  group_by(grupo_edad_id, semanas_epidemiologicas) %>%
  summarise(casos = sum(max_casos)) %>%
  group_by(semanas_epidemiologicas) %>% 
  mutate(porcent = round(casos / sum(casos) * 100, 1))

#ordeno las categorias de grupos de edad para que esten de menor a mayor
barras_apiladas <- barras_apiladas %>%
  mutate(
    grupo_edad = case_when(
      grupo_edad_id == 1 ~ "1. < 6 m",
      grupo_edad_id == 2 ~ "2. 6 a 11 m",
      grupo_edad_id == 6 ~ "6. 10 a 14",
      grupo_edad_id == 3 ~ "3. 12 a 23 m",
      grupo_edad_id == 7 ~ "7. 15 a 19",
      grupo_edad_id == 4 ~ "4. 2 a 4",
      grupo_edad_id == 8 ~ "8. 20 a 24",
      grupo_edad_id == 9~ "9. 25 a 34",
      grupo_edad_id == 10 ~ "10. 35 a 44",
      grupo_edad_id == 11 ~ "11. 45 a 64",
      grupo_edad_id == 5~ "5. 5 a 9",
      grupo_edad_id == 12 ~ "12. 65 a 74",
      grupo_edad_id == 13 ~ "13. >= a 75",
      grupo_edad_id == 17~ "14. Edad Sin Esp.",
      TRUE ~ as.character(grupo_edad_id)
    )
  ) %>%
  arrange(as.numeric(substring(grupo_edad, 1, 2))) %>% 
  mutate(grupo_edad = grupo_edad %>% fct_reorder(grupo_edad_id, .desc = FALSE))

bar_chart <- highchart() %>%
  hc_xAxis(categories = unique(barras_apiladas$semanas_epidemiologicas),title = list(text = "Semana Epidemiológica"),title = list(text = "Semana EPI")) %>%
  hc_yAxis(
    title = list(text = "%")
  ) %>%  
  hc_add_series(
    data = barras_apiladas,
    type = "column",
    hcaes(x = semanas_epidemiologicas, y = porcent, group = grupo_edad,
          name= grupo_edad),
    stacking = "percent",  # Apilar al 100%
    
  ) %>%
  
  hc_tooltip(
    headerFormat = "",
    pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: <b>{point.y:.1f}%</b>"
  )%>% 
  hc_plotOptions(
    
    column = list(groupPadding = 0,#características de las barras
                  pointPadding = 0,
                  borderWidth = 0.3,
                  borderColor = "white",
                  stacking = TRUE)
  ) %>% 
  hc_size(height = 300, width =500) %>% 
  hc_legend(itemStyle = list(fontSize = "7px")) %>%  # Cambiar el tamaño de la leyenda
  hc_exporting(enabled = TRUE) # enable exporting optio


# Mostrar el gráfico de barras apiladas al 100%
chart2 <- highchart() %>%
  hc_add_series(# agrego serie para barras
    barras,
    "column", hcaes(
      x = semanas_epidemiologicas, y = casos
    ),
    name = "Casos de ETI"
  ) %>%
  ## en ociones puedo definir como quiero ver los labels,y la ubicacion y tamaño
  hc_plotOptions(
    
    series = list(
      showInLegend = FALSE,
      pointFormat = "{point.y}%",
      colorByPoint = FALSE
    ), 
    
    column = list(groupPadding = 0,#características de las barras
                  pointPadding = 0,
                  borderWidth = 0.3,
                  borderColor = "white",# color de los bordes
                  color= "#377eb8" #color de la barrra
    )
  )%>%
  ## Axis
  hc_yAxis(
    title = list(text = "Número de casos"),
    labels = list(format = "{value}"),
    max = 30000
  ) %>%
  hc_xAxis(title = list(text = "Semana EPI"),
           categories = barras$semanas_epidemiologicas
  ) %>%
  hc_size(height = 300, width = 800) %>% 
  
  hc_exporting(enabled = TRUE) # enable exporting option

### los uno en un grid
hw_grid(list(chart2, bar_chart), ncol=1,rowheight=300)
