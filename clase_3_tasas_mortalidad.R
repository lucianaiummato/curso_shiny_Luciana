library(dplyr)
options(timeout=1000000) # incrementamos el timeout debido a que la descarga es lenta

url = "http://datos.salud.gob.ar/dataset/27c588e8-43d0-411a-a40c-7ecc563c2c9f/resource/fab9e990-865c-43c4-a643-3dbc3b70a934/download/defunciones-ocurridas-y-registradas-en-la-republica-argentina-anos-2005-2021.csv"

download.file(url, destfile = "defunciones.csv")

defunciones = read.csv("defunciones.csv", encoding = "latin1")
unlink("defunciones.csv")

defunciones = defunciones[defunciones$anio >= 2010,] 

# observamos la estructura del data frame
str(defunciones)

# eliminamos las variables cie10_causa_id, muerte_materna_id y muerte_materna_clasificacion que no vamos a usar
defunciones$cie10_causa_id = NULL
defunciones$cie10_clasificacion = NULL
defunciones$muerte_materna_id = NULL
defunciones$muerte_materna_clasificacion = NULL

unique(defunciones$grupo_edad)

library(readr)
url = "https://raw.githubusercontent.com/agsantoro/untref2023/main/RMD/RMD01_Poblacion/problacion_prov.csv"
download.file(url,dest="poblacion.csv")
poblacion = read_xlsx("proyecciones.xlsx") %>% as.data.frame()

unique(poblacion$edad)

unique(defunciones$grupo_edad)

grupos = unique(poblacion$edad)

poblacion$edad[poblacion$edad %in% grupos[1:3]] = "01.De a 0  a 14 anios"
poblacion$edad[poblacion$edad %in% grupos[4:7]] = "02.De 15 a 34 anios"
poblacion$edad[poblacion$edad %in% grupos[8:11]] = "03.De 35 a 54 anios"
poblacion$edad[poblacion$edad %in% grupos[12:15]] = "04.De 55 a 74 anios"
poblacion$edad[poblacion$edad %in% grupos[16:21]] = "05.De 75 anios y mas"

unique(poblacion$edad)
unique(defunciones$grupo_edad)

poblacion = poblacion %>% group_by(ano,juri,juri_nombre,sexo_codigo,sexo_nombre,edad) %>% summarise(poblacion=sum(poblacion))
colnames(poblacion)

colnames(poblacion)

defunciones = defunciones %>% dplyr::select(ano = anio, 
                                            juri = jurisdiccion_de_residencia_id,
                                            sexo_codigo = sexo_id,
                                            edad = grupo_edad,
                                            cantidad)

head(defunciones)

defunciones = rbind(
  defunciones,
  defunciones %>% mutate(juri=1))

defunciones = rbind(
  defunciones,
  defunciones %>% mutate(sexo_codigo=0))

grid = list(
  ano=unique(defunciones$ano),
  juri=unique(defunciones$juri),
  edad=unique(defunciones$edad),
  sexo_codigo=unique(defunciones$sexo_codigo)
)

grid = expand.grid(grid)

defunciones = left_join(grid,defunciones)
defunciones[is.na(defunciones)] = 0

# agrupamos para volver a sumarizar la tabla luego de todos los cambios
defunciones = defunciones %>% group_by(ano,juri,sexo_codigo,edad) %>% 
  summarise(cantidad=sum(cantidad))

library(epitools)

# convertimos las variables de código en numéricas para que puedan vincularse sin problema
poblacion$juri = as.numeric(poblacion$juri)
poblacion$sexo_codigo = as.numeric(poblacion$sexo_codigo)

# definimos la población total de Argentina en 2020 como población standard
poblacion_standard = poblacion$poblacion[poblacion$ano=="2020" &
                                           poblacion$sexo_nombre=="Ambos sexos" &
                                           poblacion$juri=="1"]

# creamos un data frame vacío donde se van a ir agregando los indicadores calculados
tasas_resultado = data.frame()

for (a in unique(defunciones$ano)) { # recorremos los años
  for (j in unique(defunciones$juri[defunciones$juri %in% 1:94])) { # dentro de cada jurisdicción, recorremos las jurisdicciones
    for (s in 0:2) { #dentro de cada jurisdicción, recorremos las categorías de sexo
      def_data = defunciones[defunciones$ano == a &
                               defunciones$juri == j &
                               defunciones$sexo_codigo == s,] %>% left_join(poblacion) # creamos un data frame con la información del año / jurisdicción / sexo activos
      
      defunciones_n = sum(def_data$cantidad) # calculamos el total de defunciones
      tasa_cruda = sum(def_data$cantidad)/sum(def_data$poblacion[is.na(def_data$poblacion)==F]) * 100000 # calculamos la tasa cruda
      
      def_data = def_data[def_data$edad!="06.Sin especificar",] # eliminamos la fila de edad sin especificar para cumplir con el modelo de datos que utliza la librería epitools
      
      # ahora creamos el dataset para anexar en esta vuelta del lopp
      ano = first(def_data$ano) # año
      juri_codigo = first(def_data$juri) # código de la jurisdicción
      juri_nombre = first(def_data$juri_nombre) # nombre de la jurisdicción
      sexo_codigo = first(def_data$sexo_codigo) # código de sexo
      sexo_nombre = first(def_data$sexo_nombre) # nombre de sexo
      
      # usamos la función ageadjust.direct para ajustar las tasas
      tasas = epitools::ageadjust.direct(
        count = def_data$cantidad, # en count se indica la cantidad de defunciones
        pop = def_data$poblacion, # en pop la población
        stdpop = poblacion_standard, # en stdpop la población utilizada como standard
        conf.level = .95 # en conf.level el nivel de confianza seleccionado para la estimación de los intervalos de confianza
      ) *100000
      
      # ordenamos todos los resultados en un data frame y lo añadimos a resultados
      
      append = data.frame(
        ano,
        juri_codigo,
        juri_nombre,
        sexo_codigo,
        sexo_nombre,
        defunciones_n,
        tasa_cruda,
        tasas[1],
        tasas[2],
        tasas[3],
        tasas[4]
      )
      
      colnames(append)[8:11] = names(tasas)
      rownames(append) = 1
      tasas_resultado = rbind(
        tasas_resultado,
        append 
      ) %>% arrange(juri_codigo,sexo_codigo)
    }
  }  
}

library(DT)
DT::datatable(tasas_resultado, rownames = F)
