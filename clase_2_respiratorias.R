library(readxl)
library(tidyverse)
library(ISOweek)
library(tidyr)
library(highcharter)
library(tsibble)
library(lubridate)
library(geojsonsf)
library(echarts4r)
library(sf)
library(tmap)
library(leaflet)
library(DT)

##leo la tablas
datos_respiratorias <- 
  read_excel("RMD/RMD003_Analisis/datos/informacion-publica-respiratorias-nacional-hasta-20230706.xlsx")


##para otros años
datos_respiratorias2 <- read_excel("RMD/RMD003_Analisis/datos/informacion-publica-respiratorias-nacional-hasta-20220905.xlsx")