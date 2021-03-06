## II Parte del indicador de �reas verdes.
# Este script contiene el procesamiento de los datos para obtener los 
# resutlados finales (Walksheds y metros2 per c�pita)
# El script est� compuesto por dos secciones. 
# En la primera se calcula el n�mero de metros cuadrados per c�pita.
# La segunda secci�n contiene los walksheds de accesibilidad.

#### Primera parte: C�lculo de metros cuadrados per c�pita ####

# La secci�n para metros cuadrados consiste en un loop que aplica el c�lculo para cada ciudad.
# Para ello, se utilizar� una funci�n creada con tal fin.

library(data.table); library(dplyr); library(rgdal)

# archivo .csv que contiene datos sobre Nombres de ciudades, sus comunas y sus c�digos.
city_codes <- as.data.frame(fread("E:/Cedeus Sustainability Indicators/Datos/Study_codes.csv", sep=",", header= T))
# Vector que almacena los nombres de las ciudades
ciudades <- unique(city_codes$Ciudad)
# Carpetas donde estan las �reas verdes y la poblaci�n por manzana
carpeta_manzanas <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes/Poblacion Manzanas"
carpeta_aavv <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes/Procesado"

Area_per_capita_calculator <- function(carpeta_manzanas, carpeta_aavv, Ciudad){
  # Designar Ciudad a leer, e identificar shapefiles correspondientes a esta tanto para aavv como
  # l�mite urbano. Luego se leen los shps
  lista_aavv <- gsub(".dbf", "", list.files(carpeta_aavv, ".dbf"))
  nombre_aavv <- lista_aavv[grep(Ciudad, lista_aavv)]
  
  carpeta_manzanas <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes/Poblacion Manzanas"
  lista_manzanas <- gsub(".dbf", "", list.files(carpeta_manzanas, ".dbf"))
  nombre_manzanas <- lista_manzanas[grep(Ciudad, lista_manzanas)]
  
  manzanas <- readOGR(carpeta_manzanas, nombre_manzanas, encoding = "UTF-8")
  aavv <- readOGR(carpeta_aavv, nombre_aavv, encoding = "UTF-8")
  
  # Metros cuadrados por habitante, definido como la superficie de �reas verdes previamente definidas,
  # dividida por la poblaci�n total que viven en las manzanas
  
  pob_ciudad <- sum(manzanas@data$Pob)
  aavv_surface <- sum(sapply(slot(aavv, "polygons"), slot, "area"))
  m2_capita <- aavv_surface/pob_ciudad
  return(m2_capita)
}

m2_hab <- data.frame()
for (i in seq(ciudades)){
  Ciudad <- ciudades[i]
  m2_hab <- rbind(m2_hab, 
                  data.frame("Ciudad" = Ciudad, 
                             "Superficie por Habitante" = Area_per_capita_calculator(carpeta_manzanas, carpeta_aavv, Ciudad)))
}
print(m2_hab)