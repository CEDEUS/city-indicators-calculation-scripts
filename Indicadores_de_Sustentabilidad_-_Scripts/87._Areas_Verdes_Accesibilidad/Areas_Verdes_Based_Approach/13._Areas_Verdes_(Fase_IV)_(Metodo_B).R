# Fase IV 
# En esta, se lee el shp de los walksheds y áreas verdes para determinar
# el total de población que tiene acceso a áreas verdes.
# El script se compone de un loop que:
# Por cada ciudad:
# -   Lee el shapefile de los walksheds, poblacion por manzanas, y áreas verdes
# -   re-projecta el shapefile de walksheds para compatibilizarlo con los otros dos.
# -   Hace un subset del shapefile de walkshed a partir del shape de manzanas 
#     ie: (cuantos walksheds están en contacto con áreas verdes?)
# -   Identifica las manzanas cuyos walksheds están en contacto con áreas verdes.
# -   Calcula la población con acceso a áreas verdes, la población sin acceso, y la polbación total.
# -   Añade estos resultados a una tabla, la cual posteriormente es exportada a .csv
# -   Se exportan dos shp: manzanas con acceso, y manzanas sin acceso, para posterior visualización
#     y/o análisis en GIS.
t1 <- Sys.time()

library(rgdal); library(rgeos); library(dplyr)

# Paso 1: identificar las ciudades a ser trabajadas:
codigos_ciudades <- read.csv("E:/Cedeus Sustainability Indicators/Datos/Study_codes.csv", sep=",")
# Vector que almacena los nombres de las ciudades
ciudades <- unique(codigos_ciudades$Ciudad)
# Carpetas donde están almacenados los shapefiles, así como el destino de los outputs.
# Modificar a necesidad de reproducibilidad.
areas_verdes.carpeta <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes/Areas Verdes Procesadas"
poblacion_manzanas.carpeta <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes/Poblacion Manzanas"
walksheds.carpeta <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes/Walksheds (Areas Verdes)/UnDissolved"
accesibilidad.carpeta <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes/Resultados (Areas Verdes)"
tabla_accesibilidad.carpeta <- "E:/Cedeus Sustainability Indicators/Indicadores/Indicadores/"
# Tabla donde almacenar los resultados de accesibilidad
accesibilidad.tabla <- data.frame()
# Loop descrito en la introducción.
for (c in seq(ciudades)){
  ciudad <- ciudades[c]
  # Leer cada shapefie. Cada shapefile sigue un patrón para su nombre. En el caso de áreas verdes,
  # se debe modificar el nombre según el tipo de criterio que se utilizó para su análisis (Fuerte, medio, leve).
  # Adicionalmente, se asegura que todos los shp estén en la misma projección. En este caso, el shp de areas verdes
  # se usan como base.
  areas_verdes.shp <- readOGR(areas_verdes.carpeta, paste(ciudad, "_AAVV_(Fuerte)", sep = ""), encoding = "UTF-8")
  poblacion_manzanas.shp <- readOGR(poblacion_manzanas.carpeta, paste(ciudad, "Poblacion_Manzanas", sep = "_"), encoding = "UTF-8") %>%
    spTransform(proj4string(areas_verdes.shp))
  walksheds.shp <- readOGR(walksheds.carpeta, paste(ciudad, "_Walksheds (UnDissolved)", sep = ""), encoding = "UTF-8") %>%
    spTransform(proj4string(areas_verdes.shp))
  # Los walksheds que tienen contacto con una manzana
  manzanas.subset <- poblacion_manzanas.shp[walksheds.shp,]
  # Identificar las manzanas con acceso según ID, las que no tienen acceso, y exportar a shp
  accesibilidad.shp <- poblacion_manzanas.shp[,c("ID_W", "Pob")]
  accesibilidad.shp@data$Acceso <- "Dummy"
  accesibilidad.shp@data[accesibilidad.shp@data$ID_W %in% 
                           manzanas.subset@data$ID,]$Acceso <- "Sí"
  accesibilidad.shp@data[!(accesibilidad.shp@data$ID_W %in% 
                             manzanas.subset@data$ID),]$Acceso <- "No"
  writeOGR(as(accesibilidad.shp, "SpatialPolygonsDataFrame" ), 
           paste(accesibilidad.carpeta, sep = ""), 
           paste(ciudad, "_Accesibilidad", sep = ""),
           driver = "ESRI Shapefile",overwrite_layer=TRUE)
  # Calcular la población con acceso y sin acceso
  accesibilidad.tabla_tmp <- data.frame("Ciudad" = ciudad,
                                        "Población con Acceso" = sum(accesibilidad.shp@data$Pob[accesibilidad.shp@data$Acceso == "Sí"]),
                                        "Población sin Acceso" = sum(accesibilidad.shp@data$Pob[accesibilidad.shp@data$Acceso == "No"]),
                                        "Población total" = sum(accesibilidad.shp@data$Pob))
  accesibilidad.tabla <- rbind(accesibilidad.tabla, accesibilidad.tabla_tmp)

}

# Crear uan tabla de accesibilidad, pero con datos en porcentajes.

accesibilidad.tabla_porcentajes <- accesibilidad.tabla %>%
  group_by(Ciudad) %>%
  mutate("Población.con.Acceso" = round(Población.con.Acceso/Población.total, 2),
         "Población.sin.Acceso" = round(Población.sin.Acceso/Población.total, 2),
         "Población.total" = 100)

# Exportar ambas tablas en la carpeta de indicadores

write.csv(accesibilidad.tabla, paste(tabla_accesibilidad.carpeta,"Accesibilidad ", ciudad, "(Población) (Método B).csv", sep = ""))
write.csv(accesibilidad.tabla_porcentajes, paste(tabla_accesibilidad.carpeta,"Accesibilidad ", ciudad, "(Porcentajes) (Método B).csv", sep = ""))
# Limpiar el entorno
rm(list=ls())
