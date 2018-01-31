# Script para calcular accesibilidad a equipamiento deportivo.
# Se define como la población que se encuentra a 15 minutos de distancia de
# un equipamiento deportivo (independiente de su clasificación o propiedad)
# Se define como distancia caminable aquella distancia que se recorre a una
# Velocidad de 4.8 km/hr. El algoritmo considera los tipos de calle y limita
# la accesibilidad en función de estos.
# La lista de equipamiento deportivo fue obtenida a través de solicitud de
# Transparencia al Ministerio del Deporte.

# Cargar librerias
library(dplyr); library(rgdal)

# Definir carpeta de trabajo.
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/70._Acceso_a_equipamiento_deportivo/")

eq_deportivo <- read.csv("Raw_Data/data_deportiva.csv", sep = ";", encoding = "UTF-8")
# La tabla contiene equipamiento que no corresponde a equipamiento deportivo
# por lo que se eliminan de la lista.
invalid <- grep("COLEGIO|ESCUELA|LICEO|JARDIN|INTERNADO|SCHOOL|INSTITUTO|RETEN|EDUCACIONAL", eq_deportivo$NOMBRE.RECINTO, value = T)
eq_deportivo <- eq_deportivo[!(eq_deportivo$NOMBRE.RECINTO %in% invalid),]
# A su vez, se filtran aquellos equipamientos de juntas de vecinos
# Que no correspondan a canchas, multicanchas o gimasios.
nombres_junta <- grep("JUNTA", eq_deportivo$NOMBRE.RECINTO, value = T)
eq_juntas <- eq_deportivo[(eq_deportivo$NOMBRE.RECINTO %in% nombres_junta),]
canchas <- grep("CANCHA|GIMNASIO", eq_juntas$NOMBRE.RECINTO, value = T)
eq_deportivo <- eq_deportivo[!(eq_deportivo$NOMBRE.RECINTO %in% canchas),]

# Se analizan sólo las que sean de acceso público
eq_deportivo <- eq_deportivo[!(eq_deportivo$SECTOR == "Privado"),]

coords <- eq_deportivo[,1:2]
deporte.spatial <- SpatialPointsDataFrame(coords = coords, data = eq_deportivo[3:4],
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

writeOGR(obj = deporte.spatial, dsn = "Spatial_Data", layer = "Equipamiento Deportivo", 
         driver = "ESRI Shapefile", overwrite_layer = T)


ciudades <- read.csv("Otros/Ciudades_Cedeus.csv")
# Función que lee y subsetea los puntos dentro del límite urbano
ciudades <- unique(ciudades$Ciudad)
deporte.limurbano <- function(deporte, ciudad){
  limite.urbano <- readOGR("Spatial_Data/Limite_Urbano_Ciudades", paste("LimiteUrbano_", ciudad, sep = ""))
  limite.urbano <- spTransform(limite.urbano, proj4string(deporte))
  deporte <- deporte[limite.urbano,]
  writeOGR(deporte, "Spatial_Data/Equipamiento_Deportivo_Ciudades", 
           paste("Equipamiento_Deportivo_", ciudad, sep = ""),
           driver = "ESRI Shapefile", overwrite_layer = T)
  coords <- as.data.frame(deporte@coords)
  coords$ID <- 1:nrow(coords)
  coords$Ciudad <- ciudad
  return(coords)
}
coords.dataframe <- data.frame()
# Crear un shp con el equipamiento deportivo por cada ciudad
for (i in 1:length(ciudades)){
  ciudad <- ciudades[i]
  deporte.ciudad <- deporte.limurbano(deporte.spatial, ciudad)
  coords.dataframe <- rbind(coords.dataframe, deporte.ciudad)
}
colnames(coords.dataframe) <- c("lon", "lat", "ID", "Ciudad")
write.csv(coords.dataframe, "Spatial_Data/ED_coords/Coords_Deporte.csv", row.names = F)


library(RCurl)

# Función que realiza la consulta a OTP. Utiliza 6 variables:
#   - coordenadas: las coordenadas del punto de origen del walkshed
#   - viaje.tiempo: tiempo que dura el viaje en minutos. Para este indicador, 10 minutos.
#   - viaje.velocidad: velocidad a la que se realiza el viaje. Para caminata se consideran 
#     1.38 m/s, o 5 km/h aprox.
#   Los siguientes parámetros son propios de la API de Open Trip Planner.
#   - modo: por defecto será WALK (Caminata). Pero podría ser otro tipo.
#   - outputgeom: especifíca que tipo de resultado se quiere. Por defecto es "SHED".
#   - otp_rest_url: define la url base para solicitar a la API.

Consulta_OTP <- function(coordinates, viaje.tiempo = 30, viaje.velocidad = 1.38,
                         mode = "WALK", outputgeom = "SHED", 
                         otp_rest_url='http://146.155.17.19:17050/otp/routers/chile/'){
  # Para no saturar el servidor, se realiza una consulta cada .33 segundos, o 3 consultas por segundo.
  Sys.sleep(.33)
  # Crear la URL para extraer el walkshed
  consulta.parametros = paste('isochroneOld?fromPlace=', coordinates, '&walkTime=', 
                              viaje.tiempo, '&walkSpeed=', viaje.velocidad,'&mode=', 
                              mode, '&toPlace=-33.5846161,-70.5410151&output=', outputgeom, sep = "") 
  consulta.url <- paste(otp_rest_url, consulta.parametros, sep  = "")
  # Extraer el walkshed, el cual viene en formato GeoJSON
  walkshed.texto <- getURL(consulta.url)
  # Contador que define el número de veces que se debe intentar una consulta cuando se encuentran
  # geometrías incorrectas.
  counter <- 0
  # Loop "while". Si el walkshed contiene las palabras "LineString" o "504 Gateway Time-out"
  # el request vuelve a ejecutarse hasta 5 veces, con 10 segundos entre cada interación del request.
  while((grepl("LineString", walkshed.texto) |
         grepl("504 Gateway Time-out", walkshed.texto)) &
        counter <= 5){
    print("Resultado inválido. Solicitando de nuevo.")
    Sys.sleep(10)
    walkshed.texto <- getURL(consulta.url)
    counter <- counter + 1
  }
  return(walkshed.texto)
}



##### Ejecución de la función. #####

# Definir las carpetas de trabajo: Dónde están los archivos de los centroides, y dónde se almacenarán
# Los archivos resultantes.
centroides.carpeta <- "Spatial_Data/ED_coords/"
walkshed.text.carpeta <- "Spatial_Data/Walksheds_GeoJSON/"
# Archivo .csv que contiene los centroides de las manzanas para todas las ciudades.
centroides <- read.csv(paste(centroides.carpeta, "Coords_Deporte.csv", sep = ""))
# Nombres de las ciudades
ciudades <- unique(centroides$Ciudad)
for (c in seq(ciudades)){
  # Primero se define la ciudad a trabajar
  ciudad <- ciudades[c]
  print(paste("Extrayendo datos de:", ciudad))
  # Se identifica si existen ciudades que ya se han calculado en la carpeta de los output. 
  # En caso de ser cierto,s e pasa a la siguiente iteración. 
  # Esto es para evitar calcular nuevamente una ciudad
  if (any(grepl(ciudad, list.files(walkshed.text.carpeta)))){
    print(paste(ciudad, "ya posee un archivo asociado a sus datos")); next()}
  # Definir los centroides de la ciudad
  centroides.ciudad <- centroides[centroides$Ciudad == ciudad,]
  # Linea para testear el código. añadir un # cuando se ejecute de forma total.
  #centroides.ciudad <- centroides.ciudad[sample(nrow(centroides.ciudad), 10), ]
  # Coordenadas de los centroides. Estas serán pasadas a la función Consulta_OTP
  centroides.coordenadas <- as.vector(paste(centroides.ciudad[,"lat"], centroides.ciudad[,"lon"], sep = ","))
  print("Adquiriendo geometrías")
  # Tabla donde se almacenarán los resultados de cada ciudad.
  walkshed.table <- data.frame()
  for (g in seq(centroides.coordenadas)){
    print(paste("Realizando consulta n:", g))
    # Se realiza una consulta a OTP. Si esta falla, se muestra un mensaje, se esperan 10 segundos,
    # Y se vuelve a intentar.
    # NOTA: Debe asegurarse de que todos los pasos anteriores a este se ejecuten de forma correcta.
    # Para ello, los archivos y carpetas deben ser nombrados con la convención especificada en el
    # Word adjunto.
    # Adicionalmente, si el área verde original es mayor a 20000 metros2, el
    # tiempo de caminata será de 10 min. De lo contrario, 5 min.
    walkshed.texto <- tryCatch(Consulta_OTP(centroides.coordenadas[g], viaje.tiempo = 10), 
                              error = function(e){ print("Error en la consulta. Re-intentando")
                                ; Sys.sleep(10) ;
                                Consulta_OTP(centroides.coordenadas[g], viaje.tiempo = 10)})
    # Incorporar el walkshed resultado en la tabla donde se almacenan los resultados.
    # Se incluye la ID del centroide, así como el polígono en geoJSON (texto).
    walkshed.table <- rbind(walkshed.table, data.frame("ID" = g, "walkshed.text" = walkshed.texto ))
  }
  # Exportar la tabla resultante en un csv.
  write.csv(walkshed.table, paste(walkshed.text.carpeta, ciudad, "_walksheds_text.csv", sep = ""), row.names = F)
}


library(geojsonio);library(rjson); library(raster); library(rgdal)

# Función que convierte de geojson texto, a objeto espacial

conversor_OTP <- function(walkshed.texto){
  
  # Algunos objetos vienen con una tercera coordenadas "0.0". Esta línea la elimina.
  walkshed.texto <- gsub(",0.0", "", walkshed.texto)
  
  # Transformar el objeto texto a json
  walkshed.geojson <- fromJSON(walkshed.texto)
  
  # Dado los problemas que genera esta coordenada Z, algunos polygonos quedan mal clasificados.
  # Por ello, implementé un método para abordar los dos tipos de clasificaciones que se producen de forma aleatoria
  
  # Tipo 1: Si el polígono tiene más de dos coordenadas, entonces se procede de forma normal.
  if (length(walkshed.geojson$coordinates[[1]]) > 2) {
    # Transforma el objeto a geojson, y luego a objeto SpatialPolygonsDataFrame
    walkshed.polygon <- geojson_list(walkshed.geojson$coordinates[[1]], geometry = "polygon") %>% geojson_sp
  } else {
    # Hay casos donde la API arroja un LineString (ie: el último vertice es distinto al primero)
    # Este bloque de "if/else" intenta corregirlo al copiar el primer punto y añadirlo después del último
    if (!(walkshed.geojson$coordinates[[1]][1] == walkshed.geojson$coordinates[[length(walkshed.geojson$coordinates)]][1] &
          walkshed.geojson$coordinates[[1]][2] == walkshed.geojson$coordinates[[length(walkshed.geojson$coordinates)]][2])){
      walkshed.geojson$coordinates[[length(walkshed.geojson$coordinates)+1]] <- walkshed.geojson$coordinates[[1]]
    }
    # Se imprime un mensaje señalando que se detecto un LinString, y luego se ejecuta
    # La conversion de GeoJSON a SpatialPolygonsDataframe
    print("LineString detected. Correcting")
    walkshed.polygon <- geojson_list(walkshed.geojson$coordinates, geometry = "polygon") %>% geojson_sp
  }
  return(walkshed.polygon)
}


##### Ejecución de la función. #####

# Definir las carpetas de trabajo: Dónde están los archivos de los centroides, los walksheds
# y dónde se almacenarán los archivos resultantes.
walkshed.text.carpeta <- "Spatial_Data/Walksheds_GeoJSON/"
centroides.carpeta <- "Spatial_Data/ED_coords/"
walkshed.shp.carpeta <- "Spatial_Data/Walksheds/UnDissolved"
# Archivo .csv que contiene los centroides de las manzanas para todas las ciudades.
centroides <- read.csv(paste(centroides.carpeta, "Coords_Deporte.csv", sep = ""))
# Nombres de las ciudades
ciudades <- unique(centroides$Ciudad)

# Loop para cada ciudad
for (c in seq(ciudades)){
  # Definir la ciudad
  ciudad <- ciudades[c]
  # Los centroides de esa ciudad, los cuales poseen el ID de cada manzana.
  centroides.ciudad <- centroides[centroides$Ciudad == ciudad,]
  # archivo csv que contiene los polígonos en formato GeoJson
  walkshed.text <- read.csv(paste(walkshed.text.carpeta, ciudad, "_walksheds_text.csv", sep = ""))
  # Una lista donde se almacenará cada polígono
  walkshed.shp <- list()
  # Loop para transformar a objetos espaciales.
  for (i in seq(nrow(walkshed.text))){
    print(paste("Polígono número:", i, "de", nrow(walkshed.text)))
    # Si el polígono no es válido (java.lang.NullPointerException null), se ignora. 
    # Estos son casos de manzanas cuyos centroides se encuentran alejados de cualquier red.
    if (grepl("java.lang.NullPointerException null", walkshed.text$walkshed.text[i])){print("Datos inválidos. Ignorando"); next()}
    # Se ejecuta la función conversor_OTP.
    walkshed.poligono <- conversor_OTP(walkshed.text$walkshed.text[i])
    # Si el polígono tiene un área 0 (en caso de ser línea, por ejemplo), se ignora.
    if (walkshed.poligono@polygons[[1]]@area == 0){ next()}
    # Se adjunta un identificador al polígono resultante, para homogolarlo con las manzanas.
    walkshed.poligono@data$ID <- centroides.ciudad$ID[i]
    # Se adjunta el nombre de la ciudad
    walkshed.poligono@data$properties <- ciudad
    # Se adjunta el polígono a la lista de polígonos que posteriormente será transformada en un
    # objeto único. Algo similar a un "join", o "merge"
    walkshed.shp <- append(walkshed.shp, walkshed.poligono)
  }
  # Consolidación de todos los polígonos como un solo objeto.
  walkshed.shp <- do.call(bind, walkshed.shp)
  print("Conversión exitosa. Exportando a shp.")
  # Exportación de dicho objeto en formato shp.
  writeOGR(obj = as(walkshed.shp, "SpatialPolygonsDataFrame" ), dsn = walkshed.shp.carpeta, 
           layer = paste(ciudad, "_Walksheds (UnDissolved)_wlk3", sep = ""),
           driver = "ESRI Shapefile",overwrite_layer=TRUE)
}

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
#     y/o análisis en Spatial_Data.
t1 <- Sys.time()

library(rgdal); library(rgeos); library(dplyr)

# Paso 1: identificar las ciudades a ser trabajadas:
codigos_ciudades <- read.csv("Otros/Ciudades_Cedeus.csv", sep=",")
# Vector que almacena los nombres de las ciudades
ciudades <- unique(codigos_ciudades$Ciudad)
# Carpetas donde están almacenados los shapefiles, así como el destino de los outputs.
# Modificar a necesidad de reproducibilidad.
poblacion_manzanas.carpeta <- "Spatial_Data/Poblacion_Manzanas_Ciudades"
walksheds.carpeta <- "Spatial_Data/Walksheds/UnDissolved"
accesibilidad.carpeta <- "Spatial_Data/Resultados"
tabla_accesibilidad.carpeta <- ""
# Tabla donde almacenar los resultados de accesibilidad
accesibilidad.tabla <- data.frame()
# Loop descrito en la introducción.
for (c in seq(ciudades)){
  ciudad <- ciudades[c]
  # Leer cada shapefie. Cada shapefile sigue un patrón para su nombre. En el caso de áreas verdes,
  # se debe modificar el nombre según el tipo de criterio que se utilizó para su análisis (Fuerte, medio, leve).
  # Adicionalmente, se asegura que todos los shp estén en la misma projección. En este caso, el shp de areas verdes
  # se usan como base.
  poblacion_manzanas.shp <- readOGR(poblacion_manzanas.carpeta, paste(ciudad, "Poblacion_Manzanas", sep = "_"), encoding = "UTF-8")
  walksheds.shp <- readOGR(walksheds.carpeta, paste(ciudad, "_Walksheds (UnDissolved)_wlk3", sep = ""), encoding = "UTF-8") %>%
    spTransform(proj4string(poblacion_manzanas.shp))
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
  mutate("Población.con.Acceso" = round(Población.con.Acceso/Población.total, 2)*100,
         "Población.sin.Acceso" = round(Población.sin.Acceso/Población.total, 2)*100,
         "Población.total" = 100)

# Exportar ambas tablas en la carpeta de indicadores

#write.csv(accesibilidad.tabla, paste(tabla_accesibilidad.carpeta,"Accesibilidad ", "(Población).csv", sep = ""))
write.csv(accesibilidad.tabla_porcentajes, paste(tabla_accesibilidad.carpeta,"Accesibilidad ", "(Porcentajes)_Publico.csv", sep = ""))




