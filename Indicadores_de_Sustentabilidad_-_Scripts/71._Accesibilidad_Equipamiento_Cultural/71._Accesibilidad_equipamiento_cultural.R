# Accesibilidad a equipamiento cultural
library(rgdal); library(maptools); library(RCurl)
library(geojsonio);library(rjson); library(raster); library(rgdal)

setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/71._Acceso_a_equipamiento_cultural/")

##### Parte 1: Preparar Data #####

# Función que tiene por objetivo combinar los datos de cultura entregados por
# el catastro, y hacer un subset en función del limite urbano.
# Toma el nombre de la carpeta con los puntos, y el nombre de la ciudad.
# Retorna las coords de todos los puntos, junto a su tipo

Eq_Cultura_Ciudad <- function(carpeta, ciudad){
  shape.list <- gsub(".shp", "", list.files(carpeta, ".shp"))
  limite_urbano <- limite_urbano <- readOGR("Otros/Limite_Urbano", 
                                            paste("LimiteUrbano_", ciudad, sep = ""))
  for (l in seq(shape.list)){
    if (l == 1){
      eq_cultura <- readOGR(carpeta, shape.list[l], encoding = "UTF-8")
    } else {
      shp.tmp <- readOGR(carpeta, shape.list[l], encoding = "UTF-8")
      eq_cultura <- spRbind(eq_cultura, shp.tmp) 
    }
  }
  eq_cultura <- spTransform(eq_cultura, proj4string(limite_urbano))
  eq_cultura <- eq_cultura[limite_urbano,]
  nombre.ciudad <- paste(gsub("LimiteUrbano_", "", ciudad), sep = "")
  centroides.ciudad <- data.frame(Ciudad = nombre.ciudad, 
                                  ID = 1:nrow(eq_cultura@data),
                                  X = eq_cultura@coords[,1],
                                  Y = eq_cultura@coords[,2],
                                  Tipo = eq_cultura@data$X3__Tipo_de)
  return(centroides.ciudad)
}

# Funcion que toma la lista con los eq culturales y su tipologia, y crea un
# csv para cada tipo, para cada ciudad.
# Input: .csv con coordenadas y tipo. Table con qué significa cada tipología,
# y lista con las ciudades.
# Output: multiples csv, cada uno con las coords para cada tipo de equipamiento
# seleccionado.

tipologia.subset <- function(eq_cultural, tipologia, Ciudad) {
  for (t in 1:nrow(tipologia)){
    tmp <- eq_cultural[eq_cultural$Tipo == tipologia.analisis[t],]
    nombre.csv <- paste("Output/Eq_Cultural_(puntos)", "/", 
                        Ciudad, "/", Ciudad, "_", tipologia[t,2],
                        ".csv", sep = "")
    write.csv(tmp, nombre.csv, row.names = F)
  }
}

# Obteniendo nombres de ciudades

ciudades <- gsub(" ", "_", unique(read.csv("Otros/Study_codes.csv")$Ciudad))

# Tipologias a analizar.

tipologia.analisis <- c(2, 4, 6, 8, 9, 12)

# Leer csv con tipologias y filtrar en funcion de ellas.

tipologia <- read.csv("Otros/Codificacion_Tipo.csv", sep = ",", encoding = "UTF-8") 
tipologia <- tipologia[tipologia$Codigo %in% tipologia.analisis,]

# Por cada ciudad:
#   une los shps de cultura, y crea un csv para cada tipologia para cada ciudad.

for (i in seq(ciudades)) {
  carpetas <- grep(ciudades[i], list.dirs("Raw_Data/"),  value = T)  
  eq_cultural <- Eq_Cultura_Ciudad(carpetas, ciudades[i])
  nombre.csv <- paste("Output/Eq_Cultural_(puntos)/Eq_Cultural_", 
                      gsub("LimiteUrbano_", "", ciudades[i]), ".csv", sep = "")
  tipologia.subset(eq_cultural, tipologia, ciudades[i])
}

##### Parte 2: Consulta OTP #####

##### Funciones: OTP #####

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
                         mode = "WALK,TRANSIT", outputgeom = "SHED", time = "2016-11-11T08:00:00",
                         otp_rest_url='http://146.155.17.19:17050/otp/routers/chile/'){
  # Para no saturar el servidor, se realiza una consulta cada .33 segundos, o 3 consultas por segundo.
  Sys.sleep(.1)
  # Crear la URL para extraer el walkshed
  consulta.parametros = paste('isochroneOld?fromPlace=', coordinates, '&walkTime=', 
                              viaje.tiempo, '&walkSpeed=', viaje.velocidad,'&mode=', 
                              mode, '&toPlace=-33.5846161,-70.5410151&output=', 
                              outputgeom, "&time=", time, sep = "") 
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

# Función que transforma de GeoJSON a shp
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


# Funcion que ejecuta Consulta OTP en un archivo dado.

Consultor_OTP <- function(datos, ciudad, tipo, 
                          poblacion_manzanas.shp, modo = "WALK,TRANSIT", 
                          viaje.tiempo = 30){
  # Coordenadas de los datos. Estas serán pasadas a la función Consulta_OTP
  datos.coordenadas <- as.vector(paste(datos[,"Y"], datos[,"X"], sep = ","))
  print("Adquiriendo geometrías")
  # Tabla donde se almacenarán los resultados de cada ciudad.
  walkshed.table <- data.frame()
  walkshed.shp <- list()
  for (g in seq(datos.coordenadas)){
    print(paste("Realizando consulta n:", g))
    # Se realiza una consulta a OTP. Si esta falla, se muestra un mensaje, se esperan 10 segundos,
    # Y se vuelve a intentar.
    # NOTA: Debe asegurarse de que todos los pasos anteriores a este se ejecuten de forma correcta.
    # Para ello, los archivos y carpetas deben ser nombrados con la convención especificada en el
    # Word adjunto.
    walkshed.texto <- tryCatch(Consulta_OTP(datos.coordenadas[g], viaje.tiempo = viaje.tiempo, mode = modo), 
                               error = function(e){ print("Error en la consulta. Re-intentando")
                                 ; Sys.sleep(5) ;
                                 Consulta_OTP(datos.coordenadas[g], viaje.tiempo = viaje.tiempo, mode = modo)})
    # Incorporar el walkshed resultado en la tabla donde se almacenan los resultados.
    # Se incluye la ID del centroide, así como el polígono en geoJSON (texto).
    walkshed.table <- rbind(walkshed.table, data.frame("ID" = g, "walkshed.texto" = walkshed.texto ))
    
    # TEST: Convertir a shp on the fly
    if (grepl("java.lang.NullPointerException null", walkshed.texto)){
      print("Datos inválidos. Ignorando"); next()}
    # Se ejecuta la función conversor_OTP.
    walkshed.poligono <- conversor_OTP(walkshed.texto)
    # Si el polígono tiene un área 0 (en caso de ser línea, por ejemplo), se ignora.
    if (walkshed.poligono@polygons[[1]]@area == 0){ next()}
    # Se adjunta un identificador al polígono resultante, para homogolarlo con las manzanas.
    walkshed.poligono@data$ID <- datos$ID[i]
    # Se adjunta el nombre de la ciudad
    walkshed.poligono@data$properties <- ciudad
    # Se adjunta el polígono a la lista de polígonos que posteriormente será transformada en un
    # objeto único. Algo similar a un "join", o "merge"
    walkshed.shp <- append(walkshed.shp, walkshed.poligono)
  }
  
  csv.name <- paste(ciudad, tipo, "walksheds_text.csv", sep = "_")
  # Exportar la tabla resultante en un csv.
  walkshed.text.carpeta <- paste("Output/Walksheds_GeoJSON/", ciudad, "/", sep = "")
  write.csv(walkshed.table, paste(walkshed.text.carpeta, csv.name, sep = ""), row.names = F)
  # Consolidación de todos los polígonos como un solo objeto.
  if (length(walkshed.shp) > 1){
    walkshed.shp <- do.call(bind, walkshed.shp) %>% 
      spTransform(proj4string(poblacion_manzanas.shp))
  } else {
    walkshed.shp <- walkshed.shp[[1]] %>%
      spTransform(proj4string(poblacion_manzanas.shp))
  }
    
  print("Conversión exitosa. Exportando a shp.")
  
  walkshed.shp.carpeta <- paste("Output/Walksheds/", ciudad, sep = "")
  # Exportación de dicho objeto en formato shp.
  writeOGR(obj = as(walkshed.shp, "SpatialPolygonsDataFrame" ), dsn = walkshed.shp.carpeta, 
           layer = paste(ciudad, tipo,"Walksheds", sep = "_"),
           driver = "ESRI Shapefile",overwrite_layer=TRUE)
  
  # Los walksheds que tienen contacto con una manzana
  manzanas.subset <- poblacion_manzanas.shp[walkshed.shp,]
  # Identificar las manzanas con acceso según ID, las que no tienen acceso, y exportar a shp
  accesibilidad.shp <- poblacion_manzanas.shp[,c("ID_W", "Pob")]
  accesibilidad.shp@data$Acceso <- "Dummy"
  accesibilidad.shp@data[accesibilidad.shp@data$ID_W %in% 
                           manzanas.subset@data$ID,]$Acceso <- "Sí"
  accesibilidad.shp@data[!(accesibilidad.shp@data$ID_W %in% 
                             manzanas.subset@data$ID),]$Acceso <- "No"
  accesibilidad.carpeta <- paste("Output/Accesibilidad/", ciudad, sep = "")
  writeOGR(as(accesibilidad.shp, "SpatialPolygonsDataFrame" ), 
           paste(accesibilidad.carpeta, sep = ""), 
           paste(ciudad, tipo, "Accesibilidad", sep = "_"),
           driver = "ESRI Shapefile",overwrite_layer=TRUE)
  # Calcular la población con acceso y sin acceso
  pob.acceso <- accesibilidad.shp@data$Pob[accesibilidad.shp@data$Acceso == "Sí"]
  pob.total <- accesibilidad.shp@data$Pob
  accesibilidad <- data.frame(round(sum(pob.acceso)/sum(pob.total)*100, 2))
  colnames(accesibilidad) <- tipo
  return(accesibilidad)
}

# Funcion que retorna qué porcentaje de la población tiene accesibilidad para
# un determinado número de equipamientos.

accesibilidad.ciudad <- function(carpeta, viaje.tiempo = 30, modo = "WALK,TRANSIT"){
  archivos <- list.files(carpeta)
  acceso.df <- data.frame(Ciudad = ciudad)
  for (f in seq(archivos)){
    datos.eq <- read.csv(paste(carpeta, archivos[f], sep = ""))
    tipo <- gsub(paste(ciudad, "_", sep = ""), "", archivos[f])
    tipo <- gsub(".csv", "", tipo)
    carpeta.text <- paste("Output/Walksheds_GeoJSON/", ciudad, sep = "")
    if (nrow(datos.eq) == 0) { 
      acceso.tmp <- data.frame(tipo = 0)
      colnames(acceso.tmp) <- tipo
      acceso.df <- cbind(acceso.df, acceso.tmp)
      next() 
    }
    if (tipo == "Biblioteca"){
      acceso.tmp <- Consultor_OTP(datos.eq, ciudad, tipo, 
                                  poblacion_manzanas.shp, viaje.tiempo = 15,
                                  modo = "WALK")
    } else {
      acceso.tmp <- Consultor_OTP(datos.eq, ciudad, tipo, 
                                  poblacion_manzanas.shp, viaje.tiempo = viaje.tiempo,
                                  modo = modo)
    }

    colnames(acceso.tmp) <- tipo
    acceso.df <- cbind(acceso.df, acceso.tmp)
  }
  acceso.df$Poblacion_Total <- sum(poblacion_manzanas.shp@data$Pob)
  return(acceso.df)
}

##### Test Zone #####
poblacion_manzanas.carpeta <- "Raw_Data/Poblacion_Manzanas"
accesibilidad.carpeta <- "Output/Accesibilidad"
access <- data.frame()
carpeta.puntos <- "Output/Eq_Cultural_(puntos)/"
ciudades <- list.files(carpeta.puntos)
for (i in seq(list.files(carpeta.puntos))){
  ciudad <- ciudades[i]
  carpeta <- paste("Output/Eq_Cultural_(puntos)/", ciudad, "/", sep = "")
  shp.name <- paste(ciudad, "Poblacion_Manzanas", sep = "_")
  output.name <- paste(ciudad, "_Accesibilidad.csv", sep = "")
  if (file.exists(output.name)){
    print(paste("Accesibilidad para", ciudad, "Ya ha sido calcualda"))
    accesibilidad.tmp <- read.csv(output.name)
    colnames(accesibilidad.tmp) <- colnames(access)
    access <- rbind(access, accesibilidad.tmp)
    next()
  }
  
  poblacion_manzanas.shp <- readOGR(poblacion_manzanas.carpeta, shp.name, encoding = "UTF-8")
  if (ciudad == "Copiapo"){
    accesibilidad.tmp <- accesibilidad.ciudad(carpeta, viaje.tiempo = 15, modo = "CAR")
  }
   else {
    accesibilidad.tmp <- accesibilidad.ciudad(carpeta)
   }
  print(paste("Exportando datos de accesibilidad para:", ciudad))
  write.csv(accesibilidad.tmp, output.name, row.names = F)
  access <- rbind(access, accesibilidad.tmp)
}
carpeta.resultados <- "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/71._Acceso_a_equipamiento_cultural/"
write.csv(access, paste(carpeta.resultados, "71._Acceso_a_equipamiento_cultural.csv", sep = ""))

