# Script para calcular la accesibilidad a la educaci?n en funci?n de la
# calidad de los colegios, la cual estar? determinada por la evaluaci?n
# SNED de los colegios, la cual considera, entre otros: Inclusi?n, puntaje SIMCE
# Calidad laboral, calidad docente, etc.
# El Script se compone de dos partes importantes.
# La primera parte constituye el trabajo con las bases de datos obtenidas desde
# el centro de estudios del mineduc, donde se utilizaron las bases de datos
# Directorio Oficial Establecimientos Educacionales (2015)
# Evaluaci?n SNED (Periodo 2014 - 2015).
# Se utiliz? el DOEE de 2015 dado que la evaluaci?n SNED corresponde al periodo 2014-2015

# Leer las librer?as necesarias
library(rgdal); library(readxl); library(dplyr); library(RCurl); library(raster)

# Directorio donde se encuentra la carpeta con los archivos asociados al indicador.
# Cambiar seg?n el usuario.
#setwd("E:/Cedeus Sustainability Indicators/Datos/19. Acceso y Calidad a Educaci?n/")
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/81._Acceso_y_Calidad_a_Educacion/")

# Csv con los c?digos de las comunas y las ciudades a las que estas pertenecen.
ciudades <- read.csv2("Otros/Ciudades_Cedeus.csv")

# Excel con los datos sobre calidad de educaci?n (Indicador INDICER), sleccionar
# S?lo las columnas con las ID de los Colegios (RBD y DV_RBD), el nombre (NOMB_RBD), 
# el c?digo de la comuna (COD_COM_RBD), y el puntaje INDICER (INDICER).
# No se utiliz? la variable "RURAL_RBD", la cual contiene la identificaci?n sobre
# si el colegio es rural o urbano, puesto que para la accesibilidad s?lo se utilizan
# manzanas en la zona urbana, y existen casos de colegios con clasificaci?n confusa
calidad <- read_excel("Raw Data/Datos categoría de desempeño 2016.xlsx")

# A?adir nombre de ciudades en funci?n del c?digo de cada comuna
calidad$Ciudad <- ciudades[match(calidad[["nom_com_rbd"]], toupper(ciudades[["Comuna"]] )), "Ciudad"]
# Seleccionar las ciudades, agrupar los datos en funci?n de ?stas,
# Y seleccionar s?lo el top 35%.
calidad <- calidad[(!is.na(calidad$Ciudad)),] %>%
  group_by(Ciudad) %>%
  subset(categoria_des2016_d == "Alto")

# Leer el Directorio Oficial de Establecimientos Educacionales para el a?o 2015
# Y seleccionar las columnas relevantes para el an?lisis
edu <- read.csv("Raw Data/20160923_Directorio_Oficial_EE_2016_20160430_PUBL.csv", sep = ";", quote="") %>%
  subset(select = c("RBD", "DGV_RBD", "NOM_RBD","COD_COM_RBD","DIR_RBD", 
                    "LATITUD", "LONGITUD","PAGO_MENSUAL", rep(paste("ENS_0", 1:9, sep = "")), "ESTADO_ESTAB"))
  
# seleccionar los colegios dentro de las ciudades cedeus
edu <- edu[edu$COD_COM_RBD %in% ciudades$Codigo, ]
edu$Ciudad <- ciudades[match(edu[["COD_COM_RBD"]], ciudades[["Codigo"]] ), "Ciudad"]


# Seleccionar los colegios que est?n dentro de Calidad

edu.calidad <- edu[edu$RBD %in% calidad$rbd | edu$NOM_RBD %in% calidad$nom_rbd,] %>%
  filter(ESTADO_ESTAB == 1, PAGO_MENSUAL %in% c("GRATUITO", "$1.000 A $10.000"))

# Las columnas de longitud y latitud tienen comas en vez de puntos.
# Cambiar a puntos.

edu.calidad$LONGITUD <- as.character(edu.calidad[,"LONGITUD"])
edu.calidad$LONGITUD <- as.numeric(gsub(",", ".", edu.calidad$LONGITUD))
edu.calidad$LATITUD <- as.character(edu.calidad[,"LATITUD"])
edu.calidad$LATITUD <- as.numeric(gsub(",", ".", edu.calidad$LATITUD))

# Filtrar los colegios por tipo. Se usan 4 tipos: Jardin, Basica, Media y Diferencial
# Solo se consideran colegios que atiendan a ni?os. No adultos.

codificacion_tipo <- read.csv("Otros/Codificacion_Edu.csv")

tipos <- unique(codificacion_tipo$Tipo)
ciudades <- unique(ciudades$Ciudad)
for (c in seq(ciudades)){
  for (i in seq(length(tipos))){
    edu.calidad$Ciudad <- gsub(" ", "_", edu.calidad$Ciudad)
    tipo.codes <- codificacion_tipo$Codigo[codificacion_tipo$Tipo == tipos[i]]
    edu.tipo <- edu.calidad[(edu.calidad$ENS_01 %in% tipo.codes | 
                            edu.calidad$ENS_02 %in% tipo.codes | 
                            edu.calidad$ENS_03 %in% tipo.codes | 
                            edu.calidad$ENS_04 %in% tipo.codes |
                            edu.calidad$ENS_05 %in% tipo.codes |
                            edu.calidad$ENS_06 %in% tipo.codes | 
                            edu.calidad$ENS_07 %in% tipo.codes | 
                            edu.calidad$ENS_08 %in% tipo.codes | 
                            edu.calidad$ENS_09 %in% tipo.codes),]
    edu.tipo <- cbind(ID = 1:nrow(edu.tipo), edu.tipo)


    # Nombre del archivo a crear
    ciudad <- gsub(" ", "_", ciudades[c])
    output.name <- paste(ciudad, "_", tipos[i], ".csv", sep = "")
    output.name <- gsub(" ", "_", output.name)
    # Exportar los resultados a .csv
    edu.tmp <- edu.tipo[edu.tipo$Ciudad == ciudad,]
    
    if (nrow(edu.tmp) == 0) {next()}
    write.csv(edu.tmp, paste("Output/Coords_Ciudad", ciudad, output.name, sep = "/"), row.names = F)  
    }
}


Consulta_OTP <- function(coordinates, viaje.tiempo = 10, viaje.velocidad = 1.38,
                         mode = "WALK", outputgeom = "SHED",
                         otp_rest_url='http://146.155.17.19:17050/otp/routers/chile/'){
  # Para no saturar el servidor, se realiza una consulta cada .33 segundos, o 3 consultas por segundo.
  Sys.sleep(.1)
  # Crear la URL para extraer el walkshed
  consulta.parametros = paste('isochroneOld?fromPlace=', coordinates, '&walkTime=', 
                              viaje.tiempo, '&walkSpeed=', viaje.velocidad,'&mode=', 
                              mode, '&toPlace=-33.5846161,-70.5410151&output=', 
                              outputgeom, sep = "") 
  consulta.url <- paste(otp_rest_url, consulta.parametros, sep  = "")
  # Extraer el walkshed, el cual viene en formato GeoJSON
  print(consulta.url)
  walkshed.texto <- getURL(consulta.url)
  # Contador que define el n?mero de veces que se debe intentar una consulta cuando se encuentran
  # geometr?as incorrectas.
  counter <- 0
  # Loop "while". Si el walkshed contiene las palabras "LineString" o "504 Gateway Time-out"
  # el request vuelve a ejecutarse hasta 5 veces, con 10 segundos entre cada interaci?n del request.
  while((grepl("LineString", walkshed.texto) |
         grepl("504 Gateway Time-out", walkshed.texto)) &
        counter <= 5){
    print("Resultado inv?lido. Solicitando de nuevo.")
    Sys.sleep(10)
    walkshed.texto <- getURL(consulta.url)
    counter <- counter + 1
  }
  return(walkshed.texto)
}

# Funci?n que transforma de GeoJSON a shp
conversor_OTP <- function(walkshed.texto){
  
  # Algunos objetos vienen con una tercera coordenadas "0.0". Esta l?nea la elimina.
  walkshed.texto <- gsub(",0.0", "", walkshed.texto)
  
  walkshed.polygon <- readOGR(walkshed.texto, "OGRGeoJSON", verbose = F, p4s = "+proj=longlat +datum=WGS84")
  return(walkshed.polygon)
}

# Funcion que ejecuta Consulta OTP en un archivo dado.

Consultor_OTP <- function(datos, ciudad, tipo, 
                          poblacion_manzanas.shp, modo = "WALK", 
                          viaje.tiempo = 10){
  # Coordenadas de los datos. Estas ser?n pasadas a la funci?n Consulta_OTP
  datos.coordenadas <- as.vector(paste(datos[,"LATITUD"], datos[,"LONGITUD"], sep = ","))
  print("Adquiriendo geometr?as")
  # Tabla donde se almacenar?n los resultados de cada ciudad.
  walkshed.table <- data.frame()
  walkshed.shp <- list()
  for (g in seq(datos.coordenadas)){
    print(paste("Realizando consulta n:", g))
    # Se realiza una consulta a OTP. Si esta falla, se muestra un mensaje, se esperan 10 segundos,
    # Y se vuelve a intentar.
    # NOTA: Debe asegurarse de que todos los pasos anteriores a este se ejecuten de forma correcta.
    # Para ello, los archivos y carpetas deben ser nombrados con la convenci?n especificada en el
    # Word adjunto.
    walkshed.texto <- tryCatch(Consulta_OTP(datos.coordenadas[g], viaje.tiempo = viaje.tiempo, mode = modo), 
                               error = function(e){ print("Error en la consulta. Re-intentando")
                                 ; Sys.sleep(5) ;
                                 Consulta_OTP(datos.coordenadas[g], viaje.tiempo = viaje.tiempo, mode = modo)})
    # Incorporar el walkshed resultado en la tabla donde se almacenan los resultados.
    # Se incluye la ID del centroide, as? como el pol?gono en geoJSON (texto).
    walkshed.table <- rbind(walkshed.table, data.frame("ID" = g, "walkshed.texto" = walkshed.texto ))
    
    # TEST: Convertir a shp on the fly
    if (grepl("java.lang.NullPointerException null", walkshed.texto)){
      print("Datos inv?lidos. Ignorando"); next()}
    # Se ejecuta la funci?n conversor_OTP.
    walkshed.poligono <- conversor_OTP(walkshed.texto)
    # Si el pol?gono tiene un ?rea 0 (en caso de ser l?nea, por ejemplo), se ignora.
    if (walkshed.poligono@polygons[[1]]@area == 0){ next()}
    # Se adjunta un identificador al pol?gono resultante, para homogolarlo con las manzanas.
    walkshed.poligono@data$ID <- datos$ID[i]
    # Se adjunta el nombre de la ciudad
    walkshed.poligono@data$properties <- ciudad
    # Se adjunta el pol?gono a la lista de pol?gonos que posteriormente ser? transformada en un
    # objeto ?nico. Algo similar a un "join", o "merge"
    walkshed.shp <- append(walkshed.shp, walkshed.poligono)
  }
  
  csv.name <- paste(ciudad, tipo, "walksheds_text.csv", sep = "_")
  # Exportar la tabla resultante en un csv.
  walkshed.text.carpeta <- paste("Output/Walksheds_GeoJSON/", ciudad, "/", sep = "")
  write.csv(walkshed.table, paste(walkshed.text.carpeta, csv.name, sep = ""), row.names = F)
  # Consolidaci?n de todos los pol?gonos como un solo objeto.
  if (length(walkshed.shp) > 1){
    walkshed.shp <- do.call(bind, walkshed.shp) %>% 
      spTransform(proj4string(poblacion_manzanas.shp))
  } else {
    walkshed.shp <- walkshed.shp[[1]] %>%
      spTransform(proj4string(poblacion_manzanas.shp))
  }
  
  print("Conversi?n exitosa. Exportando a shp.")
  
  walkshed.shp.carpeta <- paste("Output/Walksheds/", ciudad, sep = "")
  # Exportaci?n de dicho objeto en formato shp.
  writeOGR(obj = as(walkshed.shp, "SpatialPolygonsDataFrame" ), dsn = walkshed.shp.carpeta, 
           layer = paste(ciudad, tipo,"Walksheds", sep = "_"),
           driver = "ESRI Shapefile",overwrite_layer=TRUE)
  
  # Los walksheds que tienen contacto con una manzana
  manzanas.subset <- poblacion_manzanas.shp[walkshed.shp,]
  # Identificar las manzanas con acceso seg?n ID, las que no tienen acceso, y exportar a shp
  accesibilidad.shp <- poblacion_manzanas.shp[,c("ID_W", "Pob")]
  accesibilidad.shp@data$Acceso <- "Dummy"
  accesibilidad.shp@data[accesibilidad.shp@data$ID_W %in% 
                           manzanas.subset@data$ID,]$Acceso <- "S?"
  accesibilidad.shp@data[!(accesibilidad.shp@data$ID_W %in% 
                             manzanas.subset@data$ID),]$Acceso <- "No"
  accesibilidad.carpeta <- paste("Output/Accesibilidad/", ciudad, sep = "")
  writeOGR(as(accesibilidad.shp, "SpatialPolygonsDataFrame" ), 
           accesibilidad.carpeta, 
           paste(ciudad, tipo, "Accesibilidad", sep = "_"),
           driver = "ESRI Shapefile",overwrite_layer=TRUE)
  # Calcular la poblaci?n con acceso y sin acceso
  pob.acceso <- accesibilidad.shp@data$Pob[accesibilidad.shp@data$Acceso == "S?"]
  pob.total <- accesibilidad.shp@data$Pob
  accesibilidad <- data.frame(round(sum(pob.acceso)/sum(pob.total)*100, 2))
  colnames(accesibilidad) <- tipo
  return(accesibilidad)
}


# Funcion que retorna qu? porcentaje de la poblaci?n tiene accesibilidad para
# un determinado n?mero de equipamientos.

accesibilidad.ciudad <- function(carpeta, viaje.tiempo = 10, modo = "WALK"){
  archivos <- list.files(carpeta)
  acceso.df <- data.frame(Ciudad = ciudad)
  for (f in seq(archivos)){
    if (!grepl("Basica", archivos[f])) { next()}
    datos.edu <- read.csv(paste(carpeta, archivos[f], sep = ""))
    tipo <- gsub(paste(ciudad, "_", sep = ""), "", archivos[f])
    tipo <- gsub(".csv", "", tipo)
    carpeta.text <- paste("Output/Walksheds_GeoJSON/", ciudad, sep = "")
    if (nrow(datos.edu) == 0) { 
      acceso.tmp <- data.frame(tipo = 0)
      colnames(acceso.tmp) <- tipo
      acceso.df <- cbind(acceso.df, acceso.tmp)
      next() 
    }
    acceso.tmp <- Consultor_OTP(datos.edu, ciudad, tipo, 
                                  poblacion_manzanas.shp, viaje.tiempo = 10,
                                  modo = "WALK")
    colnames(acceso.tmp) <- tipo
    acceso.df <- cbind(acceso.df, acceso.tmp)
  }
  acceso.df$Poblacion_Total <- sum(poblacion_manzanas.shp@data$Pob)
  return(acceso.df)
}

##### Test Zone #####
poblacion_manzanas.carpeta <- "Raw Data/Poblacion_Manzanas"
accesibilidad.carpeta <- "Output/Accesibilidad"
access <- data.frame()
carpeta.puntos <- "Output/Coords_Ciudad/"
ciudades <- list.files(carpeta.puntos)
for (i in seq(list.files(carpeta.puntos))){
  ciudad <- ciudades[i]
  carpeta <- paste("Output/Coords_Ciudad/", ciudad, "/", sep = "")
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
  accesibilidad.tmp <- accesibilidad.ciudad(carpeta)
  print(paste("Exportando datos de accesibilidad para:", ciudad))
  write.csv(accesibilidad.tmp, paste("Output/Accesibilidad_Tablas/", 
                                     output.name, sep = ""), row.names = F)
  access <- rbind(access, accesibilidad.tmp)
}
carpeta.resultados <- "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/81._Acceso_y_Calidad_de_Educacion/"
write.csv(access, paste(carpeta.resultados, "81._Acceso_y_Calidad_de_Educacion.csv", sep = ""))


