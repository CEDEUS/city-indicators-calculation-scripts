library(rvest)
library(stringr)


setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/92._Accesibilidad_a_Ferias/")
setwd("Raw_Data")
region <- 8
html <- read_html(paste0("http://www.asof.cl/apps/mapaferias/regiones/",region,".php"))

urls <- html %>% html_nodes("option") %>% html_attr("value") %>% na.omit()


maps <- sapply(urls, function(x) {
  read_html(paste0("http://www.asof.cl/apps/mapaferias/regiones/",x)) %>%
    html_nodes("iframe") %>%
    html_attr("src")
}
)


ids <- sapply(maps, function(x) {
  str_extract(read_html(x) %>% html_nodes(xpath = '//meta[@itemprop="url"]') %>% html_attr('content'), "[-_0-9A-Za-z]+$")
}
)

for (mid in ids) {
  namess <- gsub("/", "_", names(ids[ids == mid]))
  namess <- gsub(".php", "", namess)
  download.file(paste0("https://www.google.com/maps/d/u/0/kml?mid=",mid,"&forcekml=1"),paste0("kml/",region,"/",namess, "_", mid,".kmz"))
}


# extract long-lat coordinates from a bunch of kmz files
# first unzips the KMZ-s then reads coordinates from each KML with getKMLcoordinates {maptools}
# (worth checking this as well: https://gist.github.com/holstius/6631918 )

library(maptools)
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/92._Accesibilidad_a_Ferias/Raw_Data")
# list the kmz files in a given folder path
KMZs <- list.files(path="kml/rms/", pattern="*.kmz", full.names=FALSE)

# unzip each KMZ file and read coordinates with getKMLcoordinates()
# select only the matrix element of the list returned by getKMLcoordinates(), 
# therefore mention the index [[1]]

From_Kmz_to_Shp_Points <- function(KMZs, folder, dest, name_output){
  LonLat <- data.frame()
  for (i in seq(KMZs)){
    tryCatch(tmp <- getKMLcoordinates(paste0("kml/",folder,"/", KMZs[i]), ignoreAltitude = T), error = function(x){print(KMZs[i]); tmp <- NA})
    if (class(tmp) == "matrix" | length(tmp) == 1) {print(KMZs[i]);next()}
    tmp <- do.call(rbind, tmp)
    colnames(tmp) <- c("lon", "lat")
    LonLat <- rbind(LonLat, tmp)
  }
  LonLat <- LonLat[!is.na(LonLat$lon),]
  sp <- SpatialPointsDataFrame(LonLat, LonLat)
  writeOGR(sp, dest, name_output, "ESRI Shapefile", overwrite_layer = T)
  return(data.frame(LonLat, Ciudad = folder))
}

setwd("Raw_Data/")
tabla <- data.frame()
for (r in seq(list.files("kml"))){
  folder <- list.files("kml")[r]
  KMZs <- list.files(path= paste0("kml/",folder), pattern="*.kmz", full.names=FALSE)
  tablita <-  From_Kmz_to_Shp_Points(KMZs, folder,
                         dest = "../Shapefiles",
                         paste0("Ferias_", folder))
  tabla <- rbind(tabla, tablita)
}

Consulta_OTP <- function(coordinates, viaje.tiempo = 10, viaje.velocidad = 1.38,
                         mode = "WALK", outputgeom = "SHED",
                         otp_rest_url='http://146.155.17.19:17050/otp/routers/chile/'){
  # Para no saturar el servidor, se realiza una consulta cada .33 segundos, o 3 consultas por segundo.
  # Sys.sleep(.1)
  # Crear la URL para extraer el walkshed
  consulta.parametros = paste('isochroneOld?fromPlace=', coordinates, '&walkTime=', 
                              viaje.tiempo, '&walkSpeed=', viaje.velocidad,'&mode=', 
                              mode, '&toPlace=-33.5846161,-70.5410151&output=', 
                              outputgeom, sep = "") 
  
  consulta.url <- paste(otp_rest_url, consulta.parametros, sep  = "")
  # Extraer el walkshed, el cual viene en formato GeoJSON
  print(consulta.url)
  walkshed.texto <- getURL(consulta.url)
  # Contador que define el número de veces que se debe intentar una consulta cuando se encuentran
  # geometrías incorrectas.
  counter <- 0
  # Loop "while". Si el walkshed contiene las palabras "LineString" o "504 Gateway Time-out"
  # el request vuelve a ejecutarse hasta 5 veces, con 10 segundos entre cada interación del request.
  while((grepl("LineString", walkshed.texto) |
         grepl("504 Gateway Time-out", walkshed.texto)) &
        counter <= 20){
    print("Resultado inválido. Solicitando de nuevo.")
    Sys.sleep(.5)
    walkshed.texto <- getURL(consulta.url)
    counter <- counter + 1
  }
  walkshed.polygon <- readOGR(walkshed.texto, "OGRGeoJSON", verbose = F, p4s = "+proj=longlat +datum=WGS84")
  return(walkshed.polygon)
}


Consultor_otp <- function(data, viaje.tiempo = 10, viaje.velocidad = 1.38, mode = "WALK"){
  lista.tmp <- list()
  for (i in seq(data)){
    print(paste("Request number:", i))
    tmp <- Consulta_OTP(data[i])
    if (class(tmp) == "SpatialLinesDataFrame") { print("linestring"); next() }
    lista.tmp <- append(lista.tmp, tmp)
  }
  shp.output <- do.call(bind,lista.tmp)
  return(shp.output)
}
accesibilidad_tabla <- data.frame()
for (c in unique(tabla$Ciudad)){
  tabla.ciudad <- tabla[tabla$Ciudad == c,]
  shp.name <- paste0(c, "_Walksheds")
  if (!file.exists(paste("../Output/Walksheds/", shp.name, ".shp",sep = ""))){
    coords <- paste(tabla.ciudad[,2], tabla.ciudad[,1], sep = ",")
    walkshed.shp <- Consultor_otp(coords)
    print("writing shp")
    writeOGR(walkshed.shp, "../Output/Walksheds", shp.name, driver = "ESRI Shapefile", overwrite_layer = T)
    
  } else {
    walkshed.shp <- readOGR("../Output/Walksheds", shp.name)
  }

  poblacion_manzanas.shp <- readOGR("../Raw_Data/Poblacion_Manzanas_Ciudades", paste0(c, "_Poblacion_Manzanas"), encoding = "UTF-8")
  walkshed.shp <- spTransform(walkshed.shp, proj4string(poblacion_manzanas.shp))
  # Los walksheds que tienen contacto con una manzana
  manzanas.subset <- poblacion_manzanas.shp[walkshed.shp,]
  # Identificar las manzanas con acceso según ID, las que no tienen acceso, y exportar a shp
  accesibilidad.shp <- poblacion_manzanas.shp[,c("ID_W", "Pob")]
  accesibilidad.shp@data$Acceso <- "Dummy"
  accesibilidad.shp@data[accesibilidad.shp@data$ID_W %in% 
                           manzanas.subset@data$ID,]$Acceso <- "Sí"
  accesibilidad.shp@data[!(accesibilidad.shp@data$ID_W %in% 
                             manzanas.subset@data$ID),]$Acceso <- "No"
  accesibilidad.carpeta <- paste("../Output/Accesibilidad", sep = "")
  writeOGR(as(accesibilidad.shp, "SpatialPolygonsDataFrame" ), 
           accesibilidad.carpeta, 
           paste(c, "Acceso_Ferias", sep = "_"),
           driver = "ESRI Shapefile",overwrite_layer=TRUE)
  # Calcular la población con acceso y sin acceso
  pob.acceso <- accesibilidad.shp@data$Pob[accesibilidad.shp@data$Acceso == "Sí"]
  pob.total <- accesibilidad.shp@data$Pob
  accesibilidad <- data.frame("Ciudad" = c, "Porcentaje Acceso" = round(sum(pob.acceso)/sum(pob.total)*100, 2))
  accesibilidad_tabla <- rbind(accesibilidad_tabla, accesibilidad)
}

write.csv(accesibilidad_tabla, "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/92._Accesibilidad_a_Ferias/92._Accesibilidad_a_Ferias.csv", row.names = F)
