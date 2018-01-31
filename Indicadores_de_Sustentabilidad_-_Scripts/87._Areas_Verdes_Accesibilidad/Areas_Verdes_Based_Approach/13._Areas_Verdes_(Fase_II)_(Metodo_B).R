# Fase II
# Esta fase consisete en extraer los walksheds a partir de los centroides de las manzanas.
# El script se compone de un loop que:
# Por cada ciudad:
#  - Se ejecuta una solicitud a OTP para obtener el polígono del walkshed de cada manzana de la ciudad.
#    Si los datos obtenidos son de tipo LineString, o arrojan error, se esperan 10 segundos,
#    y se intenta de nuevo.
#  - Se almacenan los datos en un .csv que contiene un identificador para su manipulación geoespacial.

# Librerias a usar:

library (RCurl)

# Función que realiza la consulta a OTP. Utiliza 6 variables:
#   - coordenadas: las coordenadas del punto de origen del walkshed
#   - viaje.tiempo: tiempo que dura el viaje en minutos. Para este indicador, 10 minutos.
#   - viaje.velocidad: velocidad a la que se realiza el viaje. Para caminata se consideran 
#     1.38 m/s, o 5 km/h aprox.
#   Los siguientes parámetros son propios de la API de Open Trip Planner.
#   - modo: por defecto será WALK (Caminata). Pero podría ser otro tipo.
#   - outputgeom: especifíca que tipo de resultado se quiere. Por defecto es "SHED".
#   - otp_rest_url: define la url base para solicitar a la API.

Consulta_OTP <- function(coordinates, viaje.tiempo = 10, viaje.velocidad = 1.38,
                         mode = "WALK", outputgeom = "SHED", 
                         otp_rest_url='http://146.155.17.18:23080/otp/routers/chile/'){
  # Para no saturar el servidor, se realiza una consulta cada .33 segundos, o 3 consultas por segundo.
  Sys.sleep(.33)
  # Crear la URL para extraer el walkshed
  consulta.parametros = paste('isochroneOld?fromPlace=', coordinates, '&walkTime=', 
                        viaje.tiempo, '&walkSpeed=', viaje.velocidad,'&mode=', 
                        mode, '&toPlace=-33.5846161,-70.5410151&output=', outputgeom, sep = "") 
  consulta.url <- paste(otp_rest_url, consulta.parametros, sep  = "")
  # Extraer el walkshed, el cual viene en formato GeoJSON
  walkshed.text <- getURL(consulta.url)
  # Contador que define el número de veces que se debe intentar una consulta cuando se encuentran
  # geometrías incorrectas.
  counter <- 0
  # Loop "while". Si el walkshed contiene las palabras "LineString" o "504 Gateway Time-out"
  # el request vuelve a ejecutarse hasta 5 veces, con 10 segundos entre cada interación del request.
  while((grepl("LineString", walkshed.text) |
         grepl("504 Gateway Time-out", walkshed.text)) &
        counter <= 5){
    print("Resultado inválido. Solicitando de nuevo.")
    Sys.sleep(10)
    walkshed.text <- getURL(consulta.url)
    counter <- counter + 1
  }
  return(walkshed.text)
}

##### Ejecución de la función. #####

# Definir las carpetas de trabajo: Dónde están los archivos de los centroides, y dónde se almacenarán
# Los archivos resultantes.
centroides.carpeta <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes/Centroides_Areas_Verdes/"
walkshed.text.carpeta <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes/Walksheds GeoJSON_B/"
# Archivo .csv que contiene los centroides de las manzanas para todas las ciudades.
centroides <- read.csv(paste(centroides.carpeta, "Centroides_AreasVerdes_Ciudades.csv", sep = ""))
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
  centroides.coordenadas <- as.vector(paste(centroides.ciudad[,"y"], centroides.ciudad[,"x"], sep = ","))
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
    if (centroides.ciudad$Area.Original[g] >= 20000){ 
      print("Area mayor a 2ha. viaje.tiempo = 10 min")
      walkshed.text <- tryCatch(Consulta_OTP(centroides.coordenadas[g], viaje.tiempo = 10), 
                                error = function(e){ print("Error en la consulta. Re-intentando")
                                  ; Sys.sleep(10) ;
                                  Consulta_OTP(centroides.coordenadas[g], viaje.tiempo = 10)})
    } else {
      print("Area menor a 2ha. viaje.tiempo = 5 min")
      walkshed.text <- tryCatch(Consulta_OTP(centroides.coordenadas[g], viaje.tiempo = 5), 
                                error = function(e){ print("Error en la consulta. Re-intentando")
                                  ; Sys.sleep(10) ;
                                  Consulta_OTP(centroides.coordenadas[g], viaje.tiempo = 5)})
      }

    # Incorporar el walkshed resultado en la tabla donde se almacenan los resultados.
    # Se incluye la ID del centroide, así como el polígono en geoJSON (texto).
    walkshed.table <- rbind(walkshed.table, data.frame("ID" = g, "walkshed.text" = walkshed.text ))
  }
  # Exportar la tabla resultante en un csv.
  write.csv(walkshed.table, paste(walkshed.text.carpeta, ciudad, "_walksheds_text.csv", sep = ""), row.names = F)
}

rm(list=ls())
