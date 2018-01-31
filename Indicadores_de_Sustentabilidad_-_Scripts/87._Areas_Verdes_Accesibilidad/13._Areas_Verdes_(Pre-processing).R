## Script para preparar datos para análisis de áreas verdes
# El script se compone de varias secciones:
# Primero, se filtran las áreas verdes de interés. Por ancho y área.
# Luego, se extraen los límites urbanos de las ciudades de interés (según precenso 2011) 
# y se hace un clip a las áreas verdes (ya filtradas) según los límites urbanos 
# de las áreas de interés.

# cargar librerías necesarias para este script.
library(data.table); library(dplyr); library(rgdal)

library(sp);library(rgeos); library(alphahull)   


# Una función para calcular el Minimum Bounding Rectangle, obtenida de:
# http://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points

MBR <- function(points) {
  # Analyze the convex hull edges                       
  a <- ashape(points, alpha=1000)                 # One way to get a convex hull...
  e <- a$edges[, 5:6] - a$edges[, 3:4]            # Edge directions
  norms <- apply(e, 1, function(x) sqrt(x %*% x)) # Edge lengths
  v <- diag(1/norms) %*% e                        # Unit edge directions
  w <- cbind(-v[,2], v[,1])                       # Normal directions to the edges
  
  # Find the MBR
  vertices <- (points) [a$alpha.extremes, 1:2]    # Convex hull vertices
  minmax <- function(x) c(min(x), max(x))         # Computes min and max
  x <- apply(vertices %*% t(v), 2, minmax)        # Extremes along edges
  y <- apply(vertices %*% t(w), 2, minmax)        # Extremes normal to edges
  areas <- (y[1,]-y[2,])*(x[1,]-x[2,])            # Areas
  k <- which.min(areas)                           # Index of the best edge (smallest area)
  
  # Form a rectangle from the extremes of the best edge
  cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% rbind(v[k,], w[k,])
}

# Función que analiza un polígono, y determina si este posee un área mayor a 5000 metros cuadrados,
# y si tiene un ancho mayor a 10 metros.

WorthyPolygonClassifier <- function(poligono, area_minima, ancho_minimo){
  # Area del polígono. Esta debe estar en un CRS (Cartographic Reference System) cuya unidad de medida sea en metros.
  # En caso de no ser así, definirlo en los shapefiles mediante proj4string, o en un SIG de preferencia.
  area_poligono <- poligono@Polygons[[1]]@area
  # si el área es menor a "area_minima", arrojar "FALSO" (No cumple con los requisitos mínimos)
  if (area_poligono < area_minima){ return(FALSE)}
  # Coordenadas del polígono
  coords_poligono <- unique(poligono@Polygons[[1]]@coords)
  
  # Minimum Bounding Rectangle. Ya que la función MBR utiliza la función ashape, la cual tiene
  # como requisito que no pueden haber puntos consecutivos y colineales, se randomiza el orden
  # de los puntos. Esto no altera la geometría del polígono. En caso de shapes producidos por
  # geoprocesos como "Union" o "intersect", será necesario limpiar la geometría de estos antes
  # de procesarlos con este script, puesto que la probabilidad de generar problemas por puntos
  # colineales es muy alta.
  
  # Los triangulos y trapecios arrojan errores con el minimum Bounding rectangle
  # Por ello, si el número de puntos es igual o menor a 4, añadir un punto "dummy" que consiste
  # En el promedio de los puntos X e Y.
  
  if (nrow(coords_poligono) <= 4){
    coords_poligono <- rbind(coords_poligono, c(mean(coords_poligono[,1]), mean(coords_poligono[,2])))
  }
  # Cálculo del Minimum Bounding Rectangle. En caso de que falle, se intenta de nuevo
  
  mbr <- tryCatch(MBR(coords_poligono[sample(nrow(coords_poligono)),]), 
                  error = function(e){ print("Error. Puntos colineales en polígono, reordenando")
                    ; MBR(coordenadas[sample(nrow(coords_poligono)),])})
  # Distancias entre puntos del MBR
  distancias <- unique(spDistsN1(mbr, mbr[2,]))
  # Ancho del MBR = distancia más pequeña mayor a 0
  ancho <- (min(distancias[distancias > 0]))
  # Largo del MBR Dado por la mediana (ancho es el mínimo, y la diagonal es el máximo)
  largo <- median(distancias[distancias > 0])
  # Area Hipotética Mínima: La superficie mínima que debería tener el polígono del área verde
  # si tuviese un ancho de 10 metros. De esta forma, se filtran aquellas áreas verdes que tengan
  # un ancho inferior a este.
  AHM <- largo*10
  # Si el ancho es mayor a 10, y el area mayor al ATM, retorna TRUE
  ifelse(( (ancho > 10) & (area_poligono > AHM)), return(TRUE), return(FALSE))
}

# Función que "clippea" las áreas verdes al límite urbano

Filtro_LimiteUrbano <- function(Ciudad, carpeta_aavv, carpeta_limurb){
 
  # Extrae el nombre del shp de limite urbano en la carpeta desginada, para luego leerlo
  lista_limurb <- gsub(".dbf", "", list.files(carpeta_limurb, ".dbf"))
  nombre_limurbshp <- lista_limurb[grep(Ciudad, lista_limurb)]
  limiteurbano <- readOGR(carpeta_limurb, nombre_limurbshp, encoding = "UTF-8")
  
  # Carpeta donde están los shp originales
  carpeta_aavvrawdata <- paste(carpeta_aavv, "/Raw data", sep = "")
  # Extrae el nombre del shp de áreas verdes en la carpeta desginada, para luego leerlo
  lista_aavv <- gsub(".dbf", "", list.files(carpeta_aavvrawdata, ".dbf"))
  nombre_aavv <- lista_aavv[grep(Ciudad, lista_aavv)]
  aavv <- readOGR(carpeta_aavvrawdata, nombre_aavv, encoding = "UTF-8")
  
  # Reprojectar el shp de limite urbano usando el CRS de áreas verdes
  limiteurbano <- spTransform(limiteurbano, proj4string(aavv))
  
  # El Clip de las áreas verdes según límite urbano consiste en un "intersect".
  # Ahora, hay casos en los que arroja un error, el cual se soluciona usando un buffer dummy de 0 metros.
  # Por qué ? No lo sé :/
  aavv_dummy <- gBuffer(aavv, byid=TRUE, width=0)
  limiteurbano_dummy <- gBuffer(limiteurbano, byid=TRUE, width=0)
  aavv_subset <- aavv[gIntersection(aavv_dummy, limiteurbano_dummy, byid = TRUE, drop_lower_td = TRUE),] #clip polygon 2 with polygon 1
  
  return(aavv_subset)
}

# Función para seleccionar las áreas verdes a analizar según tipo de area verde.
# El filtro varía de Fuerte, Medio, Leve, Los cuales están detallados 
# en "Inclusion_AAVV_Indicador.csv

Reglas_aavv <- function(Ciudad, reglafile, reglatipo, aavv){
  if (!(Ciudad %in% unique(reglafile$Ciudad))){ return(aavv)}
  # Primero, se hace un subset de la ciudad
  reglas_ciudad <- reglafile[reglafile$Ciudad == Ciudad,]
  # Identificar el campo que contiene los tipos de areas verdes (Hasta ahora, Tipo vs SUBTIPO)
  # Identificar los atributos del .csv que contienen las clasificaciones
  campos <- reglas_ciudad[grep("Campo_",colnames(reglas_ciudad))]
  # Registrar el nombre del campo que contiene registros válidos (no NA)
  reglanombre_old <- colnames(campos[colSums(!is.na(campos)) > 0])
  # Registrar la ID del campo.
  reglanombre_old_id <- which( colnames(reglas_ciudad)==reglanombre_old )
  # Registrar un nuevo nombre para el campo, eliminando el sufijo "Campo_". 
  reglanombre <- gsub("Campo_", "", reglanombre_old)
  # Usando la id del campo con registros válidos, cambiamos el nombre por uno sin el sufijo.
  # Esto es para posterior uso en el subset del shapefile de areas verdes.
  colnames(reglas_ciudad)[reglanombre_old_id] <- c(reglanombre)
  # Luego, un subset del campo con los tipos de areas verdes, y el tipo de reglas a usar
  reglas_ciudad <- reglas_ciudad[,c(reglanombre, reglatipo)]
  # Finalmente, un subset de aquellas que sí estén incluidas.
  reglas_ciudad <- reglas_ciudad[which(reglas_ciudad[,2] == "Si"),]
  # Se hace un subset del shape de aavv según las areas verdes definidas en reglas_ciudad.
  aavv_filtrada <- aavv[aavv[[reglanombre]] %in% reglas_ciudad[[reglanombre]],]
  return(aavv_filtrada)
}

#### Almacenar la carpeta donde están los shapes de areas verdes y límite urbano

carpeta_aavv <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes"
carpeta_limurb <- "E:/Cedeus Sustainability Indicators/GIS/Limite Urbano/Ciudades Cedeus"

# archivo .csv que contiene datos sobre Nombres de ciudades, sus comunas y sus códigos.
city_codes <- as.data.frame(fread("E:/Cedeus Sustainability Indicators/Datos/Study_codes.csv", sep=",", header= T))
# Vector que almacena los nombres de las ciudades
ciudades <- unique(city_codes$Ciudad)
# archivo .csv que contiene los criterios para definir qué áreas verdes es analizarán. 
reglafile <- as.data.frame(fread("E:/Cedeus Sustainability Indicators/Datos/Inclusion_AAVV_Indicador.csv", sep= ";", header= T))
# Tipo de clasificación a utilizar
reglatipo <- "Inclusion(Fuerte)"

# Loop que por cada ciudad:
# I) Selecciona sólo las que están dentro del límite urbano definido en el Precenso 2011
# II) Selecciona las áreas verdes cuyo tipo/subtipo calce con los criterios de selección establecidos
#     en "reglafile"
# III) Crea una lista para ejecutar un loop en el cual 
#      Por cada polígono en el shape ejecuta la función "WorthyPolygonClassifier", la cual
#      selecciona en función de área y ancho
# IV) Se crea un nuevo shape en base a un subset del shape original con los polígonos que cumplen las condiciones.

for (o in 1:length(ciudades)){
  # Identificar la Ciudad a trabajar
  Ciudad <- ciudades[o]
  # Leer el shape
  areas_verdes_limurb <- Filtro_LimiteUrbano(Ciudad, carpeta_aavv, carpeta_limurb)
  # Filtrar según criterio. Si la ciudad no está incluida en los criterios de clasificación,
  # Retorna el mismo área verde
  areas_verdes_reglas <- Reglas_aavv(Ciudad, reglafile, reglatipo, areas_verdes_limurb)
  # Crear un vector donde se almacenan los ids de los polígonos que cumplan los requisitos.
  lista <- vector()
  # Por cada polígono se ejecuta la función WorthyPolygonClassifier, que determina si cada polígono
  # Individual cumple con los requisitos especificados de área mínima y ancho del polígono.
  # Cada polígono es asignado un valor TRUE o FALSE, según si cumple o no los requisitos.
  # Luego se seleccionan sólo los polígonos que cumplan los requisitos, y se almacenan en un
  # nuevo objeto llamado new_shape.
  for (i in 1:nrow(areas_verdes_reglas)){
    poligono <- areas_verdes_reglas@polygons[[i]]
    IsItWorthy <- WorthyPolygonClassifier(poligono, area_minima = 5000, ancho_minimo = 10)
    lista <- append(lista, IsItWorthy)
  }
  new_shape <- areas_verdes_reglas[lista,]
  # Almacenar el tipo de criterio, para añadirlo como sufijo al shape final.
  reglatipo_sufijo <- gsub("Inclusion", "",reglatipo)
  
  # Se exporta el nuevo shapefile con el sufijo "AAVV" y el tipo de criterio usado (Fuerte, Medio, Leve), 
  # en la carpeta "Procesado"
  
  writeOGR(as(new_shape, "SpatialPolygonsDataFrame" ), 
           paste(carpeta_aavv, "/Procesado", sep = ""), 
           paste(Ciudad, "_AAVV_", reglatipo_sufijo,sep= ""),
           driver = "ESRI Shapefile",overwrite_layer=TRUE)
}


