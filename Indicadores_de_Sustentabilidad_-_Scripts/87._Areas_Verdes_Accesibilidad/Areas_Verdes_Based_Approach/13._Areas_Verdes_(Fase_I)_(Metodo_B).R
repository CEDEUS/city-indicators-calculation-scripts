library(raster); library(rgdal); library(rgeos)

areas_verdes.carpeta <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes/Areas Verdes Procesadas"
ciudad <- "Gran Santiago"
areas.verdes.shp <- readOGR(areas_verdes.carpeta, paste(ciudad, "_AAVV_(Fuerte)", sep = ""), encoding = "UTF-8")
areas.verdes.shp$AREA <- sapply(slot(areas.verdes.shp, 'polygons'), function(i) slot(i, 'area')) 
areas.verdes.shp <- spTransform(areas.verdes.shp, CRS("+init=epsg:4326"))
plot(areas.verdes.shp)
#areas.verdes.big <- areas.verdes.shp[areas.verdes.shp$AREA > 20000,]
#plot(areas.verdes.big)



PolySlicer <- function(poligono, cell.size){
  bounding.box <- bbox(poligono)
  cell.offset <- bounding.box[, 1] + (cell.size / 2)  # cell offset
  cells.direction <- ceiling(diff(t(bounding.box)) / cell.size)  # number of cells per direction
  grid.topology <- GridTopology(cellcentre.offset = cell.offset, 
                                cellsize = cell.size, cells.dim = cells.direction)
  grid.spatial <- SpatialGrid(grid.topology, proj4string = proj4string(poligono))
  grid.pixels <- as(grid.spatial, "SpatialPixels")
  grid.polygon <- as(grid.pixels, "SpatialPolygons")
  poligono <- gBuffer(poligono, byid = TRUE, width=0)
  poligon.splitted <- as(gIntersection(grid.polygon, poligono, byid = T), "SpatialPolygonsDataFrame")
  poligon.splitted@data <- data.frame("Area Original" = rep(poligono@data$AREA, length(poligon.splitted)))
  return(poligon.splitted)
}

# archivo .csv que contiene datos sobre Nombres de ciudades, sus comunas y sus códigos.
city_codes <- read.csv("E:/Cedeus Sustainability Indicators/Datos/Study_codes.csv", 
                       sep=",")
# Vector que almacena los nombres de las ciudades

ciudades <- unique(city_codes$Ciudad)
centroides <- data.frame()
for (c in seq(ciudades)){
  ciudad <- ciudades[c]
  areas.verdes.shp <- readOGR(areas_verdes.carpeta, paste(ciudad, "_AAVV_(Fuerte)", sep = ""), encoding = "UTF-8")
  areas.verdes.shp$AREA <- sapply(slot(areas.verdes.shp, 'polygons'), function(i) slot(i, 'area')) 
  slice <- list()
  for (p in seq(areas.verdes.shp)){
    poly.tmp <- areas.verdes.shp[p, ]
    slice.tmp <- PolySlicer(poly.tmp, c(100, 100))
    slice.tmp@data$Area.division <- sapply(slot(slice.tmp, 'polygons'), function(i) slot(i, 'area')) 
    slice <- append(slice, slice.tmp)
  }
  
  sliced <- do.call(raster::bind, slice)
  sliced.centroides <- gCentroid(sliced, byid = T)
  sliced.centroides <- spTransform(sliced.centroides, CRS("+init=epsg:4326"))
  centroides.tmp <- data.frame("ID" = seq(sliced.centroides), "Ciudad" = ciudad,
                               sliced.centroides@coords, sliced@data)
  centroides <- rbind(centroides, centroides.tmp)
}




centroides.carpeta <- "E:/Cedeus Sustainability Indicators/GIS/Areas Verdes/Centroides_Areas_Verdes/"


write.csv(centroides, paste(centroides.carpeta, "Centroides_AreasVerdes_Ciudades.csv", sep = ""))
