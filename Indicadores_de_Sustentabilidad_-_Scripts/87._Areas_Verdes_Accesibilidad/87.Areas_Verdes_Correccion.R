setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/87._Areas_Verdes_Accesibilidad/Areas_Verdes_(Areas_Verdes-Based_Approach)/Resultados_(Areas_Verdes)")

library(rgdal)

files <- list.files(pattern = "Accesibilidad.shp")
files <- gsub(".shp", "", files)
tabla.final <- data.frame()
for (i in seq(files)){
  area_verde <- readOGR(dsn = getwd(),layer = files[i], encoding = "UTF-8")
  area_verde@data$Area <- sapply(slot(area_verde, "polygons"), slot, "area")/10000
  area_verde@data$Density <- area_verde@data$Pob/area_verde@data$Area
  area_verde@data$Acceso <- as.character(area_verde@data$Acceso)
  area_verde@data$Acceso[area_verde@data$Density < 16] <- "No"
  
  writeOGR(area_verde, "Resultados_Corregidos", files[i], driver = "ESRI Shapefile", overwrite_layer = T)
  
  ciudad <- gsub("_Accesibilidad", "", files[i])
  porcentaje <- round(sum(area_verde@data$Pob[area_verde@data$Acceso == "Sí"])/sum(area_verde@data$Pob)*100,1)
  tabla.final <- rbind(tabla.final, data.frame("Ciudad" = ciudad, "Población con Acceso" = porcentaje))
}

write.csv(tabla.final, "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/87._Areas_Verdes_Accesibilidad/87._Areas_Verdes_Accesibilidad.csv", row.names = F)
