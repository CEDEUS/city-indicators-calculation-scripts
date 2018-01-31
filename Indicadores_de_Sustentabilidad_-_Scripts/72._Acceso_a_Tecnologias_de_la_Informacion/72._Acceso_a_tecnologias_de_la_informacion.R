# Script para calcular diferentes cositas del Censo 2012

# Librerías a usar. Instálalas escribiendo install.packages("nombrelibreria")
# Es importante popner el nombre de la librería en "".
library(data.table); library(dplyr); library(rgdal)

library(sp);library(dplyr);library(rgeos)

setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/72._Acceso_a_tecnologias_de_la_informacion/")

# Calcular porcentaje de niños por manzana

# Leer los archivos: población, idgeo y el archivo con datos comunales/paises
censofolder <- "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/Censo_2012/"
población <- as.data.frame(fread(paste(censofolder, "table_hogar.csv", sep = ""), sep= ";", header= T, select = c("FOLIO", "NVIV", "NHOG", "H13D")))
idgeo <- as.data.frame(fread(paste(censofolder, "table_idgeo.csv", sep = ""), sep= ";", header= T))

población$REGION <- idgeo[match(población[["FOLIO"]], idgeo[["FOLIO"]] ) , "REGION"]
población$COM <- idgeo[match(población[["FOLIO"]], idgeo[["FOLIO"]] ) , "COM"]
población$DTO <- idgeo[match(población[["FOLIO"]], idgeo[["FOLIO"]] ) , "DTO"]
población$ZONA <- idgeo[match(población[["FOLIO"]], idgeo[["FOLIO"]] ) , "ZONA"]
población$MZ <- idgeo[match(población[["FOLIO"]], idgeo[["FOLIO"]] ) , "MZ"]

pob_Summary <- población %>%
  group_by(COM, DTO, ZONA, MZ) %>%
  summarize("Con Internet" = round(sum(H13D == 1, na.rm = T),2),
            "Total Hogares" = n(), 
            Porcentaje = round(sum(H13D == 1, na.rm = T)/n()*100,2))
#### GIS processing ####

# Key para combinar posteriormente

pob_Summary$Key <- paste(pob_Summary$COM, pob_Summary$DTO, 
                         pob_Summary$ZONA, pob_Summary$MZ)

# Borrar casas que no tengan manzana o distrito identificable

pob_Summary <- pob_Summary[!(pob_Summary$MZ == 99999 |
                               pob_Summary$DTO == 99999 |
                               pob_Summary$MZ == 0),]


# Un loop para cargar, combinar y exportar el shapefile con datos por manzana
shape_merger <- function(shape_path, shapelist, variable, escala, tabla){
  for (o in 1:length(shapelist)){
    # Leer shapefile o
    shape <- readOGR(shape_path, shapelist[o])
    # Definir key para el shape
    shape@data$Key <- paste(shape$CUT, shape$DISTRITO, shape$ZONA, shape$MANZANA)
    # Combinar por Key
    shape@data <- data.frame(shape@data, 
                             pob_Summary[match(shape@data[,"Key"], 
                                               pob_Summary[["Key"]]),])
    # MANZENT estorba, definirlo como nulo.
    shape$CO_DI_ZO_M <- NULL
    ciudad <- gsub("_Manzanas", "", shapelist[o])
    tabla.tmp <- data.frame(ciudad, "Con Internet" = round(sum(shape@data$Con.Internet, na.rm = T)/sum(shape@data$Total.Hogares, na.rm = T)*100,2))
    tabla <- rbind(tabla, tabla.tmp)
    # Escribir shapefile en una nueva carpeta llamada "NiÃ±os", dentro de la carpeta de los shapes.
    writeOGR(as(shape, "SpatialPolygonsDataFrame" ), shape_path.output, 
             paste("Censo_", variable, "_", shapelist[o], sep = ""),
             driver = "ESRI Shapefile",overwrite_layer=TRUE)
    
  }
  return(tabla)
}


# carpeta donde estÃ¡n los shapefiles

shape_path.input <- "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/72._Acceso_a_tecnologias_de_la_informacion/Raw_Data"
shape_path.output <- "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/72._Acceso_a_tecnologias_de_la_informacion/Output"
# lista con los shapes

shapelist <- gsub(".dbf", "", list.files(shape_path.input, ".dbf"))
# Crear Output
tabla <- data.frame()
output <- shape_merger(shapelist = shapelist, shape_path = shape_path.input, variable = "Internet", escala = "Manzana", tabla = tabla)
output.folder <- "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/72._Acceso_a_Tecnologias_de_la_Informacion/"
write.csv(output, "72._Acceso_a_Tecnologias_de_la_Informacion.csv", row.names = F)
