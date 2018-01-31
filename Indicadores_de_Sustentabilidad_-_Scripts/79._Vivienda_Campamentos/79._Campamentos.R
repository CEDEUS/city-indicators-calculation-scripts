# Script que calcula el número de familias en situación de campamentos.
# Fuente de datos: Techo.
# Paquetes a usar: dplyr.
library(dplyr)
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/79._Vivienda_Campamentos/")
# Leer archivos necesarios
ciudades <- read.csv("Otros/Ciudades_Cedeus.csv")
viviendas_2016 <- read.csv("Raw_Data/viviendas_precenso_2016.csv", sep = ";")
# Seleccionar la población de las comunas que se encuentren en las ciudades Cedeus
viviendas_2016 <- viviendas_2016[toupper(viviendas_2016$COMUNA) %in% toupper(ciudades$Comuna),]
viviendas_2016$Ciudad <- ciudades[match(toupper(viviendas_2016[["COMUNA"]]), toupper(ciudades[["Comuna"]]) ), "Ciudad"]

campamentos <- read.csv("Raw_Data/Base_Campamentos.csv", sep = ";")

# Subset para elegir las comunas que corresponden a las ciudades cedeus
# Así como para añadir a que ciudad pertenece cada una
campamentos.subset <- campamentos[campamentos$COMUNA %in% ciudades$Comuna,]
campamentos.subset$Ciudad <- ciudades[match(campamentos.subset[["COMUNA"]], ciudades[["Comuna"]] ), "Ciudad"]
campamentos.subset$Viviendas <- viviendas_2016[match(toupper(campamentos.subset[["COMUNA"]]), viviendas_2016[["COMUNA"]] ), "CANTIDAD.VIVIENDAS"]


# Agrupar por ciudad y contar el número de familias en campamentos en cada ciudad
campamentos.cedeus <- campamentos.subset %>%
  group_by(Ciudad) %>%
  summarize("Porcentaje_Familias" = round(sum(N..DE.FAMILIAS)/sum(Viviendas)*100,1))
# Escribir Resultados
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/79._Vivienda_Campamentos/")
write.csv(campamentos.cedeus, "79._Vivienda_Campamentos.csv", row.names = F)

