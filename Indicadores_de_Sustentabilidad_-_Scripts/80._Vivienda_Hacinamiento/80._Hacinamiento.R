# Script que calcula el hacinamiento.
# poverty line and total number of children. Studies cities are Coquimbo, Copiapó, Temuco,
# Santiago, Concepción and Valdivia.
# Data files consist of CASEN 2015, poverty line by 2015, city codes and estimated population by 2015.


library(data.table); library(dplyr); library(readxl); library(bit64)
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/80._Vivienda_Hacinamiento/")

# Data

city_codes <- as.data.frame(fread("Otros/Ciudades_Cedeus.csv", sep=";", header= T))

##### Datos CASEN 2015 ####

# Loading CASEN 2015, and poverty line per number of household inhabitants 2015
# Data of interest in CASEN 2015 is:
# Hacinamiento (756)
# Factor de expansión comunal (737)

casen2015.route <- "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/CASEN_2015/CASEN_2015.csv"

casen2015 <- as.data.frame(fread(casen2015.route, sep=",", header= T, select = c(7, 756, 737)))

# subset casen2015 by city 
casen_sub <- casen2015[tolower(casen2015$comuna) %in% tolower(city_codes$Comuna),]
# match city names with comunas
casen_sub$Ciudad <- city_codes[match(tolower(casen_sub[["comuna"]]), tolower(city_codes[["Comuna"]]) ), "Ciudad"]
casen_sub$expc[is.na(casen_sub$expc)] <- 1


hacinamiento <- casen_sub %>%
  group_by(Ciudad) %>%
  summarize("Porcentaje de Hogares Hacinados" = round(sum(expc[hacinamiento != "sin hacinamiento (2,49 y menos)"])/
              sum(expc)*100,2))

# Exporting the final result

setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/80._Vivienda_Hacinamiento/")
write.csv(hacinamiento, "80._Vivienda_-_Hacinamiento.csv", row.names = F)

