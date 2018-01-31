# Indicador Consumo eléctirco.

setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/91._Consumo_Electrico/")
library(readxl); library(dplyr)

# Leer ciudades y población
city_codes <- read.csv("Otros/Ciudades_Cedeus.csv")
viviendas_2016 <- read.csv("Raw_Data/viviendas_precenso_2016.csv", sep = ";")
# Seleccionar la población de las comunas que se encuentren en las ciudades Cedeus
viviendas_2016 <- viviendas_2016[toupper(viviendas_2016$COMUNA) %in% toupper(city_codes$Comuna),]
viviendas_2016$Ciudad <- city_codes[match(toupper(viviendas_2016[["COMUNA"]]), toupper(city_codes[["Comuna"]]) ), "Ciudad"]
consumo <- read_excel(path = "Raw_Data/ConsumoElectricoporComuna_8714160129234979104.xlsx")
consumo <- consumo[tolower(consumo$Comuna) %in% tolower(city_codes$Comuna) & consumo$`Tipo de Cliente` == "Residencial",]
consumo$Comuna <- toupper(consumo$Comuna)
consumo <- merge(consumo, viviendas_2016, by.x = "Comuna", by.y = "COMUNA")

conusmo_hogar <- consumo %>%
  group_by(Ciudad) %>%
  summarise("kWh/hogar" = round(sum(`Energía (kWh)`)/sum(CANTIDAD.VIVIENDAS)/12,1))

output_folder <- "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/91._Consumo_Electrico"
write.csv(conusmo_hogar, paste(output_folder, "91._Consumo_Electrico.csv", sep = "/"), row.names = F)
