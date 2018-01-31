## Script that calculates number of waste produced per capita per day and year
# Study cities are Coquimbo, Copiapó, Temuco, Santiago, Concepción and Valdivia.
# Data files consist of "Primer reporte de residuos solidos en chile",
# population estimations for 2015 and city codes and labels for study cities.


library(data.table); library(dplyr); library(readxl)
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/78._Residuos/")

#### Calculating population number in each city ####

# Loading population proyections to 2015 data and city codes. 

pob_2015 <- read_excel(path = "Otros/poblacion_2015.xlsx" )

city_codes <- as.data.frame(fread("Otros/Ciudades_Cedeus.csv", sep=",", header= T))

# Subsetting population of each comuna of interest

pob_2015 <- pob_2015[pob_2015$`Nombre comuna` %in% city_codes$Comuna & pob_2015$Label == "Total",]
pob_2015$Ciudad <- city_codes[match(pob_2015[["Nombre comuna"]], city_codes[["Comuna"]] ), "Ciudad"]

# Quilicura no entregó registros actualizados a 2015. Por lo que se eliminar de la tabla de población.
# En caso de que los provea, eliminar esta línea.

pob_2015 <- pob_2015[!(pob_2015$`Nombre comuna` == "Quilicura"),]

# Summarizing the number of population in each city.

pob_2015 <- pob_2015 %>%
  group_by(Ciudad) %>%
  summarise("Población total (2015)" = sum(Poblacion, na.rm = T))

#### Calculating number of waste production in each city ####

# Importing waste production for each comuna of interest

waste_2015 <- as.data.frame(fread("Residuos.csv", sep=";", header= T)) %>%
  group_by(Ciudad) %>%
  summarise("Residuos producidos (ton/año)" = sum(`Residuos 2015`, na.rm = T))

# merging dataframes.

residuos <- merge(waste_2015, pob_2015, by="Ciudad") 

# Summarising waste production by kilos per capita by day (total waste*1000/total population/365) and
# waste production by kilos per capita by year (total waste*1000/total population)
# Results were rounded by two

residuos <- residuos %>%
  mutate("Residuos per cápita por día (kg)" = round(((`Residuos producidos (ton/año)`*1000)/`Población total (2015)`)/365,2),
         "Residuos per cápita por año (kg)" = round(((`Residuos producidos (ton/año)`*1000)/`Población total (2015)`),2))

# Exporting the final result
residuos <- residuos[,c(1,5)]
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/78._Residuos/")
write.csv(residuos, "78._Residuos.csv", row.names = F)

