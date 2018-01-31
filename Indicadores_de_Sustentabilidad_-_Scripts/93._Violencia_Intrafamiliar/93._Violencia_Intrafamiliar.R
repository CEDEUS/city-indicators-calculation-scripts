library(dplyr)

setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/93._Violencia_Intrafamiliar/")

violencia <- read.csv("Raw_Data/Violencia.csv", sep = ";", encoding = "UTF-8")
ciudades <- read.csv("Otros/Study_codes.csv")

violencia <- violencia[violencia$X.U.FEFF.REGIÓN..PROVINCIA.Y.COMUNA %in% ciudades$Comuna,]

violencia$Ciudad <- ciudades[match(violencia[["X.U.FEFF.REGIÓN..PROVINCIA.Y.COMUNA"]], ciudades[["Comuna"]] ), "Ciudad"]

violencia.summary <- violencia %>%
  group_by(Ciudad) %>%
  summarize("Casos de violencia Intrafamiliar" = sum(Total))

output.folder <- "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/93._Violencia_Intrafamiliar/"
write.csv(violencia.summary, paste0(output.folder, "93._Violencia_Intrafamiliar.csv"), row.names = F)
