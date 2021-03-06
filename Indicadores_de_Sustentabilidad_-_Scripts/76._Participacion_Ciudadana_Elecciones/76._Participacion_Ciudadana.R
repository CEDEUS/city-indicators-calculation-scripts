library(data.table); library(dplyr); library(readxl)
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/76._Participacion_Ciudadana_Elecciones/")

# Almacenar los nombres de los archivos con los n�meros de participaci�n en una lista
archivos_votantes <- grep("Votantes", list.files("Clean_Data"),value = T)

# Leer los archivos y almacenarlos en un dataframe

participacion <- data.frame()

for (i in 1:length(archivos_votantes)){
  tmp <- as.data.frame(read_excel(path = paste("Clean_Data/", archivos_votantes[i], sep = "")))
  participacion <- rbind(participacion, tmp)
}

# Leer archivo con comunas de inter�s

city_codes <- as.data.frame(fread("Otros/Study_codes.csv", sep=",", header= T))

# Remover acentos, y pasar todas las letras a capitales, para calzar con los datos de participaci�n

city_codes$Comuna <- chartr('�����','AEIOU', toupper(city_codes$Comuna))

# Substraer comunas de inter�s

participacion <- participacion[participacion$Comuna %in% city_codes$Comuna,]

# A�adir nombre de ciudades

participacion$Ciudad <- city_codes[match(participacion[["Comuna"]], city_codes[["Comuna"]] ), "Ciudad"]




# Resumir seg�n ciudad

participacion_ciudad <- participacion %>%
  group_by(Ciudad) %>%
  summarize("Total Votaci�n" = sum(`Total Votaci�n`),
            "Total Electores" = sum(`Total Electores`),
            "Participaci�n" = round(sum(`Total Votaci�n`)/sum(`Total Electores`),4)*100)

# Exportar a csv, tanto por ciudad
participacion_ciudad <- participacion_ciudad[,c(1,4)]
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/76._Participacion_Ciudadana_Participacion_Electoral/")
write.csv(participacion_ciudad, "76._Participacion_Ciudadana_Participacion_Electoral.csv", row.names = F)
