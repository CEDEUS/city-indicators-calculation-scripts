

library(data.table); library(dplyr); library(readxl)

#### Número de solicitudes por ley de transparencia ####

# Este sub- indicador consiste en la suma de las solicitudes por ley de transparencia dividido
# por cada 10000 habitantes, dada por sum(st2014, st2015, st2016)/pob2016 * 10000

setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/77._Participacion_Ciudadana_Solicitudes/")

# Cargar datos: solicitudes de transparencia, codigos de las ciudades

solicitudes <- as.data.frame(read_excel(path = "Clean_Data/Solicitudes.xlsx" ))

city_codes <- as.data.frame(fread("Otros/Study_codes.csv", sep=",", header= T))

# Remover acentos, y pasar todas las letras a capitales, para calzar con los datos de participación

city_codes$Comuna <- chartr('ÁÉÍÓÚ','AEIOU', toupper(city_codes$Comuna))
solicitudes$Comuna <- chartr('ÁÉÍÓÚ','AEIOU', toupper(solicitudes$Comuna))
# Substraer comunas de interés

solicitudes <- solicitudes[solicitudes$Comuna %in% city_codes$Comuna,]

# Añadir nombre de ciudades

solicitudes$Ciudad <- city_codes[match(solicitudes[["Comuna"]], city_codes[["Comuna"]] ), "Ciudad"]


# Resumir según ciudad

solicitudes_ciudad <- solicitudes %>%
  group_by(Ciudad) %>%
  summarize("Con Respuesta" = sum(`RESPUESTA ENTREGADA`, na.rm = T ),
            "Sin Respuesta" = sum(`SIN FINALIZAR`, `SOLICITUD ANULADA`, na.rm = T),
            "Total" = sum(`SOLICITUD DESISTIDA`, `RESPUESTA ENTREGADA`, `DERIVADA`, 
                          `SIN FINALIZAR`, `SOLICITUD ANULADA`, na.rm = T )) %>%
  mutate("Porcentaje Respondido" = round((`Con Respuesta`/Total*100), 2))

# Exportar a csv, tanto por ciudad
solicitudes_ciudad <- solicitudes_ciudad[,c(1,5)]
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/77._Participacion_Ciudadana_Solicitud_Transparencia/")
write.csv(solicitudes_ciudad, "77._Participacion_Ciudadana_Solicitud_Transparencia.csv", row.names = F)
