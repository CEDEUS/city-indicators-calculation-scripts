# Script para calcular el indicador de Dependencia al Fondo Común Municipal
# Que consiste en el porcentaje de Dependencia al Fondo Común Municipal (DFCM)
library(readxl); library(dplyr)
# Carpeta de origen
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/82._Dependencia_Fondo_Comun_Municipal/")
#setwd("C:/Users/rdgov/ownCloud/Indicadores_de_Sustentabilidad_-_Datos/82._Dependencia_Fondo_Comun_Municipal/")
# Leer archivos en bruto: Ciudades, Dependencia FCM.
ciudades <- read.csv("Otros/Ciudades_Cedeus.csv")
ifcm <- read_excel("Raw_Data/Dependencia FCM.xlsx")
# Hacer un match entre ambos archivos en función de los códigos para ciudades.
ifcm <- ifcm[ifcm$CODIGO %in% ciudades$Codigo,]
ifcm$Ciudad <- ciudades[match(ifcm[["CODIGO"]], ciudades[["Codigo"]] ), "Ciudad"]
# Resumir la info, obteniendo el promedio de dependencia.
ifcm.summary <- ifcm %>%
  group_by(Ciudad) %>%
  summarise(Dependencia_FCM = round(mean(`IADM75 (%) Dependencia del Fondo Común Municipal sobre los Ingresos Propios`),1))
# Exportar resultados
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/82._Dependencia_Fondo_Comun_Municipal/")
write.csv(ifcm.summary, "82._Dependencia_Fondo_Comun_Municipal.csv", row.names = F)


