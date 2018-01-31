# Script para calcular A?os de Vida Potencial Perdidos (AVPP)
# Los a?os de vida potencial perdidos corresponden a la diferencia entre
# un n?mero arbitrario (en este caso 70, en otros casos es la esperanza de vida del pa?s)
# y la edad al fallecer y representa la cantidad de a?os que a un individuo le quedaban por vivir.
# Para calcular el ratio de AVPP por cada 100000 habitantes se sum? el total de AVPP en cada ciudad
# y se dividi? por el total de la poblaci?n.

# Los pasos realizados se pueden encontrar en:
# https://www.cdc.gov/ophss/csels/dsepd/ss1978/lesson3/section3.html

#### Parte 1: Cargar paquetes, carpetas de trabajo y datos a trabajar ####
carpeta <- "E:/Owncloud Cedeus/"
#carpeta <- "C:/Users/rdgov/ownCloud/"

setwd(paste0(carpeta, "Indicadores_de_Sustentabilidad_-_Datos/66._Estado_de_Salud_Adulto/"))
library(dplyr); library(data.table)

muertes <- fread("Raw_Data/DEF_2015.csv", sep = ";", na.strings = "NULL") %>%
  as.data.frame()

pob_2015 <- readxl::read_excel(paste0(carpeta, "Indicadores_de_Sustentabilidad_-_Datos/78._Residuos/Otros/poblacion_2015.xlsx"))

city_codes <- read.csv(paste0(carpeta, "Indicadores_de_Sustentabilidad_-_Datos/78._Residuos/Otros/Ciudades_Cedeus.csv"))

#### Parte 2: Filtrar y combinar tablas para obtener informaci?n de ciudad y poblaci?n ####

# Seleccionar muertes que sean en zonas urbanas, que tengan edad en a?os, que sean v?lidos (!= 999)
# y menores a 70 a?os.
muertes <- muertes %>%
  filter(URBA_RURAL == 1, EDAD_CANT != 999, EDAD_TIPO == 1)

pob_2015 <- pob_2015[pob_2015$`Nombre comuna` %in% city_codes$Comuna & pob_2015$Label == "Total",] %>%
  as.data.frame()
pob_2015$Ciudad <- city_codes[match(pob_2015[["Nombre comuna"]], city_codes[["Comuna"]] ), "Ciudad"]
pob_2015$Codigo <- city_codes[match(pob_2015[["Nombre comuna"]], city_codes[["Comuna"]] ), "Codigo"]



# A?adir Ciudad y poblaci?n a tabla de muertes
muertes <- muertes[muertes$COMUNA %in% pob_2015$Codigo,]
# Resumir tabla de poblaci?n a poblaci?n por ciudad
pob_2015 <- pob_2015 %>%
  group_by(Ciudad) %>%
  summarise(Poblacion = sum(Poblacion)) %>%
  as.data.frame()

muertes$Ciudad <- city_codes[match(muertes[["COMUNA"]], city_codes[["Codigo"]] ), "Ciudad"]

# Calcular AVPP por cada ciudad
avpp <- muertes %>%
  group_by(Ciudad) %>%
  summarize("Muertes_post_EV" = round(sum(EDAD_CANT > 70)/n()*100, 2)) 

setwd(paste0(carpeta, "Indicadores_de_Sustentabilidad_-_Resultados/66._Estado_de_Salud_Adulto/"))
write.csv2(avpp, "66._Estado_de_Salud_Adulto.csv", row.names = F)

