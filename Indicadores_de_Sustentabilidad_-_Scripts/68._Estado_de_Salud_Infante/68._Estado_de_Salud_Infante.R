# Script que calcula el Estado de Salud Infante, el cual fue definido como "Obesidad Infantil"
# En la reuni?n del d?a 16/11/2016. Las ciudades a estudiar son: Copiap?, La Serena/Coquimbo,
# Gran Santiago, Temuco, Gran Concepci?n, y Valdivia.
# La fuente de los datos correspnde a la base de datos de la Serie P, la cual contiene informaci?n
# Sobre el Diagn?stico Nutricional Integrado en el Programa de Salud del Ninio, provisto por la
# DEIS (Departamento de Estad?sticas e Informaci?n de Salud).
# Es necesario se?alar que los datos corresponden a Ninios entre 0 y 9 a?os que se atienden en el sistema
# de salud p?blica. Los Ninios que se atienden en sistema privado no est?n considerados debido a 
# la ausencia de informaci?n#. 
# Adicionalmente, Los Ninios entre 10 y 14 a?os no se encuentran cubiertos por ning?n programa
# Y no se conocen fuentes completas sobre su estado nutricional.

##### Procesamiento de datos #####

# Primero, se cargan las librer?as a utilizar.

require(rgdal); library(data.table); library(dplyr)

# Se fija la carpeta a trabajar
#Origen <- "E:/Owncloud Cedeus"
Origen <- "C:/Users/rdgov/ownCloud/"
setwd(paste0(Origen, "Indicadores_de_Sustentabilidad_-_Datos/68._Estado_de_Salud_Infante/"))

# Se lee la base de datos de la serie P, la cual incluye los datos del Diagn?stico Nutricional Integrado

seriep <- as.data.frame(fread("Raw_Data/serieP.csv", sep= ",", header= T))

# Se definen los c?digos de los estados nutricionales a analizar. Estas corresponden a:
# P2070501: Riesgo/Bajo Peso
# P2070502: Desnutrido
# P2070503: Sobrepeso/Riesgo de Obesidad
# P2070504: Obeso
# P2070505: Normal
# P2070506: Desnutrici?n secundaria

Obesidadclass <- c("P2070501","P2070502","P2070503",
                   "P2070504","P2070505", "P2070506")

# Subset de los datos del Diagn?stico Nutricional Integrado. Se seleccionan las columnas del 1 al 13,
# Las columnas del 1 al 12 contienen info sobre mes, a?o, geograf?a, y c?digo del dato, entre otros.
# La columna 13 contiene la cantidad total de Ninios obsesos.

peso <- seriep[seriep$CodigoPrestacion %in% Obesidadclass ,1:13]

# Leer datos de las ciudades, y agrupar en funci?n de ciudades de estudio.

# Archivo con las ciudades a analizar
city_codes <- fread("Otros/Study_codes.csv", sep=",", header= T)
city_codes <- as.data.frame(city_codes)[,c(1,3)]

# Subset de pesosummary con las ciudades a analizar. Tambi?n se le a?ade el nombre de las ciudades

peso_ciudades <- as.data.frame(peso[peso$IdComuna %in% city_codes$Codigo,])
peso_ciudades$Ciudad <- city_codes[match(peso_ciudades[["IdComuna"]], city_codes[["Codigo"]] ), "Ciudad"]

# Se resume la informaci?n seg?n Ciudad para obtener: 
# Gordos: Total Ninios con sobrepeso + obesidad
# Total: TOtal de Ninios: suma de todos los Ninios
# %Gordos: Porcentaje de Ninios con sobreso + obesidad

pesosummary <- peso_ciudades %>%
  group_by(NComuna) %>%
  summarize(Gordos = sum(Col01[(CodigoPrestacion == "P2070504" | CodigoPrestacion == "P2070503") & Mes == 6], na.rm = T),
            T_Ninios = sum(Col01, na.rm = T)) %>%
  mutate("% Obeso + Sobrepeso" = round(Gordos/T_Ninios*100, 2),
         "% Sano" =  round(100-Gordos/T_Ninios*100, 2))

# Exportar a .csv
pesosummary <- pesosummary[,c(1, 4:5)]
setwd(paste0(Origen, "Indicadores_de_Sustentabilidad_-_Resultados/68._Estado_de_Salud_Infante/"))
write.csv(pesosummary, "68._Estado_de_Salud_Infante_Comuna.csv", row.names = F)






