# Script que calcula la calidad de salud para las ciudades de Concepci?n, Coquimbo, 
# Copiap?, Temuco , Santiago and Valdivia.
# La Calidad de Salud se entiende como la raz?n entre el n?mero de muertes prevenibles dividido
# Por el n?mero total de muertes.
# Las muertes prevenibles vienen dadas por el estudio "La mortalidad evitable y no 
# evitable: distribuci?n geogr?fica en ?reas peque?as de Espa?a (1990-2001)" - Vergara, M., 2007.
# Los datos usados consisten en:
#   Base de datos de defunciones para el a?o 2014.
#   Rangos_muertesprevenibles. Una categorizaci?n de los c?digos CEI-10 y rangos de edad para 
#                             las muertes prevenibles
#   study_codes. Un archivo .csv que contiene los c?digos de las comunas a estudiar.

# Cargar las librer?as a usar, y el directorio de trabajo

library(data.table); library(dplyr); library(readxl)
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/65._Calidad_de_Salud/")

# Cargar la base de datos de defunciones, los c?digos de enfermedades prevenibles, 
# y los c?digos de las ciudades

defunctions <- fread("Raw_Data/DEF2014.csv", sep= ";", header= T, select = c(5,7,8,17,18,19,20))
prev_codes <- read_excel(path = "Otros/Rangos_muertesprevenibles.xlsx" )
city_codes <- as.data.frame(fread("Otros/Study_codes.csv", sep=",", header= T))

# Subset de las defunciones cuya Comuna est? en las ciudades a estudiar

defunctions <- defunctions[defunctions$COMUNA %in% city_codes$Codigo,]

# A?adir el nombre de la ciudad.

defunctions$Ciudad <- city_codes[match(defunctions[["COMUNA"]], city_codes[["Codigo"]] ), "Ciudad"]

# Fijar como EDAD_CANT = 0 a todos los ni?os cuya edad sea menor a 1 (EDAD_TIPO > 1)

defunctions[!(test$EDAD_TIPO == 1 | defunctions$EDAD_TIPO == 9)]$EDAD_CANT <- 0

# Descartar casos donde EDAD_TIP == 9, ya que 9 es null.

defunctions <- defunctions[!(defunctions$EDAD_TIPO == 9)]

# Summarizar el total de muertes, seg?n ciudad, y lugar de defunci?n.

total_def <- defunctions %>%
  group_by(Ciudad) %>%
  summarise("Defunciones totales en Hospital o Clinica" = sum(LOCAL_DEF == 1), 
            "Defunciones totales en Casa Habitacion" = sum(LOCAL_DEF == 2), 
            "Defunciones totales en Otro" = sum(LOCAL_DEF == 3),
            "Defunciones en Total" = (n()))

# Una peque?a funci?n para hacer un subset por cada tipo de defunci?n
# inputdata hace referencia al dataframe "defunctions"
# inputcodes hace referencia a los c?digos de defunciones.

prev_filter <- function(inputdata, inputcodes){
  # Crea un dataframe vacio para almacenar resultados
  finaldata <- data.frame()
  # Por cada fila en el dataframe con los c?digos de muertes prevenibles, subset aquellos que
  # son iguales o mayores que la edad m?nima; igual o menores a la edad m?xima, y que cuya 
  # diagnosis est? entre los brackes de c?digos CEI-10 definidos por "Char_Min" y "Char_Max".

  for (i in 1:nrow(inputcodes)){
    # subset data basandose en el c?digo de muerte preenible n?mero "i"
    tmpdata <- inputdata[inputdata$EDAD_CANT >= inputcodes$Edad_min[i] &
                           inputdata$EDAD_CANT <= inputcodes$Edad_max[i] &
                           inputdata$DIAG1 >= inputcodes$Char_Min[i] &
                           inputdata$DIAG1 <= inputcodes$Char_Max[i],]
    # unir tmpdata con finaldata
    finaldata <- rbind(finaldata, tmpdata)
  }
  return(finaldata)
}

# Aplicar la funci?n a los datos, y summarizar por ciudad y lugar de muerte

prev_defunctions <- prev_filter(defunctions, prev_codes) %>%
  group_by(Ciudad) %>%
  summarise("Defunciones prevenibles Hospital o Clinica" = sum(LOCAL_DEF == 1), 
            "Defunciones prevenibles Casa Habitacion" = sum(LOCAL_DEF == 2), 
            "Defunciones prevenibles Otro" = sum(LOCAL_DEF == 3),
            "Defunciones prevenibles Total" = (n()))

# Combinar ambos dataframes (prev_defunctions y total_def), y calcular los ratios de muertes
# prevenibles para cada lugar, y en total.

health_quality <- merge(prev_defunctions, total_def, by = "Ciudad") %>%
  mutate("Razon defunciones prevenibles Hospital o Clinica" = round(`Defunciones prevenibles Hospital o Clinica`/`Defunciones totales en Hospital o Clinica`, 4),
         "Razon defunciones prevenibles Casa Habitacion" = round(`Defunciones prevenibles Casa Habitacion`/`Defunciones totales en Casa Habitacion`, 4),
         "Razon defunciones prevenibles Otro" = round(`Defunciones prevenibles Otro`/`Defunciones totales en Otro`, 4),
         "Porcentaje defunciones prevenibles Total" = round(`Defunciones prevenibles Total`/`Defunciones en Total`*100, 4))

# Re-ordenar los datos, para que tengamos "Muertes prevenibles", "Muertes totales", 
# "raz?n muertes prevenibles" para cada lugar y ciudad

health_quality <- as.data.frame(health_quality)
health_quality <- health_quality[,c(1,2,6,10,3,7,11,4,8,12,5,9,13)]

# Tabla a mostrar en la web
health_quality <- health_quality[,c(1, 13)]

# Exportar a .csv

setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/65._Calidad_de_Salud/")
write.csv(health_quality, "65._Calidad_de_Salud.csv", row.names = F)
