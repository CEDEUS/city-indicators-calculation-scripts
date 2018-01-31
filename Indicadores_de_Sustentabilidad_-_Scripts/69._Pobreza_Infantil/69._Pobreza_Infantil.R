## Script that generates child poverty ratio based on number of children under 14 years old under
# poverty line and total number of children. Studies cities are Coquimbo, Copiapó, Temuco,
# Santiago, Concepción and Valdivia.
# Data files consist of CASEN 2015, poverty line by 2015, city codes and estimated population by 2015.


library(data.table); library(dplyr); library(readxl); library(bit64)
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/69._Pobreza_Infantil/")

##### Number of children per city proyected for 2015 #####

# Loading population proyections for 2015

pob_2015 <- read_excel(path = "Otros/poblacion_2015.xlsx" )

city_codes <- as.data.frame(fread("Otros/Study_codes.csv", sep=",", header= T))

# Subsetting population of each comuna of interest

pob_2015 <- pob_2015[pob_2015$`Nombre comuna` %in% city_codes$Comuna,]
pob_2015$Ciudad <- city_codes[match(pob_2015[["Nombre comuna"]], city_codes[["Comuna"]] ), "Ciudad"]

# Summarizing the number of children under 14 in each city.
# I wasn't able to find an easier way to sum the columns :(

pob_2015 <- pob_2015 %>%
  group_by(Ciudad) %>%
  summarise("Población infantil" = sum(`0-4`,`5-9`,`10-14`))

##### Number of children under poverty line based on CASEN 2015 ####

# Loading CASEN 2015, and poverty line per number of household inhabitants 2015
# Data of interest in CASEN 2015 is:
# Comuna (7)
# Edad (18)
# Ingreso total del hogar (707)
# Numero de personas en el hogar (745)
# Factor de expansión comunal (737)

casen2015.route <- "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/CASEN_2015/CASEN_2015.csv"

casen2015 <- as.data.frame(fread(casen2015.route, sep=",", header= T, select = c(7,18,707,745,737)))

lpobreza <- as.data.frame(fread("Otros/Linea_de_pobreza_y_linea_de_probreza_extrema_2015.csv", sep=",", header= T))

# subset casen2015 by city 
casen_sub <- casen2015[casen2015$comuna %in% city_codes$Comuna_lowercase,]
# match city names with comunas
casen_sub$Ciudad <- city_codes[match(casen_sub[["comuna"]], city_codes[["Comuna_lowercase"]] ), "Ciudad"]

# subset casen_sub by children younger than 14 years old. Additionally, the cases where the number
# of people in the household where higher than 10 were ignored because we couldn't fit them in the
# poverty line provided by CASEN 2015.

casen_child <- casen_sub[casen_sub$edad <= 14 & casen_sub$numper %in% lpobreza$`Numero de personas por hogar`,]
casen_child$expc[is.na(casen_child$expc)] <- 1

# A for loop to subset people by poverty line values.

poverty <- data.frame()
for (i in 1:nrow(lpobreza)){
  tmp <- casen_child[casen_child$numper == lpobreza$`Numero de personas por hogar`[i] & casen_child$ytoth < lpobreza$`Linea de pobreza`[i],]
  poverty <- rbind(poverty,tmp)
}

# Summarizing the number of children under the poverty line per city

poverty_summary <- poverty %>%
  group_by(Ciudad) %>%
  summarise("Número de niños bajo linea de pobreza" = sum(expc))

# Summarizing the number of children in each city

childre_pop_summary <- casen_child %>%
  group_by(Ciudad) %>%
  summarise("Número total de niños" = sum(expc))

# Getting child poverty ratios

child_poverty <- merge(poverty_summary, childre_pop_summary, by="Ciudad")
child_poverty <- child_poverty %>%
  mutate("Pobreza infantil" = round(`Número de niños bajo linea de pobreza`/`Número total de niños`,4)*100)

# Exporting the final result
child_poverty <- child_poverty[,c(1, 4)]
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/69._Pobreza_Infantil/")
write.csv(child_poverty, "69._Pobreza_infantil.csv", row.names = F)

