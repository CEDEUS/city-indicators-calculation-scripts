# Script that generates female ocupation ratio, where 1 is 100% of women in working age employed and
# 0 is no women of working age employed.
# 2012 Census data was used to include urban women from chilean cities of Coquimbo, Copiapó, Temuco,
# Santiago, Concepción and Valdivia.
# Data files used consists of idgeo_table, Población-INE-Actualización-2002-2012-Proyección-2013-2020.xlsx,
# Encuestra trimestral de empleo junio-agosto 2016, and Study_codes (built from comuna codes of
# comunas of interest)


library(data.table); library(dplyr); library(readxl)
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/86._Ocupacion_Femenina_Equidad/")

#### Calculating female urban population number in each city ####

# Loading population proyections to 2016 data and city codes. 

pob_2016 <- read_excel(path = "Raw_Data/poblacion_2016.xlsx" )

city_codes <- as.data.frame(fread("Otros/Study_codes.csv", sep=",", header= T))

#### Calculating women who worked ####

# Loading encuesta nacional de empleo trimestre junio-agosto 2016

empleo <- as.data.frame(fread("Raw_Data/ene-2016-07-jja.csv", sep= ",", header= T, select = c(5, 18, 22, 36, 243)))

# Subsetting women who worked at least an hour in the last week

empleo <- empleo[empleo$`Codigo Comuna - Region Provincia Comuna` %in% city_codes$Codigo 
                 & empleo$`A1. La semana pasada, es decir, entre lunes y domingo, Â¿trabajo por lo menos una hora?_labels` == "Si"
                 & empleo$`Edad de la persona` > 15
                 & empleo$`Edad de la persona` <= 65,]

# giving city names to the comunas of interest

empleo$Ciudad <- city_codes[match(empleo[["Codigo Comuna - Region Provincia Comuna"  ]], city_codes[["Codigo"]] ), "Ciudad"]

# Summarizing the number of working women by city

empleo_mujer <- empleo[empleo$Sexo_labels == "Mujer" & empleo$`Edad de la persona` <= 60,] %>%
  group_by(Ciudad) %>%
  summarise("Mujeres trabajando" = round(sum(`FACTOR DE EXPANSION TRIMESTRAL`)))


# Summarizing the number of working men by city
empleo_hombre <- empleo[empleo$Sexo_labels == "Hombre",] %>%
  group_by(Ciudad) %>%
  summarise("Hombres trabajando" = round(sum(`FACTOR DE EXPANSION TRIMESTRAL`)))


# Merging both tables and obtaining "working/work" able ratio. Result is rounded to the 4th digit.

ocupacion_femenina_b <- merge(empleo_mujer, empleo_hombre, by="Ciudad") %>%
  group_by(Ciudad) %>%
  mutate("Ratio ocupación" = round(`Mujeres trabajando`/`Hombres trabajando`*100,2))

# Exporting the final result
ocupacion_femenina_b <- ocupacion_femenina_b[,c(1,4)]
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/86._Ocupacion_Femenina_Equidad/")
write.csv(ocupacion_femenina_b, "86._Ocupacion_Femenina_Equidad.csv", row.names = F)
