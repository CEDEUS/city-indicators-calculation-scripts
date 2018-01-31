# Script para calcular la partición modal de cada ciudad.
# Input: Tablas de Viajes, encontradas dentro de la EOD de cada ciudad.
#        Tablas con las reclasificaciones de los modos de transporte de cada viaje.
#         para agruparlos en 5 categorías.
# Output: Tabla con la partición modal de cada ciudad.

library(data.table); library(dplyr)
# Carpeta donde se encuentra el script. Modificar según necesidades del usuario.
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/74._Particion_Modal/")

# Funciones de ayuda:

# Corrector.nombre:
#   Input: nombres de columna, data.frame, variable a buscar (persona, tiempo, etc...)
#          variable a cambiar (IDPersona, TiempoViajes, etc)
#   Output: Dataset con nombre de variable cambiado.
# Nota: Acepta sólo una variable.
Corrector.nombre <- function(dataset, nombres, variable.buscar, variable.cambiar, exact.match = F){
  if (exact.match == T){
    colname <- grep(paste("^", variable.buscar, "$", sep = ""), nombres, ignore.case = T)
  } else {
    colname <- grep(variable.buscar, nombres, ignore.case = T)
  }
  if (colnames(dataset)[colname] != variable.cambiar){
    colnames(dataset)[colname] <- variable.cambiar
  }
  return(dataset)
}

# Estandarizador:
#   Input: Tabla EOD en bruto
#   Output: Tabla EOD estandarizada
#   Nota: cuando una variable tiene el sufijo "_", se ejecuta "exact.macth" de Corrector.nombre
Estandarizador <- function(Tabla_EOD, variables.buscar, variables.cambiar){
  for (i in seq(variables.buscar)){
    if (grepl("_", variables.buscar[i])){
      variables.buscar[i] <- gsub("_", "", variables.buscar[i])
      Tabla_EOD <- Corrector.nombre(Tabla_EOD, colnames(Tabla_EOD), variables.buscar[i], variables.cambiar[i], exact.match = T)
    } else { 
      Tabla_EOD <- Corrector.nombre(Tabla_EOD, colnames(Tabla_EOD), variables.buscar[i], variables.cambiar[i])
    }
  }
  return(Tabla_EOD)
}

ciudades <- read.csv("Otros/Ciudades_Cedeus.csv")
ciudades <- unique(ciudades$Ciudad)
ciudades <- ciudades[!(ciudades =="Gran Concepcion")]
ciudades <- sort(ciudades)
files.viajes <- list.files("Raw_Data", pattern = "viajes") %>%
  sort()
files.modo <- list.files("Raw_Data", pattern = "ModoReclass") %>%
  sort()

particion.modal <- data.table()

for (c in seq(files.viajes)){
  ciudad.nombre <- ciudades[c]
  
  if (ciudad.nombre %in% c("Gran Santiago")){
    viajes <- read.csv(paste("Raw_Data/", files.viajes[c], sep = ""), sep = ";") %>%
      Estandarizador(c("hogar", "persona", "Viaje_", "ModoAgregado"),
                     c("IDFolio", "IDPersona", "IDViaje", "IDModo")) %>%
      subset(select = c("IDFolio",  "IDPersona", "IDViaje", "IDModo"))
    
    viajes.modo <- read.csv(paste("Raw_Data/", files.modo[c], sep = ""), sep = ";")
    viajes <- merge(viajes, viajes.modo, by = "IDModo")
    
    viajes.summary <- viajes %>%
      summarise(Ciudad = as.character(ciudad.nombre),
                'Transporte Privado/Auto' = round(100*sum(ModoAgregado == "Transporte Privado/Auto")/n(),2),
                'Transporte Publico' = round(100*sum(ModoAgregado == "Transporte Publico")/n(),2),
                Bicicleta = round(100*sum(ModoAgregado == "Bicicleta")/n(),2),
                Caminata = round(100*sum(ModoAgregado == "Caminata")/n(),2),
                Otros = round(100*sum(ModoAgregado == "Otros")/n(),2),
                Total = 100)
  }
  else if (ciudad.nombre == "Gran Coquimbo") {
    viajes <- read.csv(paste("Raw_Data/", files.viajes[c], sep = ""), sep = ";") %>%
      Estandarizador(c("folio", "persona", "idViaje"),
                     c("IDFolio", "IDPersona", "IDViaje")) %>%
      subset(select = c("IDFolio",  "IDPersona", "IDViaje", "IDModo"))
    
    viajes.modo <- read.csv(paste("Raw_Data/", files.modo[c], sep = ""))
    viajes <- merge(viajes, viajes.modo, by = "IDModo")
    
    viajes.summary <- viajes %>%
      summarise(Ciudad = as.character(ciudad.nombre),
                'Transporte Privado/Auto' = round(100*sum(ModoAgregado == "Transporte Privado/Auto")/n(),2),
                'Transporte Publico' = round(100*sum(ModoAgregado == "Transporte Publico")/n(),2),
                Bicicleta = round(100*sum(ModoAgregado == "Bicicleta")/n(),2),
                Caminata = round(100*sum(ModoAgregado == "Caminata")/n(),2),
                Otros = round(100*sum(ModoAgregado == "Otros")/n(),2),
                Total = 100)
    } else {
    viajes <- read.csv(paste("Raw_Data/", files.viajes[c], sep = ""), sep = ";") %>%
      Estandarizador(c("folio", "persona", "idViaje", "Factor_"),
                     c("IDFolio", "IDPersona", "IDViaje", "Factor_Viaje")) %>%
      subset(select = c("IDFolio",  "IDPersona", "IDViaje", "Factor_Viaje", "IDModo"))
    viajes$Factor_Viaje <- as.numeric(gsub(",", ".", viajes$Factor_Viaje))
    
    viajes.modo <- read.csv(paste("Raw_Data/", files.modo[c], sep = ""))
    viajes <- merge(viajes, viajes.modo, by = "IDModo")
    
    viajes.summary <- viajes %>%
      summarise(Ciudad = as.character(ciudad.nombre),
                'Transporte Privado/Auto' = round(100*sum(Factor_Viaje[ModoAgregado == "Transporte Privado/Auto"])/sum(Factor_Viaje),2),
                'Transporte Publico' = round(100*sum(Factor_Viaje[ModoAgregado == "Transporte Publico"])/sum(Factor_Viaje),2),
                Bicicleta = round(100*sum(Factor_Viaje[ModoAgregado == "Bicicleta"])/sum(Factor_Viaje),2),
                Caminata = round(100*sum(Factor_Viaje[ModoAgregado == "Caminata"])/sum(Factor_Viaje),2),
                Otros = round(100*sum(Factor_Viaje[ModoAgregado == "Otros"])/sum(Factor_Viaje),2),
                Total = 100)
    }
  particion.modal <- rbind(particion.modal, viajes.summary)
}

#Escribir resultados
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/74._Particion_Modal/")
write.csv(particion.modal, "74._Particion_Modal.csv", row.names = F)
            