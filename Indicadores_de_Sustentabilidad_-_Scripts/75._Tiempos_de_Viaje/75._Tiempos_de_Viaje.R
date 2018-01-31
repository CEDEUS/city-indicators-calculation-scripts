# Script to calculate the total ratio of travel means to work vs total travels to work.
# It uses data from the Encuesta Origen-Destino published by SECTRA (Subsecretaria de Transporte)
# Study cities are Coquimbo, Copiapó, Temuco, Santiago, Concepción and Valdivia.
# Code written by Rodrigo Villegas. E-Mail: rdgo.villegas@gmail.com

# Loading libraries and setting working directory

# TODO: Incluir validación de viajes


library(data.table); library(dplyr); library(chron); library(lubridate)
#setwd("E:/Cedeus Sustainability Indicators/Datos/EOD")
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/75._Tiempos_de_viaje/")

# funciones de ayuda

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

porcentaje_viaje <- function(data, umbral, sexo = F){
  travel_data <- data %>%
    group_by(IDPersona, IDSexo) %>%
    summarise(TiempoViaje = sum(TiempoViaje, na.rm = T),
              Factor_Persona = unique(Factor_Persona))
  if (sexo != F){
    greater <- travel_data[travel_data$TiempoViaje > umbral & travel_data$IDSexo == sexo,]
    pop_greater <- sum(greater$Factor_Persona)
    pop_total <- sum(travel_data$Factor_Persona[travel_data$IDSexo == sexo])
    percentage <- round(pop_greater/pop_total * 100, 2)
  }  else {
    greater <- travel_data[travel_data$TiempoViaje > umbral,]
    pop_greater <- sum(greater$Factor_Persona)
    pop_total <- sum(travel_data$Factor_Persona)
    percentage <- round(pop_greater/pop_total * 100, 2)
  }
  return(percentage)
}








ciudades <- read.csv("Otros/Ciudades_Cedeus.csv")
ciudades <- unique(ciudades$Ciudad)
# Attempt to clean data
setwd("Raw Data/")
files.viajes <- list.files(pattern = "viajes")
files.personas <- list.files(pattern = "personas")
files.modo <- list.files(pattern = "ModoReclass")
total.ciudad <- data.frame()
total.ciudad.sexo <- data.frame()
modo.ciudad <- data.frame()
for (c in seq(files.viajes)){
  tmp.viajes <- as.data.frame(fread(files.viajes[c], sep = ";", header = T))
  tmp.personas <- as.data.frame(fread(files.personas[c], sep = ";", header = T))
  tmp.modo <- as.data.frame(fread(files.modo[c], sep = ",", header = T))
  nombre.ciudad <- sub( "_.*$", "", files.viajes[c])
  # Almacenar los nombres de las columnas.
  column_names <- colnames(tmp.viajes)
  if (grepl("Santiago", files.viajes[c])){
    
    # Unificar los factores
    Factores <- paste(tmp.viajes$FactorLaboralNormal)
    Factores <- as.numeric(gsub(",", ".", Factores))
    tmp.viajes$Factores <- Factores
    
    tmp.viajes <- Corrector.nombre(tmp.viajes, colnames(tmp.viajes), "persona", "IDPersona") %>%
      Corrector.nombre(colnames(tmp.viajes), "hogar", "IDFolio") %>%
      Corrector.nombre(colnames(tmp.viajes), "viaje", "IDViaje", exact.match = T) %>%
      Corrector.nombre(colnames(tmp.viajes), "ModoAgregado", "IDModo") %>%
      Corrector.nombre(colnames(tmp.viajes), "LaboralNormal", "Factor_Viaje") %>%
      subset(select = c("IDFolio",  "IDPersona", "IDViaje", "TiempoViaje", "Factor_Viaje", "IDModo"))
    
    # El campo $TiempoViaje está en minutos. Multiplicar * 60 para pasar a segundos
    # Y convertirlo en tiempo
    tmp.viajes$Factor_Viaje <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.viajes$Factor_Viaje))
    tmp.viajes$TiempoViaje <- chron(times = tmp.viajes$TiempoViaje/(24*60))
    tmp.viajes <- tmp.viajes[!is.na(tmp.viajes$TiempoViaje) & !is.na(tmp.viajes$Factor_Viaje),]
    
    # Tabla de personas
    tmp.personas <- Corrector.nombre(tmp.personas, colnames(tmp.personas), "persona", "IDPersona") %>%
      Corrector.nombre(colnames(tmp.personas), "hogar", "IDFolio") %>%
      Corrector.nombre(colnames(tmp.personas), "factor", "Factor_Persona", exact.match =T) %>%
      Corrector.nombre(colnames(tmp.personas), "sexo", "IDSexo", exact.match =T) %>%
      subset(select = c("IDFolio", "IDPersona", "IDSexo","Factor_Persona"))
    # Correcciones de formato a la tabla de personas
    tmp.personas$IDSexo <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.personas$IDSexo))
    tmp.personas$Factor_Persona <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.personas$Factor_Persona))
    tmp.personas$IDFolio <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.personas$IDFolio))
    tmp.personas$IDPersona <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.personas$IDPersona))
    # Combinar ambos databases
    tmp <- merge(tmp.personas, tmp.viajes, by = c("IDFolio", "IDPersona")) %>%
      merge(tmp.modo, by = "IDModo")
    
    
    porcentaje.viajes.mayor.1hora <- porcentaje_viaje(tmp, "01:00:00")
    porcentaje.ciudad <- data.frame(Ciudad = nombre.ciudad, Porcentaje = porcentaje.viajes.mayor.1hora)
    
    total.ciudad <- rbind(total.ciudad, porcentaje.ciudad)
    
    porcentaje.viajes.mayor.1hora.hombre <- porcentaje_viaje(tmp, "01:00:00", sexo = 1)
    porcentaje.viajes.mayor.1hora.mujer <- porcentaje_viaje(tmp, "01:00:00", sexo = 2)
    porcentaje.ciudad.sexo <- data.frame(Ciudad = nombre.ciudad, 
                                         Hombres = porcentaje.viajes.mayor.1hora.hombre,
                                         Mujeres = porcentaje.viajes.mayor.1hora.mujer)
    total.ciudad.sexo <- rbind(total.ciudad.sexo, porcentaje.ciudad.sexo)
    
    
    

  } else if (grepl("Coquimbo", files.viajes[c])){
    tmp.viajes <- Corrector.nombre(tmp.viajes, colnames(tmp.viajes), "tiempo", "TiempoViaje") %>%
      Corrector.nombre(colnames(tmp.viajes), "persona", "IDPersona") %>%
      Corrector.nombre(colnames(tmp.viajes), "folio", "IDFolio") %>%
      Corrector.nombre(colnames(tmp.viajes), "idviaje", "IDViaje") %>%
      subset(select = c("IDFolio",  "IDPersona", "IDViaje", "TiempoViaje", "IDModo"))
    
    # Si los tiempos de viaje están en formato "YMD HMS", corregir
    if (nchar(tmp.viajes$TiempoViaje[1]) == 18){
      tmp.viajes$TiempoViaje <- strptime(tmp.viajes$TiempoViaje, format = "%d-%m-%Y %H:%M:%S") 
      tmp.viajes$TiempoViaje <- format(tmp.viajes$TiempoViaje,"%H:%M:%S")
    }
    # Transformar las unidades de tiempo de char a tiempo
    tmp.viajes$TiempoViaje <- chron(times= tmp.viajes$TiempoViaje)
    
    # Importar tabla con población
    tmp.personas <- Corrector.nombre(tmp.personas, colnames(tmp.personas), "persona", "IDPersona") %>%
      Corrector.nombre(colnames(tmp.personas), "folio", "IDFolio") %>%
      Corrector.nombre(colnames(tmp.personas), "factor", "Factor_Persona") %>%
      subset(select = c("IDFolio", "IDPersona", "IDSexo", "Factor_Persona"))
    
    if (class(tmp.personas$Factor_Persona) != "numeric"){
      tmp.personas$Factor_Persona <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.personas$Factor_Persona))
    }
    
    # Combinar ambos databases (viajes y personas).
    tmp <- merge(tmp.personas, tmp.viajes, by = c("IDFolio", "IDPersona")) %>%
      merge(tmp.modo, by = "IDModo")
    tmp$IDPersona <- paste(tmp$IDFolio, tmp$IDPersona, sep = "")
    
    porcentaje.viajes.mayor.1hora <- porcentaje_viaje(tmp, "01:00:00")
    porcentaje.ciudad <- data.frame(Ciudad = nombre.ciudad, Porcentaje = porcentaje.viajes.mayor.1hora)
    
    total.ciudad <- rbind(total.ciudad, porcentaje.ciudad)
    
    porcentaje.viajes.mayor.1hora.hombre <- porcentaje_viaje(tmp, "01:00:00", sexo = 1)
    porcentaje.viajes.mayor.1hora.mujer <- porcentaje_viaje(tmp, "01:00:00", sexo = 2)
    porcentaje.ciudad.sexo <- data.frame(Ciudad = nombre.ciudad, 
                                         Hombres = porcentaje.viajes.mayor.1hora.hombre,
                                         Mujeres = porcentaje.viajes.mayor.1hora.mujer)
    total.ciudad.sexo <- rbind(total.ciudad.sexo, porcentaje.ciudad.sexo)
    
  
    } else {
    # Estandarizar nombres
    tmp.viajes <- Corrector.nombre(tmp.viajes, colnames(tmp.viajes), "tiempo", "TiempoViaje") %>%
      Corrector.nombre(colnames(tmp.viajes), "persona", "IDPersona") %>%
      Corrector.nombre(colnames(tmp.viajes), "folio", "IDFolio") %>%
      Corrector.nombre(colnames(tmp.viajes), "idviaje", "IDViaje") %>%
      Corrector.nombre(colnames(tmp.viajes), "Factor", "Factor_Viaje", exact.match = T) %>%
      subset(select = c("IDFolio",  "IDPersona", "IDViaje", "TiempoViaje", "Factor_Viaje", "IDModo"))
    
    # Si los tiempos de viaje están en formato "YMD HMS", corregir
    if (nchar(tmp.viajes$TiempoViaje[1]) == 18){
      tmp.viajes$TiempoViaje <- strptime(tmp.viajes$TiempoViaje, format = "%d-%m-%Y %H:%M:%S") 
      tmp.viajes$TiempoViaje <- format(tmp.viajes$TiempoViaje,"%H:%M:%S")
    }
    # Transformar las unidades de tiempo de char a tiempo
    tmp.viajes$TiempoViaje <- chron(times= tmp.viajes$TiempoViaje)
    
    
    if (class(tmp.viajes$Factor_Viaje) != "numeric"){
      tmp.viajes$Factor_Viaje <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.viajes$Factor_Viaje))
    }
    
    # Corregir nombres, en caso de que no estén bien asignados
    tmp.personas <- Corrector.nombre(tmp.personas, colnames(tmp.personas), "persona", "IDPersona") %>%
      Corrector.nombre(colnames(tmp.personas), "folio", "IDFolio") %>%
      Corrector.nombre(colnames(tmp.personas), "factor", "Factor_Persona") %>%
      subset(select = c("IDFolio", "IDPersona", "IDSexo", "Factor_Persona"))
    
    # Si los factorese están con separación por ",", reemplazar por "."
    if (class(tmp.personas$Factor_Persona) != "numeric"){
      tmp.personas$Factor_Persona <- as.numeric(gsub(pattern = ",",replacement = ".", x = tmp.personas$Factor_Persona))
    }
    # Combinar ambos databases (viajes y personas).
    tmp <- merge(tmp.personas, tmp.viajes, by = c("IDFolio", "IDPersona")) %>%
      merge(tmp.modo, by = "IDModo")
    # Asignar ID unico a personas, para posterior análisis
    id.personas <- paste(tmp$IDFolio, tmp$IDPersona, sep = "")
    tmp$IDPersona <- id.personas
    # Cuanto tiempo al año gastan los habitantes de las otras ciudades en transporte?
   
    
    
    
    porcentaje.viajes.mayor.1hora <- porcentaje_viaje(tmp, "01:00:00")
    porcentaje.ciudad <- data.frame(Ciudad = nombre.ciudad, Porcentaje = porcentaje.viajes.mayor.1hora)
    
    total.ciudad <- rbind(total.ciudad, porcentaje.ciudad)
    
    porcentaje.viajes.mayor.1hora.hombre <- porcentaje_viaje(tmp, "01:00:00", sexo = 1)
    porcentaje.viajes.mayor.1hora.mujer <- porcentaje_viaje(tmp, "01:00:00", sexo = 2)
    porcentaje.ciudad.sexo <- data.frame(Ciudad = nombre.ciudad, 
                                         Hombres = porcentaje.viajes.mayor.1hora.hombre,
                                         Mujeres = porcentaje.viajes.mayor.1hora.mujer)
    total.ciudad.sexo <- rbind(total.ciudad.sexo, porcentaje.ciudad.sexo)
    
  }
  
  
  
  
}  


setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/75._Tiempos_de_Viaje/")
write.csv(total.ciudad, "75._Tiempos_de_viaje.csv")
#write.csv(total.ciudad.sexo, "75._Tiempos_de_viaje(porsexo).csv")

