## Script that calculates ratio of days over 20 PM2.5 over the year.
# Study cities are Coquimbo, Copiapó, Temuco, Santiago, Concepción and Valdivia.
# Data files consist of PM2.5 data from monitoring stations in comunas of interest.
# Code written by Rodrigo Villegas. E-Mail: rdgo.villegas@gmail.com

library(data.table); library(dplyr); library(readxl)

calidad_aire_estacion <- function(station_data, año_ini, año_fin, Estacion.name, ciudad){
  year_ini <- paste(substr(año_ini, 3, 4),"0101", sep="")
  year_end <- paste(substr(año_fin, 3, 4),"1231", sep="")
  reg_days <- 0
  station_data.subset <- station_data[station_data$`FECHA (YYMMDD)` >= year_ini 
                                      & station_data$`FECHA (YYMMDD)` <= year_end,]
  for (o in 3:5){
    # Data has "," as decimal sep. So, we'll replace it in order to convert it to numeric class.
    station_data.subset[,o] <- as.numeric(gsub("," , ".", station_data.subset[,o]))
    # Number of days with registers
    reg_days <- sum(reg_days,!(is.na(station_data[,o])))
  }
  station_data.subset$V6 <- rowSums(station_data.subset[,c("Registros validados", "Registros preliminares" ,"Registros no validados")], na.rm = T)
  promedio_validado <- mean(station_data.subset$`Registros validados`, na.rm = T)
  promedio_total <- mean(station_data.subset$V6)
  output <- data.frame(Estacion = Estacion.name, Ciudad = ciudad, 
                       Periodo = paste(año_ini, año_fin, sep = (" - ")), 
                       promedio_validado = promedio_validado, 
                       promedio_total = promedio_total)
  return(output)
}



oldwd <- "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/73._Calidad_Aire/"
setwd(oldwd)
# Create an empty dataframe to store the results
city_df <- data.frame()


# For every folder:
for (e in 1:length(list.files("Raw_Data"))){
  setwd(oldwd)
  # Store the folder name to analyze, which also is the city name.
  city <- list.files("Raw_Data")[e]
  # Set as working directory the folder with the stations data
  newwd <- setwd(paste("Raw_Data", list.files("Raw_Data")[e], sep = "/"))
  # For every file in folder:
  for (i in 1:length(list.files())){
    # Reading file [i]
    station_data <- as.data.frame(fread(list.files()[i], sep=";",header= T))
    # Getting the station name from the file name.
    station_name <- gsub(".csv", "", list.files()[i])
    # years to analyze: 2013, 2014, 2015
    tmp_df <- calidad_aire_estacion(station_data, 2013, 2016, station_name, city)
    # Merging tmp_df with the output dataframe
    city_df <- rbind(city_df, tmp_df)
    }
}


# Summarizing the data by city and by year.

city_summary <- city_df %>%
  group_by(Ciudad) %>%
  summarise(Promedio_PM2.5_Validado = round(mean(promedio_validado, na.rm = T),3), 
            Promedio_PM2.5_Total = round(mean(promedio_total, na.rm = T),3))

# Exporting the final results
city_summary <- city_summary[,1:2]

setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/73._Calidad_de_Aire/")
write.csv(city_summary, "73._Calidad_de_Aire.csv", row.names = F)
