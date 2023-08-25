rm(list = ls())
graphics.off()
gc()

# Cargar las bibliotecas necesarias
library(sp)             # Trabajo con datos espaciales
library(maptools)       # Manipulación y visualización de datos espaciales
library(RColorBrewer)   # Generación de paletas de colores
library(classInt)       # Clasificación de intervalos para variables continuas
library(fields)         # Análisis de campos y grillas
library(s2dverification)# Verificación de pronósticos
library(maps)           # Creación de mapas
library(pracma)         # Funciones matemáticas prácticas
library(verification)   # Evaluación y verificación de pronósticos
library(psych)          # Análisis psicométricos
library(ncdf4)          # Manipulación de archivos NetCDF

data(wrld_simpl)

##################################################
# Definición de directorios, carga de coordenadas y mascara
dir_drop = '~/Chapter5-4FIRE/Data/'
load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "inout.RData"))
ni = length(lon)
nj = length(lat)

time_scale = c(3,6,12)
## fix parameters
anni = 1981:2020
mesi = rep(1:12, length(anni))
anno_2000 = which(anni == 2020)
mese_ind = which(mesi == 12)
last_month = mese_ind[anno_2000]

## load data
pred = array(data = NA, dim = c(length(lon), length(lat), length(mesi), 11))
dim(pred)

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  nam <- paste("spi", sc, sep = "")
  print(nam)
  
  load(file.path(
    paste(dir_drop, "SPI", sc, "_GPCP_1981_2020.RData", sep = "") ))
  aux = get(nam)
  pred[, , , 1] = aux[, ,]
  
  load(file.path(
    paste(dir_drop,"SPI",sc,"_CAMS_OPI_1981_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 2] = aux[, ,]
  
  load(file.path(
    paste(
      dir_drop,"SPI",sc,"_CHIRPS_1981_2020.RData",sep = "") ))
  aux = get(nam)
  pred[, , , 3] = aux[, ,]
  
  load(file.path(
    paste(
      dir_drop,"SPI", sc,"_CPC_1981_2020.RData",sep = "")  ))
  
  aux = get(nam)
  pred[, , , 4] = aux[, ,]
  
  load(file.path(
    paste(dir_drop, "SPI", sc, "_GPCC_1981_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 5] = aux[, ,]
  
  load(file.path(
    paste(dir_drop, "SPI", sc, "_JRA55_1981_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 6] = aux[, ,]
  
  load(file.path(
    paste(dir_drop, "SPI", sc, "_PRECL_1981_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 7] = aux[, ,]
  
  load(file.path(
    paste(dir_drop, "SPI", sc, "_ERA5_1981_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 8] = aux[, ,]
  
  load(file.path(
    paste(dir_drop, "SPI", sc, "_NCEP_1981_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 9] = aux[, ,]
  
  load(file.path(
    paste(dir_drop,"SPI", sc, "_MERRA2_1981_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 10] = aux[, ,]
  
  load(file.path(
    paste(dir_drop,"SPI", sc, "_MSWEP_1981_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 11] = aux[, ,]
  
  rm(aux)
  aux=pred[,,1:last_month,]
  rm(pred)
  pred=aux
  rm(aux)
  
  spi = (apply(pred, c(1, 2, 3), mean, na.rm = TRUE))
  # save(spi, file = paste0(dir_drop,"/SPI", sc, "_ENS_1981_2020_no_scaled.RData"))
  
  for (i in 1:dim(pred)[1]) {
    for (j in 1:dim(pred)[2]) {
      if (sum(!is.na(spi[i,j,]))!=0) {
        spi[i,j,]=scale(spi[i,j,])
      }
    }
  }
  
  save(spi, file = paste0(dir_drop, "/SPI", sc, "_ENS_1981_2020.RData"))
  
}



