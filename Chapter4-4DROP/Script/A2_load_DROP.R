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

# Cargar scripts personalizados
source("~/Chapter4-4DROP/Data/common/ColorBarM.R") # Carga un script para crear barras de color
source("~/Chapter4-4DROP/Data/common/CorrMIO.R")   # Carga un script para calcular correlaciones
source("~/Chapter4-4DROP/Data/common/mioplot_global.R") # Carga un script para crear gráficos
data(wrld_simpl)

##################################################
# Definición de directorios, carga de coordenadas y mascara
dir_drop = '~/Chapter4-4DROP/Data/'
dir_out  = '~/Chapter4-4DROP/Data/'
load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "inout.RData"))
ni = length(lon)
nj = length(lat)

anni = 1981:2020
mesi = rep(1:12, length(anni))
anno_2000 = which(anni == 2020)
mese_ind = which(mesi == 12)
last_month = mese_ind[anno_2000]

time_scale = c(6)

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
  save(spi, file = paste0(dir_out,"/DROP_SPI", sc,"_ENS_no_scaled.RData"))
  
  for (i in 1:dim(pred)[1]) {
    for (j in 1:dim(pred)[2]) {
      if (sum(!is.na(spi[i,j,]))!=0) {
        spi[i,j,]=scale(spi[i,j,])
      }
    }
  }
  
  image.plot(lon, lat, spi[, , last_month])
  plot(wrld_simpl, add = TRUE)
  
  save(spi, file = paste0(dir_out, "/DROP_SPI", sc,"_ENS.RData"))
  
  
  # 
  spi_sd = apply(pred, c(1, 2, 3), sd, na.rm = TRUE)
  save(spi_sd, file = paste0(dir_out, "/DROP_SPI", sc,"_ENS_SPREAD.RData"))

  
  spi_prob = array(data = NA, dim = c(length(lon), length(lat), dim(pred)[3]))
  for (im in 1:dim(pred)[3]) {
    ## Plot Probability Moderate Drought
    for (i in 1:dim(pred)[1]) {
      for (j in 1:dim(pred)[2]) {
        aux = pred[i, j,im,]
        spi_prob[i, j,im] = (sum(aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)))
        
      }
    }
    spi_prob[, ,im]=spi_prob[, ,im]*inout
  } 
  save(spi_prob, file = paste0(dir_out, "/DROP_PROB", sc,"_DROP.RData"))

  ## traffic light
  spi_tl = array(data = NA, dim = c(length(lon), length(lat), dim(pred)[3]))
  for (im in 1:dim(pred)[3]) {
    for (i in 1:dim(pred)[1]) {
      for (j in 1:dim(pred)[2]) {
        aux = pred[i, j, im,]
        
        
        if (sum(!is.na(aux))!=0) {
          if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) > 0 &
              sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) <= 0.25)  {
            spi_tl[i, j, im] = 2 #yellow code
          } else if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) > 0.25 &
                     sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) <= 0.75)  {
            spi_tl[i, j, im] = 3 #orange code
          } else if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) > 0.75)  {
            spi_tl[i, j, im] = 4 #red code
          
          } else  if (sum(aux[!is.na(aux)] > -1.30 & aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)) > 0 &
                      sum(aux[!is.na(aux)] > -1.30 & aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)) <= 0.5)  { 
            spi_tl[i, j, im] = 2 #yellow code
          } else if (sum(aux[!is.na(aux)] > -1.30 & aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)) > 0.5)  {
            spi_tl[i, j, im] = 3 #orange code
            
          } else if (sum(aux[!is.na(aux)] > -0.8 & aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux)) > 0.5)  {
            spi_tl[i, j, im] = 2 #yellow code
            
          } else  if (sum(aux[!is.na(aux)] > -0.8 & aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux)) > 0 &
                      sum(aux[!is.na(aux)] > -0.8 & aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux)) <= 0.5)  {
           
             spi_tl[i, j, im] = 1 #green code
          } else  {
            spi_tl[i, j, im] = 1 #green code
          }
        }
      }
    }
    spi_tl[, , im] = spi_tl[, , im] * inout
  } 
  
  save(spi_tl, file = paste0(dir_out, "/DROP_TRAF_LIG_", sc,"_DROP.RData"))
}



