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
dir_s5 = '~/Chapter4-4DROP/Data/'
dir_out  = '~/Chapter4-4DROP/Data/'
load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "inout.RData"))
ni = length(lon)
nj = length(lat)

anni = 1981:2020
mesi = rep(1:12, length(anni))
anno_2000 = which(anni == 2020)
mesi_8 = which(mesi == 8)

mes=c("07")
est="JJA"
sc=6
mb=c(1:25) 
dt=c(1:11)

## load data
pred = array(data = NA, dim = c(length(lon), length(lat), length(mesi_8), length(mb), length(dt)))

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  for (me in 1:length(mes)) {
    ms = mes[me]
    
    nam <- paste('spi', sc, 'pred',sep = "")
    print(nam)
    
    load(file.path(
      paste(dir_s5, 'SPI',sc,'SEAS5_',ms,'_',est,"_GPCP.RData", sep = "")))
    aux = get(nam)
    pred[, , , ,1] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_s5,'SPI',sc,'SEAS5_',ms,'_',est,"_CAMS_OPI.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,2] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(
        dir_s5,'SPI',sc,'SEAS5_',ms,'_',est,"_CHIRPS.RData",sep = "") ))
    aux = get(nam)
    pred[, , , ,3] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(
        dir_s5,'SPI',sc,'SEAS5_',ms,'_',est,"_CPC.RData",sep = "") ))
    aux = get(nam)
    pred[, , , ,4] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_s5, 'SPI',sc,'SEAS5_',ms,'_',est,"_GPCC.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,5] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_s5, 'SPI',sc,'SEAS5_',ms,'_',est,"_JRA55.RData", sep = "")  ))
    aux = get(nam)
    pred[, , , ,6] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_s5, 'SPI',sc,'SEAS5_',ms,'_',est,"_PRECL.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,7] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_s5, 'SPI',sc,'SEAS5_',ms,'_',est,"_ERA5.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,8] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_s5, 'SPI',sc,'SEAS5_',ms,'_',est,"_NCEP.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,9] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_s5,'SPI',sc,'SEAS5_',ms,'_',est,"_MERRA2.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,10] = aux[, ,mesi_8,]
    
    
    load(file.path(
      paste(dir_s5,'SPI',sc,'SEAS5_',ms,'_',est,"_MSWEP.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,11] = aux[, ,mesi_8,]
    
    pred[is.infinite(pred)] <- NA
    pred[is.na(pred)] <- NA
    
    
    spi6pred = (apply(pred, c(1, 2, 3), mean, na.rm = TRUE))
    
    save(spi6pred, file = paste0(dir_out, "/S5_SPI", sc,"_",ms,"_",est, "_ENS_no_scaled.RData"))
    
    for (i in 1:dim(pred)[1]) {
      for (j in 1:dim(pred)[2]) {
        if (sum(!is.na(spi6pred[i,j,]))!=0) {
          spi6pred[i,j,]=scale(spi6pred[i,j,])
        }
      }
    }
    save(spi6pred, file = paste0(dir_out, "/S5_SPI", sc,"_",ms,"_",est, "_ENS.RData"))

    spi6pred_sd = apply(pred, c(1, 2, 3), sd, na.rm = TRUE)
    save(spi6pred_sd, file = paste0(dir_out, "/S5_SPI", sc,"_",ms,"_",est, "_ENS_SPREAD.RData"))

    dim(inout) <- c(nrow(pred), ncol(pred))
    inout[inout == 0] = NA
    dim(pred)
    
    spi6pred_prob = array(data = NA, dim = c(length(lon), length(lat), dim(pred)[3]))
    for (im in 1:dim(pred)[3]) {
      ## Plot Probability Moderate Drought
      
      for (i in 1:dim(pred)[1]) {
        for (j in 1:dim(pred)[2]) {
          aux = pred[i, j,im, ,]
          spi6pred_prob[i, j,im] = (sum(aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)))
          
        }
      }
      spi6pred_prob[, ,im]=spi6pred_prob[, ,im]*inout
    }
    save(spi6pred_prob, file = paste0(dir_out, "/S5_PROB", sc,"_",ms,"_",est, "_DROP.RData"))
    
    ## traffic light
    spi6pred_tl = array(data = NA, dim = c(length(lon), length(lat), dim(pred)[3]))
    for (im in 1:dim(pred)[3]) {
      for (i in 1:dim(pred)[1]) {
        for (j in 1:dim(pred)[2]) {
          # aux = pred[i, j, im, ,] #without ENS
          aux = spi6pred[i, j, im] #without ENS
          aux1 =spi6pred_prob[i, j, im]
          
          if (sum(!is.na(aux))!=0) {
            if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux1)) > 0 &
                sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux1)) <= 0.25)  {
              spi6pred_tl[i, j, im] = 2 #yellow code
            } else if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux1)) > 0.25 &
                       sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux1)) <= 0.75)  {
              spi6pred_tl[i, j, im] = 3 #orange code
            } else if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux1)) > 0.75)  {
              spi6pred_tl[i, j, im] = 4 #red code
            } else  if (sum(aux[!is.na(aux)] > -1.30 & aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux1)) > 0 &
                        sum(aux[!is.na(aux)] > -1.30 & aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux1)) <= 0.5)  {
              spi6pred_tl[i, j, im] = 2 #yellow code
            } else if (sum(aux[!is.na(aux)] > -1.30 & aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux1)) > 0.5)  {
              spi6pred_tl[i, j, im] = 3 #orange code
            } else if (sum(aux[!is.na(aux)] > -0.8 & aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux1)) > 0.5)  {
              spi6pred_tl[i, j, im] = 2 #yellow code
            } else  if (sum(aux[!is.na(aux)] > -0.8 & aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux1)) > 0 &
                        sum(aux[!is.na(aux)] > -0.8 & aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux1)) <= 0.5)  {
              spi6pred_tl[i, j, im] = 1 #green code
            } else  {
              spi6pred_tl[i, j, im] = 1 #green code
            }
          }
        }
      }
      spi6pred_tl[, , im] = spi6pred_tl[, , im] * inout
    }
    save(spi6pred_tl, file = paste0(dir_out, "/S5_TRAF_LIG_", sc,"_",ms,"_",est, "_DROP.RData"))
  }
}
image.plot(lon, lat, spi6pred_tl[,,20])
