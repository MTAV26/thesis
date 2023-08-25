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
source("~/Chapter4-4DROP/Data/common/my_boxplot_stat.R")
source("~/Chapter4-4DROP/Data/common/my_boxplot.R")
source("~/Chapter4-4DROP/Data/common/ReliabilityDiagram_MIO2.R")
source("~/Chapter4-4DROP/Data/common/myreliability.R")
##################################################
# Definición de directorios, carga de coordenadas y mascara
dir_drop =  '~/Chapter4-4DROP/Data/'
dir_oss=  '~/Chapter4-4DROP/Data/'
dir_drop2 =  '~/Chapter4-4DROP/Data/'
dir_out=  '~/Chapter4-4DROP/Data/'
dir_out_ok= '~/Chapter4-4DROP/Data/'

load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
lat2 = lat[which(lat > -60 & lat < 85)]
inout2 = inout[, which(lat > -60 & lat < 85)]
load(file.path(dir_drop, "inout.RData"))
data(wrld_simpl)

anni = 1981:2020
mesi = rep(1:12, length(anni))
nb = 1000
time_scale = c(6)
sc=6

datasets = c('CPC',
             'GPCC',
             'PRECL',
             'ERA5',
             'JRA55',
             'NCEP',
             'MERRA2',
             'CAMS_OPI',
             'CHIRPS',
             'GPCP',
             'MSWEP')

mes=c("11","01",
"02","04",
"05", "07",
"08","10")

forecast=c("SEAS5")
mb=25
# forecast=c("ESP")
# mb=39

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  for (me in 1:length(mes)) {
    ms = mes[me]
    
    if (ms == "11") {
      start_date = 11
      mesi_8 = which(mesi == 2)[-1]
      est = "DJF"
      
      
    } else if (ms == "01") {
      start_date = 1
      mesi_8 = which(mesi == 2)[-1]
      est = "DJF"
      
    } else if (ms == "02") {
      start_date = 2
      mesi_8 = which(mesi == 5)[-1]
      est = "MAM"
      
    } else if (ms == "04") {
      start_date = 4
      mesi_8 = which(mesi == 5)[-1]
      est = "MAM"
      
    } else if (ms == "05") {
      start_date = 5
      mesi_8 = which(mesi == 8)
      est = "JJA"
      
    } else if (ms == "07") {
      start_date = 7
      mesi_8 = which(mesi == 8)
      est = "JJA"
      
    } else if (ms == "08") {
      start_date = 8
      mesi_8 = which(mesi == 11)
      est = "SON"
      
    } else if (ms == "10") {
      start_date = 10
      mesi_8 = which(mesi == 11)
      est = "SON"
      
    } else {
      print('dataset not known')
    }
    
    
    if (ms  == "11" | ms  =="01" | ms  =="02" | ms  =="04"){
      pred = array(data = NA, dim = c(dim(inout)[1], dim(inout)[2], length(anni[-1]),mb ,(length(datasets))))
    } else {
      pred = array(data = NA, dim = c(dim(inout)[1], dim(inout)[2], length(anni),mb, (length(datasets))))
    }
    
    # convertimos a DROP a binario  
    load(file.path(dir_drop, paste("SPI6_ENS_1981_2020.RData", sep = "" ))) #DROP
    spi[is.infinite(spi)]=NA
    DROP_BIN<-ifelse(spi <= -0.8, 1, 0)
    DROP_BIN= DROP_BIN[,,mesi_8]
    rm(spi)

    nam <- paste('spi', sc, 'pred',sep = "")
    print(nam)
    
    load(file.path(
      paste(dir_drop2, 'SPI',sc,forecast,'_',ms,'_',est,"_GPCP.RData", sep = "")))
    aux = get(nam)
    pred[, , , ,1] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_drop2,'SPI',sc,forecast,'_',ms,'_',est,"_CAMS_OPI.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,2] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(
        dir_drop2,'SPI',sc,forecast,'_',ms,'_',est,"_CHIRPS.RData",sep = "") ))
    aux = get(nam)
    pred[, , , ,3] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(
        dir_drop2,'SPI',sc,forecast,'_',ms,'_',est,"_CPC.RData",sep = "") ))
    aux = get(nam)
    pred[, , , ,4] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_drop2, 'SPI',sc,forecast,'_',ms,'_',est,"_GPCC.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,5] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_drop2, 'SPI',sc,forecast,'_',ms,'_',est,"_JRA55.RData", sep = "")  ))
    aux = get(nam)
    pred[, , , ,6] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_drop2, 'SPI',sc,forecast,'_',ms,'_',est,"_PRECL.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,7] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_drop2, 'SPI',sc,forecast,'_',ms,'_',est,"_ERA5.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,8] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_drop2, 'SPI',sc,forecast,'_',ms,'_',est,"_NCEP.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,9] = aux[, ,mesi_8,]
    
    load(file.path(
      paste(dir_drop2,'SPI',sc,forecast,'_',ms,'_',est,"_MERRA2.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,10] = aux[, ,mesi_8,]
    
    
    load(file.path(
      paste(dir_drop2,'SPI',sc,forecast,'_',ms,'_',est,"_MSWEP.RData", sep = "") ))
    aux = get(nam)
    pred[, , , ,11] = aux[, ,mesi_8,]
    
    pred[is.infinite(pred)] <- NA
    pred[is.na(pred)] <- NA

    spi6pred = pred
    spi_prob = array(data = NA, dim = c(length(lon), length(lat), dim(spi6pred)[3]))
    # 
     for (im in 1:dim(spi6pred)[3]) {
      for (i in 1:dim(spi6pred)[1]) {
        for (j in 1:dim(spi6pred)[2]) {
          aux = spi6pred[i, j,im,,]
          spi_prob[i, j,im] = (sum(aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)))

        }
      }
      spi_prob[, ,im]=spi_prob[, ,im]*inout
    }
    
    #hago la media de las once probabilidades
    prob_mean = spi_prob
    prob_mean[is.infinite(prob_mean)]=NA
    DROP_BIN[is.infinite(DROP_BIN)]=NA
    
    ni = dim(prob_mean)[1]
    nj = dim(prob_mean)[2]
    
    bs <- matrix(data = NA,nrow = ni, ncol = nj)
    bs_dum = vector()
    
    # calculamos el Brier Score      
    for (i in 1:ni) {
      for (j in 1:nj) {
        
        if (length(which(is.na(DROP_BIN[i, j,] & prob_mean[i, j,])))) {
          next
        }
        
        bs_dum= brier(DROP_BIN[i, j,], prob_mean[i, j,], na.rm=TRUE)
        bs[i, j] = as.numeric(bs_dum$bs)
      }
    }
    
    image.plot(lon, lat, bs)
    # Guardamos   
    # save(bs, file = file.path(dir_out_ok, paste("BS_4DROP_SPI",sc,"_",ms,"_",est,"_media_prob.RData", sep = "") ))
    save(bs, file = file.path(dir_out_ok, paste("BS_",forecast,"_SPI",sc,"_",ms,"_",est,"_ok.RData", sep = "") ))
  }
}

