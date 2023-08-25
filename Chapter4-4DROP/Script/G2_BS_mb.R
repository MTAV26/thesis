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

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    
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
      
      
# convertimos a DROP a binario  
      load(file.path(dir_drop, paste("SPI6_ENS_1981_2020.RData", sep = "" ))) #DROP
      spi[is.infinite(spi)]=NA
      DROP_BIN<-ifelse(spi <= -0.8, 1, 0)
      DROP_BIN= DROP_BIN[,,mesi_8]
      rm(spi)

# convertimos las predicciones de los once miembros de 4DROP en probabilidades
      load(file.path(dir_drop2, paste("SPI",sc,"ESP_",ms,"_",est, "_",dataset,".RData", sep = "" )))
      #load(file.path(dir_drop2, paste("SPI",sc,"SEAS5_",ms,"_",est, "_",dataset,".RData", sep = "" )))
      dim(spi6pred)
      spi6pred[is.infinite(spi6pred)]=NA
      pred= spi6pred[,,mesi_8,]
      
      spi_prob = array(data = NA, dim = c(length(lon), length(lat), dim(pred)[3]))
      for (im in 1:dim(pred)[3]) {
        for (i in 1:dim(pred)[1]) {
          for (j in 1:dim(pred)[2]) {
            aux = pred[i, j,im,]
            spi_prob[i, j,im] = (sum(aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)))
            
          }
        }
        spi_prob[, ,im]=spi_prob[, ,im]*inout
      } 
      
      
      spi_prob[is.infinite(spi_prob)]=NA
      DROP_BIN[is.infinite(DROP_BIN)]=NA
      
      ni = dim(spi_prob)[1]
      nj = dim(spi_prob)[2]

      bs <- matrix(data = NA,nrow = ni, ncol = nj)
      bs_dum = vector()
      
      print(paste0("dataset: ",dataset,"; en ",ms," para ", est))

# calculamos el Brier Score      
      for (i in 1:ni) {
        for (j in 1:nj) {
          
          if (length(which(is.na(DROP_BIN[i, j,] & spi_prob[i, j,])))) {
            next
          }
          bs_dum= brier(DROP_BIN[i, j,], spi_prob[i, j,], na.rm=TRUE)
          # bs_dum$bs[is.infinite(bs_dum$bs)]=NA
          bs[i, j] = as.numeric(bs_dum$bs)
        }
      }
      
      # image.plot(lon, lat, bs)
# Guardamos   
     save(bs, file = file.path(dir_out_ok, paste("BS_ESP_SPI",sc,"_",ms,"_",est,"_",dataset,".RData", sep = "") ))
      # save(bs, file = file.path(dir_out_ok, paste("BS_SEAS5_SPI",sc,"_",ms,"_",est,"_",dataset,".RData", sep = "") ))
      
    }
  }
}


############################################
# Leemos todos los BS
# hacemos la media de los BS
###########################################



for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
      
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
      
      nam <- paste('bs')
      print(nam)
      obs_bs = array(data = NA, dim = c(length(lon), length(lat), 11))
      
      load(file.path(paste(dir_out_ok,"BS_",forecast,"_SPI",sc,"_",ms,"_",est, "_CPC.RData", sep = "") ))
      aux = get(nam)
      obs_bs[, , 1] = aux[,]
      
      load(file.path(paste(dir_out_ok,"BS_",forecast,"_SPI",sc,"_",ms,"_",est,"_GPCC.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 2] = aux[ , ]
      
      load(file.path(paste(dir_out_ok,"BS_",forecast,"_SPI",sc,"_",ms,"_",est,"_PRECL.RData",sep = "") ))
      aux = get(nam)
      obs_bs[, , 3] = aux[ , ]
      
      load(file.path(paste(dir_out_ok,"BS_",forecast,"_SPI",sc,"_",ms,"_",est,"_ERA5.RData",sep = "")  ))
      aux = get(nam)
      obs_bs[, , 4] = aux[ , ]
      
      load(file.path(paste(dir_out_ok,"BS_",forecast,"_SPI",sc,"_",ms,"_",est, "_JRA55.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 5] = aux[ , ]
      
      load(file.path(paste(dir_out_ok,"BS_",forecast,"_SPI",sc,"_",ms,"_",est, "_NCEP.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 6] = aux[ , ]
      
      load(file.path( paste(dir_out_ok,"BS_",forecast,"_SPI",sc,"_",ms,"_",est, "_MERRA2.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 7] = aux[ , ]
      
      load(file.path(paste(dir_out_ok,"BS_",forecast,"_SPI",sc,"_",ms,"_",est, "_CAMS_OPI.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 8] = aux[ , ]
      
      load(file.path(paste(dir_out_ok,"BS_",forecast,"_SPI",sc,"_",ms,"_",est, "_CHIRPS.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 9] = aux[ , ]
      
      load(file.path(paste(dir_out_ok,"BS_",forecast,"_SPI",sc,"_",ms,"_",est, "_GPCP.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 10] = aux[ , ]
      
      load(file.path(paste(dir_out_ok,"BS_",forecast,"_SPI",sc,"_",ms,"_",est,  "_MSWEP.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 11] = aux[ , ]
      
      bs_mean = (apply(obs_bs, c(1, 2), mean, na.rm = TRUE))
      
     
      #save(bs_mean, file = file.path(dir_out_ok, paste("BS_MIEMBROS_SEAS5_SPI",sc,"_",ms,"_",est,"_ENS_MEAN.RData", sep = "") ))
      save(bs_mean, file = file.path(dir_out_ok, paste("BS_MIEMBROS_4DROP_SPI",sc,"_",ms,"_",est,"_ENS_MEAN.RData", sep = "") ))
      
    }
  }
}







