
# Limpieza del ambiente de trabajo previo
rm(list = ls())
graphics.off()
gc()

# Carga de paquetes necesarios
library(sp)               # Proporciona clases y métodos para datos espaciales
library(maptools)         # Funciones para manipular datos espaciales y geográficos
library(RColorBrewer)     # Esquemas de colores predefinidos y personalizados
library(classInt)         # Ayuda a encontrar intervalos de clase para variables continuas
library(fields)           # Herramientas para interpolación y análisis de datos espaciales
library(s2dverification)  # Utilizado en la verificación de pronósticos y observaciones
library(maps)             # Para trazar mapas y agregar información geográfica
library(pracma)           # Funciones matemáticas prácticas
library(verification)     # Utilizado en la verificación de pronósticos
library(psych)            # Funciones para el análisis psicométrico y estadístico
library(sf)               # Clases y funciones para datos espaciales
library(oce)              # Análisis y visualización de datos oceanográficos
library(raster)           # Clases y métodos para manejo de datos raster
library(ggplot2)          # Crear gráficos en capas
library(terra)            # Funcionalidades para análisis espacial
library(graticule)        # Agregar líneas graticulares a gráficos de mapas
library(rgdal)            # Lectura y escritura de datos geoespaciales
library(rworldmap)        # Funciones para trazar mapas mundiales
library(rnaturalearth)    # Proporciona datos geoespaciales naturales del mundo

data(coastlineWorld)
data(wrld_simpl)

dir_drop = '~/Chapter4-4DROP/Data/'
dir_4drop= '~/Chapter4-4DROP/Data/'
dir_s5= '~/Chapter4-4DROP/Data/'
dir_out= '~/Chapter4-4DROP/Figures/'
load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))

anni = 1981:2020
mesi = rep(1:12, length(anni))
mesi_8 = which(mesi == 8)

ms=c("07")
est=c('JJA')
sc=c(6)

mb=c(1:39)
mb2=c(1:25)
dt=c(1:11)
casi = c('SUDAMERICA')

for (icaso in 1:length(casi)) {
  #for (icaso in 1:1) {
  caso = casi[icaso]

  if (caso == "EUROPA") {
    lon1=13
    lat1=56
  } else if (caso == "SUDAMERICA") {
    lon1=-63.75
    lat1=-33.75 
  } else {
    print('case study not known')
  }
  
  i = which(round(lon*100) == round(lon1*100))
  lon[i]
  j = which(round(lat*100) == round(lat1*100))
  lat[j]
  i
  j
  load(file.path(
    paste(dir_drop, "/SPI6_ENS_1981_2020.RData", sep = "")
  ))
  
  obs = spi[,,mesi_8]
  plot(obs[i,j,], type="l")
  plot(wrld_simpl, add = TRUE)
  points(x = lon1, y = lat1, pch = 16, col = "black", cex = 1.5)
  

  ## load data
  pred = array(data = NA, dim = c(dim(obs)[1], dim(obs)[2], dim(obs)[3], 11))
  load(file.path(
    paste(dir_drop, "SPI6_GPCP_1981_2020.RData", sep = "")))
  pred[, , , 1] = spi6[,,mesi_8]
  
  load(file.path(
    paste(dir_drop, "SPI6_CAMS_OPI_1981_2020.RData", sep = "")))
  pred[, , , 2] = spi6[,,mesi_8]
  
  load(file.path(
    paste(dir_drop, "SPI6_CHIRPS_1981_2020.RData",sep = "")))
  pred[, , , 3] = spi6[,,mesi_8]
  
  load(file.path(
    paste(dir_drop,"SPI6_CPC_1981_2020.RData", sep = "")))
  pred[, , , 4] = spi6[,,mesi_8]
  
  load(file.path(
    paste(dir_drop, "SPI6_GPCC_1981_2020.RData", sep = "")))
  pred[, , , 5] = spi6[,,mesi_8]
  
  load(file.path(
    paste(dir_drop, "SPI6_JRA55_1981_2020.RData", sep = "")))
  pred[, , , 6] = spi6[,,mesi_8]
  
  load(file.path(
    paste(dir_drop, "SPI6_PRECL_1981_2020.RData", sep = "")))
  pred[, , , 7] = spi6[,,mesi_8]
  
  load(file.path(
    paste(dir_drop, "SPI6_ERA5_1981_2020.RData", sep = "")))
  pred[, , , 8] = spi6[,,mesi_8]
  
  load(file.path(
    paste(dir_drop, "/SPI6_NCEP_1981_2020.RData", sep = "")))
  pred[, , , 9] = spi6[,,mesi_8]
  
  load(file.path(
    paste(dir_drop,"SPI6_MERRA2_1981_2020.RData",sep = "")))
  pred[, , , 10] = spi6[,,mesi_8]
  
  load(file.path(
    paste(dir_drop,"SPI6_MSWEP_1981_2020.RData",sep = "")))
  pred[, , , 11] = spi6[,,mesi_8]
  
  pred[is.infinite(pred)] <- NA
  pred[is.na(pred)] <- NA
  
  
  mb=c(1:39)
  dt=c(1:11)
  
  ## load data
  pred1 = array(data = NA, dim = c(length(lon), length(lat), length(mesi_8), length(mb), 11))
  nam <- paste('spi', sc, 'pred',sep = "")
  print(nam)
  
  load(file.path(
    paste(dir_4drop, 'SPI',sc,'ESP_',ms,'_',est,"_GPCP.RData", sep = "")))
  aux = get(nam)
  pred1[, , , ,1] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_4drop,'SPI',sc,'ESP_',ms,'_',est,"_CAMS_OPI.RData", sep = "") ))
  aux = get(nam)
  pred1[, , , ,2] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(
      dir_4drop,'SPI',sc,'ESP_',ms,'_',est,"_CHIRPS.RData",sep = "") ))
  aux = get(nam)
  pred1[, , , ,3] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(
      dir_4drop,'SPI',sc,'ESP_',ms,'_',est,"_CPC.RData",sep = "") ))
  aux = get(nam)
  pred1[, , , ,4] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_4drop, 'SPI',sc,'ESP_',ms,'_',est,"_GPCC.RData", sep = "") ))
  aux = get(nam)
  pred1[, , , ,5] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_4drop, 'SPI',sc,'ESP_',ms,'_',est,"_JRA55.RData", sep = "")  ))
  aux = get(nam)
  pred1[, , , ,6] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_4drop, 'SPI',sc,'ESP_',ms,'_',est,"_PRECL.RData", sep = "") ))
  aux = get(nam)
  pred1[, , , ,7] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_4drop, 'SPI',sc,'ESP_',ms,'_',est,"_ERA5.RData", sep = "") ))
  aux = get(nam)
  pred1[, , , ,8] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_4drop, 'SPI',sc,'ESP_',ms,'_',est,"_NCEP.RData", sep = "") ))
  aux = get(nam)
  pred1[, , , ,9] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_4drop,'SPI',sc,'ESP_',ms,'_',est,"_MERRA2.RData", sep = "") ))
  aux = get(nam)
  pred1[, , , ,10] = aux[, ,mesi_8,]
  
  
  load(file.path(
    paste(dir_4drop,'SPI',sc,'ESP_',ms,'_',est,"_MSWEP.RData", sep = "") ))
  aux = get(nam)
  pred1[, , , ,11] = aux[, ,mesi_8,]
  
  pred1[is.infinite(pred1)] <- NA
  pred1[is.na(pred1)] <- NA
  
  
  mb2=c(1:25)
  dt=c(1:11)
  
  ## load data
  pred2 = array(data = NA, dim = c(length(lon), length(lat), length(mesi_8), length(mb2),11))
  
  nam <- paste('spi', sc, 'pred',sep = "")
  print(nam)
  
  load(file.path(
    paste(dir_s5, 'SPI',sc,'SEAS5_',ms,'_',est,"_GPCP.RData", sep = "")))
  aux = get(nam)
  pred2[, , , ,1] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_s5,'SPI',sc,'SEAS5_',ms,'_',est,"_CAMS_OPI.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,2] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(
      dir_s5,'SPI',sc,'SEAS5_',ms,'_',est,"_CHIRPS.RData",sep = "") ))
  aux = get(nam)
  pred2[, , , ,3] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(
      dir_s5,'SPI',sc,'SEAS5_',ms,'_',est,"_CPC.RData",sep = "") ))
  aux = get(nam)
  pred2[, , , ,4] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_s5, 'SPI',sc,'SEAS5_',ms,'_',est,"_GPCC.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,5] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_s5, 'SPI',sc,'SEAS5_',ms,'_',est,"_JRA55.RData", sep = "")  ))
  aux = get(nam)
  pred2[, , , ,6] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_s5, 'SPI',sc,'SEAS5_',ms,'_',est,"_PRECL.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,7] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_s5, 'SPI',sc,'SEAS5_',ms,'_',est,"_ERA5.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,8] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_s5, 'SPI',sc,'SEAS5_',ms,'_',est,"_NCEP.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,9] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_s5,'SPI',sc,'SEAS5_',ms,'_',est,"_MERRA2.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,10] = aux[, ,mesi_8,]
  
  
  load(file.path(
    paste(dir_s5,'SPI',sc,'SEAS5_',ms,'_',est,"_MSWEP.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,11] = aux[, ,mesi_8,]
  
  pred2[is.infinite(pred2)] <- NA
  pred2[is.na(pred2)] <- NA

  ni = length(lon)
  nj = length(lat)
  ilon = which(lon >= lon1-3 & lon <= lon1+3)
  ilat = which(lat >= lat1-3 & lat <= lat1+3)

  months=mesi_8
  years=rep(anni,each=12)
  fecha=paste0(as.character(years[months]),"-",formatC(mesi[months], width=2, flag="0"))
  
  aux0=pred[ilon, ilat,,]
  aux1=apply(pred1[ilon, ilat,,,], c(1, 2, 3, 5), mean, na.rm = TRUE)
  aux2=apply(pred2[ilon, ilat,,,], c(1, 2, 3, 5), mean, na.rm = TRUE)
  dim(aux2)

  box=aux0[2,2,,]
  box1=aux1[2,2,,]
  box2=aux2[2,2,,]
  
  
  plot_data <-
    data.frame(
      box[ 1,], box[ 2,],box[ 3,],box[ 4,],box[ 5,],box[ 6,],box[ 7,], box[ 8,],box[ 9,],box[ 10,],box[ 11,],
      box[ 12,],box[ 13,],box[ 14,],box[ 15,],box[ 16,],box[ 17,], box[ 18,],box[ 19,],box[ 20,],box[ 21,],
      box[ 22,],box[ 23,],box[ 24,], box[ 25,],box[ 26,],box[ 27,],box[ 28,],box[ 29,],box[ 30,],box[ 31,],
      box[ 32,],box[ 33,],box[ 34,],box[ 35,],box[ 36,],box[ 37,],box[ 38,],box[ 39,],box[ 40,]
    )
  
  plot_data1 <-
    data.frame(
      box1[ 1,], box1[ 2,],box1[ 3,],box1[ 4,],box1[ 5,],box1[ 6,],box1[ 7,], box1[ 8,],box1[ 9,],box1[ 10,],box1[ 11,],
      box1[ 12,],box1[ 13,],box1[ 14,],box1[ 15,],box1[ 16,],box1[ 17,], box1[ 18,],box1[ 19,],box1[ 20,],box1[ 21,],
      box1[ 22,],box1[ 23,],box1[ 24,], box1[ 25,],box1[ 26,],box1[ 27,],box1[ 28,],box1[ 29,],box1[ 30,],box1[ 31,],
      box1[ 32,],box1[ 33,],box1[ 34,],box1[ 35,],box1[ 36,],box1[ 37,],box1[ 38,],box1[ 39,],box1[ 40,]
    )
  
  plot_data2 <-
    data.frame(
      box2[ 1,], box2[ 2,],box2[ 3,],box2[ 4,],box2[ 5,],box2[ 6,],box2[ 7,], box2[ 8,],box2[ 9,],box2[ 10,],box2[ 11,],
      box2[ 12,],box2[ 13,],box2[ 14,],box2[ 15,],box2[ 16,],box2[ 17,], box2[ 18,],box2[ 19,],box2[ 20,],box2[ 21,],
      box2[ 22,],box2[ 23,],box2[ 24,], box2[ 25,],box2[ 26,],box2[ 27,],box2[ 28,],box2[ 29,],box2[ 30,],box2[ 31,],
      box2[ 32,],box2[ 33,],box2[ 34,],box2[ 35,],box2[ 36,],box2[ 37,],box2[ 38,],box2[ 39,],box2[ 40,]
    )
  
  colgr<-alpha('green',0.4)
  colbl<-alpha('blue',0.5)
  dev.off()
  pdf(file.path(dir_out, paste("serie_temporal_", caso,"_spi6_",ms,"_",est, ".pdf",sep = "")),
      width = 8, height = 4.5)
  
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  
  boxplotperc(
    na.omit(plot_data),
    outline = FALSE,
    las = 2,
    ylim = c(-3.2, 3.2),
    col='yellow',
    border='orange',
    names = fecha,
    xlab="",
    ylab='',
    cex.main = 1.5,
    cex.lab = 1.3,at=c(1,3,5,7,9,
                       11,13,15,17,19,
                       21,23,25,27,29,
                       31,33,35,37,39,
                       41,43,45,47,49,
                       51,53,55,57,59,
                       61,63,65,67,69,
                       71,73,75,77,79)
    
  )
  
  par(new=TRUE)
  boxplotperc(
    na.omit(plot_data1),
    outline = FALSE,
    las = 2,
    ylim = c(-3.2, 3.2),
    col=colbl,
    border="blue",
    names = fecha,
    xlab="",
    ylab='',
    cex.main = 1.5,
    cex.lab = 1.3,at=c(1,3,5,7,9,
                       11,13,15,17,19,
                       21,23,25,27,29,
                       31,33,35,37,39,
                       41,43,45,47,49,
                       51,53,55,57,59,
                       61,63,65,67,69,
                       71,73,75,77,79)
  )
  
  par(new=TRUE)
  boxplotperc(
    na.omit(plot_data2),
    outline = FALSE,
    las = 2,
    ylim = c(-3.2, 3.2),
    col=colgr,
    border='green',
    names = fecha,
    ylab='',
    xlab="",
    cex.main = 1.5,
    cex.lab = 1.3,at=c(1,3,5,7,9,
                       11,13,15,17,19,
                       21,23,25,27,29,
                       31,33,35,37,39,
                       41,43,45,47,49,
                       51,53,55,57,59,
                       61,63,65,67,69,
                       71,73,75,77,79)
  )
  abline(h = -0.8, lwd = 1)  
  abline(v = c(0,2,4,6,8,10,12,14,16,18,20,
               22,24,26,28,30,32,34,36,38,40,
               42,44,46,48,50,52,54,56,58,60,
               62,64,66,68,70,72,74,76,78,80), lwd = 1, col = "cornsilk2") 
  grid (NULL, NULL, lty = 6, col = "cornsilk2")
  
  legend("bottomleft",
         c("4DROP","S5","DROP"), fill=topo.colors(3, alpha=0.5), horiz=T, cex=0.9)
  
  
   dev.off()
}   
