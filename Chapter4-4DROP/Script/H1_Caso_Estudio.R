
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


#####################################################################
# Dirección de los datos
#####################################################################
source("'~/Chapter4-4DROP/script/Common/ColorBarM.R")
dir_drop = '~/Chapter4-4DROP/Data/'
dir_ensD= '~/Chapter4-4DROP/Figures/'

#####################################################################
#Mascara
#####################################################################
load(file.path(dir_drop, "inout.RData"))

data(coastlineWorld)
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
worldmap <- rworldmap::getMap(resolution = "coarse") # Load countries 
raster::crs(worldmap)
worldmap <- sp::spTransform(worldmap,crs) # Project to Robinson

lati <- c(-90, -45, 0, 45, 90)
long <- c(-180, -135,-90, -45, 0, 45, 90, 135, 180)
labs <- graticule::graticule_labels(lons = long, lats = lati, xline = -180, yline = 90, proj = crs) # labels for the graticules
lines <- graticule::graticule(lons = long, lats = lati, proj = crs) # graticules

#####################################################################
# Mascara de Oceanos
#####################################################################
shapename <- read_sf('~/Chapter3-4SPAIN/Data/ne_10m_ocean.shp')
ocean <- sf::st_transform(shapename , crs = crs) # cha
rm(shapename)
light_blue <- "#E1F5FE"

#####################################################################
# coindiciones del estudio
#####################################################################

load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
ni = length(lon)
nj = length(lat)
## select case study

anni = 1981:2020
mesi = rep(1:12, length(anni))
mesi_8 = which(mesi == 8)

ms=c("08")
est=c('JJA')
ani=c(1981:2020)
mon=c(1:40)
df<-data.frame(ani, mon)
df
casi = c('SUDAMERICA')

mesi=1:40
# anno_for = which(mesi== anno_case)

datos=c("DROP", "4DROP", "S5")


for (icaso in 1:length(casi)) {
  caso = casi[icaso]
  if (caso == "SUDAMERICA") {
    lon1 = -85
    lon2 = -30
    lat1 = -65
    lat2 = 15
    anno_case = 29 #2009

  } else if (caso == "EUROPA") {
    lon1 = -15
    lon2 = 60
    lat1 = 30
    lat2 = 75
    anno_case = 38 #2017
    
  } else {
    print('dataset not known')
  }
  
  
  ## select case study
  ilon = which(lon > lon1 & lon < lon2)
  ilat = which(lat > lat1 & lat < lat2)
  anno_for = which(mesi== anno_case)
  
  
  for (idatos in 1:length(datos)) {
    dat = datos[idatos]
  ## load dati
  if(dat == "DROP"){
    load(file.path( dir_ensD, paste("DROP_SPI6_ENS.RData", sep = "") ))
    
    obs = spi[,,mesi_8]
  } else if (dat == "4DROP"){
    load(file.path( dir_ensD, paste("4DROP_SPI6_07_JJA_ENS.RData", sep = "") ))
    obs = spi6pred
    
  } else if (dat == "S5"){
    load(file.path( dir_ensD, paste("S5_SPI6_07_JJA_ENS.RData", sep = "") ))
    obs = spi6pred
  }
  
  
  sid = obs[ilon, ilat , anno_for]
  lonsid = lon[ilon]
  latsid = lat[ilat]
  idx = which(sid[, ] == min(sid, na.rm = TRUE), arr.ind = T)
  lonsid[idx[1, 1]]
  latsid[idx[1, 2]]
  sid[idx[1, 1], idx[1, 2]] = -5
  spei6obs = obs[ilon, ilat, anno_for]
  
  
  ##########################################################################
  ## plot obs spi
  ##########################################################################
  
  brk2 = c(0:12)
  col_mean <- (colorRampPalette(brewer.pal(13, "BrBG"))(12))
  
  obs_drought_ens = spei6obs*NA
  obs_drought_ens[(spei6obs >= 2)] = 12
  obs_drought_ens[(spei6obs < 2 ) & (spei6obs >= 1.6)] = 11
  obs_drought_ens[(spei6obs < 1.6) & (spei6obs >= 1.3)] = 10
  obs_drought_ens[(spei6obs < 1.3) & (spei6obs >= 0.8)] = 9
  obs_drought_ens[(spei6obs < 0.8) & (spei6obs >= 0.5)] = 8
  obs_drought_ens[(spei6obs < 0.5) & (spei6obs >= 0)] = 7
  # obs_drought[(spei6obs == 0)] = 7
  obs_drought_ens[(spei6obs < 0) & (spei6obs > -0.5)] = 6
  obs_drought_ens[(spei6obs <= -0.5) & (spei6obs > -0.8)] = 5
  obs_drought_ens[(spei6obs <= -0.8) & (spei6obs > -1.3)] = 4
  obs_drought_ens[(spei6obs <= -1.3) & (spei6obs > -1.6)] = 3
  obs_drought_ens[(spei6obs <= -1.6) & (spei6obs > -2)] = 2
  obs_drought_ens[(spei6obs <= -2)] = 1
  
  # pdf(paste(dir_ensD,"/SPI_DROP_07_JJA.pdf",sep = ""),width=9, height=11)
  mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
          latitudelim=c(-60, 10),drawBox = TRUE, main = paste(dat), cex.main = 2,
          line = 1, adj = 0.5)
  mapImage(lon[ilon], lat[ilat], obs_drought_ens, col= col_mean, breaks = brk2)
  # plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
  # mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)
  #dev.off()
  save(obs_drought_ens, file = file.path(dir_ensD, paste("SPI_",dat,"_",caso,".RData", sep = "") ))
  
  ##########################################################################
  ## plot conditions SPI
  ##########################################################################
  
  obs_drought = spei6obs*NA
  obs_drought[(spei6obs > -0.5)] = 0
  obs_drought[(spei6obs <= -0.5) & (spei6obs > -0.8)] = 1
  obs_drought[(spei6obs <= -0.8) & (spei6obs > -1.3)] = 2
  obs_drought[(spei6obs <= -1.3) & (spei6obs > -1.6)] = 3
  obs_drought[(spei6obs <= -1.6) & (spei6obs > -2)] = 4
  obs_drought[(spei6obs <= -2)] = 5
  
  brk_cond <- seq(-1, 5, length.out = 7)
  pal.1 = colorRampPalette(c("yellow", "red", "black"), space = "rgb")
  col_cond = pal.1(length(brk_cond) - 1)
  col_cond[1] = "#C0C0C0"
  
  # pdf(paste(dir_ensD,"/CONDITION_DROP_07_JJA.pdf",sep = ""),width=9, height=11)
  mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
           latitudelim=c(-60, 10),drawBox = TRUE, main = paste(dat, " Drought condition", sep=""), cex.main = 2,
           line = 1, adj = 0.5)
   mapImage(lon[ilon], lat[ilat],  obs_drought, col= col_cond, breaks = brk_cond)
  # plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue, add = TRUE)
  # mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)
  # dev.off()
  save(obs_drought, file = file.path(dir_ensD, paste("SPI_",dat,"_conditions_",caso,".RData", sep = "") ))


  ##########################################################################
  ## plot WARNING LEVELS DROP
  ##########################################################################
  if(dat == "DROP"){
    load(file.path( dir_ensD, paste("DROP_TRAF_LIG_6_DROP.RData", sep = "") ))
    aux = spi_tl[ilon, ilat,mesi_8]
  } else if (dat == "4DROP"){
    load(file.path( dir_ensD, paste("4DROP_TRAF_LIG_6_07_JJA_DROP.RData", sep = "") ))
    aux = spi6pred_tl[ilon, ilat,]
    
  } else if (dat == "S5"){
    load(file.path( dir_ensD, paste("S5_TRAF_LIG_6_07_JJA_DROP.RData", sep = "") ))
    aux = spi6pred_tl[ilon, ilat,]
  }

  aux=aux[,,anno_for]
  wl=aux
  rm(aux)
  
  cols_tl=c('#C0C0C0','#FFFF00','#FFA500','#FF0000')
  brk_tl <-c(1:5)
  # # pdf(paste(dir_ensD,"/WL_DROP_07_JJA.pdf",sep = ""),width=9, height=11)
   mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
           latitudelim=c(-60, 10),drawBox = TRUE, main = paste(dat, " Warning levels", sep=""), cex.main = 2,
          line = 1, adj = 0.5)
   mapImage(lon[ilon], lat[ilat],wl, col = cols_tl, zlim=c(1,4))
  # plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
  # mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)
  #dev.off()
  save(wl, file = file.path(dir_ensD, paste("SPI_",dat,"_warning_levels_",caso,".RData", sep = "") ))
#   }
# }
#   
  ########################################################################## 
  ## Plot Probability Moderate Drought
  ##########################################################################
  
  if(dat == "DROP"){
    load(file.path( dir_ensD, paste("DROP_PROB6_DROP.RData", sep = "") ))
    aux = spi_prob[ilon, ilat,mesi_8]
  } else if (dat == "4DROP"){
    load(file.path( dir_ensD, paste("4DROP_PROB6_07_JJA_DROP.RData", sep = "") ))
    aux = spi6pred_prob[ilon, ilat,]
    
  } else if (dat == "S5"){
    load(file.path( dir_ensD, paste("S5_PROB6_07_JJA_DROP.RData", sep = "") ))
    aux = spi6pred_prob[ilon, ilat,]
  }

  aux=aux[,,anno_for]
  prob = aux
  rm(aux)
  
  brk_prob <- seq(0, 1, length.out = 6)
  col_prob <-
    (colorRampPalette(brewer.pal(length(brk_prob), "YlOrBr"))(length(brk_prob) -
                                                                1))
  col_prob[1] = "#C0C0C0"
  
  # pdf(paste(dir_ensD,"/WL_DROP_07_JJA.pdf",sep = ""),width=9, height=11)
  mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
           latitudelim=c(-60, 10),drawBox = TRUE, main = paste(dat, " Probability", sep=""), cex.main = 2,
           line = 1, adj = 0.5)
   mapImage(lon[ilon], lat[ilat],  prob, col= col_prob, breaks = brk_prob)
  # plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
  # mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)
  #dev.off()
   save(prob, file = file.path(dir_ensD, paste("SPI_",dat,"_probability_",caso,".RData", sep = "") ))
   
  }
}
