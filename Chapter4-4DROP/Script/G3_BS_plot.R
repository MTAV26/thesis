
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

# Proyecci?n de Robinson y mapa base (robin)
data(coastlineWorld)
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

worldmap <- rworldmap::getMap(resolution = "coarse") # Load countries 
raster::crs(worldmap)
worldmap <- sp::spTransform(worldmap,crs) # Project to Robinson

lati <- c(-90, -45, 0, 45, 90)
long <- c(-180, -135,-90, -45, 0, 45, 90, 135, 180)
labs <- graticule::graticule_labels(lons = long, lats = lati, xline = -180, yline = 90, proj = crs) # labels for the graticules 
lines <- graticule::graticule(lons = long, lats = lati, proj = crs) # graticules 

# Mascara de Oceanos
shapename <- read_sf('C:/Users/Usuario/Dropbox/capas/ne_10m_ocean.shp')
ocean <- sf::st_transform(shapename , crs = crs) # cha
light_blue <- "#E1F5FE"

# Paleta Correlation
zz<- range(0, 0.35) # s
brk_cor <- seq(0, 0.35, 0.05)
col_cor <- (colorRampPalette(rev(brewer.pal(8, "PiYG")))(7))


load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "inout.RData"))
lat2 = lat[which(lat > -60 & lat < 85)]


mes=c("01",
      "02","04",
      "05", "07",
      "08","10")

dir_out_ok= '~/Chapter4-4DROP/Data/'
dir_out2= '~/Chapter4-4DROP/Figures/'

for (me in 1:length(mes)) {
  ms = mes[me]
  try({
  
  if (ms == "11") {
    est = "DJF"
  } else if (ms == "01") {
    est = "DJF"
  } else if (ms == "02") {
    est = "MAM"
  } else if (ms == "04") {
    est = "MAM"
  } else if (ms == "05") {
    est = "JJA"
  } else if (ms == "07") {
    est = "JJA"
  } else if (ms == "08") {
    est = "SON"
  } else if (ms == "10") {
    est = "SON"
  } else {
    print('dataset not known')
  }

  load(paste(dir_out_ok,"BS_SEAS5_SPI6_",ms,"_",est,"_ok.RData", sep=""))
  bs = bs[, which(lat > -60 & lat < 85)]
  bs_1=bs
  
  rm(bs)
  load(paste(dir_out_ok,"BS_MIEMBROS_SEAS5_SPI6_",ms,"_",est,"_ENS_MEAN.RData", sep=""))
  bs_mean = bs_mean[, which(lat > -60 & lat < 85)]
  bs_2=bs_mean
  rm(bs_mean)

  load(paste(dir_out_ok,"BS_ESP_SPI6_",ms,"_",est,"_ok.RData", sep=""))
  bs = bs[, which(lat > -60 & lat < 85)]
  bs_3=bs
  rm(bs)
  load(paste(dir_out_ok,"BS_MIEMBROS_4DROP_SPI6_",ms,"_",est,"_ENS_MEAN.RData", sep=""))
  bs_mean = bs_mean[, which(lat > -60 & lat < 85)]
  bs_4=bs_mean
  rm(bs_mean)

  pdf(paste(dir_out2,"/BS_",ms,"_",est,".pdf",sep = ""),width=12, height=8.5)
  set.panel() 
  split.screen( rbind(c(0, 1,.1,1), c(0,1,0,.1)))
  
  split.screen(c(2,2), screen=1)-> ind
  zr<- range(0,1)

  screen( ind[1])
  par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
  mapPlot(coastlineWorld, col="lightgray", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, 
          main = "(a) S5 against DROP",
          cex.main = 1,
          line = -1.5, adj = 0.5)
  
  mapImage(lon, lat2, bs_1, col = col_cor, breaks = brk_cor)
  plot(ocean, col = light_blue, add = TRUE) 
  plot(lines, lty = 5, col = "grey", add = TRUE) # plots graticules 
  
  
  screen( ind[2])
  par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
  mapPlot(coastlineWorld, col="lightgray", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, 
          main = "(b) S5 against DROP members",
          cex.main = 1,
          line = -1.5, adj = 0.5)
  mapImage(lon, lat2, bs_2, col= col_cor, breaks = brk_cor)
  
  plot(ocean, col = light_blue, add = TRUE) 
  plot(lines, lty = 5, col = "grey", add = TRUE) # plots graticules 
  
  screen( ind[3])
  par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
  mapPlot(coastlineWorld, col="lightgray", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, 
          main = "(c) 4DROP against DROP",
          cex.main = 1,
          line = -1.5, adj = 0.5)
  
  mapImage(lon, lat2, bs_3, col= col_cor, breaks = brk_cor)
  plot(ocean, col = light_blue, add = TRUE) 
  plot(lines, lty = 5, col = "grey", add = TRUE) # plots graticules 
  
  
  screen( ind[4])
  par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
  mapPlot(coastlineWorld, col="lightgray", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, 
          main = "(d) 4DROP against DROP members", 
          cex.main = 1,
          line = -1.5, adj = 0.5)
  mapImage(lon, lat2, bs_4, col= col_cor, breaks = brk_cor)
  
  plot(ocean, col = light_blue, add = TRUE) 
  plot(lines, lty = 5, col = "grey", add = TRUE) # plots graticules 
  
  
  screen( 2)
  image.plot(zlim=zz,
             legend.only=TRUE, 
             smallplot=c(.25,.75, .6,.9),
             col=col_cor, 
             breaks = brk_cor,
             horizontal = TRUE)
  close.screen( all=TRUE)
  }, silent = TRUE) # envuelvo el error de old.par en el bucle
  dev.off()
}

dev.off()
