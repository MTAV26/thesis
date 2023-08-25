
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



dir_out2= "C:/Users/Usuario/OneDrive/Escritorio/"

# Definición de directorios, carga de coordenadas y mascara
dir_drop = '~/Chapter4-4DROP/Data/'
dir_out2 = '~/Chapter4-4DROP/Figures/'
load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "inout.RData"))
lat2 = lat[which(lat > -60 & lat < 85)]


load(file.path(dir_drop, "COR_S5_spi6_11_DJF_ENS_original.RData"))
cor_1=corre2
rm(corre2)
load(file.path(dir_drop, "pvalue_S5_spi6_11_DJF_ENS_original.RData"))
pval_1=pvalue_adj2
rm(pvalue_adj2)
load(file.path(dir_drop, "COR_ESP_spi6_11_DJF_ENS_original.RData"))
cor_2=corre2
rm(corre2)
load(file.path(dir_drop, "pvalue_ESP_spi6_11_DJF_ENS_original.RData"))
pval_2=pvalue_adj2
rm(pvalue_adj2)

load(file.path(dir_drop, "COR_S5_spi6_11_DJF_MSWEP_original.RData"))
cor_3=corre2
rm(corre2)
load(file.path(dir_drop, "PVALUE_S5_spi6_11_DJF_MSWEP_original.RData"))
pval_3=pvalue_adj2
rm(pvalue_adj2)
load(file.path(dir_drop, "COR_ESP_spi6_11_DJF_MSWEP_original.RData"))
cor_4=corre2
rm(corre2)
load(file.path(dir_drop, "PVALUE_ESP_spi6_11_DJF_MSWEP_original.RData"))
pval_4=pvalue_adj2
rm(pvalue_adj2)

load(file.path(dir_drop, "COR_S5_spi6_11_DJF_MSWEP_original.RData"))
cor_5=corre2
rm(corre2)
load(file.path(dir_drop, "pvalue_S5_spi6_11_DJF_MSWEP_original.RData"))
pval_5=pvalue_adj2
rm(pvalue_adj2)
load(file.path(dir_drop, "COR_ESP_spi6_11_DJF_MSWEP_original.RData"))
cor_6=corre2
rm(corre2)
load(file.path(dir_drop, "pvalue_ESP_spi6_11_DJF_MSWEP_original.RData"))
pval_6=pvalue_adj2
rm(pvalue_adj2)


####################################################
# Mascara de los valores no significativos 
####################################################

pvalue1 =pval_1
pvalue1[pvalue1 <= 0.05] = NA
pvalue1[pvalue1 >= 0.05] = -1

pvalue2 =pval_2
pvalue2[pvalue2 <= 0.05] = NA
pvalue2[pvalue2 >= 0.05] = -1

pvalue3 =pval_3
pvalue3[pvalue3 <= 0.05] = NA
pvalue3[pvalue3 >= 0.05] = -1

pvalue4 =pval_4
pvalue4[pvalue4 <= 0.05] = NA
pvalue4[pvalue4 >= 0.05] = -1

pvalue5 =pval_5
pvalue5[pvalue5 <= 0.05] = NA
pvalue5[pvalue5 >= 0.05] = -1

pvalue6 =pval_6
pvalue6[pvalue6 <= 0.05] = NA
pvalue6[pvalue6 >= 0.05] = -1


####################################################
# Proyecci?n de Robinson y mapa base (robin)
####################################################
data(coastlineWorld)
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

worldmap <- rworldmap::getMap(resolution = "coarse") # Load countries 
raster::crs(worldmap)
worldmap <- sp::spTransform(worldmap,crs) # Project to Robinson

lat <- c(-90, -45, 0, 45, 90)
long <- c(-180, -135,-90, -45, 0, 45, 90, 135, 180)
labs <- graticule::graticule_labels(lons = long, lats = lat, xline = -180, yline = 90, proj = crs) # labels for the graticules 
lines <- graticule::graticule(lons = long, lats = lat, proj = crs) # graticules 

####################################################
# Mascara de Oceanos
####################################################
# Cargar y transformar máscaras geográficas
ocean <- sf::st_transform(read_sf('~/Chapter3-4SPAIN/Data/mascaras/ne_10m_ocean.shp'), crs = crs)


####################################################
# Paleta Correlation
####################################################

brk_cor <- seq(0, 1, 0.2)
col_cor <- c("#E6F3E3", "#B7E2B1", "#6AB86F", "#227E3B", "#00441B")

paleta<-c("gray", "#FFE4C4", "#E6F3E3", "#B7E2B1", "#6AB86F", "#227E3B", "#00441B")
colns<-c("#FFE4C4")


zz<- range(-0.1,1.1)
zr<- range(-1,1)

####################################################
# PLOT
####################################################
set.panel() 
split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
split.screen(c(3,2), screen=1)-> ind
screen( ind[1])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
mapPlot(coastlineWorld, col="lightgray", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, 
        main = "S5 against DROP", 
        cex.main = 0.8,
        line = -1, adj = 0.5)

mapImage(lon, lat2, cor_1, col= col_cor, breaks = brk_cor)
mapImage(lon, lat2, pvalue1, col= colns)
plot(ocean, col = ggplot2::alpha("slategray1", 0.3), add = TRUE) 
# plot(worldmap$continent, xaxt="n", yaxt= "n",add=T)
# plot(lines, lty = 5, col = "grey", add = TRUE) # plots graticules 
text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8) # plots latitude labels


screen( ind[2])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
mapPlot(coastlineWorld, col="lightgray", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, 
        main = "4DROP against DROP", 
        cex.main = 0.8,
        line = -1, adj = 0.5)
mapImage(lon, lat2, cor_2, col= col_cor, breaks = brk_cor)
mapImage(lon, lat2, pvalue2, col= colns)
# plot(worldmap$continent, xaxt="n", yaxt= "n",add=T)
# plot(lines, lty = 5, col = "grey", add = TRUE) # plots graticules 
plot(ocean, col = ggplot2::alpha("slategray1", 0.3), add = TRUE) 
# points(pvalue2,pch = 21,cex = 2,col = 'red')
text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8) # plots latitude labels


screen( ind[3])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
mapPlot(coastlineWorld, col="lightgray", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, 
        main = "S5 against MSWEP observed", 
        cex.main = 0.8,
        line = -1, adj = 0.5)
mapImage(lon, lat2, cor_3, col= col_cor, breaks = brk_cor)
mapImage(lon, lat2, pvalue3, col= colns)
plot(ocean, col = ggplot2::alpha("slategray1", 0.3), add = TRUE) 
text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8) # plots latitude labels


screen( ind[4])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
mapPlot(coastlineWorld, col="lightgray", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, 
        main = "4DROP against MSWEP observed", 
        cex.main = 0.8,
        line = -1, adj = 0.5)
mapImage(lon, lat2, cor_4, col= col_cor, breaks = brk_cor)
mapImage(lon, lat2, pvalue4, col= colns)
plot(ocean, col = ggplot2::alpha("slategray1", 0.3), add = TRUE) 
text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8) # plots latitude labels


screen( ind[5])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
mapPlot(coastlineWorld, col="lightgray", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, 
        main = "MSWEP prediction (S5) against DROP", 
        cex.main = 0.8,
        line = -1, adj = 0.5)
mapImage(lon, lat2, cor_5, col= col_cor, breaks = brk_cor)
mapImage(lon, lat2, pvalue5, col= colns)
# plot(worldmap$continent, xaxt="n", yaxt= "n",add=T)
# plot(lines, lty = 5, col = "grey", add = TRUE) # plots graticules 
plot(ocean, col = ggplot2::alpha("slategray1", 0.3), add = TRUE) 
# points(pvalue2,pch = 21,cex = 2,col = 'red')
text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8)

screen( ind[6])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
mapPlot(coastlineWorld, col="lightgray", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, 
        main = "MSWEP prediction (ESP) against DROP", 
        cex.main = 0.8,
        line = -1, adj = 0.5)
mapImage(lon, lat2, cor_6, col= col_cor, breaks = brk_cor)
mapImage(lon, lat2, pvalue6, col= colns)
# plot(worldmap$continent, xaxt="n", yaxt= "n",add=T)
# plot(lines, lty = 5, col = "grey", add = TRUE) # plots graticules 
plot(ocean, col = ggplot2::alpha("slategray1", 0.3), add = TRUE) 
# points(pvalue2,pch = 21,cex = 2,col = 'red')
text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8) # plots latitude labels


screen( 2)
image.plot(zlim=zz,
           legend.only=TRUE, 
           smallplot=c(.25,.75, .8,1),
           col=paleta,
           horizontal = TRUE)

close.screen( all=TRUE)
dev.off()
