rm(list = ls())
graphics.off()
gc()
data(wrld_simpl)

#library(ncdf)
library(sp)
library(maptools) # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(fields)
library(s2dverification)
library(maps)
library(pracma)
library(verification)
library(psych)
library(sf)

library(oce)
library(raster)
library(ggplot2)

library(terra)
library(raster)
# install.packages("graticule")
# library(graticule)
library(rgdal)
library(rworldmap)
#install.packages("rnaturalearth")
library(rnaturalearth)


data(coastlineWorld)
data(wrld_simpl)

dir_drop = 'C:/Users/Usuario/Dropbox/4DROP/DROP/'
# dir_drop = 'C:/Users/Usuario/Dropbox/4EUROPE/'
dir_4drop = 'C:/Users/Usuario/Dropbox/4DROP/events/'
# dir_out= 'C:/Users/Usuario/Dropbox/4EUROPE/'


time_scale = c(6)
mes=c("07")
est="JJA"
sc=6
anni = 1981:2020
mesi = rep(1:12, length(anni))
mesi_8 = which(mesi == 08)

load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
##########################################
#Mascara
##########################################
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

# 

####################################################
# Mascara de Oceanos
####################################################
shapename <- read_sf('C:/Users/Usuario/Dropbox/capas/ne_10m_ocean.shp')
ocean <- sf::st_transform(shapename , crs = crs) # cha
rm(shapename)
light_blue <- "#E1F5FE"

load(file.path(paste(dir_4drop, 'SPI_DROP_SUDAMERICA.RData', sep = "")))
plot1=obs_drought_ens
rm(obs_drought_ens)
load(file.path(paste(dir_4drop, 'SPI_4DROP_SUDAMERICA.RData', sep = "")))
plot2=obs_drought_ens
rm(obs_drought_ens)
load(file.path(paste(dir_4drop, 'SPI_S5_SUDAMERICA.RData', sep = "")))
plot3=obs_drought_ens
rm(obs_drought_ens)


load(file.path(paste(dir_4drop, 'SPI_DROP_conditions_SUDAMERICA.RData', sep = "")))
plot4=obs_drought
rm(obs_drought)
load(file.path(paste(dir_4drop, 'SPI_4DROP_conditions_SUDAMERICA.RData', sep = "")))
plot5=obs_drought
rm(obs_drought)
load(file.path(paste(dir_4drop, 'SPI_S5_conditions_SUDAMERICA.RData', sep = "")))
plot6=obs_drought
rm(obs_drought)

load(file.path(paste(dir_4drop, 'SPI_DROP_warning_levels_SUDAMERICA.RData', sep = "")))
plot7=wl
rm(wl)
load(file.path(paste(dir_4drop, 'SPI_4DROP_warning_levels_SUDAMERICA.RData', sep = "")))
plot8=wl
rm(wl)
load(file.path(paste(dir_4drop, 'SPI_S5_warning_levels_SUDAMERICA.RData', sep = "")))
plot9=wl
rm(wl)

load(file.path(paste(dir_4drop, 'SPI_DROP_probability_SUDAMERICA.RData', sep = "")))
plot10=prob
rm(prob)
load(file.path(paste(dir_4drop, 'SPI_4DROP_probability_SUDAMERICA.RData', sep = "")))
plot11=prob
rm(prob)
load(file.path(paste(dir_4drop, 'SPI_S5_probability_SUDAMERICA.RData', sep = "")))
plot12=prob
rm(prob)




brk2 = c(0:12)
col_mean <- (colorRampPalette(brewer.pal(13, "BrBG"))(12))


brk_cond <- seq(-1, 5, length.out = 7)
pal.1 = colorRampPalette(c("yellow", "red", "black"), space = "rgb")
col_cond = pal.1(length(brk_cond) - 1)
col_cond[1] = "#C0C0C0"

cols_tl=c('white','#FFFF00','#FFA500','#FF0000')
brk_tl <-c(1:5)

brk_prob <- seq(0, 1, length.out = 6)
col_prob <-
  (colorRampPalette(brewer.pal(length(brk_prob), "YlOrBr"))(length(brk_prob) -
                                                              1))
col_prob[1] = "#C0C0C0"




lon1 = -85
lon2 = -30
lat1 = -65
lat2 = 15
# anno_case = 29 #2009


## select case study
ilon = which(lon > lon1 & lon < lon2)
ilat = which(lat > lat1 & lat < lat2)











close.screen( all=TRUE)
dev.off()
pdf(paste(dir_4drop,"/CASO_DE_ESTUDIO_SSA_07_JJA.pdf",sep = ""),width=11, height=12)
# set.panel() 
# split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
split.screen( rbind(c(0, .9,0,1), c(.9,1,0,1)))

# split.screen( rbind(c(0, .9,0.3,1), c(.9,1,0,0.3)))


split.screen(c(4,3), screen=1)-> ind
# zr<- range(-1,1)
screen( ind[1])
par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
par(mar=rep(0.6, 4))

mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),drawBox = TRUE, main = "DROP", cex.main = 0.8,
        line = 0.15, adj = 0.5)
mapImage(lon[ilon], lat[ilat], plot1, col= col_mean, breaks = brk2)
plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)
# mapScalebar(x = "topright", y = NULL, length = 1000, lwd = 1.5 * par("lwd"), col = "black")
screen( ind[2])
par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
par(mar=rep(0.6, 4))

mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),drawBox = TRUE, main = "4DROP", cex.main = 0.8,
        line = 0.15, adj = 0.5) 
mapImage(lon[ilon], lat[ilat], plot2, col= col_mean, breaks = brk2)
plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)

screen( ind[3])
par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
par(mar=rep(0.6, 4))

mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),drawBox = TRUE, main = "S5", cex.main = 0.8,
        line = 0.15, adj = 0.5)
mapImage(lon[ilon], lat[ilat], plot3, col= col_mean, breaks = brk2)
plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)






screen( ind[4])
par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
par(mar=rep(0.6, 4))

mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),drawBox = TRUE, main = "", cex.main = 0.8,
        line = 0.15, adj = 0.5)
mapImage(lon[ilon], lat[ilat],  plot4, col= col_cond, breaks = brk_cond)
plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)
# mapScalebar(x = "topright", y = NULL, length = 1000, col = "black")

screen( ind[5])
par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
par(mar=rep(0.6, 4))

mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),drawBox = TRUE, main = "", cex.main = 0.8,
        line = 0.15, adj = 0.5)
mapImage(lon[ilon], lat[ilat],  plot5, col= col_cond, breaks = brk_cond)
plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)
# mapScalebar(x = "topright", y = NULL, length = 1000, col = "black")

screen( ind[6])
par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
par(mar=rep(0.6, 4))

mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),drawBox = TRUE, main = "", cex.main = 0.8,
        line = 0.2, adj = 0.5)
mapImage(lon[ilon], lat[ilat],  plot6, col= col_cond, breaks = brk_cond)
plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)









screen( ind[7])
par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
par(mar=rep(0.6, 4))

mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),drawBox = TRUE, main = "", cex.main = 0.8,
        line = 0.15, adj = 0.5)
mapImage(lon[ilon], lat[ilat],plot7, col= cols_tl,  zlim=c(1,4))
plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)
# mapScalebar(x = "topright", y = NULL, length = 1000, col = "black")

screen( ind[8])
par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
par(mar=rep(0.6, 4))

mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),drawBox = TRUE, main = "", cex.main = 0.8,
        line = 0.15, adj = 0.5)
mapImage(lon[ilon], lat[ilat],plot8, col= cols_tl,  zlim=c(1,4))
plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)
# mapScalebar(x = "topright", y = NULL, length = 1000, col = "black")

screen( ind[9])
par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
par(mar=rep(0.6, 4))

mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),drawBox = TRUE, main = "", cex.main = 0.8,
        line = 0.15, adj = 0.5)
mapImage(lon[ilon], lat[ilat],plot9, col= cols_tl, zlim=c(1,4))
plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)









screen( ind[10])
par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
par(mar=rep(0.6, 4))

mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),drawBox = TRUE, main = "", cex.main = 0.8,
        line = 0.2, adj = 0.5)
mapImage(lon[ilon], lat[ilat],  plot10, col= col_prob, breaks = brk_prob)
plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)
# mapScalebar(x = "topright", y = NULL, length = 1000, col = "black")

screen( ind[11])
par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
par(mar=rep(0.6, 4))

mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),drawBox = TRUE, main = "", cex.main = 0.8,
        line = 0.2, adj = 0.5)
mapImage(lon[ilon], lat[ilat],plot11, col= col_prob, breaks = brk_prob)
plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)

screen( ind[12])
par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
par(mar=rep(0.6, 4))

mapPlot(coastlineWorld, col="lightgray",projection="+proj=robin",longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),drawBox = TRUE, main = "", cex.main = 0.8,
        line = 0.2, adj = 0.5)
mapImage(lon[ilon], lat[ilat],  plot12 , col= col_prob, breaks = brk_prob)
plot(ocean,longitudelim=c(-85,-30),latitudelim=c(-60, 10),col =light_blue,add = TRUE)
mapGrid(dlongitude = 15, dlatitude = 15, lty = 10,lwd = .5)

# zz<- range(0,1, 0.2) # solo lo uso para plot luego lo cambio a mano

# paleta<-c("gray", "#FFE4C4", "#E6F3E3", "#B7E2B1", "#6AB86F", "#227E3B", "#00441B")
# colns<-c("#FFE4C4")
# c2<- range(1:12)
# brk2 <- c(-2, -1.6, -0.8, -0.5, 0, 0.5, 0.8, 1.6, 2)
# dev.off()
split.screen(c(4,1), screen=2)-> ind
screen( ind[1])
# screen( ind)
image.plot(zlim=c(1:12),
           legend.only=TRUE, 
           smallplot=c(.05,.2, .05,.95),
           # smallplot=c(.25,.75, .8,1),
           col=col_mean,
           breaks = brk2,
           horizontal = FALSE,
           axis.args = list(at = brk2,
                            labels=c("",
                                     "",
                                     "",
                                     "",
                                     "",
                                     "",
                                     "",
                                     "",
                                     "",
                                     "",
                                     "",
                                     "",
                                     ""))
           
           )

screen( ind[2])
zc<- range(0,5, 1)
image.plot(zlim=zc,
           legend.only=TRUE, 
           smallplot=c(.05,.2, .05,.95),
           # smallplot=c(.25,.75, .8,1),
           col=col_cond,
           breaks=brk_cond,
           horizontal = FALSE,
           axis.args = list(at = brk_cond,
                            labels=c("",
                                     "",
                                     "",
                                     "",
                                     "",
                                     "",
                                     ""))
           )

screen( ind[3])
# screen( ind)
image.plot(
  zlim=c(1, 4),
  legend.only=TRUE,
  smallplot=c(.05,.2, .05,.95),
  col=cols_tl,
  horizontal = FALSE,
  axis.args = list(at = brk_tl,
                   labels=c("",
                            "",
                            "",
                            "",
                            ""))
)

  
  

screen( ind[4])
# screen( ind)
# zz<- range(0.2,1, 0.2)
# dev.off()
image.plot( zlim=c(0, 1),
  legend.only=TRUE,
  smallplot=c(.05,.2, .05,.95),
  col=col_prob,
  breaks = brk_prob,
  horizontal = FALSE
)


close.screen( all=TRUE)
dev.off()




# mapScalebar(x = "topright", y = NULL, length = 1000, col = "black")







cc














































mapPlot(coastlineWorld, 
        col="lightgray",
        projection="+proj=robin",
        longitudelim=c(-85,-30),
        latitudelim=c(-60, 10),
        # breaks = brk_cor,
        drawBox = TRUE, 
        main = " (a) DROP", 
        cex.main = 0.8,
        line = 1, adj = 0.5)

# mar<-(alpha("lightskyblue", 0.1))

# mapImage(lon, lat2, pvalue_adj2)
mapImage(lon[ilon], lat[ilat],  obs_drought, col= col, breaks = brk)
plot(ocean,
     longitudelim=c(-85,-30),
     latitudelim=c(-60, 10),
     col ="light blue",
     # col = rgb(0,0,8, 0.3),
     add = TRUE)

mapGrid(
  dlongitude = 15, 
  dlatitude = 15, 
  lty = 10,
  lwd = .5)
## add text on map
mapScalebar(x = "topleft", y = NULL, length = 1000, col = "black")


local(envir=.PBSmapEnv,expr={
  data(nepacLL,envir=.PBSmapEnv)
  par(mfrow=c(1,1),mar=c(3,4,0.5,0.5))
  plotMap(nepacLL, xlim=c(-134.5,-124.5), ylim=c(48,55), plt=NULL,
          col="lightyellow", cex.axis=1.2, cex.lab=1.5)
  addCompass(-132, 49.5, rot=-12, cex=1.5)
})

# plot(ocean, 
#      longitudelim=c(-85,-30),
#      latitudelim=c(-60, 10),
#      col = ggplot2::alpha("slategray1", 0.3), add = TRUE) 




mapPlot(coastlineWorld, 
        col=alpha,
        projection="+proj=robin",
        longitudelim=c(-85,-30),
        latitudelim=c(-60, 10), add=TRUE)




plot(ocean,
     col = "blue",
     # col = rgb(0.1,1,1, 0.3),
     add = TRUE) 




quant_ras = raster::stackApply( obs_drought, 
                                fun=quant_fun,
                                indices=c(1,1,2,2,2)) 
names(quant_ras) = c("Output of rasters 1 to 2", "Output of rasters 3 to 5")
# Two layers are formed, one for each group of indices

# Lets plot the two output rasters
plot(quant_ras[[1:2]], # Layers to plot
     asp=NA,           # Aspect ratio. NA= No white space
     colNA="gray95",   # Background color
     nr=2) 
# plot(coastlineWorldMedium , clongitude=c(-85,-30), clatitude=c(-60, 10), span=600, col="blue")
# plot(lon, lat, z, colormap=cm)

# mapPlot(coastlineWorld, projection="+proj=robin", grid=FALSE, col="lightgray")
# mapImage(lon, lat, z, colormap=cm)
# mapImage(lon[ilon], lat[ilat], pvalue1, col= colns)
# plot(worldmap$continent, xaxt="n", yaxt= "n",add=T)
# plot(lines, lty = 5, col = "grey", add = TRUE) # plots graticules 
plot(ocean, col = ggplot2::alpha("slategray1", 0.3), add = TRUE) 
# points(pvalue2,pch = 21,cex = 2,col = 'red')
# text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
# text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8) # plots latitude labels
# 
dev.off()


install.packages("ocedata")
data(coastlineWorldMedium, package="ocedata")

plot(coastlineWorldMedium, clongitude=-63, clatitude=45.5, span=600, col="blue")
















lonlim = c(45.0, 55)
latlim = c(-35,2)
Zlim = c(-6000,0)

par(mar=c(2, 2, 1, 1))
install.packages("ocedata")
require(ocedata)
data("coastlineWorldFine")

## draw scalebar
drawPalette(zlim = Zlim, 
            col=oce.colorsGebco(120), 
            at = seq(-6000,0,1000), 
            pos = 4, 
            drawTriangles = FALSE)
## make a base
mapPlot(coastlineWorld, 
        projection="+proj=mill",
        col="lightgray", 
        longitudelim=lonlim, 
        latitudelim=latlim, 
        clip = TRUE)
## ## overlay gridded sst layer
mapImage(longitude = Lon,
         latitude = Lat, 
         z = etopo.mat, 
         zlim = Zlim,
         filledContour = TRUE, 
         # zclip = TRUE,
         col = oceColorsGebco(120))
## add polygon
mapPolygon(coastlineWorldFine, 
           col="lightgray")
