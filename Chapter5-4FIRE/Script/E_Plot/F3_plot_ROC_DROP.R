rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(fields)
library(maptools)
library(verification)
library(RColorBrewer)
source('C:/Users/Usuario/Dropbox/4FIRE/script/model_BA/image_mask.R')

data(wrld_simpl)


years = 2001:2020 #antes en el original: 2001:2019
num_ens = 25
FDR=1
where = 'miguel'

season = c("DJF", "MAM", "JJA", "SON")

if (where == 'mac') {
  dir_f51 = '/Users/marco/Documents/dati/fire_climate_data/climate_fire/datos/BA/FIRECCI511-BA/'
  dir_data = '~/Dropbox/estcena/scripts/miguel/4FIRE/data/'
  dir_out = '~/Dropbox/estcena/scripts/miguel/4FIRE/out/'
  dir_drop = '/Users/marco/Documents/dati/fire_climate_data/climate_fire/datos/drop_spi/'
  dir_s5 = '/Users/marco/Documents/dati/4FIRE/'
} else if (where == 'miguel') {
  
  dir_f51 = "C:/Users/Usuario/Dropbox/4FIRE/data/BA/"
  dir_drop ="C:/Users/Usuario/Dropbox/4FIRE/data/DROP/"
  dir_data="C:/Users/Usuario/Dropbox/4FIRE/data/model_obs_FDR/"
  dir_out="C:/Users/Usuario/Dropbox/4FIRE/data/model_obs_FDR/"
  dir_out1="C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs_FDR_mean1"
  dir_mask="C:/Users/Usuario/Dropbox/4FIRE/data/mask_season/"
  dir_pdf="C:/Users/Usuario/Dropbox/4FIRE/"
  
}  else if (where == 'onfire') {
  dir_f51 = '/diskonfire/CLIMATE_FIRE/datos/BA/FIRECCI511-BA/'
  dir_data = '~/Dropbox/estcena/scripts/miguel/4FIRE/data/'
  dir_out = '~/Dropbox/estcena/scripts/miguel/4FIRE/out/'
  dir_drop = '/diskonfire/CLIMATE_FIRE/datos/drop_spi/'
  dir_s5 = '/diskonfire/4FIRE/'
}


brk_prob <- seq(0.6, 1, length.out = 5)
col_prob <-
  (colorRampPalette(brewer.pal(length(brk_prob), "YlGn"))(length(brk_prob) -
                                                            1))

## load SPI
fname <- file.path(dir_drop, 'DROP_3_1981_2020.nc')
obs.nc <- nc_open(fname)
obs.nc$dim$lon$vals -> lon
obs.nc$dim$lat$vals -> lat
ilat = which(lat > -60 & lat < 75)
ilon = which(lon > -165 & lon < 180)


library(sp) # loads sp library too # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(maps)
library(oce)
library(ggplot2)
library(terra)
library(raster)
library(rgdal)
library(rworldmap)
library(rnaturalearth)
library(sf) 
##########################################################
#creo una mascara de todo el globo para tapar los paises
##########################################################
dir_oss="C:/Users/Usuario/Dropbox/4DROP/data"
fname <- file.path(dir_oss, 'MSWEP_1981_2021.nc')
obs.nc <- nc_open(fname)
obs <- ncvar_get(obs.nc, "precipitation")
obs[obs == "-9999.f"] <- NA
obs1 = apply(obs, c(1, 2), mean)
load(file.path(dir_drop, "inout.RData"))
obs_mask= obs1*inout
obs_mask[!is.na(obs_mask)]=1
image.plot(lon, lat, obs_mask)
dev.off()


##########################################################
#Let todas las mascaras
##########################################################

load(paste0(dir_mask, "mask_DJF.RData"))
mask_djf=mask
rm(mask)
load(paste0(dir_mask, "mask_MAM.RData"))
mask_mam=mask
rm(mask)
load(paste0(dir_mask, "mask_JJA.RData"))
mask_jja=mask
rm(mask)
load(paste0(dir_mask, "mask_SON.RData"))
mask_son=mask
rm(mask)

##########################################################
#Let todas estaciones de roca PRED BA OBS
##########################################################

load(paste0(dir_out1, "/roc_drop_DJF_FDR.RData"))
roca_djf=roca_drop
rm(roca_drop)
load(paste0(dir_out1, "/roc_drop_MAM_FDR.RData"))
roca_mam=roca_drop
rm(roca_drop)
load(paste0(dir_out1, "/roc_drop_JJA_FDR.RData"))
roca_jja=roca_drop
rm(roca_drop)
load(paste0(dir_out1, "/roc_drop_SON_FDR.RData"))
roca_son=roca_drop
rm(roca_drop)

load(paste0(dir_f51, "/BA_200101_202012_nat.RData"))

ni = dim(BA)[1]
nj = dim(BA)[2]


data(coastlineWorld)
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

worldmap <- rworldmap::getMap(resolution = "coarse") # Load countries 
raster::crs(worldmap)
worldmap <- sp::spTransform(worldmap,crs) # Project to Robinson

lati <- c(-90, -45, 0, 45, 90)
long <- c(-180, -135,-90, -45, 0, 45, 90, 135, 180)
labs <- graticule::graticule_labels(lons = long, lats = lati, xline = -180, yline = 90, proj = crs) # labels for the graticules 
lines <- graticule::graticule(lons = long, lats = lati, proj = crs) # graticules 

####################################################
# Mascara de Oceanos
####################################################
# ocean <- rnaturalearth::ne_download(scale = 10, type = 'ocean', category = 'physical', returnclass = "sf")
# ocean <- sf::st_transform(ocean, crs = crs) # changes the projection
shapename <- read_sf('C:/Users/Usuario/Dropbox/capas/ne_10m_ocean.shp')
ocean <- sf::st_transform(shapename , crs = crs) # cha
# ocean <- ocean[,summary(as.vector(mask))1]
# col <- (colorRampPalette(brewer.pal(12, "BrBG"))(11))

brk_cor <- seq(0.5, 1, length.out = 6)
col_cor <- c( "#E6F3E3", "#B7E2B1", "#6AB86F", "#227E3B", "#00441B")

# brk_prob <- seq(0.5, 1, length.out = 6)
# # col_prob <- c("#FFFFBF" ,"#D9EF8B",  "#A2D889", "#78C679","#227E3B")
# col_prob <- c("#E6F3E3", "#B7E2B1", "#6AB86F", "#227E3B", "#00441B")
# # colns<-c("#34495E")
# colns<-c("orange")

close.screen( all=TRUE)
dev.off()


pdf(paste(dir_pdf,"Figure2.pdf"), width=9, height=6.2)

set.panel() 
split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
# split.screen( rbind(c(0, .9,0,1), c(.9,1,0,1)))
split.screen(c(2,2), screen=1)-> ind
zr<- range(-1,1)
screen( ind[1])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 


load(paste0(dir_out1, "/preBAobs_drop_DJF_FDR.RData"))
PredBAobs_ens=(apply(predBAobs, c(1, 2, 3), mean, na.rm = TRUE))
spi3=PredBAobs_ens

mask_dataset = array(0, dim = c(ni, nj))
for (i in 1:ni) {
  for (j in 1:nj) {
    if (length(which(!is.na(spi3[i, j, ]))) && mask_djf[i, j ] == 1 ) {
      mask_dataset[i, j] = 1
    }
  }
}

best_roc_obs=roca_djf
best_roc_obs1= best_roc_obs*NA
best_roc_obs1[!is.na(best_roc_obs)]=1

tit <-
  paste('ROC area CLIBA DROP (DJF): 
Skilfull surface (', round(((sum (best_roc_obs1, na.rm = TRUE)/sum(mask_dataset, na.rm = TRUE))*100),0),'%); median (', round((median(best_roc_obs, na.rm = TRUE)),2),")", sep="")

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_prob, drawBox = FALSE, 
        main = paste(tit, sep=""), 
        cex.main = 0.8,line = -1.5, adj = 0.5)
mapImage(lon, lat, obs_mask, col= "white")
mask_dataset[ mask_dataset == 0] = NA
mapImage(lon, lat, mask_dataset, col= "orange")
mapImage(lon,lat, roca_djf, col= col_cor, breaks = brk_cor)

plot(ocean, col = "lightblue", add =TRUE) 
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 


screen( ind[2])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 

load(paste0(dir_out1, "/preBAobs_drop_MAM_FDR.RData"))
PredBAobs_ens=(apply(predBAobs, c(1, 2, 3), mean, na.rm = TRUE))
spi3=PredBAobs_ens

mask_dataset = array(0, dim = c(ni, nj))
for (i in 1:ni) {
  for (j in 1:nj) {
    if (length(which(!is.na(spi3[i, j, ]))) && mask_mam[i, j ] == 1 ) {
      mask_dataset[i, j] = 1
    }
  }
}

best_roc_obs=roca_mam
best_roc_obs1= best_roc_obs*NA
best_roc_obs1[!is.na(best_roc_obs)]=1

tit <-
  paste('ROC area CLIBA DROP (MAM): 
Skilfull surface (', round(((sum (best_roc_obs1, na.rm = TRUE)/sum(mask_dataset, na.rm = TRUE))*100),0),'%); median (', round((median(best_roc_obs, na.rm = TRUE)),2),")", sep="")

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_prob, drawBox = FALSE, 
        main = paste(tit, sep=""), 
        cex.main = 0.8,line = -1.5, adj = 0.5)
mapImage(lon, lat, obs_mask, col= "white")
mask_dataset[ mask_dataset == 0] = NA
mapImage(lon, lat, mask_dataset, col= "orange")
mapImage(lon,lat, roca_mam, col= col_cor, breaks = brk_cor)

plot(ocean, col = "lightblue", add =TRUE) 
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 



screen( ind[3])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 

load(paste0(dir_out1, "/preBAobs_drop_JJA_FDR.RData"))
PredBAobs_ens=(apply(predBAobs, c(1, 2, 3), mean, na.rm = TRUE))
spi3=PredBAobs_ens

mask_dataset = array(0, dim = c(ni, nj))
for (i in 1:ni) {
  for (j in 1:nj) {
    if (length(which(!is.na(spi3[i, j, ]))) && mask_jja[i, j ] == 1 ) {
      mask_dataset[i, j] = 1
    }
  }
}

best_roc_obs=roca_jja
best_roc_obs1= best_roc_obs*NA
best_roc_obs1[!is.na(best_roc_obs)]=1

tit <-
  paste('ROC area CLIBA DROP (JJA): 
Skilfull surface (', round(((sum (best_roc_obs1, na.rm = TRUE)/sum(mask_dataset, na.rm = TRUE))*100),0),'%); median (', round((median(best_roc_obs, na.rm = TRUE)),2),")", sep="")

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_prob, drawBox = FALSE, 
        main = paste(tit, sep=""), 
        cex.main = 0.8,line = -1.5, adj = 0.5)
mapImage(lon, lat, obs_mask, col= "white")
mask_dataset[ mask_dataset == 0] = NA
mapImage(lon, lat, mask_dataset, col= "orange")
mapImage(lon,lat, best_roc_obs, col= col_cor, breaks = brk_cor)

plot(ocean, col = "lightblue", add =TRUE) 
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 



screen( ind[4])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 

load(paste0(dir_out1, "/preBAobs_drop_SON_FDR.RData"))
PredBAobs_ens=(apply(predBAobs, c(1, 2, 3), mean, na.rm = TRUE))
spi3=PredBAobs_ens

mask_dataset = array(0, dim = c(ni, nj))
for (i in 1:ni) {
  for (j in 1:nj) {
    if (length(which(!is.na(spi3[i, j, ]))) && mask_son[i, j ] == 1 ) {
      mask_dataset[i, j] = 1
    }
  }
}

best_roc_obs=roca_son
best_roc_obs1= best_roc_obs*NA
best_roc_obs1[!is.na(best_roc_obs)]=1

tit <-
  paste('ROC area CLIBA DROP (SON): 
Skilfull surface (', round(((sum (best_roc_obs1, na.rm = TRUE)/sum(mask_dataset, na.rm = TRUE))*100),0),'%); median (', round((median(best_roc_obs, na.rm = TRUE)),2),")", sep="")

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_prob, drawBox = FALSE, 
        main = paste(tit, sep=""), 
        cex.main = 0.8,line = -1.5, adj = 0.5)
mapImage(lon, lat, obs_mask, col= "white")
mask_dataset[ mask_dataset == 0] = NA
mapImage(lon, lat, mask_dataset, col= "orange")
mapImage(lon,lat, best_roc_obs, col= col_cor, breaks = brk_cor)

plot(ocean, col = "lightblue", add =TRUE) 
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 

zz<- range(0.5, 1) # s
split.screen(c(1,2), screen=2)-> ind2
screen( ind2[1])

image.plot(zlim=zz,legend.only=TRUE, smallplot=c(.25,.75, .69,.99),
           col=col_cor, breaks = brk_cor, horizontal = TRUE)

screen( ind2[2])

nombres_paleta <- c("No climate effect", "No data")
image.plot(zlim=zr,legend.only=TRUE, smallplot=c(.25,.75, .69,.99),col=c("orange", "white"), 
           horizontal = TRUE,axis.args = list(at = c(zr,zr[length(zr)]+diff(zr)[1]),
                                              labels=c(nombres_paleta,'No model')))

close.screen( all=TRUE)
dev.off()

# 
# zz<- range(0.4,1)
# brk_prob <- seq(0.4, 1, length.out = 7)
# col_prob <- c("gray", "#FFE4C4","#E6F3E3", "#B7E2B1", "#6AB86F", "#227E3B", "#00441B")
# colns<-c("#FFE4C4")
# 
# image.plot(zlim=zz,
#            legend.only=TRUE, 
#            smallplot=c(.25,.75, .7,1),
#            col=col_prob,
#            horizontal = TRUE,
#            # labels=c('No BA', "No model", "0.6", "0.7", "0.8", "0.9", "1"))
#            axis.args = list(at = brk_prob,
#                             labels=c('No BA', "No model", "0.6", "0.7", "0.8", "0.9", "1")))
# 
# 
# close.screen( all=TRUE)
# dev.off()

