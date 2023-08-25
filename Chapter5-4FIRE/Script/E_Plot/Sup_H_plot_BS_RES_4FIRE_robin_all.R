rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(fields)
library(maptools)
library(verification)
library(RColorBrewer)
library(maps)
library(oce)
library(sf)

data(wrld_simpl)

## fix parameters
years = 2001:2020 #antes en el original: 2001:2019
num_ens = 25
season = 'JJA'
FDR=1
if (FDR==1) {
  version = paste0(season,'_FDR')
} else {
  version = paste0(season)
}

where = 'miguel'

if (season == 'JJA') {
  first_month = 6
  last_month = 8
} else if (season == 'MAM') {
  first_month = 3
  last_month = 5
} else if (season == 'SON') {
  first_month = 9
  last_month = 11
} else {
  print('no season, call terminator')
}

if (where == 'mac') {
  dir_f51 = '/Users/marco/Documents/dati/fire_climate_data/climate_fire/datos/BA/FIRECCI511-BA/'
  dir_data = '~/Dropbox/estcena/scripts/miguel/4FIRE/data/'
  dir_out = '~/Dropbox/estcena/scripts/miguel/4FIRE/out/'
  dir_drop = '/Users/marco/Documents/dati/fire_climate_data/climate_fire/datos/drop_spi/'
  dir_s5 = '/Users/marco/Documents/dati/4FIRE/'
} else if (where == 'miguel') {
  dir_f51 = "C:/Users/Usuario/Dropbox/4FIRE/data/BA/"
  dir_drop ="C:/Users/Usuario/Dropbox/4FIRE/data/DROP/"
  dir_data="C:/Users/Usuario/Dropbox/miguel/4FIRE/out/"
  #dir_out_pre="C:/Users/Usuario/Dropbox/4FIRE/Results/model_4fire/"
  #dir_out_obs="C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs/"
  
  # dir_out_pre="C:/Users/Usuario/Dropbox/4FIRE/Results/model_4fire_FDR_binario/"
  # dir_out_obs="C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs_FDR_binario/"
  
  dir_out_pre="C:/Users/Usuario/Dropbox/4FIRE/Results/model_4fire_FDR_mean/"
  dir_out_obs="C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs_FDR_mean/"
  dir_s5 = 'C:/Users/Usuario/Dropbox/4FIRE/data/forecast_s5/'
  source('C:/Users/Usuario/Dropbox/4FIRE/script/model_BA/image_mask.R')
  
} else if (where == 'miguel_mesa') {
  dir_f51 = "C:/Users/migue/Dropbox/4FIRE/data/BA/"
  dir_drop ="C:/Users/migue/Dropbox/4FIRE/data/DROP/"
  dir_data="C:/Users/migue/Dropbox/miguel/4FIRE/out/"
  dir_out_pre="C:/Users/migue/Dropbox/4FIRE/Results/model_4fire_FDR/"
  dir_out_obs="C:/Users/migue/Dropbox/4FIRE/Results/model_obs_FDR/"
  dir_s5 = 'C:/Users/migue/Dropbox/4FIRE/data/forecast_s5/'
  source('C:/Users/migue/Dropbox/4FIRE/script/model_BA/image_mask.R')
  
}  else if (where == 'onfire') {
  dir_f51 = '/diskonfire/CLIMATE_FIRE/datos/BA/FIRECCI511-BA/'
  dir_data = '~/Dropbox/estcena/scripts/miguel/4FIRE/data/'
  dir_out = '~/Dropbox/estcena/scripts/miguel/4FIRE/out/'
  dir_drop = '/diskonfire/CLIMATE_FIRE/datos/drop_spi/'
  dir_s5 = '/diskonfire/4FIRE/'
}

brk_prob <- seq(0.6, 1, length.out = 5)
col_prob <-(colorRampPalette(brewer.pal(length(brk_prob), "YlGn"))(length(brk_prob)-1))

## load SPI
fname <- file.path(dir_drop, 'DROP_3_1981_2020.nc')
obs.nc <- nc_open(fname)
obs.nc$dim$lon$vals -> lon
obs.nc$dim$lat$vals -> lat
ilat = which(lat > -60 & lat < 75)
ilon = which(lon > -165 & lon < 180)

## BA
load(paste0(dir_f51, "/BA_200101_202012_nat.RData"))

ni = dim(BA)[1]
nj = dim(BA)[2]

#monthly BA --> JJA BA
BAS = array(0, dim = c(ni, nj, length(years)))

for (i in 1:ni) {
  for (j in 1:nj) {
    for (iyear in 1:length(years)) {
      i1 = (iyear - 1) * 12 + first_month
      i2 = (iyear - 1) * 12 + last_month
      BAS[i, j, iyear] = sum(BA[i, j, i1:i2], na.rm = TRUE)
    }
  }
}

mask = array(0, dim = c(ni, nj))
for (i in 1:ni) {
  for (j in 1:nj) {
    # print(length(BA[i,j,]==0))
    if (length(which(BAS[i, j, ] > 0)) >= 10) {
      mask[i, j] = 1
    }
  }
}
####################################################
# Proyecci?n de Robinson y mapa base (robin)
####################################################
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

shapename <- read_sf('C:/Users/Usuario/Dropbox/capas/ne_10m_ocean.shp')
ocean <- sf::st_transform(shapename , crs = crs) # cha
rm(shapename)

##########################################################
#creo una mascara de todo el globo para tapar los paises
##########################################################
dir_drop ="C:/Users/Usuario/Dropbox/4FIRE/data/DROP/"
dir_oss="C:/Users/Usuario/Dropbox/4DROP/data"
fname <- file.path(dir_oss, 'MSWEP_1981_2021.nc')
obs.nc <- nc_open(fname)
obs <- ncvar_get(obs.nc, "precipitation")
obs[obs == "-9999.f"] <- NA
obs1 = apply(obs, c(1, 2), mean)
load(file.path(dir_drop, "inout.RData"))
obs_mask= obs1*inout
obs_mask[!is.na(obs_mask)]=1
# ocean_mask= obs1*inout
# ocean_mask[!is.na(ocean_mask)]=1
# ocean_mask[is.na(ocean_mask)]=0
# ocean_mask[(ocean_mask==1)]=NA

# image.plot(lon, lat, ocean_mask)
# dev.off()

dir_mask="C:/Users/Usuario/Dropbox/4FIRE/data/mask_season/"
load(paste0(dir_mask, "mask_JJA.RData"))
mask_jja=mask

rm(mask)












# summary(as.vector(best_roc_obs))
####################################################
# Paleta Correlation
####################################################
brk_cor <- seq(0, 0.25, length.out = 6)
col_cor <- c( "#6E016B","#C51B7D", "#B7E2B1", "#6AB86F",  "#00441B")

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
             'MSWEP',
             '4FIRE')

# dataset="CHIRPS"
close.screen( all=TRUE)
dev.off()
set.panel() 
split.screen( rbind(c(0, 1,.1,1), c(0,1,0,.1)))
split.screen(c(4,3), screen=1)-> ind
zr<- range(0,0.25)

for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  print(dataset)
  
  if(dataset=="4FIRE"){
    load(paste0(dir_out_pre, "preBApre_4FIRE_JJA_FDR.RData"))
    PredBApre_ens=(apply(predBA, c(1, 2, 3), mean, na.rm = TRUE))
    spi3=PredBApre_ens
  } else {
    load(file.path(paste0(dir_drop, "SPI3_",dataset,"_1981_2020.RData" )))
  }
  
  mask_dataset = array(0, dim = c(ni, nj))
  # mask_dataset = array(0, dim = c(ni, nj))
  for (i in 1:ni) {
    for (j in 1:nj) {
      # print(length(BA[i,j,]==0))
      if (length(which(!is.na(spi3[i, j, ]))) && mask_jja[i, j ] == 1 ) {
        mask_dataset[i, j] = 1
      }
    }
  }
  
  # image.plot(lon, lat, mask_dataset)
  # mask_dataset[ mask_dataset == 0] = NA
  # image.plot(lon, lat, mask_dataset)
  
  if (dataset=="4FIRE"){
    load(paste(dir_out_pre, "bs_4fire_res_JJA_FDR_pval.RData", sep="")) #best_roc_drop
    best_roc_obs=bs_4fire_res_pval
  } else {
    load(paste(dir_out_pre,"bs_pred_res_JJA_FDR_",dataset,"_pval.RData", sep="")) #best_roc_drop
    best_roc_obs=bs_pred_res_pval
  }
  
  
  best_roc_obs1=best_roc_obs
  best_roc_obs1[best_roc_obs1 > 0.25] = 0.25
  
  ##########----plot 1---#############
  screen( ind[idata])
  par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
  
  mapPlot(coastlineWorld, col="white", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, 
          main = paste('Brier Score Resolution CLIBA-',dataset,' (S5; ',season,'): 
       median (', round((median(best_roc_obs, na.rm = TRUE)),2),")", sep=""),
          cex.main = 0.8,line = -1.5, adj = 0.5)
  # round(((sum (best_roc_obs1, na.rm = TRUE)/sum(mask_jja, na.rm = TRUE))*100),0),
  # mapImage(lon, lat, mask_dataset, col= "orange")
  mapImage(lon, lat, obs_mask, col= "white")
  mask_dataset[ mask_dataset == 0] = NA
  mapImage(lon, lat, mask_dataset, col= "orange")
  mapImage(lon, lat, best_roc_obs1, col= col_cor, breaks = brk_cor)
  # mapImage(lon, lat, ocean_mask, col = "lightblue", breaks = brk_cor)
  # plot(wrld_simpl,  crs = crs, add = TRUE)
  # mapImage(lon, lat, wrld_simpl)
  plot(ocean, col = "lightblue", add =TRUE)
  plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 
}

zz<- range(0, 0.25) # s
split.screen(c(1,2), screen=2)-> ind2
screen( ind2[1])

image.plot(zlim=zz,legend.only=TRUE, smallplot=c(.25,.95, .6,.9),
           col=col_cor, breaks = brk_cor, horizontal = TRUE)

screen( ind2[2])

nombres_paleta <- c("No climate effect", "No data")
image.plot(zlim=zr,legend.only=TRUE, smallplot=c(.25,.75, .6,.9),col=c("orange", "white"), 
           horizontal = TRUE,axis.args = list(at = c(zr,zr[length(zr)]+diff(zr)[1]),
                                              labels=c(nombres_paleta,'No model')))

close.screen( all=TRUE)
dev.off()