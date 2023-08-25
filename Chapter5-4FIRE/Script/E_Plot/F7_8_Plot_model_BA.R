rm(list = ls())
graphics.off()
gc()

library(pracma)
library(ncdf4)
library(fields)
library(maptools)
library(RColorBrewer)
library(viridis)



source('C:/Users/Usuario/Dropbox/4FIRE/script/model_BA/image_mask.R')
data(wrld_simpl)

where='miguel'
if (where == 'mac') {
  dir_data='~/Dropbox/estcena/scripts/miguel/4FIRE/data/'
  dir_out = '~/Dropbox/estcena/scripts/miguel/4FIRE/out/'
  dir_drop='/Users/marco/Documents/dati/fire_climate_data/climate_fire/datos/drop_spi/'
} else if (where == 'miguel') {
  dir_f51 = 'C:/Users/Usuario/Dropbox/4FIRE/data/BA'
  dir_data ="C:/Users/Usuario/Dropbox/4FIRE/data/DROP/"
  dir_out="C:/Users/Usuario/Dropbox/4FIRE/data/model_obs_FDR/"
  dir_out1="C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs_FDR/"
  dir_drop ="C:/Users/Usuario/Dropbox/4FIRE/data/DROP/"
  dir_mask="C:/Users/Usuario/Dropbox/4FIRE/data/mask_season/"
  dir_pdf="C:/Users/Usuario/Dropbox/4FIRE/"
}

fname<-file.path(dir_data, '/DROP_3_1981_2020.nc')
obs.nc <- nc_open(fname)
obs.nc$dim$lon$vals -> lon
obs.nc$dim$lat$vals -> lat
ni=length(lon)
nj=length(lat)
ilat = which(lat > -60 & lat < 75)
ilon = which(lon > -165 & lon < 180)



# seasons= c("DJF")
# seasons= c("DJF", "MAM", "JJA", "SON")

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







# load(paste0(dir_mask, "mask_JJA.RData"))
# mask_jja=mask
# rm(mask)
# season = c("DJF")
season = c("DJF", "MAM", "JJA", "SON")
FDR=1


for (seas in 1:length(season)) {
  seaJJA = season[seas]
  
  
  if (FDR==1) {
    version = paste0(seaJJA,'_FDR')
  } else {
    version = paste0(seaJJA)
  }
  
  load(paste0(dir_mask, "mask_",seaJJA,".RData"))
  mask_jja=mask
  rm(mask)
  
  load(paste0(dir_out, "best_m_spi_a_fin_",seaJJA,"_FDR_MSWEP.RData"))
  load(paste0(dir_out, "best_t_spi_a_fin_",seaJJA,"_FDR_MSWEP.RData"))
  load(paste0(dir_out, "best_m_spi_c_fin_",seaJJA,"_FDR_MSWEP.RData"))
  load(paste0(dir_out, "best_t_spi_c_fin_",seaJJA,"_FDR_MSWEP.RData"))
  
  load(paste0(dir_out, "best_x1_",seaJJA,"_FDR_MSWEP.RData"))
  best_x1_fin =-best_x1_fin
  load(paste0(dir_out, "best_x2_",seaJJA,"_FDR_MSWEP.RData"))
  # best_x2_fin=-best_x2_fin
  load(paste0(dir_out, "roc_area_",version,"_MSWEP.RData"))
  load(paste0(dir_out, "sig_",version,"_MSWEP.RData"))
  
  
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
  ocean <- rnaturalearth::ne_download(scale = 10, type = 'ocean', category = 'physical', returnclass = "sf")
  ocean <- sf::st_transform(ocean, crs = crs)
  
  # colns<-c("#34495E")
  colns<-c("grey")
  mask_plot_jja= mask_jja
  mask_plot_jja[(mask_jja==0)]=NA
  ##############################################################################
  #COEFICIENTE SPI AC JJA
  ##############################################################################
  
  pdf(paste(dir_pdf,"beta_AC_",seaJJA,".pdf", sep=""), width=11, height=7.5)
  # split.screen( rbind(c(0, .9,0,1), c(.9,1,0,1)))
  split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
  split.screen(c(1,1), screen=1)-> ind
  screen( ind[1])
  par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
  par(mar=rep(0.6, 4))
  
  
  max_coef=3
  min_coef=-max_coef
  brk_coef <- seq(min_coef, max_coef, length.out = 11)
  col_coef <-
    (colorRampPalette(brewer.pal(length(brk_coef), "BrBG"))(length(brk_coef) -
                                                              1))
  best_x2_fin_fin=apply(best_x2_fin, c(1, 2), mean, na.rm = TRUE)
  mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_coef, drawBox = FALSE, 
          main = paste("?? (AC; ",seaJJA,")",sep=""), 
          cex.main = 2,
          line = -1.7, adj = 0.5)
  
  # mapImage(lon, lat2, pvalue_adj2)
  mapImage(lon, lat, obs_mask, col= "white")
  mapImage(lon, lat, mask_plot_jja, col=  colns)
  mapImage(lon,lat, best_x2_fin_fin, col= col_coef, breaks = brk_coef)
  
  # summary(as.vector(best_x2_fin_fin))
  
  plot(ocean, col = "lightblue", add =TRUE) 
  plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 
  
  text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
  text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8)
  
  split.screen(c(1,1), screen=2)-> ind
  c2<- range(-1.2, 3.1)
  col_c2 <- c("white", "grey", "#EFF6E5",  "#DBEEEC", "#97D6CD", "#45A39A", "#066B63", "#003C30")
  brk_coef <- c(-1.2, -0.6, 0, 0.6, 1.2, 1.8, 2.4, 3)
  
  screen( ind[1])
  image.plot(zlim=  c2,
             legend.only=TRUE, 
             # smallplot=c(.05,.2, .05,.95),
             smallplot=c(.15,.85, .7,1),
             col=  col_c2,
             horizontal = TRUE,
             axis.args = list(at =   brk_coef,
                              labels=c('No BA', "No model", "0", "0.6", "1.2", "1.8", "2.4", "3")))
  
  close.screen( all=TRUE)
  dev.off()
  
  
  
  ##############################################################################
  #COEFICIENTE SPI CC JJA
  ##############################################################################
  
  pdf(paste(dir_pdf,"beta_CC_",seaJJA,".pdf", sep=""), width=11, height=7.5)
  split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
  split.screen(c(1,1), screen=1)-> ind
  screen( ind[1])
  par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
  par(mar=rep(0.6, 4))
  
  max_coef=3
  min_coef=-max_coef
  brk_coef <- seq(min_coef, max_coef, length.out = 11)
  col_coef <-
    (colorRampPalette(brewer.pal(length(brk_coef), "PiYG"))(length(brk_coef) - 1))
  
  mask_plot_jja= mask_jja
  mask_plot_jja[(mask_jja==0)]=NA
  
  best_x1_fin_fin=apply(best_x1_fin, c(1, 2), mean, na.rm = TRUE)
  best_x1_fin_fin[is.na(best_roc)] = NA
  
  mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_coef, drawBox = FALSE, 
          main = paste("?? (CC; ",seaJJA,")",sep=""), 
          cex.main = 2,
          line = -1.7, adj = 0.5)
  
  mapImage(lon, lat, obs_mask, col= "white")
  mapImage(lon, lat, mask_plot_jja, col=  colns)
  mapImage(lon,lat, best_x1_fin_fin, col= col_coef, breaks = brk_coef)
  
  # summary(as.vector(best_x1_fin_fin))
  
  plot(ocean, col = "lightblue", add =TRUE) 
  plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 
  
  text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
  text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8)
  
  
  split.screen(c(1,1), screen=2)-> ind
  c2<- range(-4.2, 0)
  col_c2 <- c("white","grey", "#8E0152", "#C72582", "#E284B7", "#F4C3E1" ,"#FAEAF2", "#EFF6E5")
  brk_coef <- c(-4.2, -3.6, -3, -2.4, -1.8, -1.2, -0.6, 0)
  
  screen( ind[1])
  image.plot(zlim=  c2,
             legend.only=TRUE, 
             smallplot=c(.15,.85, .7,1),
             col=  col_c2,
             horizontal = TRUE,
             axis.args = list(at =   brk_coef,
                              labels=c(
                                "No BA", "No model", "-3", "-2.4", "-1.8", "-1.2", "-0.6", "0")))
  
  
  close.screen( all=TRUE)
  dev.off()
  
  
  ##############################################################################
  #mes SPI AC JJA
  ##############################################################################
  
  pdf(paste(dir_pdf,"mes_AC_",seaJJA,".pdf", sep=""), width=11, height=7.5)
  split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
  split.screen(c(1,1), screen=1)-> ind
  screen( ind[1])
  par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
  par(mar=rep(0.6, 4))
  
  brk_mes <- seq(-14, -2, length.out = 13)
  col_mes <-
    (colorRampPalette(brewer.pal(length(brk_mes), "Spectral"))(length(brk_mes)))
  
  mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_mes, drawBox = FALSE, 
          main = paste("m (AC; ",seaJJA,")",sep=""), 
          cex.main = 2,
          line = -1.7, adj = 0.5)
  
  # mapImage(lon, lat2, pvalue_adj2)
  mapImage(lon, lat, obs_mask, col= "white")
  mapImage(lon, lat, mask_plot_jja, col=  colns)
  mapImage(lon,lat, best_m_spi_a_fin, col=   col_mes , breaks = brk_mes)
  
  plot(ocean, col = "lightblue", add =TRUE) 
  plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 
  
  text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
  text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8)
  
  split.screen(c(1,1), screen=2)-> ind
  screen( ind[1])
  zc<- range(-16, -2)
  col_mes1<-c("white", "grey", "#9E0142", "#CB334C",
              "#E95D47", "#F88D51","#FDBE6E", "#FEE593",
              "#FFFFBF", "#EAF69E","#BEE5A0", "#88CFA4",
              "#54AEAC", "#397EB8", "#5E4FA2")
  brk_mes<-c(-2, -3,
             -4, -5, -6, -7,
             -8, -9, -10, -11,
             -12, -13, -14, -15, -16)
  image.plot(zlim=zc,
             legend.only=TRUE, 
             smallplot=c(.15,.85, .7,1),
             col=col_mes1,
             horizontal = TRUE,
             axis.args = list(at = brk_mes,
                              labels=c( "-2", "-3",
                                        "-4", "-5", "-6", "-7",
                                        "-8", "-9", "-10", "-11",
                                        "-12", "-13", "-14",'No model', "No BA")))
  close.screen( all=TRUE)
  dev.off()
  
  
  
  ##############################################################################
  #mes SPI CC JJA
  ##############################################################################
  
  
  pdf(paste(dir_pdf,"mes_CC_",seaJJA,".pdf", sep=""), width=11, height=7.5)
  split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
  split.screen(c(1,1), screen=1)-> ind
  screen( ind[1])
  par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
  par(mar=rep(0.6, 4))
  
  brk_prob <- seq(0, 3, length.out = 5)
  col_bias <-rev(inferno(length(brk_prob)+1))
  col_bias=col_bias[1:4]
  
  best_m_spi_c_fin_fin=best_m_spi_c_fin+3
  
  
  mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_prob, drawBox = FALSE, 
          main = paste("m (CC; ",seaJJA,")",sep=""), 
          cex.main = 2,
          line = -1.7, adj = 0.5)
  
  # mapImage(lon, lat2, pvalue_adj2)
  mapImage(lon, lat, obs_mask, col= "white")
  mapImage(lon, lat, mask_plot_jja, col=  colns)
  mapImage(lon,lat, best_m_spi_c_fin_fin, col=   col_bias, breaks = brk_prob, zlim=c(0,3))
  
  plot(ocean, col = "lightblue", add =TRUE) 
  plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 
  
  text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
  text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8)
  
  split.screen(c(1,1), screen=2)-> ind
  screen( ind[1])
  zc<- range(-2, 3)
  col_mes1<-c("white","grey","#FCFFA4FF", "#FCA50AFF", "#DD513AFF", 
              "#932667FF")
  brk_mes<-c(-2,-1, 0,1,2,3)
  image.plot(zlim=zc,
             legend.only=TRUE, 
             smallplot=c(.15,.85, .7,1),
             col=col_mes1,
             horizontal = TRUE,
             axis.args = list(at = brk_mes,
                              labels=c("No BA","No model" ,"0", "1",
                                       "2", "3")))
  
  close.screen( all=TRUE)
  dev.off()
  
  
  ##############################################################################
  #time scale SPI AC JJA
  ##############################################################################
  
  
  pdf(paste(dir_pdf,"time_AC_",seaJJA,".pdf", sep=""), width=11, height=7.5)
  split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
  split.screen(c(1,1), screen=1)-> ind
  screen( ind[1])
  par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
  par(mar=rep(0.6, 4))
  
  brk_spi <- seq(1, 3, length.out = 4)
  col_spi = c("#D98880", "#F9F89C", "#ABEBC6")
  
  mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_spi, drawBox = FALSE, 
          main = paste("t (AC; ",seaJJA,")",sep=""), 
          cex.main = 2,
          line = -1.7, adj = 0.5)
  
  mapImage(lon, lat, obs_mask, col= "white")
  mapImage(lon, lat, mask_plot_jja, col=  colns)
  mapImage(lon,lat, best_t_spi_a_fin, col= col_spi, breaks = brk_spi)
  
  plot(ocean, col = "lightblue", add =TRUE) 
  plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 
  text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
  text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8)
  
  
  split.screen(c(1,1), screen=2)-> ind
  screen( ind[1])
  zr<- range(1, 5)
  
  brk_spi1 <- c(1,2,3, 4, 5)
  
  col_spi1<-c("white", "grey", "#D98880", "#F9F89C", "#ABEBC6")
  image.plot(zlim=zr,
             legend.only=TRUE, 
             smallplot=c(.15,.85, .7,1),
             col=col_spi1,
             horizontal = TRUE,
             axis.args = list(at = brk_spi1,
                              labels=c('No BA', "No model",'SPI3','SPI6','SPI12')))
  
  
  close.screen( all=TRUE)
  dev.off()
  
  
  ##############################################################################
  #time scale SPI CC JJA
  ##############################################################################
  pdf(paste(dir_pdf,"time_CC_",seaJJA,".pdf", sep=""), width=11, height=7.5)
  split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
  split.screen(c(1,1), screen=1)-> ind
  screen( ind[1])
  par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
  par(mar=rep(0.6, 4))
  
  brk_spi <- seq(1, 3, length.out = 4)
  col_spi = c("#D98880", "#F9F89C", "#ABEBC6")
  
  mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_spi, drawBox = FALSE, 
          main = paste("t (CC; ",seaJJA,")",sep=""), 
          cex.main = 2,
          line = -1.7, adj = 0.5)
  
  # mapImage(lon, lat2, pvalue_adj2)
  mapImage(lon, lat, obs_mask, col= "white")
  mapImage(lon, lat, mask_plot_jja, col=  colns)
  mapImage(lon,lat, best_t_spi_c_fin, col= col_spi, breaks = brk_spi)
  
  plot(ocean, col = "lightblue", add =TRUE) 
  plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 
  text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
  text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8)
  
  split.screen(c(1,1), screen=2)-> ind
  screen( ind[1])
  zr<- range(1, 5)
  
  brk_spi1 <- c(1,2,3, 4, 5)
  
  col_spi1<-c("white", "grey", "#D98880", "#F9F89C", "#ABEBC6")
  image.plot(zlim=zr,
             legend.only=TRUE, 
             smallplot=c(.15,.85, .7,1),
             col=col_spi1,
             horizontal = TRUE,
             axis.args = list(at = brk_spi1,
                              labels=c('No BA', "No model",'SPI3','SPI6','SPI12')))
  
  
  close.screen( all=TRUE)
  dev.off()
}
