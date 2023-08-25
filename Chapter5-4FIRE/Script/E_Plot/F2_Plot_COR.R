
rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(sp)
library(maptools) # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(fields)
library(maps)
library(s2dverification)
library(ggplot2)
library(reshape)

library(pracma)
library(verification)
library(psych)
library(sf)
# install.packages("oce")
library(oce)
library(raster)
# install.packages("terra")
library(terra)
library(raster)
# install.packages("graticule")
library(graticule)
library(rgdal)
library(rworldmap)
# install.packages("rnaturalearth")
library(rnaturalearth)

source("C:/Users/Usuario/Dropbox/4DROP/script/Common/CorrMIO.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/ColorBarM.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/mioplot_global.R")


dir_drop = 'C:/Users/Usuario/Dropbox/4FIRE/data/DROP/'
dir_s5 = 'C:/Users/Usuario/Dropbox/4FIRE/data/forecast_s5/'
dir_out= 'C:/Users/Usuario/Dropbox/4FIRE/data/forecast_s5'

brk <- seq(-1, 1, 0.2)
col <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))

brk_cor <- seq(-1, 1, 0.2)
col_cor <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))



load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat

anni = 2000:2020
mesi = rep(1:12, length(anni))
# 


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
# shapename <- read_sf('~/Dropbox/estcena/miguel/capa_oceanos/ne_10m_ocean.shp')
ocean <- sf::st_transform(shapename , crs = crs) # cha
rm(shapename)


# datasets =c("GPCC")
datasets = c(
  # 'CPC',
  # 'GPCC',
  # 'PRECL',
  # 'ERA5',
  # 'JRA55',
  # 'NCEP',
  # 'MERRA2',
  # 'CAMS_OPI',
  # 'CHIRPS',
  # 'GPCP',
  'MSWEP')


start_date=05
ms="05"
#sc=c(6)
time_scale = c(3,6,12)
# mes_forecast = c(
#   "NOV",
#   "DEC",
#   "JAN",
#   "FEB")

mes_forecast = c(
  "MAY",
  "JUN",
  "JUL",
  "AUG")
# mes_forecast = c("MAY")

# sc= 6
# dataset="MSWEP"
# est="FEB"



for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  print(sc)
  load(file.path(dir_drop,paste("SPI", sc, "_ENS_1981_2020.RData", sep = "")))
  spi = spi[, , 229:480]
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    print(dataset)
    
    
    for (mes_for in 1:length(mes_forecast)) {
      est = mes_forecast[mes_for]
      print(est)
      
      # mesi_8 = which(mesi == 08)
      # mesi_9 = which(mesi == 09)
      # mesi_10 = which(mesi == 10)
      # mesi_11 = which(mesi == 11)
      if (est =="FEB") {
        mesi_8 =  which(mesi == 02)
      } else if (est =="MAR") {
        mesi_8 =  which(mesi == 03)
      } else if (est =="APR") {
        mesi_8 =  which(mesi == 04)
      } else if (est =="MAY"){
        mesi_8 =  which(mesi == 05)
      } else if (est =="JUN") {
        mesi_8 =  which(mesi == 06)
      } else if (est =="JUL") {
        mesi_8 =  which(mesi == 07)
      } else if (est =="AUG") {
        mesi_8 =  which(mesi == 08)
      } else if (est =="SEP") {
        mesi_8 =  which(mesi == 09)
      } else if (est =="OCT") {
        mesi_8 =  which(mesi == 10)
      } else if (est =="NOV") {
        mesi_8 =  which(mesi == 11)
      } else if (est =="DEC") {
        mesi_8 =  which(mesi == 12)
      } else if (est =="JAN") {
        mesi_8 =  which(mesi == 01)
      }
      
      
      
      
      ens = spi[,,mesi_8]  
      dim(ens)
      
      # dev.off()
      
      load(file.path(dir_s5, paste('SPI',sc,'SEAS5_',ms,'_',est,"_",dataset,".RData",sep = "")))
      # summary(as.vector(spi6pred4))
      
      
      # load(file.path(dir_s5, paste("SPI6SEAS5_11_DEC_MSWEP.RData",sep = "")))
      # summary(as.vector(spi6pred2))
      
      
      # image.plot(lon,lat,spi6pred2[,,25,22] )
      if (sc == "3" & est =="MAY"){
        pred =  spi3pred1
      } else if (sc == "6" & est =="MAY") {
        pred =  spi6pred1
      } else if (sc == "12" & est =="MAY") {
        pred =  spi12pred1

      } else if (sc == "3" & est =="JUN"){
        pred =  spi3pred2
      } else if (sc == "6" & est =="JUN") {
        pred =  spi6pred2
      } else if (sc == "12" & est =="JUN") {
        pred =  spi12pred2


      } else if (sc == "3" & est =="JUL"){
        pred =  spi3pred3
      } else if (sc == "6" & est =="JUL") {
        pred =  spi6pred3
      } else if (sc == "12" & est =="JUL") {
        pred =  spi12pred3


      } else if (sc == "3" & est =="AUG"){
        pred =  spi3pred4
      } else if (sc == "6" & est =="AUG") {
        pred =  spi6pred4
      } else if (sc == "12" & est =="AUG") {
        pred =  spi12pred4
      } else {
        print('dataset not known')
      }
      
      
      # if (sc == "3" & est =="AUG"){
      #   pred =  spi3pred1
      # } else if (sc == "6" & est =="AUG") {
      #   pred =  spi6pred1
      # } else if (sc == "12" & est =="AUG") {
      #   pred =  spi12pred1
      #   
      # } else if (sc == "3" & est =="SEP"){
      #   pred =  spi3pred2
      # } else if (sc == "6" & est =="SEP") {
      #   pred =  spi6pred2
      # } else if (sc == "12" & est =="SEP") {
      #   pred =  spi12pred2
      #   
      #   
      # } else if (sc == "3" & est =="OCT"){
      #   pred =  spi3pred3
      # } else if (sc == "6" & est =="OCT") {
      #   pred =  spi6pred3
      # } else if (sc == "12" & est =="OCT") {
      #   pred =  spi12pred3
      #   
      #   
      # } else if (sc == "3" & est =="NOV"){
      #   pred =  spi3pred4
      # } else if (sc == "6" & est =="NOV") {
      #   pred =  spi6pred4
      # } else if (sc == "12" & est =="NOV") {
      #   pred =  spi12pred4
      # } else {
      #   print('dataset not known')
      # }
      
      
      # if (sc == "3" & est =="FEB"){
      #   pred =  spi3pred1
      # } else if (sc == "6" & est =="FEB") {
      #   pred =  spi6pred1
      # } else if (sc == "12" & est =="FEB") {
      #   pred =  spi12pred1
      #   
      # } else if (sc == "3" & est =="MAR"){
      #   pred =  spi3pred2
      # } else if (sc == "6" & est =="MAR") {
      #   pred =  spi6pred2
      # } else if (sc == "12" & est =="MAR") {
      #   pred =  spi12pred2
      #   
      #   
      # } else if (sc == "3" & est =="APR"){
      #   pred =  spi3pred3
      # } else if (sc == "6" & est =="APR") {
      #   pred =  spi6pred3
      # } else if (sc == "12" & est =="APR") {
      #   pred =  spi12pred3
      #   
      #   
      # } else if (sc == "3" & est =="MAY"){
      #   pred =  spi3pred4
      # } else if (sc == "6" & est =="MAY") {
      #   pred =  spi6pred4
      # } else if (sc == "12" & est =="MAY") {
      #   pred =  spi12pred4
      # } else {
      #   print('dataset not known')
      # }
      
      
      
      # if (sc == "3" & est =="NOV"){
      #   pred =  spi3pred1
      # } else if (sc == "6" & est =="NOV") {
      #   pred =  spi6pred1
      # } else if (sc == "12" & est =="NOV") {
      #   pred =  spi12pred1
      #   
      # } else if (sc == "3" & est =="DEC"){
      #   pred =  spi3pred2
      # } else if (sc == "6" & est =="DEC") {
      #   pred =  spi6pred2
      # } else if (sc == "12" & est =="DEC") {
      #   pred =  spi12pred2
      #   
      #   
      # } else if (sc == "3" & est =="JAN"){
      #   pred =  spi3pred3
      # } else if (sc == "6" & est =="JAN") {
      #   pred =  spi6pred3
      # } else if (sc == "12" & est =="JAN") {
      #   pred =  spi12pred3
      #   
      #   
      # } else if (sc == "3" & est =="FEB"){
      #   pred =  spi3pred4
      # } else if (sc == "6" & est =="FEB") {
      #   pred =  spi6pred4
      # } else if (sc == "12" & est =="FEB") {
      #   pred =  spi12pred4
      # } else {
      #   print('dataset not known')
      # }
      pred[is.infinite(pred)]=NA
      
      
      data = apply(pred[,,mesi_8,], c(1,2,3), mean, na.rm=TRUE)
      data[is.infinite(data)]=NA
      data[is.na(data)] <- NA
      
      dim(data)
      dim(ens)
      
      summary(as.vector(data))
      summary(as.vector(ens))
      
      ni = dim(data)[1]
      nj = dim(data)[2]
      nt = dim(ens)[3]
      
      corre <- matrix(data = NA,nrow = ni, ncol = nj)
      pvalue <- matrix(data = NA, nrow = ni, ncol = nj)
      
      
      for (i in 1:ni) {
        for (j in 1:nj) {
          
          OK <- complete.cases(ens[i, j,], data[i, j,])
          x <- ens[i, j, OK]
          y <- data[i, j, OK]
          n <- length(x)
          #if (n >= anniok * 12) {
          if (n >= nt*0.9) {
            
            dum = CorrMIO((x), (y), method = 'pearson', pval = TRUE )
            corre[i, j] = as.numeric(dum)[1]
            pvalue[i, j] <- as.numeric(dum)[4]
            rm(dum)
            
          }
          
          rm(OK, n, x, y)
        }
      }
      
      
      
      pvalue_adj = p.adjust(pvalue, method = "fdr", n = length(pvalue[!is.na(pvalue)]))
      pvalue_adj = matrix(pvalue_adj, nrow = ni, ncol = nj)
      # 
      # lat2 = lat[which(lat > -60 & lat < 85)]
      # corre2 = corre[, which(lat > -60 & lat < 85)]
      # pvalue2 = pvalue[, which(lat > -60 & lat < 85)]
      # pvalue_adj2 = pvalue_adj[, which(lat > -60 & lat < 85)]
      
      save(corre, file = file.path(dir_out, paste("COR_S5_spi",sc,"_",ms,"_",est,"_",dataset,"_original.RData", sep = "") ))
      save(pvalue_adj, file = file.path(dir_out, paste("/PVAL_S5_spi",sc,"_",ms,"_",est,"_",dataset,"_original.RData", sep = "") ))
      
       # dev.off()
    }
  }
}
      ## plot 1
#pdf(paste(dir_out,paste("COR_S5_spi",sc,"_",ms,"_",est,"_",dataset,".pdf",sep = "")),width=12, height=9)
      
      
      layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
      par(oma = c(1, 1, 4, 1))
      tit <-
        paste(
          'Correlation for SPI',sc, ' in ',est,', ',dataset,' (S5) against DROP\n Start date: ',
          month.name[ start_date],
          ' - Period: 2000/2020\n (points: p<0.05 )', sep=""
        )
      
      
      mioplot(corre2, lon,lat2,
              toptitle = '',sizetit = 0.8,
              brks = brk_cor,  cols = col_cor,
              axelab =F, filled.continents = FALSE,
              drawleg = F,  
              # dots = pvalue2 <= 0.05,
              dots2 = pvalue_adj2 <= 0.05 
      )
      title(tit, line = 0.5, outer = T)
      ce = 1.4
      ColorBar(
        brks = brk,
        cols = col,
        vert = T
      )
      
#       save(corre2, file = file.path(dir_out, paste("COR_S5_spi",sc,"_",ms,"_",est,"_",dataset,"_original.RData", sep = "") ))
#       
#       dev.off()
#     }
#   }
# }
      
      
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
      
      
      
  ##############################################
      ## PLOT WITH PROJECT ROBINSON
  ##############################################
      dev.off()
      
      pdf(paste(dir_out,"/COR_DROP_JJA_MSWEP_PRUEBA.pdf",sep = ""),width=11.5, height=12)
      # set.panel() 
      # split.screen( rbind(c(0, 1,.1,1), c(0,1,0,.1)))
      # split.screen(c(4,3), screen=1)-> ind
      # zr<- range(0,1)
      
      set.panel() 
      split.screen( rbind(c(0, 1,.1,1), c(0,1,0,.1)))
      split.screen(c(4,3), screen=1)-> ind
      zr<- range(0,1)
      
      contador <- 0
      
    for (mes_for in 1:length(mes_forecast)) {
        est = mes_forecast[mes_for]
        print(est)
        
        ##########----plot 1---#############
      
      for (isc in 1:length(time_scale)) {
        sc = time_scale[isc]
        print(sc)
        
        contador <- contador + 1
        
            for (idata in 1:length(datasets)) {
              dataset = datasets[idata]
              print(dataset)
            
            # load(file.path(dir_out, paste("COR_S5_spi",sc,"_",ms,"_",est,"_",dataset,"_original.RData", sep = "") ))
              
            
            load(file.path(dir_out, paste("COR_S5_spi",sc,"_",ms,"_",est,"_",dataset,"_original.RData", sep = "")))
            load(file.path(dir_out, paste("/PVAL_S5_spi",sc,"_",ms,"_",est,"_",dataset,"_original.RData", sep = "")))
            
        
            pvalue1 =pvalue_adj
            pvalue1[pvalue1 <= 0.05] = NA
            pvalue1[pvalue1 >= 0.05] = -1
            
            pvalue1[is.infinite(pvalue1)]=NA
            pvalue1[is.na(pvalue1)]=NA
            colns<-c("#FFE4C4")
            

      
      interaction_number <- (mes_for  - 1) * 3 + isc 
      screen( ind[interaction_number])
        # screen( ind[contador])
        par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
        
        
        if(interaction_number == 1){
          panel = "a"
        } else if (interaction_number == 2){
          panel = "b"
        } else if (interaction_number == 3){
          panel = "c"
        } else if (interaction_number == 4){
          panel = "d"
        } else if (interaction_number == 5){
          panel = "e"
        } else if (interaction_number == 6){
          panel = "f"
        } else if (interaction_number == 7){
          panel = "g"
        } else if (interaction_number == 8){
          panel = "h"
        } else if (interaction_number == 9){
          panel = "i"
        } else if (interaction_number == 10){
          panel = "j"
        } else if (interaction_number == 11){
          panel = "k"
        } else if (interaction_number == 12){
          panel = "l"
        }
        
        
        tit <-
          paste(
            panel,') COR for SPI',sc, ' in ',est,', ',dataset,' (S5) against DROP\n Start date: ',
            month.name[ start_date],
            ' - Period: 2000/2020', sep=""
          )
        
        mapPlot(coastlineWorld, col="white",
                projection="+proj=robin", 
                breaks = brk_cor, drawBox = FALSE, 
                main = tit, cex.main = 0.8,line = -1.5, adj = 0.5)
         
         mapImage(lon, lat, obs_mask, col= "white")
         mapImage(lon, lat, corre, col= col_cor, breaks = brk_cor)
         mapImage(lon, lat, pvalue1, col = colns)
         
         # Encontrar las coordenadas en la matriz para el valor -1 en pvalue1
         # indices <- which(pvalue1 == -1, arr.ind = TRUE)
         # # Obtener las coordenadas de fila y columna
         # filas <- indices[, 1]
         # columnas <- indices[, 2]
        plot(ocean, col = "lightblue", add =TRUE)
        plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules
        }
      }
    }

      # zr<- seq(0,1, 0.5)
      zz<- range(-1, 1) # s
      split.screen(c(1,2), screen=2)-> ind2
      screen( ind2[1])
      
      image.plot(zlim=zz,legend.only=TRUE, smallplot=c(.25,.95, .6,.9),
                 col=col_cor, breaks = brk_cor, horizontal = TRUE)
      
      screen( ind2[2])
      # zr<- range(0,1, 0.5)
      nombres_paleta <- c("No significative", "No data")
      image.plot(zlim=zr,legend.only=TRUE, smallplot=c(.25,.75, .6,.9),col=c("#FFE4C4","white"),
                 horizontal = TRUE,
                  axis.args = list(at = c(zr,zr[length(zr)]+diff(zr)[1]),
                                                    labels=c(nombres_paleta,'No model')))
      
      
      # zr <- seq(-1, 1, 1)
      # nombres_paleta <- c("No significative", "No data", "Ocean")
      # image.plot(zlim = zr, legend.only = TRUE, smallplot = c(.25, .75, .6, .9),
      #            col = c("#FFE4C4", "white", "lightblue"), horizontal = TRUE,
      #            axis.args = list(at = c(1:4),
      #                             labels = nombres_paleta[1:3], "no model"))
      
      close.screen( all=TRUE)
      dev.off()     
      
      
      
      
      
      
      
      
      
      
      
      
      