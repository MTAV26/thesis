

rm(list = ls())
graphics.off()
gc()

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



# source("/home/miguel/4DROP/script/Common/CorrMIO.R")
# source("/home/miguel/4DROP/script/Common/ColorBarM.R")
# source("/home/miguel/4DROP/script/Common/mioplot_global.R")

source("C:/Users/Usuario/Dropbox/4DROP/script/Common/CorrMIO.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/ColorBarM.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/mioplot_global.R")


## fixed parameters
#time_scale = c(1, 3, 6, 12)
time_scale = c(6)
data(wrld_simpl)

# dir_drop = '/home/miguel/4DROP/DROP/'
# dir_oss = '/home/miguel/4DROP/results/spi/'
# dir_out= '/home/miguel/4DROP/results/cor'


dir_drop = 'C:/Users/Usuario/Dropbox/4DROP/DROP/'
dir_oss = 'C:/Users/Usuario/Dropbox/4DROP/spi/'
dir_out= 'C:/Users/Usuario/Dropbox/4DROP/cor'

anni = 1981:2020
mesi = rep(1:12, length(anni))


load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat
lat2 = lat[which(lat > -60 & lat < 85)]

forecast= c("ESP", "S5")

datasets = c('MSWEP', 'ERA5',     'CHIRPS', 'NCEP','MERRA2', 'CPC',
             'PRECL', 'CAMS_OPI', 'GPCC',   'GPCP','JRA55')

# brk_cor <- seq(-1, 1, 0.2)
# col_cor <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))

start_dates = c(5, 7, 8, 10, 2, 4, 1, 11)

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    
     for (forst in 1:length(forecast)) {
       pd = forecast[forst]

      
      for (istart_date in 1:length(start_dates)) {
        start_date = start_dates[istart_date]
        

        if (start_date == 7) {
          target_season = 'JJA'
        } else if (start_date == 5) {
          target_season = 'JJA'
        } else if (start_date == 8) {
          target_season = 'SON'
        } else if (start_date == 10) {
          target_season = 'SON'
        } else if (start_date == 2) {
          target_season = 'MAM'
        } else if (start_date == 4) {
          target_season = 'MAM'
        } else if (start_date == 1) {
          target_season = 'DJF'
        } else if (start_date == 11) {
          target_season = 'DJF'
        }
        
        
        
        if (target_season == 'MAM') {
          mesi_8 = which(mesi== 05)
        } else if (target_season == 'JJA') {
          mesi_8 = which(mesi == 08)
        } else if (target_season == 'SON') {
          mesi_8 = which(mesi == 11)
        } else if (target_season == 'DJF') {
          mesi_8 = which(mesi == 02)
        }
      
      
      brk <- seq(-1, 1, 0.2)
      col <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))
      
      nam <- paste('corre2')
      print(nam)
      obs_bs = array(data = NA, dim = c(length(lon), length(lat2), 11))
      
      load(file.path(paste(dir_out,"/COR_S5_spi",sc,"_",sprintf("%02d", start_date),
                           "_",  target_season,"_GPCP_original.RData", sep = "") ))
      aux = get(nam)
      obs_bs[, , 1] = aux[,]
      
      load(file.path(paste(dir_out,"/COR_", pd, "_spi",sc,"_",sprintf("%02d", start_date),
                           "_",  target_season,"_CAMS_OPI_original.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 2] = aux[ , ]
      
      load(file.path(paste(dir_out,"/COR_", pd, "_spi",sc,"_",sprintf("%02d", start_date),
                           "_",  target_season,"_CHIRPS_original.RData",sep = "") ))
      aux = get(nam)
      obs_bs[, , 3] = aux[ , ]
      
      load(file.path(paste(dir_out,"/COR_", pd, "_spi",sc,"_",sprintf("%02d", start_date),
                           "_",  target_season,"_CPC_original.RData",sep = "")  ))
      aux = get(nam)
      obs_bs[, , 4] = aux[ , ]
      
      load(file.path(paste(dir_out,"/COR_", pd, "_spi",sc,"_",sprintf("%02d", start_date),
                           "_",  target_season,"_GPCC_original.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 5] = aux[ , ]
      
      load(file.path(paste(dir_out,"/COR_", pd, "_spi",sc,"_",sprintf("%02d", start_date),
                           "_",  target_season,"_JRA55_original.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 6] = aux[ , ]
      
      load(file.path( paste(dir_out,"/COR_", pd, "_spi",sc,"_",sprintf("%02d", start_date),
                            "_",  target_season,"_PRECL_original.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 7] = aux[ , ]
      
      load(file.path(paste(dir_out,"/COR_", pd, "_spi",sc,"_",sprintf("%02d", start_date), 
                           "_",  target_season,"_ERA5_original.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 8] = aux[ , ]
      
      load(file.path(paste(dir_out,"/COR_", pd, "_spi",sc,"_",sprintf("%02d", start_date),
                           "_",  target_season,"_NCEP_original.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 9] = aux[ , ]
      
      load(file.path(paste(dir_out,"/COR_", pd, "_spi",sc,"_",sprintf("%02d", start_date),
                           "_",  target_season,"_MERRA2_original.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 10] = aux[ , ]
      
      load(file.path(paste(dir_out,"/COR_", pd, "_spi",sc,"_",sprintf("%02d", start_date),
                           "_",  target_season, "_MSWEP_original.RData", sep = "")  ))
      aux = get(nam)
      obs_bs[, , 11] = aux[ , ]
      
      bs_mean = (apply(obs_bs, c(1, 2), mean, na.rm = TRUE))
      #bs_mean = bs_mean[, which(lat > -60 & lat < 85)]

      
      postscript(
        file.path(dir_out,paste("COR_", pd, "_spi",sc,"_",sprintf("%02d", start_date),
                                "_",  target_season,"_ens_mean_drop_original.eps",sep = "")),
        paper = "special",
        width = 11,
        height = 7,
        horizontal = T
      )
      
    
      layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
      par(oma = c(1, 1, 4, 1))
      tit <-
        paste(
           'Correlation for SPI6 in ',target_season,'; ', pd, ' against  DROP members'
           #\n Start date: ',
          # month.name[ start_date],
          # ' - Period: 1981/2020\n'
          , sep=""
        )
      
      
      mioplot(bs_mean, lon,lat2,
              toptitle = '',
              sizetit = 0.8,
              brks = brk,  
              cols = col,
              axelab =F, 
              filled.continents = FALSE,
              drawleg = F)
      title(tit, line = 0.5, outer = T)
      ce = 1.4
      ColorBar(
        brks = brk,
        cols = rev(col),
        vert = T
      )
      
      save(corre2, file = file.path(dir_out,
                                    paste("COR_", pd, "_spi",sc,"_",
                                          sprintf("%02d", start_date),  "_",
                                          target_season,"_ENS_MEAN_original.RData",
                                          sep = "") ))
      dev.off()
      
   }
   }
}
}

