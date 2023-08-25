# Cargar las bibliotecas necesarias
library(sp)             # Trabajo con datos espaciales
library(maptools)       # Manipulación y visualización de datos espaciales
library(RColorBrewer)   # Generación de paletas de colores
library(classInt)       # Clasificación de intervalos para variables continuas
library(fields)         # Análisis de campos y grillas
library(s2dverification)# Verificación de pronósticos
library(maps)           # Creación de mapas
library(pracma)         # Funciones matemáticas prácticas
library(verification)   # Evaluación y verificación de pronósticos
library(psych)          # Análisis psicométricos
library(ncdf4)          # Manipulación de archivos NetCDF

# Cargar scripts personalizados
source("~/Chapter4-4DROP/Data/common/ColorBarM.R") # Carga un script para crear barras de color
source("~/Chapter4-4DROP/Data/common/CorrMIO.R")   # Carga un script para calcular correlaciones
source("~/Chapter4-4DROP/Data/common/mioplot_global.R") # Carga un script para crear gráficos

# Definición de directorios, carga de coordenadas y mascara
dir_drop = '~/Chapter4-4DROP/Data/'
dir_s5  = '~/Chapter4-4DROP/Data/'
dir_out = '~/Chapter4-4DROP/Figures/'
load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "inout.RData"))


## fixed parameters
#time_scale = c(1, 3, 6, 12)
time_scale = c(6)
anni = 1981:2020
mesi = rep(1:12, length(anni))
anniok = 36 # 33/37 ?? circa il 90%

brk_cor <- seq(-1, 1, 0.2)
col_cor <- (colorRampPalette(brewer.pal(11, "PiYG"))(10))

brk <- seq(-1, 1, 0.2)
col <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))

brk_cor <- seq(-1, 1, 0.2)
col_cor <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))



datasets = c(
  # 'MSWEP')
  # 'ERA5',  'CHIRPS',   'NCEP',   'MERRA2', 'CPC',
  # 'PRECL', 'CAMS_OPI', 'GPCC',   'GPCP',   'JRA55',
  'ENS' )

# start_dates = c(5, 7, 8, 10, 2, 4, 1, 11)
start_dates = c(11)

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    
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
      
      
      nam <- paste("spi", sep = "")
      print(nam)
      load(file.path(dir_drop,paste("SPI", sc, "_ENS_1981_2020.RData", sep = ""))) # DROP
      ens = spi[,,mesi_8]
      
      
      # Calcular el promedio de los datos en cada celda y ajustar los datos
      # aux = apply(ens, c(1, 2), mean, na.rm = TRUE)
      # for (i in 1:dim(ens)[3]) {
      #   ens[, , i] = aux/aux * ens[, , i]
      # }

      nam <- paste("spi",sc,"pred", sep = "")
      print(nam)

      if (dataset == "MSWEP") {
        load(file.path(dir_s5,paste('SPI',sc,'SEAS5_',sprintf("%02d", start_date),  "_",  target_season, "_MSWEP.RData", sep = "")))
      } else if (dataset == "JRA55") {
        load(file.path(dir_s5,paste('SPI',sc,'SEAS5_',sprintf("%02d", start_date),  "_",  target_season,"_JRA55.RData",sep = "")))
      } else if (dataset == "GPCP") {
        load(file.path(dir_s5, paste('SPI',sc,'SEAS5_',sprintf("%02d", start_date),  "_",  target_season, "_GPCP.RData", sep = "")))
      } else if (dataset == "GPCC") {
        load(file.path(dir_s5, paste('SPI',sc,'SEAS5_',sprintf("%02d", start_date),  "_",  target_season,"_GPCC.RData",sep = "")))
      } else if (dataset == "CAMS_OPI") {
        load(file.path(dir_s5, paste('SPI',sc,'SEAS5_',sprintf("%02d", start_date),  "_",  target_season,"_CAMS_OPI.RData",sep = "")))
      } else if (dataset == "PRECL") {
        load(file.path(dir_s5, paste('SPI',sc,'SEAS5_',sprintf("%02d", start_date),  "_",  target_season, "_PRECL.RData",sep = "" )))
      } else if (dataset == "CPC") {
        load(file.path(dir_s5, paste('SPI',sc,'SEAS5_',sprintf("%02d", start_date),  "_",  target_season, "_CPC.RData",sep = "" )))
      } else if (dataset == "CHIRPS") {
        load(file.path(dir_s5, paste('SPI',sc,'SEAS5_',sprintf("%02d", start_date),  "_",  target_season, "_CHIRPS.RData", sep = "" )))
      } else if (dataset == "ERA5") {
        load(file.path(dir_s5, paste('SPI',sc,'SEAS5_',sprintf("%02d", start_date),  "_",  target_season,  "_ERA5.RData", sep = "" ) ))
      } else if (dataset == "NCEP") {
        load(file.path(dir_s5, paste('SPI',sc,'SEAS5_',sprintf("%02d", start_date),  "_",  target_season, "_NCEP.RData", sep = "" )))
      } else if (dataset == "MERRA2") {
        load(file.path(dir_s5, paste('SPI',sc,'SEAS5_',sprintf("%02d", start_date),  "_",  target_season, "_MERRA2.RData", sep = "" )))
      } else if (dataset == "ENS") {
        load(file.path(dir_s5, paste("S5_SPI", sc,"_",sprintf("%02d", start_date),  "_",  target_season,"_ENS.RData", sep = "" )))
      } else {
        print('dataset not known')
      }
      
      data = spi6pred
      data[is.infinite(data)]=NA
      data = get(nam)
      
      if (dataset != "ENS") {
        data = apply(spi6pred[,,mesi_8,], c(1,2,3), mean, na.rm=TRUE)  # S5
      } else if (dataset == "ENS") {
        data = spi6pred
      } else {
        print('dataset not known')
      }

      data[is.infinite(data)]=NA

      
      ni = dim(data)[1]
      nj = dim(data)[2]
      nt = dim(data)[3]
      
      corre <- matrix(data = NA,nrow = ni, ncol = nj)
      pvalue <- matrix(data = NA, nrow = ni, ncol = nj)
      corre_det <- matrix(data = NA,nrow = ni, ncol = nj)
      pvalue_det <- matrix(data = NA, nrow = ni, ncol = nj)
      
      for (i in 1:ni) {
        for (j in 1:nj) {
          
          OK <- complete.cases(ens[i, j,], data[i, j,])
          x <- ens[i, j, OK]
          y <- data[i, j, OK]
          n <- length(x)
          #if (n >= anniok * 12) {
          # if (n >= nt*0.9) {
          # if (!is.na(aux[i, j]/aux[i, j])) { 
            
            dum = CorrMIO((x), (y), method = 'pearson', pval = TRUE)
            corre[i, j] = as.numeric(dum)[1]
            pvalue[i, j] <- as.numeric(dum)[4]
            rm(dum)
            
          # }
          
          
          OK1 <- complete.cases(ens[i, j,], data[i, j,])
          x1 <- ens[i, j, OK1]
          y1 <- data[i, j, OK1]
          n1 <- length(x1)
          #if (n1 >= anniok * 12) {
          # if (n1 >= nt*0.9) {
          # if (!is.na(aux[i, j]/aux[i, j])) { 
            
            x1d = as.vector(detrend(x1, tt = 'linear', bp = c()))
            y1d = as.vector(detrend(y1, tt = 'linear', bp = c()))
            
            dum = CorrMIO((x1d), (y1d), method = 'pearson', pval = TRUE)
            corre_det[i, j] = as.numeric(dum)[1]
            pvalue_det[i, j] <- as.numeric(dum)[4]
            rm(dum)
          # }
          
          rm(OK, n, x, y, x1d, y1d, x1, y1, n1)
        }
      }
      pvalue_adj = p.adjust(pvalue, method = "fdr", n = length(pvalue[!is.na(pvalue)]))
      pvalue_adj = matrix(pvalue_adj, nrow = ni, ncol = nj)
      # 
      lat2 = lat[which(lat > -60 & lat < 85)]
      corre2 = corre[, which(lat > -60 & lat < 85)]
      pvalue2 = pvalue[, which(lat > -60 & lat < 85)]
      pvalue_adj2 = pvalue_adj[, which(lat > -60 & lat < 85)]
      
      pvalue_adj_det = p.adjust(pvalue_det, method = "fdr", n = length(pvalue_det[!is.na(pvalue_det)]))
      pvalue_adj_det = matrix(pvalue_adj_det, nrow = ni, ncol = nj)
      # 
      corre2_det = corre_det[, which(lat > -60 & lat < 85)]
      pvalue2_det = pvalue_det[, which(lat > -60 & lat < 85)]
      pvalue_adj2_det = pvalue_adj_det[, which(lat > -60 & lat < 85)]
      
      
      ## plot 1
      
      postscript(
        file.path(dir_out,paste("COR_S5_spi",sc,"_", sprintf("%02d", start_date),  "_",  target_season,"_",dataset,"_original.eps",sep = "")),
        paper = "special",
        width = 11,
        height = 7,
        horizontal = T
      )
      
      
      layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
      par(oma = c(1, 1, 4, 1))
      tit <-
        paste(
          'Correlation for SPI6 in ', dataset, ' (S5)  against DROP', sep=""
        )
      
      #\n Start date: ',
      #month.name[ start_date],
      ##' - Period: 1981/2020 \n (Original data; points: p<0.05 )', sep=""
      #  )
      
      
      
      mioplot(corre2, lon,lat2,
              toptitle = '',sizetit = 0.8,
              brks = brk_cor,  cols = col_cor,
              axelab =F, filled.continents = FALSE,
              drawleg = F,  
              #dots = pvalue2 <= 0.05,
              dots2 = pvalue_adj2 <= 0.05 
      )
      title(tit, line = 0.5, outer = T)
      ce = 1.4
      ColorBar(
        brks = brk,
        cols = col,
        vert = T
      )
      
      save(corre2, file = file.path(dir_s5, paste("COR_S5_spi",sc,"_",sprintf("%02d", start_date),  "_",  target_season,"_",dataset,"_original.RData", sep = "") ))
      save(pvalue_adj2, file = file.path(dir_s5, paste("pvalue_S5_spi",sc,"_",sprintf("%02d", start_date),  "_",  target_season,"_",dataset,"_original.RData", sep = "") ))
      
      dev.off()
      
      
      ## plot 2
      
      # brk <- seq(-1, 1, 0.2)
      # col <- (colorRampPalette(brewer.pal(11, "PiYG"))(10))
      # brk_cor <- seq(-1, 1, 0.2)
      # col_cor <- (colorRampPalette(brewer.pal(11, "PiYG"))(10))
      
      postscript(
        file.path(dir_out,paste("COR_S5_spi",sc,"_",sprintf("%02d", start_date),  "_",  target_season,"_",dataset,"_detrended.eps",sep = "")),
        paper = "special",
        width = 11,
        height = 7,
        horizontal = T
      )
      
      
      layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
      par(oma = c(1, 1, 4, 1))
      tit <-
        paste(
          'Correlation for SPI6 in ', dataset, ' (S5)  against DROP', sep=""
        )
      
      #\n Start date: ',
      #month.name[ start_date],
      ##' - Period: 1981/2020 \n (detrended data; points: p<0.05 )', sep=""
      #  )
      
      
      mioplot(corre2_det, lon,lat2,
              toptitle = '',sizetit = 0.8,
              brks = brk_cor,  cols = col_cor,
              axelab =F, filled.continents = FALSE,
              drawleg = F,  
              #dots = pvalue2_det <= 0.05,
              dots2 = pvalue_adj2_det <= 0.05 
      )
      title(tit, line = 0.5, outer = T)
      ce = 1.4
      ColorBar(
        brks = brk,
        cols = col,
        vert = T
      )
      
      save(corre2_det, file = file.path(dir_s5, paste("COR_S5_spi",sc,"_",sprintf("%02d", start_date),  "_",  target_season,"_",dataset,"_detrended.RData", sep = "") ))
      save(pvalue_adj2_det, file = file.path(dir_s5, paste("pvalue_S5_spi",sc,"_",sprintf("%02d", start_date),  "_",  target_season,"_",dataset,"_detrended.RData", sep = "") ))
      
      dev.off()
    }
  }
}


