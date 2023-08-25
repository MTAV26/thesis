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
source("~/Chapter4-4DROP/Data/common/my_boxplot_stat.R")
source("~/Chapter4-4DROP/Data/common/my_boxplot.R")
source("~/Chapter4-4DROP/Data/common/ReliabilityDiagram_MIO2.R")
source("~/Chapter4-4DROP/Data/common/myreliability.R")
##################################################
# Definición de directorios, carga de coordenadas y mascara
dir_drop = '~/Chapter4-4DROP/Data/'
dir_oss  = '~/Chapter4-4DROP/Data/'
dir_out = '~/Chapter4-4DROP/Figures/'
load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "inout.RData"))
ni = length(lon)
nj = length(lat)

data(wrld_simpl)

anni = 1981:2020
mesi = rep(1:12, length(anni))
nb = 1000
time_scale = c(6)
sc=6
thresholds = c(-0.8)

pstep = 0.2
prob1 = seq(0, 1 - pstep, pstep)
prob1[1] = -prob1[length(prob1)]
prob2 = seq(0 + pstep, 1, pstep)

regions <- c(
  'Australia', 'Amazon Basin', 'Southern South America', 'Central America',
  'Western North America', 'Central North America', 'Eastern North America',
  'Alaska', 'Greenland', 'Mediterranean Basin', 'Northern Europe',
  'Western Africa', 'Eastern Africa', 'Southern Africa', 'Sahara',
  'Southeast Asia', 'East Asia', 'South Asia', 'Central Asia',
  'Tibet', 'North Asia'
)

reg <- c(
  'AUS', 'AMZ', 'SSA', 'CAM', 'WNA',
  'CNA', 'ENA', 'ALA', 'GRL', 'MED',
  'NEU', 'WAF', 'EAF', 'SAF', 'SAH',
  'SEA', 'EAS', 'SAS', 'CAS', 'TIB',
  'NAS'
)

coordreg <- matrix(
  c(
    -45,-11,  110, 155,
    -20, 12, -82, -34,
    -56,-20, -76, -40,
    10, 30, -116,-83,
    30, 60, -130,-103,
    30, 50, -103,-85,
    25, 50, -85, -60,
    60, 72, -170,-103,
    50, 85, -103,-10,
    30, 48, -10,  40,
    48, 75, -10,  40,
    -12, 18, -20,  22,
    -12, 18,  22,  52,
    -35,-12, -10,  52,
    18, 30, -20,  65,
    -11, 20,  95,  155,
    20, 50,  100, 145,
    5, 30,  65,  100,
    30, 50,  40,  75,
    30, 50,  75,  100,
    50, 70,  40,  180
  ),
  nrow = length(reg),
  ncol = 4,
  byrow = TRUE
)

colgr<- alpha("green", 0.5)

start_dates = c(2)
#start_dates = c(5, 7, 8, 10, 2, 4, 1, 11)
  
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

  load(file.path(
    paste(dir_drop,"SPI6_ENS_1981_2020.RData", sep = "")  ))
  obs = spi[,,mesi_8]
  
  nam <- paste("spi", sc,"pred", sep = "")
  print(nam)
  
  ## load data
  mb=c(1:39) 
  dt=c(1:11)
  
  ## load data
  pred = array(data = NA, dim = c(dim(obs)[1], dim(obs)[2], dim(obs)[3], length(mb), length(dt)))
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season,  "_GPCP.RData", sep = "")))
  aux = get(nam)
  pred[, , , ,1] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss,'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season,  "_CAMS_OPI.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,2] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(
      dir_oss,'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season,  "_CHIRPS.RData",sep = "") ))
  aux = get(nam)
  pred[, , , ,3] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(
      dir_oss,'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season,  "_CPC.RData",sep = "") ))
  aux = get(nam)
  pred[, , , ,4] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season,  "_GPCC.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,5] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season,  "_JRA55.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , ,6] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season,  "_PRECL.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,7] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season,  "_ERA5.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,8] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season,  "_NCEP.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,9] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss,'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season,  "_MERRA2.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,10] = aux[, ,mesi_8,]

  load(file.path(
    paste(dir_oss,'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season,  "_MSWEP.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,11] = aux[, ,mesi_8,]
  
  pred[is.infinite(pred)] <- NA
  pred[is.na(pred)] <- NA
  
  pred = (apply(pred, c(1, 2, 3, 5), mean, na.rm = TRUE))
  
  mb=c(1:25)
  dt=c(1:11)
  
  ## load data
  pred2 = array(data = NA, dim = c(dim(obs)[1], dim(obs)[2], dim(obs)[3], length(mb), length(dt)))
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season,  "_GPCP.RData", sep = "")))
  aux = get(nam)
  pred2[, , , ,1] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss,'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season,  "_CAMS_OPI.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,2] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(
      dir_oss,'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season,  "_CHIRPS.RData",sep = "") ))
  aux = get(nam)
  pred2[, , , ,3] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(
      dir_oss,'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season,  "_CPC.RData",sep = "") ))
  aux = get(nam)
  pred2[, , , ,4] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season,  "_GPCC.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,5] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season,  "_JRA55.RData", sep = "")  ))
  aux = get(nam)
  pred2[, , , ,6] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season,  "_PRECL.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,7] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season,  "_ERA5.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,8] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season,  "_NCEP.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,9] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss,'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season,  "_MERRA2.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,10] = aux[, ,mesi_8,]

  load(file.path(
    paste(dir_oss,'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season,  "_MSWEP.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,11] = aux[, ,mesi_8,]
  
  pred2[is.infinite(pred2)] <- NA
  pred2[is.na(pred2)] <- NA
  pred2 = (apply(pred2, c(1, 2, 3, 5), mean, na.rm = TRUE))
  

  for (ith in 1:length(thresholds)) {
    th = thresholds[ith]
    print(th)
    for (izone in 1:dim(coordreg)[1]) {
      #for (izone in 1:21) {
      zona = regions[izone]
      print(zona)

      inout1 = inout
      idxlon = which(coordreg[izone, 3] <= lon &
                       lon <= coordreg[izone, 4])
      idxlat = which(coordreg[izone, 1] <= lat &
                       lat <= coordreg[izone, 2])
      inout1[idxlon, idxlat] = 1
      
      inout1 = inout1 + inout
      inout1[inout1 != 1] = NaN
      image.plot(lon, lat, inout1)
      plot(wrld_simpl, add = TRUE)
      
      ###############
      ## roc
      ###############
      
      roc_1 <- myroc(obs, pred, th, pstep, lat, inout1)
      hr_1 = as.numeric(unlist(roc_1[1]))
      far_1 = as.numeric(unlist(roc_1[2]))
      hr_1[length(hr_1) + 1] = 0
      far_1[length(far_1) + 1] = 0
      
      plot(far_1, hr_1, xlim = c(0, 1), ylim = c(0, 1))
      
      ##boot
      far_b_1 = matrix(NA, nb, length(hr_1) - 1)
      hr_b_1 = matrix(NA, nb, length(hr_1) - 1)
      auc_b_1 = matrix(NA, nb, 1)
      
      for (ib in 1:nb) {
        cat('Processing ', ib, 'of', nb, 'boostraps', '\n')
        
        ind <-
          sample(1:dim(obs)[3],
                 size = dim(obs)[3],
                 replace = TRUE)
        
        obs_b = obs * NA
        for (i in 1:ni) {
          for (j in 1:nj) {
            obs_b[i, j, ] = obs[i, j, ind]
          }
        }
        
        pred_b = pred * NA
        for (i in 1:ni) {
          for (j in 1:nj) {
            for (k in 1:dim(pred)[4]) {
              pred_b[i, j, , k] = pred[i, j, ind, k]
            }
          }
        }
        
        roc_b <- myroc(obs_b, pred_b, th, pstep, lat, inout1)
        hr_b_1[ib,] = as.numeric(unlist(roc_b[1]))
        far_b_1[ib,] = as.numeric(unlist(roc_b[2]))
        auc_b_1[ib] = as.numeric(unlist(roc_b[3]))
        rm(roc_b)

      } 

      hr_ci_1 = matrix(0, dim(hr_b_1)[2] + 1, 2)
      far_ci_1 = matrix(0, dim(hr_b_1)[2] + 1, 2)
      for (i in 1:dim(hr_b_1)[2]) {
        hr_ci_1[i,] = as.numeric(quantile(hr_b_1[, i], c(0.025, 0.975), na.rm = TRUE))
        far_ci_1[i,] = as.numeric(quantile(far_b_1[, i], c(0.025, 0.975), na.rm = TRUE))
      }
      
      roc_1 = auc_b_1
      
      save(roc_1, file = file.path(
        dir_out,
        paste("rocarea_ESP_",
              (th), "_",
              reg[izone],
              "_spi", sc,"_", sprintf("%02d", start_date),  "_",  target_season,
              ".RData",
              sep = "")
      ))
      
      ###############
      ## roc
      ###############
      
      roc_2 <- myroc(obs, pred2, th, pstep, lat, inout1)
      hr_2 = as.numeric(unlist(roc_2[1]))
      far_2 = as.numeric(unlist(roc_2[2]))
      hr_2[length(hr_2) + 1] = 0
      far_2[length(far_2) + 1] = 0
      
      plot(far_2, hr_2, xlim = c(0, 1), ylim = c(0, 1))
      
      ##boot
      far_b_2 = matrix(NA, nb, length(hr_2) - 1)
      hr_b_2 = matrix(NA, nb, length(hr_2) - 1)
      auc_b_2 = matrix(NA, nb, 1)
      
      for (ib in 1:nb) {
        cat('Processing ', ib, 'of', nb, 'boostraps', '\n')
        
        ind <-
          sample(1:dim(obs)[3],
                 size = dim(obs)[3],
                 replace = TRUE)
        
        obs_b = obs * NA
        for (i in 1:ni) {
          for (j in 1:nj) {
            obs_b[i, j, ] = obs[i, j, ind]
          }
        }
        
        pred_b2 = pred2 * NA
        for (i in 1:ni) {
          for (j in 1:nj) {
            for (k in 1:dim(pred2)[4]) {
              pred_b2[i, j, , k] = pred2[i, j, ind, k]
            }
          }
        }

        roc_b2 <- myroc(obs_b, pred_b2, th, pstep, lat, inout1)
        hr_b_2[ib,] = as.numeric(unlist(roc_b2[1]))
        far_b_2[ib,] = as.numeric(unlist(roc_b2[2]))
        auc_b_2[ib] = as.numeric(unlist(roc_b2[3]))
        rm(roc_b2)
      }

      hr_ci_2 = matrix(0, dim(hr_b_2)[2] + 1, 2)
      far_ci_2 = matrix(0, dim(hr_b_2)[2] + 1, 2)
      for (i in 1:dim(hr_b_2)[2]) {
        hr_ci_2[i,] = as.numeric(quantile(hr_b_2[, i], c(0.025, 0.975), na.rm = TRUE))
        far_ci_2[i,] = as.numeric(quantile(far_b_2[, i], c(0.025, 0.975), na.rm = TRUE))
      }

      roc_2 = auc_b_2
      save(roc_2, file = file.path(
        dir_out,
        paste("rocarea_S5_",
              (th), "_",
              reg[izone],
              "_spi", sc,"_", sprintf("%02d", start_date),  "_",  target_season,
              ".RData",
              sep = "")
      ))
     
      pdf(file.path(dir_out, paste("roc_", (th), "_",reg[izone],"_spi",sc,"_", sprintf("%02d", start_date),  "_",  target_season, ".pdf",sep = "")),
          width = 5.5, height = 5.5) #units = "px", pointsize = 12,

      par(pty = "s")

      plot( far_1, hr_1, xlim = c(0, 1), ylim = c(0, 1), col = "white", xlab = "False Alarm Rate", ylab = "Hit Rate" )
      
      polygon(c(far_1, rev(far_1)), c(hr_ci_1[, 1], rev(hr_ci_1[, 2])), col = rgb(0, 0, 1), border = NA)
      polygon(c(far_2, rev(far_2)), c(hr_ci_2[, 1], rev(hr_ci_2[, 2])), col = colgr, border = NA)
  
      lines(
        x = c(0, 1),
        y = c(0, 1),
        col = "darkgrey",
        lwd = identity.lwd,
        lty = identity.lty
      )
      
      pp <- par("plt")
      par(plt = c(pp[2] - 0.4, pp[2], pp[3], pp[3] + 0.28))
      par(new = TRUE)
      colgry<- alpha("gray", 0.2)
      image(lon, lat, inout1, col="brown", ylab="", xlab="", xaxt="n", yaxt="n")
      plot(wrld_simpl,col=colgry, add=TRUE)
      
      segments(x0 = 110, x1 = 155, y0 = -45, y1 = -45, lwd = 4, col = "red", lty = 1) 
      segments(x0 = 110, x1 = 155, y0 = -11, y1 = -11, lwd = 4, col = "red", lty = 1)
      segments(x0 = 110, x1 = 110, y0 = -11, y1 = -45, lwd = 4, col = "red", lty = 1)
      segments(x0 = 155, x1 = 155, y0 = -11, y1 = -45, lwd = 4, col = "red", lty = 1)
      text( 135, -25, "AUS", col="red", cex=0.5)
      

      segments(x0 = -82, x1 = -34, y0 = -20, y1 = -20, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -82, x1 = -34, y0 = 12, y1 = 12, lwd = 4, col = "red", lty = 1)
      segments(x0 = -82, x1 = -82, y0 = -20, y1 = 12, lwd = 4, col = "red", lty = 1)
      segments(x0 = -34, x1 = -34, y0 = -20, y1 = 12, lwd = 4, col = "red", lty = 1)
      text( -55, -5, "AMZ", col="red", cex=0.5)

      segments(x0 = -76, x1 = -40, y0 = -56, y1 = -56, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -76, x1 = -40, y0 = -20, y1 = -20, lwd = 4, col = "red", lty = 1)
      segments(x0 = -76, x1 = -76, y0 = -56, y1 = -20, lwd = 4, col = "red", lty = 1)
      segments(x0 = -40, x1 = -40, y0 = -56, y1 = -20, lwd = 4, col = "red", lty = 1)
      text( -55, -35, "SSA", col="red", cex=0.5)

      segments(x0 = -116, x1 = -83, y0 = 10, y1 = 10, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -116, x1 = -83, y0 = 30, y1 = 30, lwd = 4, col = "red", lty = 1)
      segments(x0 = -116, x1 = -116, y0 = 10, y1 = 30, lwd = 4, col = "red", lty = 1)
      segments(x0 = -83, x1 = -83, y0 = 10, y1 = 30, lwd = 4, col = "red", lty = 1)
      text( -100, 20, "CAM", col="red", cex=0.4)
      # 10, 30, -116, -83,
      segments(x0 = -130, x1 = -103, y0 = 30, y1 = 30, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -130, x1 = -103, y0 = 60, y1 = 60, lwd = 4, col = "red", lty = 1)
      segments(x0 = -130, x1 = -130, y0 = 30, y1 = 60, lwd = 4, col = "red", lty = 1)
      segments(x0 = -103, x1 = -103, y0 = 30, y1 = 60, lwd = 4, col = "red", lty = 1)
      text( -116, 45, "WNA", col="red", cex=0.4) 
      #     30, 60, -130,  -103,
      segments(x0 = -103, x1 = -85, y0 = 30, y1 = 30, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -103, x1 = -85, y0 = 50, y1 = 50, lwd = 4, col = "red", lty = 1)
      segments(x0 = -103, x1 = -103, y0 = 30, y1 = 50, lwd = 4, col = "red", lty = 1)
      segments(x0 = -85, x1 = -85, y0 = 30, y1 = 50, lwd = 4, col = "red", lty = 1)
      text( -93, 40, "CNA", col="red", cex=0.2) 
      #     30, 50,  -103,  -85,
      segments(x0 = -85, x1 = -60, y0 = 25, y1 = 25, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -85, x1 = -60, y0 = 50, y1 = 50, lwd = 4, col = "red", lty = 1)
      segments(x0 = -85, x1 = -85, y0 = 25, y1 = 50, lwd = 4, col = "red", lty = 1)
      segments(x0 = -60, x1 = -60, y0 = 25, y1 = 50, lwd = 4, col = "red", lty = 1)
      text( -72, 37, "ENA", col="red", cex=0.4) 
      #     25,  50,  -85,  -60,
      segments(x0 = -170, x1 = -103, y0 = 60, y1 = 60, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -170, x1 = -103, y0 = 72, y1 = 72, lwd = 4, col = "red", lty = 1)
      segments(x0 = -170, x1 = -170, y0 = 60, y1 = 72, lwd = 4, col = "red", lty = 1)
      segments(x0 = -103, x1 = -103, y0 = 60, y1 = 72, lwd = 4, col = "red", lty = 1)
      text( -135, 66, "ALA", col="red", cex=0.5)   
      #     60,  72,  -170,  -103,
      segments(x0 = -103, x1 = -10, y0 = 50, y1 = 50, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -103, x1 = -10, y0 = 83, y1 = 83, lwd = 4, col = "red", lty = 1)
      segments(x0 = -103, x1 = -103, y0 = 50, y1 =83, lwd = 4, col = "red", lty = 1)
      segments(x0 = -10, x1 = -10, y0 = 50, y1 = 83, lwd = 4, col = "red", lty = 1)
      text( -60, 68, "GRL", col="red", cex=0.5)   
      #     50,  85, -103,  -10,
      segments(x0 = -10, x1 = 40, y0 = 30, y1 = 30, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -10, x1 = 40, y0 = 48, y1 = 48, lwd = 4, col = "red", lty = 1)
      segments(x0 = -10, x1 = -10, y0 = 30, y1 =48, lwd = 4, col = "red", lty = 1)
      segments(x0 = 40, x1 = 40, y0 = 30, y1 = 48, lwd = 4, col = "red", lty = 1)
      text( 15, 40, "MED", col="red", cex=0.5) 
      #30, 48, -10, 40,
      segments(x0 = -10, x1 = 40, y0 = 48, y1 = 48, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -10, x1 = 40, y0 = 75, y1 = 75, lwd = 4, col = "red", lty = 1)
      segments(x0 = -10, x1 = -10, y0 = 48, y1 =75, lwd = 4, col = "red", lty = 1)
      segments(x0 = 40, x1 = 40, y0 = 48, y1 = 75, lwd = 4, col = "red", lty = 1)
      text( 15, 60, "NEU", col="red", cex=0.5) 
      # 48, 75, -10, 40,
      segments(x0 = -20, x1 = 22, y0 = -12, y1 = -12, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -20, x1 = 22, y0 = 18, y1 = 18, lwd = 4, col = "red", lty = 1)
      segments(x0 = -20, x1 = -20, y0 = -12, y1 =18, lwd = 4, col = "red", lty = 1)
      segments(x0 = 22, x1 = 22, y0 = -12, y1 = 18, lwd = 4, col = "red", lty = 1)
      text( 0, 5, "WAF", col="red", cex=0.5) 
      #-12, 18,-20, 22,
      segments(x0 = 22, x1 = 52, y0 = -12, y1 = -12, lwd = 4, col = "red", lty = 1) 
      segments(x0 = 22, x1 = 52, y0 = 18, y1 = 18, lwd = 4, col = "red", lty = 1)
      segments(x0 = 22, x1 = 22, y0 = -12, y1 =18, lwd = 4, col = "red", lty = 1)
      segments(x0 = 52, x1 = 52, y0 = -12, y1 = 18, lwd = 4, col = "red", lty = 1)
      text( 38, 5, "EAF", col="red", cex=0.5)
      #-12, 18, 22, 52,
      segments(x0 = -10, x1 = 52, y0 = -35, y1 = -35, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -10, x1 = 52, y0 = -12, y1 = -12, lwd = 4, col = "red", lty = 1)
      segments(x0 = -10, x1 = -10, y0 = -35, y1 =-12, lwd = 4, col = "red", lty = 1)
      segments(x0 = 52, x1 = 52, y0 = -35, y1 = -12, lwd = 4, col = "red", lty = 1)
      text( 20, -20, "SAF", col="red", cex=0.5)
      #-35, -12, -10, 52,
      segments(x0 = -20, x1 = 65, y0 = 18, y1 = 18, lwd = 4, col = "red", lty = 1) 
      segments(x0 = -20, x1 = 65, y0 = 30, y1 = 30, lwd = 4, col = "red", lty = 1)
      segments(x0 = -20, x1 = -20, y0 = 18, y1 =30, lwd = 4, col = "red", lty = 1)
      segments(x0 = 65, x1 = 65, y0 = 18, y1 = 30, lwd = 4, col = "red", lty = 1)
      text( 22, 25, "SAH", col="red", cex=0.5)
      # 18, 30, -20, 65,
      segments(x0 = 95, x1 = 155, y0 = -11, y1 = -11, lwd = 4, col = "red", lty = 1) 
      segments(x0 = 95, x1 = 155, y0 = 20, y1 = 20, lwd = 4, col = "red", lty = 1)
      segments(x0 = 95, x1 = 95, y0 = -11, y1 =20, lwd = 4, col = "red", lty = 1)
      segments(x0 = 155, x1 = 155, y0 = -11, y1 = 20, lwd = 4, col = "red", lty = 1)
      text( 125, 5, "SEA", col="red", cex=0.5)
      #-11, 20, 95, 155,
      segments(x0 = 100, x1 = 145, y0 = 20, y1 = 20, lwd = 4, col = "red", lty = 1) 
      segments(x0 = 100, x1 = 145, y0 = 50, y1 = 50, lwd = 4, col = "red", lty = 1)
      segments(x0 = 100, x1 = 100, y0 = 20, y1 =50, lwd = 4, col = "red", lty = 1)
      segments(x0 = 145, x1 = 145, y0 = 20, y1 = 50, lwd = 4, col = "red", lty = 1)
      text( 123, 35, "EAS", col="red", cex=0.5)
      # 20, 50, 100, 145,
      segments(x0 = 65, x1 = 100, y0 = 5, y1 = 5, lwd = 4, col = "red", lty = 1) 
      segments(x0 = 65, x1 = 100, y0 = 30, y1 = 30, lwd = 4, col = "red", lty = 1)
      segments(x0 = 65, x1 = 65, y0 = 5, y1 =30, lwd = 4, col = "red", lty = 1)
      segments(x0 = 100, x1 = 100, y0 = 5, y1 = 30, lwd = 4, col = "red", lty = 1)
      text( 82, 17, "SAS", col="red", cex=0.4)
      # 5, 30,  65,  100
      segments(x0 = 40, x1 = 75, y0 = 30, y1 = 30, lwd = 4, col = "red", lty = 1) 
      segments(x0 = 40, x1 = 75, y0 = 50, y1 = 50, lwd = 4, col = "red", lty = 1)
      segments(x0 = 40, x1 = 40, y0 = 30, y1 =50, lwd = 4, col = "red", lty = 1)
      segments(x0 = 75, x1 = 75, y0 = 30, y1 = 50, lwd = 4, col = "red", lty = 1)
      text( 57, 40, "CAS", col="red", cex=0.5)
      #30, 50, 40, 75,
      segments(x0 = 75, x1 = 100, y0 = 30, y1 = 30, lwd = 4, col = "red", lty = 1) 
      segments(x0 = 75, x1 = 100, y0 = 50, y1 = 50, lwd = 4, col = "red", lty = 1)
      segments(x0 = 75, x1 = 75, y0 = 30, y1 =50, lwd = 4, col = "red", lty = 1)
      segments(x0 = 100, x1 = 100, y0 = 30, y1 = 50, lwd = 4, col = "red", lty = 1)
      text( 88, 40, "TIB", col="red", cex=0.4)
      #30,  50, 75, 100
      segments(x0 = 40, x1 = 180, y0 = 50, y1 = 50, lwd = 4, col = "red", lty = 1) 
      segments(x0 = 40, x1 = 180, y0 = 70, y1 = 70, lwd = 4, col = "red", lty = 1)
      segments(x0 = 40, x1 = 40, y0 = 70, y1 =50, lwd = 4, col = "red", lty = 1)
      segments(x0 = 180, x1 = 180, y0 = 50, y1 = 70, lwd = 4, col = "red", lty = 1)
      text( 110, 60, "NAS", col="red", cex=0.5)
      
      dev.off()

    }
  }
}



