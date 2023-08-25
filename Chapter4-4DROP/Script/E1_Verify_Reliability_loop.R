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
 
start_dates = c(4)
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
    paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season, "_GPCP.RData", sep = "")))
  aux = get(nam)
  pred[, , , ,1] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss,'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season, "_CAMS_OPI.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,2] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(
      dir_oss,'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season, "_CHIRPS.RData",sep = "") ))
  aux = get(nam)
  pred[, , , ,3] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(
      dir_oss,'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season, "_CPC.RData",sep = "") ))
  aux = get(nam)
  pred[, , , ,4] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season, "_GPCC.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,5] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season, "_JRA55.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , ,6] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season, "_PRECL.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,7] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season, "_ERA5.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,8] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season, "_NCEP.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,9] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss,'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season, "_MERRA2.RData", sep = "") ))
  aux = get(nam)
  pred[, , , ,10] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss,'SPI',sc,'ESP_', sprintf("%02d", start_date),  "_",  target_season, "_MSWEP.RData", sep = "") ))
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
    paste(dir_oss, 'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season, "_GPCP.RData", sep = "")))
  aux = get(nam)
  pred2[, , , ,1] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss,'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season, "_CAMS_OPI.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,2] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(
      dir_oss,'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season, "_CHIRPS.RData",sep = "") ))
  aux = get(nam)
  pred2[, , , ,3] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(
      dir_oss,'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season, "_CPC.RData",sep = "") ))
  aux = get(nam)
  pred2[, , , ,4] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season, "_GPCC.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,5] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season, "_JRA55.RData", sep = "")  ))
  aux = get(nam)
  pred2[, , , ,6] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season, "_PRECL.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,7] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season, "_ERA5.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,8] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss, 'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season, "_NCEP.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,9] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss,'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season, "_MERRA2.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,10] = aux[, ,mesi_8,]
  
  load(file.path(
    paste(dir_oss,'SPI',sc,'SEAS5_', sprintf("%02d", start_date),  "_",  target_season, "_MSWEP.RData", sep = "") ))
  aux = get(nam)
  pred2[, , , ,11] = aux[, ,mesi_8,]
  
  pred2[is.infinite(pred2)] <- NA
  pred2[is.na(pred2)] <- NA
  pred2 = (apply(pred2, c(1, 2, 3, 5), mean, na.rm = TRUE))
  
  
  for (ith in 1:length(thresholds)) {
    th = thresholds[ith]
    print(th)
    
    for (izone in 1:dim(coordreg)[1]) {
      zone = reg[izone]
      print(zone)
      
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
      ## reliability
      ###############
      rel_s4 <- myreliability(obs, pred, th, pstep, lat, inout1)
      freq_s4 = as.numeric(unlist(rel_s4[1]))
      h_s4 = as.numeric(unlist(rel_s4[2]))
      g_s4 = as.numeric(unlist(rel_s4[3]))
      slope_s4 = as.numeric(unlist(rel_s4[4]))
      obs.clim = sum(g_s4) / sum(h_s4)
      
      ##boot
      slope_s4_b = matrix(NA, nb, 1)

      if (!is.na(slope_s4)) {
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

          rel_b <-
            myreliability(obs_b, pred_b, th, pstep, lat, inout1)
          slope_s4_b[ib] = as.numeric(unlist(rel_b[4]))
          rm(rel_b)
        }
      }
      
      rel <- list()
      rel$slope1 <- slope_s4_b
     
      save(rel, file = file.path(
        dir_out,
        paste("reliability_ESP_", zone, "_", (th), "_spi", sc,"_", sprintf("%02d", start_date),  "_",  target_season,  ".RData", sep = "")
      ))
      
      
      ###############
      ## reliability
      ##############
      rel_s5 <- myreliability(obs, pred2, th, pstep, lat, inout1)
      freq_s5 = as.numeric(unlist(rel_s5[1]))
      h_s5 = as.numeric(unlist(rel_s5[2]))
      g_s5 = as.numeric(unlist(rel_s5[3]))
      slope_s5 = as.numeric(unlist(rel_s5[4]))
      obs.clim = sum(g_s5) / sum(h_s5)
      
      ##boot
      slope_s5_b = matrix(NA, nb, 1)
      
      if (!is.na(slope_s5)) {
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
 
          rel_b2 <-
            myreliability(obs_b, pred_b2, th, pstep, lat, inout1)
          slope_s5_b[ib] = as.numeric(unlist(rel_b2[4]))
          rm(rel_b2)
  
        }
      }

      rel2 <- list()
      rel2$slope1 <- slope_s5_b
      
      save(rel2, file = file.path(
        dir_out,
        paste("reliability_S5_", zone, "_", (th), "_spi", sc,"_",sprintf("%02d", start_date),  "_",  target_season,  ".RData", sep = "")
      ))
 
      pdf(file.path(dir_out, paste("reliability_", (th), "_",reg[izone],"_spi",sc,"_",sprintf("%02d", start_date),  "_",  target_season,".pdf",sep = "")),
          width = 6.5, height = 4.5)
      
      old.par <- par(no.readonly = TRUE)
      on.exit(par(old.par))
      
      slope1.stat <- quantile(slope_s4_b, probs = c(.025, 0.5, .975), na.rm = TRUE)
      inter1.stat1 <- obs.clim * (1 - slope1.stat[1])
      inter1.stat2 <- obs.clim * (1 - slope1.stat[2])
      inter1.stat3 <- obs.clim * (1 - slope1.stat[3])
      
      slope2.stat <- quantile(slope_s5_b, probs = c(.025, 0.5, .975), na.rm = TRUE)
      inter2.stat1 <- obs.clim * (1 - slope2.stat[1])
      inter2.stat2 <- obs.clim * (1 - slope2.stat[2])
      inter2.stat3 <- obs.clim * (1 - slope2.stat[3])
      
      plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        xlab = "Forecast probability",
        ylab = "Observed relative frequency"
      )
      a <- (1 - obs.clim) / 2 + obs.clim
      b <- obs.clim / 2
      x.p <- c(obs.clim, obs.clim, 1, 1, 0, 0)
      y.p <- c(0, 1, 1, a, b, 0)
      polygon(x.p, y.p, col = "#e6e6e6")
      abline(h = obs.clim, lty = 2)
      text(0.9, obs.clim, "No resolution", pos = 3)
      text(
        0.9,
        obs.clim + (a - b) * (0.9 - obs.clim),
        "No skill",
        pos = 1,
        srt = atan(a - b) / (2 * pi) *
          360
      )
      
      pointxlow1 = c(max(0,-inter1.stat1 / slope1.stat[1]), min(1, (1 - inter1.stat1) / slope1.stat[1]))
      if (pointxlow1[1]>1) {pointxlow1 = c(0,1)}
      pointxhigh1 = c(max(0,-inter1.stat3 / slope1.stat[3]),min(1, (1 - inter1.stat3) / slope1.stat[3]))
      
      pointxlow2 = c(max(0,-inter2.stat1 / slope2.stat[1]), min(1, (1 - inter2.stat1) / slope2.stat[1]))
      pointxhigh2 = c(max(0,-inter2.stat3 / slope2.stat[3]),min(1, (1 - inter2.stat3) / slope2.stat[3]))
      
      y1 = 1
      y3 = 1
      
      if (slope1.stat[1] <= 1) {
        y1 = inter1.stat1 + slope1.stat[1]
      }
      if (slope1.stat[3] <= 1) {
        y3 = inter1.stat3 + slope1.stat[3]
      }
      pointylow1 = c(max(0, inter1.stat1), y1)
      pointyhigh1 = c(max(0, inter1.stat3), y3)
      
      y1 = 1
      y3 = 1
      
      if (slope2.stat[1] <= 1) {
        y1 = inter2.stat1 + slope2.stat[1]
      }
      if (slope2.stat[3] <= 1) {
        y3 = inter2.stat3 + slope2.stat[3]
      }
      
      pointylow2 = c(max(0, inter2.stat1), y1)
      pointyhigh2 = c(max(0, inter2.stat3), y3)
      
      polygon(
        c(pointxhigh1, rev(pointxlow1)),
        c(pointyhigh1, rev(pointylow1)),
        col =  rgb(0,0,1,0.6),
        border = NA
      )
      
      polygon(
        c(pointxhigh2, rev(pointxlow2)),
        c(pointyhigh2, rev(pointylow2)),
        col = rgb(0,1,0,0.6),
        border = NA
      )
      
      legend("topleft",inset=.04,
             c("4DROP","S5"), fill=topo.colors(3), horiz=F, cex=0.9)

      points(
        prob2 - (pstep / 2),
        freq_s4,
        col = "black",
        pch = 21,
        bg = "blue",
        lwd = 2,
        type = "p"
      ) 
      
      points(
        prob2 - (pstep / 2),
        freq_s5,
        col = "black",
        pch = 21,
        bg = "green",
        lwd = 2,
        type = "p"
      ) 

      lines(c(0, 1), c(0, 1), lty = 1)
      
      pp <- par("plt")
      par(plt = c(pp[2] - 0.2, pp[2], pp[3], pp[3] + 0.2))
      par(new = TRUE)
      barplot(
        rbind(h_s4, h_s5),
        beside = TRUE,
        axes = FALSE,
        axisnames = FALSE,
        col = c("blue", "green")
      )
      axis(4)
      box()
   
      dev.off()

    }
  }
}



