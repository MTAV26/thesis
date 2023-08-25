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
cth = c('moderate_drought')

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


lt=c()
nb = 1000
start_dates = c(5, 7, 8, 10, 2, 4, 1, 11)

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
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

    if (start_date == 11) { lt= " four "
    } else if (start_date == 2) { lt = " four "
    } else if (start_date == 5) { lt = " four "
    } else if (start_date == 8) { lt = " four "
    } else if (start_date == 1) { lt = " two "
    } else if (start_date == 4) { lt = " two "
    } else if (start_date == 7) { lt = " two "
    } else if (start_date == 10) { lt = " two "
    } else {
      print('dataset not known')
    }

    for (ith in 1:length(thresholds)) {
      #for (ith in 1:1) {
      th = thresholds[ith]
      print(th)
      roc_box <-
        matrix(data = NA,
               nrow = nb,
               ncol = length(reg))
      for (idata in 1:length(reg)) {
        region = reg[idata]
        print(region)
        load(paste(dir_out, "/reliability_ESP_", region, "_", (th), "_spi", sc,"_",sprintf("%02d", start_date),  "_",  target_season, ".RData", sep = ""))
        roc_box[, idata] = (rel$slope1)
      }
      
      roc_box2 <-
        matrix(data = NA,
               nrow = nb,
               ncol = length(reg))
      for (idata in 1:length(reg)) {
        region = reg[idata]
        print(region)
        load(paste(dir_out, "/reliability_S5_", region, "_", (th), "_spi", sc,"_",sprintf("%02d", start_date),  "_",  target_season, ".RData", sep = ""))
        roc_box2[, idata] = (rel2$slope1)
      }
      
      
      plot_data <-
        data.frame(
          roc_box[, 1],roc_box2[, 1], roc_box[, 2],roc_box2[, 2],roc_box[, 3],roc_box2[, 3],
          roc_box[, 4], roc_box2[, 4],roc_box[, 5],roc_box2[, 5],roc_box[, 6],roc_box2[, 6],
          roc_box[, 7], roc_box2[, 7],roc_box[, 8],roc_box2[, 8],roc_box[, 9],roc_box2[, 9],
          roc_box[, 10],roc_box2[, 10],roc_box[, 11],roc_box2[, 11],roc_box[, 12],roc_box2[, 12],
          roc_box[, 13],roc_box2[, 13],roc_box[, 14],roc_box2[, 14],roc_box[, 15],roc_box2[, 15],
          roc_box[, 16], roc_box2[, 16],roc_box[, 17],roc_box2[, 17],roc_box[, 18],roc_box2[, 18],
          roc_box[, 19],roc_box2[, 19],roc_box[, 20],roc_box2[, 20],roc_box[, 21],roc_box2[, 21]
        )
      
      setEPS()
       pdf(file.path(dir_out, paste("boxplot_reliability_", (cth[ith]), "_spi", sc,"_",sprintf("%02d", start_date),  "_",  target_season,  ".pdf", sep = "")),
          width = 6.5, height = 4.5)

      boxplotperc(
        na.omit(plot_data),
        quant = c(0.025, 0.975),
        outline = FALSE,
        las = 2,
        ylab="Reliability Slope",
        ylim = c(-0.1, 1.8),
        main=paste('Forecast for ',target_season,'; Lead time:', lt, 'months', sep=""),
        cex.main = 1,
        at=c(1,2,4,5,7,8,10,11,13,14,16,17,19,20,
        22,23,25,26,28,29,31,32,34,35,37,38,40,41,
        43,44,46,47,49,50,52,53,55,56,58,59,61,62),
        col=c('blue','green','blue','green',
              'blue','green','blue','green',
              'blue','green','blue','green',
              'blue','green','blue','green',
              'blue','green','blue','green',
              'blue','green','blue','green',
              'blue','green','blue','green',
              'blue','green','blue','green',
              'blue','green','blue','green',
              'blue','green','blue','green',
              'blue','green'),
        
        names =  c(
          'AUS', 'AUS', 'AMZ', 'AMZ', 'SSA', 'SSA', 'CAM', 'CAM', 'WNA', 'WNA',
          'CNA', 'CNA', 'ENA', 'ENA', 'ALA', 'ALA', 'GRL', 'GRL', 'MED', 'MED',
          'NEU', 'NEU', 'WAF', 'WAF', 'EAF', 'EAF', 'SAF', 'SAF', 'SAH', 'SAH',
          'SEA', 'SEA', 'EAS', 'EAS', 'SAS', 'SAS', 'CAS', 'CAS', 'TIB', 'TIB',
          'NAS', 'NAS'
        )
      )
      
      abline(v = c(0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63), lty = 6,col = "gray")
      abline(h = c(0, 0.5, 1.0, 1.5), lty = 6,col = "gray")
      abline(h = 1, lwd = 1)
      dev.off()
    }
  }
}
dev.off()
