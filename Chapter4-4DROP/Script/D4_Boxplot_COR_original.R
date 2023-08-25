# Limpieza del entorno de trabajo
rm(list = ls())
graphics.off()
gc()

library(ncdf4)        # Permite trabajar con archivos NetCDF, que son comunes para datos climáticos y geoespaciales.
library(sp)           # Proporciona clases y métodos para datos geoespaciales y permite manipulaciones espaciales.
library(maptools)     # Extiende la funcionalidad de 'sp' y proporciona herramientas para manipular y visualizar datos espaciales.
library(fields)       # Proporciona herramientas para el análisis y la visualización de campos y superficies.
library(maps)         # Proporciona datos de mapas y herramientas para trazar mapas.
library(SPEI)         # Utilizado para calcular el Índice de Sequía Estandarizado (SPEI) que mide las condiciones de sequía.
library(RColorBrewer) # Proporciona paletas de colores útiles para representar datos.
library(StatDA)

# Cargar scripts personalizados
source("~/Chapter4-4DROP/Data/common/ColorBarM.R") # Carga un script para crear barras de color
source("~/Chapter4-4DROP/Data/common/CorrMIO.R")   # Carga un script para calcular correlaciones
source("~/Chapter4-4DROP/Data/common/mioplot_global.R") # Carga un script para crear gráficos
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot_stat.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot.R")

## fixed parameters
#time_scale = c(1, 3, 6, 12)
time_scale = c(6)
# sc=6
anni = 1981:2020
mesi = rep(1:12, length(anni))

# Definición de directorios, carga de coordenadas y mascara
dir_drop = '~/Chapter4-4DROP/Data/'
dir_out = '~/Chapter4-4DROP/Figure/'
load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "inout.RData"))
lat2 = latGPCP[which(latGPCP > -60 & latGPCP < 85)]
ni = length(lon)
nj = length(lat2)



lt=c()
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
             'ENS')



mes=c("07")
est="JJA"
mesi_8 = which(mesi == 08)


for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  for (me in 1:length(mes)) {
    ms = mes[me]
    
    if (ms == "11") {
      lt= " 4"
    } else if (ms == "02") {
      lt = " 4"
    } else if (ms == "05") {
      lt = " 4"
    } else if (ms == "08") {
      lt = " 4"
    } else if (ms == "04") {
      lt = " 2"
    } else if (ms == "07") {
      lt = " 2"
    } else if (ms == "10") {
      lt = " 2"
    } else if (ms == "01") {
      lt = " 2"
    } else {
      print('dataset not known')
    }
    
    
    corre_box <-
      matrix(data = NA,
             nrow = ni * nj,
             ncol = length(datasets))
    
    
    for (idata in 1:length(datasets)) {
      dataset = datasets[idata]
      print(dataset)
      
      load(paste(dir_out,"COR_ESP_spi", sc,"_",ms,"_",est, "_",dataset, "_original.RData", sep = ""))
      corre_box[, idata] = as.vector(corre2)
      
    }
    
    corre_box2 <-
      matrix(data = NA,
             nrow = ni * nj,
             ncol = length(datasets))
    
    for (idata2 in 1:length(datasets)) {
      dataset2 = datasets[idata2]
      print(dataset2)
      
      load(paste(dir_out,"COR_S5_spi", sc,"_",ms,"_",est, "_",dataset2, "_original.RData", sep = ""))
      corre_box2[, idata2] = as.vector(corre2)
      
    }
    
    plot_data <-
      data.frame(
        corre_box[, 1],
        corre_box2[, 1],
        corre_box[, 2],
        corre_box2[, 2],
        corre_box[, 3],
        corre_box2[, 3],
        corre_box[, 4],
        corre_box2[, 4],
        corre_box[, 5],
        corre_box2[, 5],
        corre_box[, 6],
        corre_box2[, 6],
        corre_box[, 7],
        corre_box2[, 7],
        corre_box[, 8],
        corre_box2[, 8],
        corre_box[, 9],
        corre_box2[, 9],
        corre_box[, 10],
        corre_box2[, 10],
        corre_box[, 11],
        corre_box2[, 11],
        corre_box[, 12],
        corre_box2[, 12]
      )
    
    
    
    setEPS()
    postscript(
      file.path(
        dir_out,
        paste("BOX_COR_MB_DIF_spi", sc,"_",ms,"_",est,  "_original.eps", sep = "")
      ),
      horiz = FALSE,
      onefile = FALSE,
      width = 8.5,
      height = 5.5
    )
    par(mar = c(10, 4, 2, 1))
    boxplotperc(
      na.omit(plot_data),
      quant = c(0.025, 0.975),
      outline = FALSE,
      las = 2,
      ylim = c(-0.33, 1.1),
      col=c('blue','green',
            'blue','green',
            'blue','green',
            'blue','green',
            'blue','green',
            'blue','green',
            'blue','green',
            'blue','green',
            'blue','green',
            'blue','green',
            'blue','green',
            'blue','green'),

      names = c('CPC',
                '',
                'GPCC',
                '',
                'PRECL',
                '',
                'ERA5',
                '',
                'JRA55',
                '',
                'NCEP',
                '',
                'MERRA2',
                '',
                'CAMS-OPI',
                '',
                'CHIRPS',
                '',
                'GPCP',
                '',
                'MSWEP',
                '',
                '4DROP',
                'S5'),
      #axis(1, at = seq(10, 200, by = 10), las=2)
      at =c(1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35),
      main=paste('Forecast for ',est,'; Lead time:', lt, 'M', sep=""),
      ylab='Correlation', 
      cex.main = 1.5,
      cex.lab = 1.3
    )
    
    
    
    abline(v = c(3, 6,9,12,15,18,21,24,27,30,33,36), lty = 6,col = "gray")
    abline(h = c(-0.2,0, 0.2, 0.4, 0.6, 0.8,1.0), lty = 6,col = "gray")
    
    segments(x0 = 33,
             x1 = 36,
             y0 = -0.2,
             y1 = -0.2,
             lwd = 2,
             col = "red",
             lty = 2:3) 
    
    segments(x0 = 33,
             x1 = 36,
             y0 = 1.05,
             y1 = 1.05,
             lwd = 2,
             col = "red",
             lty = 2:3) 
    
    segments(x0 = 33,
             x1 = 33,
             y0 = -0.2,
             y1 = 1.05,
             lwd = 2,
             col = "red",
             lty = 2:3) 
    
    segments(x0 = 36,
             x1 = 36,
             y0 = -0.2,
             y1 = 1.05,
             lwd = 2,
             col = "red",
             lty = 2:3) 
    
    
    legend("bottomleft",inset=.04,
           c("4DROP","S5"), fill=topo.colors(3), horiz=F, cex=0.9)

    dev.off()
    
  }
}



