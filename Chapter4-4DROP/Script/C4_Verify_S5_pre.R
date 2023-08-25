# Limpia el espacio de trabajo, cierra gráficos y realiza la recolección de basura
rm(list = ls()) 
graphics.off() 
gc()

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

# Definir años y meses
anni = 1981:2020
mesi = rep(1:12, length(anni))

# Cargar datos simplificados del mundo (para mapas)
data(wrld_simpl)

# Definir intervalos y colores para las correlaciones
brk_cor <- seq(-1, 1, 0.2)
col_cor <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))

# Definir límites y colores para el sesgo (bias)
minC = -1
maxC = 1
brk_bias <- seq(minC, maxC, 0.25)
brk2_bias = union(-1e+05, brk_bias)
brk2_bias = union(brk2_bias, 1e+05)
col_bias <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))

# Definir el modelo y la variable a analizar
model = "seas5"
nome_variable = "prlr"

# Lista de conjuntos de datos disponibles
datasets = c(
  'CAMS_OPI',
  'CHIRPS',
  'CPC',
  'ERA5',
  'GPCC',
  'GPCP',
  'JRA55',
  'MERRA2',
  'MSWEP',
  'NCEP',
  'PRECL'
)


# Definición de directorios, carga de coordenadas y mascara
dir_oss = '~/Chapter4-4DROP/Data/'
dir_s5  = '~/Chapter4-4DROP/Data/'
dir_out = '~/Chapter4-4DROP/Data/'
load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "inout.RData"))




mesi = rep(1:12, length(anni))

for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  print(dataset)
  
  # Cargar los datos según el conjunto de datos especificado por 'dataset'
  if (dataset == "JRA55") {
    # Cargar datos de JRA55
    fname <- file.path(dir_oss, 'JRA55_1981_2021_25_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prlr")
  } else if (dataset == "GPCP") {
    # Cargar datos de GPCP
    fname <- file.path(dir_oss, 'gpcp_cdr_v23rB1_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-9999"] <- NA
  } else if (dataset == "GPCC") {
    # Cargar datos de GPCC
    fname <- file.path(dir_oss, 'prec_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-99999.9921875"] <- NA
  } else if (dataset == "CAMS_OPI") {
    # Cargar datos de CAMS_OPI
    fname <- file.path(dir_oss, 'camsopi_timecorrect-2.5-1981-2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prcp")
    obs[obs == "-9.99e+08"] <- NA
    obs[, , 63] = NA * obs[, , 63] # Marcar datos faltantes
  } else if (dataset == "PRECL") {
    # Cargar datos de PRECL
    fname <- file.path(dir_oss, 'precip.mon.mean.2.5x2.5.1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "rain")
  } else if (dataset == "CPC") {
    # Cargar datos de CPC
    fname <- file.path(dir_oss, 'precip_1981_2021_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs = obs[, , -dim(obs)[3]] # Eliminar el mes actual
    obs[obs == "-9.96920996838687e+36"] <- NA
  } else if (dataset == "CHIRPS") {
    # Cargar datos de CHIRPS
    fname <- file.path(dir_oss, 'chirps-v2.0.monthly-2.5.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-9999"] <- NA
  } else if (dataset == "ERA5") {
    # Cargar datos de ERA5
    fname <- file.path(dir_oss, 'ERA5-drop.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "tp")
    obs[obs == "-32767s"] <- NA
  } else if (dataset == "NCEP") {
    # Cargar datos de NCEP
    fname <- file.path(dir_oss, 'prate_1981_2021_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prate")
  } else if (dataset == "MERRA2") {
    # Cargar datos de MERRA2
    fname <- file.path(dir_oss, 'MERRA2_1981_2021_25_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "PRECTOTLAND")
    obs[obs == "999999986991104"] <- NA
  } else if (dataset == "MSWEP") {
    # Cargar datos de MSWEP
    fname <- file.path(dir_oss, 'MSWEP_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precipitation")
    obs[obs == "-9999.f"] <- NA
  } else {
    # Imprimir mensaje en caso de conjunto de datos desconocido
    print('dataset not known')
  }
  

  # Realizar manipulaciones en los datos
  obs = obs[, ncol(obs):1, ]  # Reordenar columnas de los datos
  dum = obs  # Crear una copia temporal de los datos
  
  # Cambiar la mitad superior y la mitad inferior de las matrices
  dum[1:(nrow(obs) / 2), , ] = obs[(nrow(obs) / 2 + 1):nrow(obs), , ]
  dum[(nrow(obs) / 2 + 1):nrow(obs), , ] = obs[1:(nrow(obs) / 2), , ]
  
  rm(obs)     # Eliminar la matriz original
  obs = dum   # Asignar la copia manipulada a la matriz original
  rm(dum)     # Eliminar la copia temporal
  
  # Mantener solo el período de 1981-2020 de los datos
  obs = obs[, , 1:480]
  
  # Multiplicar los valores de los datos por 'inout' (variable no definida en el fragmento)
  for (i in 1:dim(obs)[3]) {
    obs[, , i] = inout * obs[, , i]
  }
  
  # Calcular el promedio de los datos en cada celda y ajustar los datos
  aux = apply(obs, c(1, 2), mean, na.rm = TRUE)
  for (i in 1:dim(obs)[3]) {
    obs[, , i] = aux/aux * obs[, , i]
  }
  
  # Obtener las dimensiones de la matriz
  ni = dim(obs)[1]
  nj = dim(obs)[2]
  
  
  
  # Iterar a través de los meses de inicio
  for (start_date in 8:8) {    # CUIDADO AQUÍ CAMBIAR
    dates = seq(start_date, start_date + 3)
    
    if (start_date == 11) {
      dates = c(11, 12, 1, 2)
      
    } else if (start_date == 1) {
      dates = c(1, 2)
    }
    else if (start_date == 2) {
      dates = c(2, 3, 4, 5)
    } else if (start_date == 4) {
      dates = c(4, 5)
    }
    else if (start_date == 5) {
      dates = c(5, 6, 7, 8)
    } else if (start_date == 7) {
      dates = c(7, 8)
    }
    else if (start_date == 8) {
      dates = c(8, 9, 10, 11)
    } else if (start_date == 10) {
      dates = c(10, 11)
    }
    
    # Cargar datos de SEASONAL5.TP
    load(paste0(
      dir_s5,
      'SEASONAL5.TP.',
      sprintf('%02d', start_date),'_',dataset,
      '_adj.Rdata'
    ))
    
    # Manejo de valores infinitos y faltantes
    s5_adj[is.infinite(s5_adj)] <- NA
    s5_adj[is.na(s5_adj)] <- NA
    
    # Calcular la media de los datos
    s5_adj <- apply(s5_adj, c(1, 2, 3, 4), mean, na.rm = TRUE)
    s5_adj[is.infinite(s5_adj)] <- NA
    s5_adj[is.na(s5_adj)] <- NA
    
    # Inicializar matrices para resultados de verificación
    corre <- matrix(data = NA, nrow = ni, ncol = nj)
    pvalue <- matrix(data = NA, nrow = ni, ncol = nj)
    bias <- matrix(data = NA, nrow = ni, ncol = nj)
    corre_det <- matrix(data = NA, nrow = ni, ncol = nj)
    pvalue_det <- matrix(data = NA, nrow = ni, ncol = nj)
    
    # Iterar a través de los meses para verificación
    for (ilead in 1:length(dates)) {
      cat('Processing month', dates[ilead], '\n')
      mesi_verifica = which(mesi == dates[ilead])
      
      # Iterar a través de las ubicaciones geográficas
      for (i in 1:ni) {
        for (j in 1:nj) {
          if (!is.na(aux[i, j]/aux[i, j])) {
            cat("start-date", start_date, " - lat", i, "of ", ni, "\n")
            x1 = obs[i, j, mesi_verifica]
            y1 <- s5_adj[i, j, ilead,]
            
            # Calcular el sesgo
            bias[i, j] = mean(y1) - mean(x1)
            
            # Calcular la correlación y el valor p
            corre[i, j] = cor((x1), (y1))
            dum = cor.test((x1), (y1), alternative = c("greater"))
            pvalue[i, j] <- dum$p.value
            
            rm(dum)
          }
        }
      }
      
      # Ajustar los valores p para controlar la tasa de falsos descubrimientos
      pvalue_adj = p.adjust(pvalue, method = "fdr", n = length(pvalue[!is.na(pvalue)]))
      pvalue_adj = matrix(pvalue_adj, nrow = ni, ncol = nj)
      
      # Iterar a través de las ubicaciones geográficas para la verificación con datos detrended
      for (i in 1:ni) {
        for (j in 1:nj) {
          if (!is.na(aux[i, j]/aux[i, j])) {
            cat("start-date", start_date, " - lat", i, "of ", ni, "\n")
            x1 = obs[i, j, mesi_verifica]
            y1 <- s5_adj[i, j, ilead,]
            
            # Calcular tendencia lineal y correlación para los datos detrended
            x1d = as.vector(detrend(x1, tt = 'linear', bp = c()))
            y1d = as.vector(detrend(y1, tt = 'linear', bp = c()))
            
            corre_det[i, j] = cor((x1d), (y1d))
            dum = cor.test((x1d), (y1d), alternative = c("greater"))
            pvalue_det[i, j] <- dum$p.value
            
            rm(dum)
          }
        }
      }
      
      # Ajustar los valores p para controlar la tasa de falsos descubrimientos (datos detrended)
      pvalue_adj_det = p.adjust(pvalue_det, method = "fdr", n = length(pvalue_det[!is.na(pvalue_det)]))
      pvalue_adj_det = matrix(pvalue_adj_det, nrow = ni, ncol = nj)
      
      

      # Filtrar latitudes para el rango deseado
      lat2 = lat[which(lat > -60 & lat < 85)]
      bias2 = bias[, which(lat > -60 & lat < 85)]
      corre2 = corre[, which(lat > -60 & lat < 85)]
      pvalue2 = pvalue[, which(lat > -60 & lat < 85)]
      pvalue2_adj = pvalue_adj[, which(lat > -60 & lat < 85)]
      
      corre2_det = corre_det[, which(lat > -60 & lat < 85)]
      pvalue2_det = pvalue_det[, which(lat > -60 & lat < 85)]
      pvalue2_adj_det = pvalue_adj_det[, which(lat > -60 & lat < 85)]
      
      ## Crear gráfico 1
      postscript(
        file.path(
          dir_out,
          paste(
            "cor_seas5_",
            nome_variable,
            "_",
            sprintf("%02d", start_date),
            "_",
            sprintf("%02d", ilead),
            "_",dataset,"_original.eps",
            sep = ""
          )
        ),
        paper = "special",
        width = 11,
        height = 7,
        horizontal = T
      )
      layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
      par(oma = c(1, 1, 4, 1))
      tit <- paste(
        'SEAS5 prlr forecasts\n correlation (bias adj data) \n Start date:',
        month.name[start_date],
        '- Forecast date:',
        month.name[dates[ilead]], '(Original data; points: p<0.05)'
      )
      
      # Crear el gráfico usando la función "mioplot"
      mioplot(corre2, lon,lat2,
              toptitle = '',sizetit = 0.8,
              brks = brk_cor,  cols = col_cor,
              axelab =F, filled.continents = FALSE,
              drawleg = F,  
              dots = pvalue2 <= 0.05,
              dots2 = pvalue2_adj <= 0.05 
      )
      
      title(tit, line = 0.5, outer = T)
      ce = 1.4
      
      # Agregar barra de color al gráfico
      ColorBar(
        brks = brk_cor,
        cols = col_cor,
        vert = T,
        cex = 1
      )
      
      # Finalizar el gráfico
      dev.off()
      
      
      
      
      ## plot 2
      postscript(
        file.path(
          dir_out,
          paste(
            "cor_seas5_",
            nome_variable,
            "_",
            sprintf("%02d", start_date),
            "_",
            sprintf("%02d", ilead),
            "_",dataset,"_detrended.eps",
            sep = ""
          )
        ),
        paper = "special",
        width = 11,
        height = 7,
        horizontal = T
      )
      layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
      par(oma = c(1, 1, 4, 1))
      tit <-
        paste(
          'SEAS5 prlr forecasts\n correlation (bias adj data) \n Start date:',
          month.name[start_date],
          '- Forecast date:',
          month.name[dates[ilead]], '(Detrended data; points: p<0.05)'
        )
      mioplot(corre2_det, lon,lat2,
              toptitle = '',sizetit = 0.8,
              brks = brk_cor,  cols = col_cor,
              axelab =F, filled.continents = FALSE,
              drawleg = F,  
              dots = pvalue2_det <= 0.05,
              dots2 = pvalue2_adj_det <= 0.05 
      )
      title(tit, line = 0.5, outer = T)
      ce = 1.4
      ColorBar(
        brks = brk_cor,
        cols = col_cor,
        vert = T,
        cex = 1
      )
      dev.off()
      
      
      bias2[bias2<minC]=minC
      bias2[bias2>maxC]=maxC
      
      
      ## plot 3
      postscript(
        file.path(
          dir_out,
          paste(
            "bias_seas5_",
            nome_variable,
            "_",
            sprintf("%02d", start_date),
            "_",
            sprintf("%02d", ilead),
            "_",dataset,".eps",
            sep = ""
          )
        ),
        paper = "special",
        width = 11,
        height = 7,
        horizontal = T
      )
      layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
      par(oma = c(1, 1, 4, 1))
      tit <-
        paste(
          'SEAS5 prlr forecasts\n bias (mm/month) \n Start date:',
          month.name[start_date],
          '- Forecast date:',
          month.name[dates[ilead]]
        )
      mioplot(bias2, lon,lat2,
              toptitle = '',sizetit = 0.8,
              brks = brk_cor,  cols = col_cor,
              axelab =F, filled.continents = FALSE,
              drawleg = F
      )
      title(tit, line = 0.5, outer = T)
      ce = 1.4
      ColorBarM(
        brks = brk2_bias,
        cols = col_bias,
        vert = T,
        cex = 1,
        labs = seq(2, length(brk2_bias) - 1, 1)
      )
      dev.off()
    }
  }
}
