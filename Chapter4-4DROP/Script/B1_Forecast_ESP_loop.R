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


# Carga de datos geográficos
data(wrld_simpl)

# Definición de años y meses
anni = 1981:2020
mesi = rep(1:12, length(anni))

# Definición de directorios y carga de coordenadas
dir_oss = '~/Chapter4-4DROP/Data/'
dir_out = '~/Chapter4-4DROP/Data/'
load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "inout.RData"))
# Definición de índices para meses
mesi_start = which(mesi == 1)
mesi_1 = which(mesi == 1)
mesi_2 = which(mesi == 2)
mesi_3 = which(mesi == 3)
mesi_4 = which(mesi == 4)
mesi_5 = which(mesi == 5)
mesi_6 = which(mesi == 6)
mesi_7 = which(mesi == 7)
mesi_8 = which(mesi == 8)
mesi_9 = which(mesi == 9)
mesi_10 = which(mesi == 10)
mesi_11 = which(mesi == 11)
mesi_12 = which(mesi == 12)


# Definición de conjuntos de datos climáticos
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

# Definición de meses de inicio
start_dates = c(5, 7, 8, 10, 2, 4, 1, 11)

# Bucle para cada mes de inicio
for (istart_date in 1:length(start_dates)) {
  start_date = start_dates[istart_date]
  

  # Determinar la temporada objetivo en función del mes de inicio
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
  
  ## Carga de datos
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    print(dataset)
    
    # Cargar datos según el conjunto de datos actual
    if (dataset == "JRA55") {
      fname <- file.path(dir_oss, 'JRA55_1981_2021_25_monthly.nc')
      obs.nc <- nc_open(fname)
      obs <- ncvar_get(obs.nc, "prlr")
    } else if (dataset == "GPCP") {
      fname <- file.path(dir_oss, 'gpcp_cdr_v23rB1_1981_2021.nc')
      obs.nc <- nc_open(fname)
      obs <- ncvar_get(obs.nc, "precip")
      obs[obs == "-9999"] <- NA
      obs[obs == "-9999"] <- NA
    } else if (dataset == "GPCC") {
      fname <- file.path(dir_oss, 'prec_1981_2021.nc')
      obs.nc <- nc_open(fname)
      obs <- ncvar_get(obs.nc, "precip")
      obs[obs == "-99999.9921875"] <- NA
    } else if (dataset == "CAMS_OPI") {
      fname <- file.path(dir_oss, 'camsopi_timecorrect-2.5-1981-2021.nc')
      obs.nc <- nc_open(fname)
      obs <- ncvar_get(obs.nc, "prcp")
      obs[obs == "-9.99e+08"] <- NA
      obs[, , 63] = NA * obs[, , 63]
    } else if (dataset == "PRECL") {
      fname <- file.path(dir_oss, 'precip.mon.mean.2.5x2.5.1981_2021.nc')
      obs.nc <- nc_open(fname)
      obs <- ncvar_get(obs.nc, "rain")
    } else if (dataset == "CPC") {
      fname <- file.path(dir_oss, 'precip_1981_2021_monthly.nc')
      obs.nc <- nc_open(fname)
      obs <- ncvar_get(obs.nc, "precip")
      obs = obs[, , -dim(obs)[3]]
      obs[obs == "-9.96920996838687e+36"] <- NA
    } else if (dataset == "CHIRPS") {
      fname <- file.path(dir_oss, 'chirps-v2.0.monthly-2.5.nc')
      obs.nc <- nc_open(fname)
      obs <- ncvar_get(obs.nc, "precip")
      obs[obs == "-9999"] <- NA
    } else if (dataset == "ERA5") {
      fname <- file.path(dir_oss, 'ERA5-drop.nc')
      obs.nc <- nc_open(fname)
      obs <- ncvar_get(obs.nc, "tp")
      obs[obs == "-32767s"] <- NA
    } else if (dataset == "NCEP") {
      fname <- file.path(dir_oss, 'prate_1981_2021_monthly.nc')
      obs.nc <- nc_open(fname)
      obs <- ncvar_get(obs.nc, "prate")
    } else if (dataset == "MERRA2") {
      fname <- file.path(dir_oss, 'MERRA2_1981_2021_25_monthly.nc')
      obs.nc <- nc_open(fname)
      obs <- ncvar_get(obs.nc, "PRECTOTLAND")
      obs[obs == "999999986991104"] <- NA
    } else if (dataset == "MSWEP") {
      fname <- file.path(dir_oss, 'MSWEP_1981_2021.nc')
      obs.nc <- nc_open(fname)
      obs <- ncvar_get(obs.nc, "precipitation")
      obs[obs == "-9999.f"] <- NA
    } else {
      print('dataset not known')
    }
    
    # Ajustes de datos y manipulaciones
    obs = obs[, ncol(obs):1,]
    dum = obs
    dum[1:(nrow(obs) / 2), ,] = obs[(nrow(obs) / 2 + 1):nrow(obs), ,]
    dum[(nrow(obs) / 2 + 1):nrow(obs), ,] = obs[1:(nrow(obs) / 2), ,]
    rm(obs)
    obs = dum
    rm(dum)

    ## Aseguramos el período 1981-2020
    prec = obs[, , 1:480]
    
    # Ajustar los datos en función de 'inout'
    for (i in 1:dim(prec)[3]) {
      prec[, , i] = inout * prec[, , i]
    }
  
  # aux = apply(prec, c(1, 2), mean, na.rm = TRUE)
  # image.plot(lon, lat, aux / aux)
  # plot(wrld_simpl, add = TRUE)
  
  # for (i in 1:dim(prec)[3]) {
  #   prec[, , i] = aux / aux * prec[, , i]
  # }
  

  
    # Crear una matriz vacía para almacenar las predicciones del SPI6
    spi6pred = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3], (length(anni) - 1)))
    
    # Bucle para cada año en el conjunto de años (anni)
    for (ianni in 1:length(anni)) {
      print(paste0('loop ', ianni, ' of ', length(anni)))
    
    
      # Identificar el índice del año actual en el vector anni
      anno_for = which(anni == anni[ianni])
      
      # Obtener el mes de inicio para este año
      mese_start = mesi_start[anno_for]
      
      # Crear un vector de años sin el año actual
      anni_resampling = anni[-anno_for]
    
      # Bucle para remuestrear los datos históricos para cada año en anni_resampling
      for (ires in 1:length(anni_resampling)) {
        cat("loop", ires, "\n")
        
        # Crear una copia de los datos originales de precipitación
        PRE_FOR = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], length(anni) * 12))
        PRE_FOR = prec
        
        # Lógica para modificar los datos de acuerdo con el mes de inicio y la temporada objetivo
       if (target_season == 'DJF' && start_date==11) {
         PRE_FOR[, , (mese_start  - 2):(mese_start + 11)] = NA  
       } else {
         PRE_FOR[, , (mese_start + start_date - 1):(mese_start + 11)] = NA  
       }
      
      
      anno_tmp = which(anni == anni_resampling[ires])
      mese_1 = mesi_1[anno_tmp]
      mese_2 = mesi_2[anno_tmp]
      mese_3 = mesi_3[anno_tmp]
      mese_4 = mesi_4[anno_tmp]
      mese_5 = mesi_5[anno_tmp]
      mese_6 = mesi_6[anno_tmp]
      mese_7 = mesi_7[anno_tmp]
      mese_8 = mesi_8[anno_tmp]
      mese_9 = mesi_9[anno_tmp]
      mese_10 = mesi_10[anno_tmp]
      mese_11 = mesi_11[anno_tmp]
      mese_12 = mesi_12[anno_tmp]
      
      # djfmamjjason
      if (target_season == 'JJA' &&start_date == 7) {
        PRE_FOR[, , mese_start + 6] = prec[, , mese_7]
        PRE_FOR[, , mese_start + 7] = prec[, , mese_8]
      } else if (target_season=='JJA' && start_date ==5){
        PRE_FOR[, , mese_start + 4] = prec[, , mese_5]
        PRE_FOR[, , mese_start + 5] = prec[, , mese_6]
        PRE_FOR[, , mese_start + 6] = prec[, , mese_7]
        PRE_FOR[, , mese_start + 7] = prec[, , mese_8]
      
        } else if (target_season == 'SON' &&start_date == 10) {
        PRE_FOR[, , mese_start + 9 ] = prec[, , mese_10]
        PRE_FOR[, , mese_start + 10] = prec[, , mese_11]
      } else if (target_season == 'SON' && start_date == 8) {
        PRE_FOR[, , mese_start + 7] = prec[, , mese_8]
        PRE_FOR[, , mese_start + 8] = prec[, , mese_9]
        PRE_FOR[, , mese_start + 9] = prec[, , mese_10]
        PRE_FOR[, , mese_start + 10] = prec[, , mese_11]
      
        } else if (target_season == 'MAM' && start_date == 4) {
        PRE_FOR[, , mese_start + 3] = prec[, , mese_4]
        PRE_FOR[, , mese_start + 4] = prec[, , mese_5]
      } else if (target_season == 'MAM' && start_date == 2) {
        PRE_FOR[, , mese_start + 1] = prec[, , mese_2]
        PRE_FOR[, , mese_start + 2] = prec[, , mese_3]
        PRE_FOR[, , mese_start + 3] = prec[, , mese_4]
        PRE_FOR[, , mese_start + 4] = prec[, , mese_5]
      
        } else if (target_season == 'DJF' && start_date == 1) {
        PRE_FOR[, , mese_start    ] = prec[, , mese_1]
        PRE_FOR[, , mese_start + 1] = prec[, , mese_2]
      } else if (target_season == 'DJF' && start_date == 11) {
        mese_11 = mesi_11[anno_tmp-1]
        mese_12 = mesi_12[anno_tmp-1]
        PRE_FOR[, , mese_start - 2] = prec[, , mese_11]
        PRE_FOR[, , mese_start - 1] = prec[, , mese_12]
        PRE_FOR[, , mese_start    ] = prec[, , mese_1 ]
        PRE_FOR[, , mese_start + 1] = prec[, , mese_2 ]
        
      } else {
        print('start date not known')
      }
      
      ## calculate SPI
      spitmp = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3]))
      
      # Bucle para cada punto de la cuadrícula
      for (i in 1:dim(prec)[1]) {
        print(paste0('grid ', i, ' of ', length(lon)))
        
        for (j in 1:dim(prec)[2]) {
          if (!is.na(aux[i, j] / aux[i, j])) {
            
            # Calcular el índice SPI6 para cada punto de la cuadrícula
            dum <- spi(PRE_FOR[i, j, ], 6, na.rm = TRUE)
            dum <- spi(PRE_FOR[i, j, ], 6, na.rm = TRUE)
            spitmp[i, j, ] = dum$fitted
            rm(dum)
            
          }
        }
      }
      
      # Almacenar las predicciones del SPI6 en la matriz spi6pred según la temporada objetivo
      if (target_season == 'JJA') {
        spi6pred[, , mese_start + 7, ires] = spitmp[, , mese_start + 7]
      } else if (target_season == 'SON') {
        spi6pred[, , mese_start + 10, ires] = spitmp[, , mese_start + 10]
      } else if (target_season == 'MAM') {
        spi6pred[, , mese_start + 4, ires] = spitmp[, , mese_start + 4]
      } else if (target_season == 'DJF') {
        spi6pred[, , mese_start + 1, ires] = spitmp[, , mese_start + 1]
      }
      
      rm(spitmp)
      
      }
    }
    
    # Guardar las predicciones del SPI6 en un archivo .RData
    save(spi6pred, file = file.path(
      dir_out,
      paste(
        "SPI6ESP_",
        sprintf("%02d", start_date),
        "_",
        target_season,
        "_",
        dataset,
        ".RData",
        sep = ""
      )
    ))
  
  }
}


