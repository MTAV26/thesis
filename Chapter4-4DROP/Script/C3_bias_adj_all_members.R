# Limpiar el espacio de trabajo y liberar memoria
rm(list = ls())
graphics.off()
gc()

# Cargar las librerías necesarias
library(ncdf4)    # Para trabajar con archivos NetCDF
library(fields)   # Para análisis de campos y grillas
library(maps)     # Para trazar mapas
library(maptools) # Para trabajar con datos geoespaciales

# Definir el año de inicio y la secuencia de años
start_date = 1
anni = 1981:2020
# Definición de índices para meses
if (start_date == 7) {
  target_season = 'JJA'
  dates = c(7, 8)
} else if (start_date == 5) {
  target_season = 'JJA'
  dates = c(5, 6, 7, 8)
} else if (start_date == 10) {
  target_season = 'SON'
  dates = c(10, 11)
} else if (start_date == 8) {
  target_season = 'SON'
  dates = c(8,9,10,11)
} else if (start_date == 4) {
  target_season = 'MAM'
  dates = c(4,5)
} else if (start_date == 2) {
  target_season = 'MAM'
  dates = c(2,3,4,5)
} else if (start_date == 1) {
  target_season = 'DJF'
  dates = c(1,2)
  anni = 1982:2020
} else if (start_date == 11) {
  target_season = 'DJF'
  dates = c(11,12,1,2)
  anni = 1982:2020
}

## Parámetros fijos
data(wrld_simpl) # Cargar datos del mapa mundial simplificado
num_ens = 25    # Número de miembros del ensemble
nome_variable = 'TP' # Nombre de la variable: precipitación total ('TP')

# Definición de directorios y carga de coordenadas
dir_oss = '~/Chapter4-4DROP/Data/'
dir_s5  = '~/Chapter4-4DROP/Data/'
dir_out = '~/Chapter4-4DROP/Data/'
load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "inout.RData"))

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


mesi = rep(1:12, length(anni))

for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  print(dataset)
  
  # Condicionales para cargar diferentes conjuntos de datos de precipitación
  if (dataset == "JRA55") {
    # Cargar datos del conjunto de datos JRA55
    fname <- file.path(dir_oss, 'JRA55_1981_2021_25_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prlr")
  } else if (dataset == "GPCP") {
    # Cargar datos del conjunto de datos GPCP
    fname <- file.path(dir_oss, 'gpcp_cdr_v23rB1_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-9999"] <- NA
  } else if (dataset == "GPCC") {
    # Cargar datos del conjunto de datos GPCC
    fname <- file.path(dir_oss, 'prec_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-99999.9921875"] <- NA
  } else if (dataset == "CAMS_OPI") {
    # Cargar datos del conjunto de datos CAMS_OPI
    fname <- file.path(dir_oss,'camsopi_timecorrect-2.5-1981-2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prcp")
    obs[obs == "-9.99e+08"] <- NA
    obs[, , 63] = NA * obs[, , 63] # Faltan datos en 198603
  } else if (dataset == "PRECL") {
    # Cargar datos del conjunto de datos PRECL
    fname <- file.path(dir_oss, 'precip.mon.mean.2.5x2.5.1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "rain")
  } else if (dataset == "CPC") {
    # Cargar datos del conjunto de datos CPC
    fname <- file.path(dir_oss, 'precip_1981_2021_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs = obs[, , -dim(obs)[3]] # Eliminar el mes actual
    obs[obs == "-9.96920996838687e+36"] <- NA
  } else if (dataset == "CHIRPS") {
    # Cargar datos del conjunto de datos CHIRPS
    fname <- file.path(dir_oss, 'chirps-v2.0.monthly-2.5.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-9999"] <- NA
  } else if (dataset == "ERA5") {
    # Cargar datos del conjunto de datos ERA5
    fname <- file.path(dir_oss, 'ERA5-drop.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "tp")
    obs[obs == "-32767s"] <- NA
  } else if (dataset == "NCEP") {
    # Cargar datos del conjunto de datos NCEP
    fname <- file.path(dir_oss, 'prate_1981_2021_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prate")
  } else if (dataset == "MERRA2") {
    # Cargar datos del conjunto de datos MERRA2
    fname <- file.path(dir_oss, 'MERRA2_1981_2021_25_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "PRECTOTLAND")
    obs[obs == "999999986991104"] <- NA
  } else if (dataset == "MSWEP") {
    # Cargar datos del conjunto de datos MSWEP
    fname <- file.path(dir_oss, 'MSWEP_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precipitation")
    obs[obs == "-9999.f"] <- NA
  } else {
    # Si el conjunto de datos no es conocido, imprimir un mensaje
    print('dataset not known')
  }
  
  # Reorganizar los datos
  obs = obs[, ncol(obs):1,]
  dum = obs
  dum[1:(nrow(obs) / 2), ,] = obs[(nrow(obs) / 2 + 1):nrow(obs), ,]
  dum[(nrow(obs) / 2 + 1):nrow(obs), ,] = obs[1:(nrow(obs) / 2), ,]
  rm(obs)
  obs = dum
  rm(dum)
  
  # Filtrar y ajustar el período de los datos
  prec = obs[, , 1:480]
  if (start_date == 1) {
    prec = obs[, , 13:480] # si es DJF, mantener solo el período 1982-2020
  }
  
  # Multiplicar los valores por 'inout'(mascara)
  for (i in 1:dim(prec)[3]) {
    prec[, , i] = inout * prec[, , i]
  }
  
  # Calcular el promedio espacial
  aux = apply(prec, c(1, 2), mean, na.rm = TRUE)
  for (i in 1:dim(prec)[3]) {
    prec[, , i] = aux / aux * prec[, , i]
  }
  
  # Cargar el archivo 's5'
  load(paste0(
    dir_s5,
    'SEASONAL5.TP.',
    sprintf('%02d', start_date),
    '_all_members.Rdata'
  ))
  
  # Calcular el promedio espacial de 's5'
  s5_ens = apply(s5, c(1, 2, 3, 4), mean, na.rm = T)
  
  # Ajuste de sesgo
  s5_adj = array(NA, dim = c(
    length(lon),
    length(lat),
    length(dates),
    length(anni),
    num_ens
  ))
  
  # Bucle para el ajuste de sesgo por mes
  for (ilead in 1:length(dates)) {
    cat('Processing month', dates[ilead], '\n')
    mesi_calibration = which(mesi == dates[ilead])
    
    ind = 1:length(mesi_calibration)
    # Bucle para cada año
    for (iyear in 1:length(anni)) {
      # Calcular índices de entrenamiento y prueba
      itrain_obs = mesi_calibration[-iyear]
      itest_s5 = ind[iyear]
      itrain_s5 = ind[-iyear]
      # Bucle para cada punto en la grilla
      for (i in 1:length(lon)) {
        for (j in 1:length(lat)) {
          if (!is.na(inout[i, j])) {
            x_train = prec[i, j, itrain_obs]
            y_train <- s5_ens[i, j, ilead, itrain_s5]
            
            bias = mean(x_train, na.rm = TRUE) / mean(y_train, na.rm = TRUE)
            
            # Aplicar ajuste de sesgo a los miembros de ensemble de 's5'
            for (iens in 1:num_ens) {
              y_test <- s5[i, j, ilead, itest_s5, iens]
              s5_adj[i, j, ilead, itest_s5, iens] = y_test * bias
            }
          }
        }
      }
    }
  }
  
  # Guardar los resultados ajustados
  save(s5_adj,
       file = paste0(
         dir_out,
         'SEASONAL5.TP.',
         sprintf('%02d', start_date),
         "_",
         dataset,
         '_adj.Rdata'
       ))
}
