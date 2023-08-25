rm(list = ls())
graphics.off()
gc()

#Cargar las bibliotecas necesarias
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

data(wrld_simpl)

dir_oss = '~/Chapter5-4FIRE/Data/'
dir_out = '~/Chapter5-4FIRE/Data/'
dir_s5 = '~/Chapter5-4FIRE/Data/'
load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "inout.RData"))

start_date = 11
anni = 2000:2020
num_ens = 25
nome_variable = 'tp'

if (start_date == 11) {
  target_season = 'NDJF'
  dates = c(11,12,1,2)

} else if (start_date == 2) {
  target_season = 'FMAM'
  dates = c(2,3,4,5)

} else if (start_date == 5) {
  target_season = 'MJJA'
  dates = c(5,6,7,8)
  

} else if (start_date == 8) {
  target_season = 'ASON'
  dates = c(8,9,10,11)

}

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
  
  if (dataset == "JRA55") {
    fname <- file.path(dir_oss, 'JRA55_1981_2021_25_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prlr")
  } else if (dataset == "GPCP") {
    fname <- file.path(dir_oss, 'gpcp_cdr_v23rB1_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-9999"] <- NA
  } else if (dataset == "GPCC") {
    fname <- file.path(dir_oss, 'prec_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-99999.9921875"] <- NA
  } else if (dataset == "CAMS_OPI") {
    fname <-
      file.path(dir_oss,
                'camsopi_timecorrect-2.5-1981-2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prcp")
    # obs=obs[,,1:(dim(obs)[3]-12)]
    obs[obs == "-9.99e+08"] <- NA
    obs[, , 63] = NA * obs[, , 63] #BE CAREFULL, 198603 is missing
  } else if (dataset == "PRECL") {
    fname <-
      file.path(dir_oss, 'precip.mon.mean.2.5x2.5.1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "rain")
  } else if (dataset == "CPC") {
    fname <- file.path(dir_oss, 'precip_1981_2021_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs = obs[, , -dim(obs)[3]] #eliminate current month
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
  
  obs = obs[, ncol(obs):1,]
  dum = obs
  dum[1:(nrow(obs) / 2), ,] = obs[(nrow(obs) / 2 + 1):nrow(obs), ,]
  dum[(nrow(obs) / 2 + 1):nrow(obs), ,] = obs[1:(nrow(obs) / 2), ,]
  rm(obs)
  obs = dum
  rm(dum)

  prec = obs[, , 229:480]

  # if (start_date == 1) {
  #   prec = obs[, , 13:480] # if DJF keep only 1982-2020 period
  # }
  
  for (i in 1:dim(prec)[3]) {
    prec[, , i] = inout * prec[, , i]
  }
  
  aux = apply(prec, c(1, 2), mean, na.rm = TRUE)
  for (i in 1:dim(prec)[3]) {
    prec[, , i] = aux / aux * prec[, , i]
  }

  ## load s5
  load(paste0(
    dir_s5,
    'SEASONAL5.TP.',
    sprintf('%02d', start_date),
    '_all_members.Rdata'
  ))
  
  
  dim(s5)
  
  # s5 = array(NA, dim = c(length(lon), length(lat),length(dates),length(anni),num_ens))
  s5_ens = apply(s5, c(1, 2, 3, 4), mean, na.rm = T)
  
  ## bias correction
  s5_adj = array(NA, dim = c(
    length(lon),
    length(lat),
    length(dates),
    length(anni),
    num_ens
  ))
  
  for (ilead in 1:length(dates)) {
    cat('Processing month', dates[ilead], '\n')
    mesi_calibration = which(mesi == dates[ilead])
    
    ind = 1:length(mesi_calibration)
    for (iyear in 1:length(anni)) {
      #itest_obs = mesi_calibration[iyear]
      itrain_obs = mesi_calibration[-iyear]
      itest_s5 = ind[iyear]
      itrain_s5 = ind[-iyear]
      for (i in 1:length(lon)) {
        for (j in 1:length(lat)) {
          if (!is.na(inout[i, j])) {
            x_train = prec[i, j, itrain_obs]
            y_train <- s5_ens[i, j, ilead, itrain_s5]

            bias = mean(x_train, na.rm = TRUE) / mean(y_train, na.rm = TRUE)

            for (iens in 1:num_ens) {
              y_test <- s5[i, j, ilead, itest_s5, iens]
              s5_adj[i, j, ilead, itest_s5, iens] = y_test * bias
            }
          }
        }
      }
    }
  }
  
  
  save(s5_adj,
       file = paste0(
         dir_out,
         'SEASONAL5.TP.',
         sprintf('%02d', start_date),
         "_",target_season, "_",
         dataset,
         '_adj.Rdata'
       ))
  
}
