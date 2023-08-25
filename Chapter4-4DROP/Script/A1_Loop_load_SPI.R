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

data(wrld_simpl)

##################################################
# Definición de directorios, carga de coordenadas y mascara
dir_oss = '~/Chapter4-4DROP/Data/'
dir_out  = '~/Chapter4-4DROP/Data/'
load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "inout.RData"))
ni = length(lon)
nj = length(lat)

anni = 2000:2020
mesi = rep(1:12, length(anni))
anno_2000 = which(anni == 2020)
mese_ind = which(mesi == 12)
last_month = mese_ind[anno_2000]

# datasets=c('MSWEP')
datasets = c('ERA5',
             'CHIRPS',
             'CPC',
             'PRECL',
             'CAMS_OPI',
             'GPCC',
             'GPCP',
             'JRA55',
             'NCEP',
             'MSWEP',
             'MERRA2')

## load data and spi calculation
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
  
    obs = obs[, ncol(obs):1, ]
    dum = obs
    dum[1:(nrow(obs) / 2), , ] = obs[(nrow(obs) / 2 + 1):nrow(obs), , ]
    dum[(nrow(obs) / 2 + 1):nrow(obs), , ] = obs[1:(nrow(obs) / 2), , ]
    rm(obs)
    obs = dum
    rm(dum)

    obs=obs[,,1:480]
    for (i in 1:dim(obs)[3]) {
      obs[, , i] = inout * obs[, , i]
    }
    
    aux=apply(obs,c(1,2),mean,na.rm=TRUE)
    for (i in 1:dim(obs)[3]) {
      obs[,,i] = aux/aux * obs[,,i]
    }
    
    spi3 = obs * NA
    spi6 = obs * NA
    spi12 = obs * NA
    
    
    for (i in 1:length(lon)) {
      print(paste0('grid ',i,' of ',length(lon)))   
      for (j in 1:length(lat)) {
        if (!is.na(aux[i, j]/aux[i, j])) {
          
          
          dum <- spi(obs[i, j,], 6, na.rm = TRUE)
          spi6[i, j,] = dum$fitted
          rm(dum)
          
        }
      }
    }
    
    spi6[is.infinite(spi6)] <- NA
    spi6[is.na(spi6)] <- NA
    
    ### SAVE
    save(spi6, file = file.path(dir_drop, paste0("SPI6_", dataset, "_1981_2020.RData")))
}

