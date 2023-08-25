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
dir_s5 = '~/Chapter4-4DROP/Data/'
load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "inout.RData"))

## fix parameters
num_ens = 25
nome_variable = 'TP'

datasets = c('MSWEP')

# datasets = c(
#   'CAMS_OPI',
#   'CHIRPS',
#   'CPC',
#   'ERA5',
#   'GPCC',
#   'GPCP',
#   'JRA55',
#   'MERRA2',
#   'MSWEP',
#   'NCEP',
#   'PRECL'
# )

#start_dates = c(5, 8, 10, 2, 4, 1, 11)
start_dates = c(2)

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
  
  ## load data and spi calculation
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
    
    obs = obs[, ncol(obs):1,]
    dum = obs
    dum[1:(nrow(obs) / 2), ,] = obs[(nrow(obs) / 2 + 1):nrow(obs), ,]
    dum[(nrow(obs) / 2 + 1):nrow(obs), ,] = obs[1:(nrow(obs) / 2), ,]
    rm(obs)
    obs = dum
    rm(dum)
    
    image(lon, lat, apply(obs, c(1, 2), mean))
    plot(wrld_simpl, add = TRUE)
    # dim(obs)
    
    ## keep only 1981-2020 period
    prec = obs[, , 1:480]
    
    for (i in 1:dim(prec)[3]) {
      prec[, , i] = inout * prec[, , i]
    }
    
    aux = apply(prec, c(1, 2), mean, na.rm = TRUE)
    image.plot(lon, lat, aux / aux)
    plot(wrld_simpl, add = TRUE)
    
    for (i in 1:dim(prec)[3]) {
      prec[, , i] = aux / aux * prec[, , i]
    }
    
    image.plot(lon, lat, apply(prec, c(1, 2), mean, na.rm = TRUE))
    plot(wrld_simpl, add = TRUE)
    
    mesi_start = which(mesi == 1)
    
    load(file = paste0(
      dir_s5,
      'SEASONAL5.TP.',
      sprintf('%02d', start_date),
      "_",
      dataset,
      '_adj.Rdata'
    ))

    
    ## predict SPI6 for august, july and august are predicted, march, april, may, june, observed
    ## spei forecast
    spi6pred = array(data = NA,
                     dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3], num_ens))
    
  
    for (ianni in 1:length(anni)) {
      if (target_season == 'DJF' && ianni == 1) {
        next
      }
      
      
      anno_for = which(anni == anni[ianni])
      mese_start = mesi_start[anno_for]
      
      
      #resampling dati storici
      for (ires in 1:num_ens) {
        PRE_FOR = array(data = NA,
                        dim = c(dim(prec)[1], dim(prec)[2], length(anni) * 12))
        PRE_FOR = prec
        # PRE_FOR[, , (mese_start + start_date - 1):(mese_start + 11)] = NA
        
        if (target_season == 'DJF' && start_date == 11) {
          PRE_FOR[, , (mese_start  - 2):(mese_start + 11)] = NA
        } else {
          PRE_FOR[, , (mese_start + start_date - 1):(mese_start + 11)] = NA
        }
        
        if (start_date == 7) {
          PRE_FOR[, , (mese_start + 6)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR[, , (mese_start + 7)] = s5_adj[, , 2, ianni, ires]
        } else if (start_date == 5) {
          PRE_FOR[, , (mese_start + 4)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR[, , (mese_start + 5)] = s5_adj[, , 2, ianni, ires]
          PRE_FOR[, , (mese_start + 6)] = s5_adj[, , 3, ianni, ires]
          PRE_FOR[, , (mese_start + 7)] = s5_adj[, , 4, ianni, ires]
        } else if (start_date == 8) {
          PRE_FOR[, , (mese_start + 7)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR[, , (mese_start + 8)] = s5_adj[, , 2, ianni, ires]
          PRE_FOR[, , (mese_start + 9)] = s5_adj[, , 3, ianni, ires]
          PRE_FOR[, , (mese_start + 10)] = s5_adj[, , 4, ianni, ires]
        } else if (start_date == 10) {
          PRE_FOR[, , (mese_start + 9)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR[, , (mese_start + 10)] = s5_adj[, , 2, ianni, ires]
        }  else if (start_date == 2) {
          PRE_FOR[, , (mese_start + 1)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR[, , (mese_start + 2)] = s5_adj[, , 2, ianni, ires]
          PRE_FOR[, , (mese_start + 3)] = s5_adj[, , 3, ianni, ires]
          PRE_FOR[, , (mese_start + 4)] = s5_adj[, , 4, ianni, ires]
        }  else if (start_date == 4) {
          PRE_FOR[, , (mese_start + 3)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR[, , (mese_start + 4)] = s5_adj[, , 2, ianni, ires]
        }  else if (start_date == 1) {
          PRE_FOR[, , (mese_start)] = s5_adj[, , 1, ianni - 1, ires]
          PRE_FOR[, , (mese_start + 1)] = s5_adj[, , 2, ianni - 1, ires]
        }  else if (start_date == 11) {
          PRE_FOR[, , (mese_start - 2)] = s5_adj[, , 1, ianni - 1, ires]
          PRE_FOR[, , (mese_start - 1)] = s5_adj[, , 2, ianni - 1, ires]
          PRE_FOR[, , (mese_start)] = s5_adj[, , 3, ianni - 1, ires]
          PRE_FOR[, , (mese_start + 1)] = s5_adj[, , 4, ianni - 1, ires]
        } else {
          print('start date not known')
        }
        
        
        ## calculate SPI
        spitmp = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3]))
        
        
        
        for (i in 1:dim(prec)[1]) {
          print(paste0(
            "anni ",
            anni[ianni],
            " ensemble ",
            ires,
            ' grid ',
            i,
            ' of ',
            length(lon)
          ))

          for (j in 1:dim(prec)[2]) {
            if (!is.na(inout[i, j])) {
              dum <- spi(PRE_FOR[i, j,], 6, na.rm = TRUE)
              spitmp[i, j,] = dum$fitted
              rm(dum)
              
            }
          }
        }
        
        
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
    
    
    save(spi6pred, file = file.path(
      dir_out,
      paste(
        "SPI6SEAS5_",
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