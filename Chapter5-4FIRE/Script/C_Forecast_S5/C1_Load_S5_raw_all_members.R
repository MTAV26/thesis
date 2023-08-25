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

## fix parameters
data(wrld_simpl)
num_ens = 25
nome_variable = 'tp'
anni = 2000:2020

dir_oss = '~/Chapter5-4FIRE/Data/'
dir_out = '~/Chapter5-4FIRE/Data/'
dir_s5 = '~/Chapter5-4FIRE/Data/'
load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))



dates = seq(1, 4)
mesi = rep(1:12, length(anni))
start_date = 2

s5 = array(NA, dim = c(length(lon), length(lat), length(dates), length(anni), num_ens))
for (iyear in 1:length(anni)) {
  cat('Processing ', anni[iyear], 'of', length(anni), 'anni', '\n')

  for (iens in 1:num_ens) {
    # print(iens)
    # 1981/SEASONAL5.TP.19810701.0_deacc.nc
    fname <-
      paste0(
        dir_s5,
        'ECMWF-S5-prlr-',
        anni[iyear],'-',
        sprintf('%02d', start_date),'-',
        sprintf('%02d', iens - 1), '.nc'
        
      )
    s5.nc <- nc_open(fname)
    obs <- ncvar_get(s5.nc, nome_variable)

    s5[, , , iyear, iens] = obs[,,1:4]
  }

}
save(s5,
     file = paste0(
       dir_out,
       'SEASONAL5.TP.',
       sprintf('%02d', start_date),
       '_all_members.Rdata'
     ))

