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

# Si el año de inicio es 1, ajustar la secuencia de años
if (start_date == 1) {
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

# Crear secuencia de fechas y vector de meses
dates = seq(1, 4)
mesi = rep(1:12, length(anni))

# 1, 2, 4, 5, 7, 8, 10, 11
# Inicializar un arreglo vacío para almacenar los datos
s5 = array(NA, dim = c(length(lon), length(lat), length(dates), length(anni)))
# Bucle anidado para el procesamiento de datos
for (iyear in 1:length(anni)) {
  cat('Processing ', anni[iyear], 'of', length(anni), 'anni', '\n')
  aux = array(NA, dim = c(length(lon), length(lat), length(dates), num_ens))
  
  for (iens in 1:num_ens) {
    # print(iens)
    # 1981/SEASONAL5.TP.19810701.0_deacc.nc
    fname <-
      paste0(
        dir_s5,
        '/SEASONAL5.TP.',
        anni[iyear],
        sprintf('%02d', start_date),
        '01.',
        iens - 1,
        '_deacc.nc'
      )
    s5.nc <- nc_open(fname)
    obs <- ncvar_get(s5.nc, nome_variable)

    aux[, , , iens] = obs
  }
  s5[, , , iyear] = apply(aux, c(1, 2, 3), mean, na.rm = T)
}
# Guardar el arreglo 's5' como un archivo RData
save(s5, file = paste0(dir_out, 'SEASONAL5.TP.',
                       sprintf('%02d', start_date), '.Rdata'))
