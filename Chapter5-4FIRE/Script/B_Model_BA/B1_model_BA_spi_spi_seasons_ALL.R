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

dir_f51 = '~/Chapter5-4FIRE/Data/'
dir_data = '~/Chapter5-4FIRE/Data/'
dir_out = '~/Chapter5-4FIRE/Data/'
dir_drop = '~/Chapter5-4FIRE/Data/'
dir_drop_all = '~/Chapter5-4FIRE/Data/'

#===============================================================================
# load lon lat
#===============================================================================

fname <- file.path(dir_drop, 'DROP_3_1981_2020.nc')
obs.nc <- nc_open(fname)
obs.nc$dim$lon$vals -> lon
obs.nc$dim$lat$vals -> lat

## fix parameters
years = 2001:2020

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

#===============================================================================
# to select -----
#===============================================================================

#seasons = c('DJF', 'MAM', 'JJA', 'SON')
seasons = c('DJF')
FDR=1
if (FDR==1) {
  version = paste0(seasons,'_FDR')
} else {
  version = paste0(seasons)
}

for (iseas in 1:length(seasons)) {
  season = seasons[iseas]
  print(season)

#===============================================================================
# define local parameters
#===============================================================================

if (season == 'JJA') {
  first_month = 6
  last_month = 8
} else if (season == 'MAM') {
  first_month = 3
  last_month = 5
} else if (season == 'SON') {
  first_month = 9
  last_month = 11
} else if (season == 'DJF') {
  first_month = 12
  last_month = 2
} else {
  print('no season, call terminator')
}


#===============================================================================
# load BA
#===============================================================================

## BA
load(paste0(dir_f51, "/BA_200101_202012_nat.RData"))
image.plot(lon, lat, apply(BA, c(1, 2), mean, na.rm = TRUE))
plot(wrld_simpl, add = TRUE)
ni = dim(BA)[1]
nj = dim(BA)[2]
#monthly BA --> JJA BA
BAS = array(0, dim = c(ni, nj, length(years)))
for (i in 1:ni) {
  for (j in 1:nj) {
    for (iyear in 1:length(years)) {
      
      if (season == 'DJF' && first_month == 12) {
        i1 = (iyear - 1) * 12
        i2 = (iyear - 1) * 12 + last_month
        
      } else {
        
        i1 = (iyear - 1) * 12 + first_month
        i2 = (iyear - 1) * 12 + last_month
      }
      BAS[i, j, iyear] = sum(BA[i, j, i1:i2], na.rm = TRUE)  # SON: 237, 238, 239
    }                                                        # DJF: 228, 229, 230 
  }
}

dim(BAS)
mask = array(0, dim = c(ni, nj))
for (i in 1:ni) {
  for (j in 1:nj) {
    # print(length(BA[i,j,]==0))
    if (length(which(BAS[i, j, ] > 0)) >= 10) {
      mask[i, j] = 1
    }
  }
}
image.plot(lon, lat, mask)
plot(wrld_simpl, add = TRUE)
rm(BA)
BA = BAS * NA

#===============================================================================
# main loop
#===============================================================================

for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  print(dataset)
  
  load(file.path(paste0(
    dir_drop_all, "SPI3_", dataset, "_1981_2020.RData"
  )))
  
  load(file.path(paste0(
    dir_drop_all, "SPI6_", dataset, "_1981_2020.RData"
  )))
  
  load(file.path(paste0(
    dir_drop_all, "SPI12_", dataset, "_1981_2020.RData"
  )))
  
  dim(spi3)
  
  ## load spi_c
  years_spi_c = 1981:2020
  iok_spi_c = match((years[1] - 1):years[length(years)], years_spi_c) #we need also 2000
  #spi_c coef is suppose to be >0, while the spi effect is supposet to be <0. to have the same considtions below, i changes the sign of the spi
  spi_c3 = -spi3[, , (((iok_spi_c[1] - 1) * 12) + 1):(iok_spi_c[length(iok_spi_c)] *
                                                        12)]
  spi_c6 = -spi6[, , (((iok_spi_c[1] - 1) * 12) + 1):(iok_spi_c[length(iok_spi_c)] *
                                                        12)]
  spi_c12 = -spi12[, , (((iok_spi_c[1] - 1) * 12) + 1):(iok_spi_c[length(iok_spi_c)] *
                                                          12)]
  
  dim(spi_c12)
  
  image.plot(lon, lat, spi_c6[, , 240])
  
  
  ## load SPEI
  years_spi_a = 1981:2020
  years = 2000:2020  #el area quemada solo tiene datos hasta 2019
  iok_spi_a = match((years[1] - 1):years[length(years)], years_spi_a) #we need also 1999
  spi_a3 = spi3[, , (((iok_spi_a[1] - 1) * 12) + 1):(iok_spi_a[length(iok_spi_a)] *
                                                       12)]
  spi_a6 = spi6[, , (((iok_spi_a[1] - 1) * 12) + 1):(iok_spi_a[length(iok_spi_a)] *
                                                       12)]
  spi_a12 = spi12[, , (((iok_spi_a[1] - 1) * 12) + 1):(iok_spi_a[length(iok_spi_a)] *
                                                         12)]
  dim(spi_a6)
  
  
  years = 2001:2020
  image.plot(lon, lat, spi_a6[, , 264])
  
  
  spi_c = array(NA, dim = c(ni, nj, dim(spi_c3)[3], 3))
  dim(spi_c)
  spi_c[, , , 1] = spi_c3
  spi_c[, , , 2] = spi_c6
  spi_c[, , , 3] = spi_c12
  rm(spi_c3)
  rm(spi_c6)
  rm(spi_c12)
  spi_c[is.infinite(spi_c)]=NA
  
  spi_a = array(NA, dim = c(ni, nj, dim(spi_a3)[3], 3))
  spi_a[, , , 1] = spi_a3
  spi_a[, , , 2] = spi_a6
  spi_a[, , , 3] = spi_a12
  rm(spi_a3)
  rm(spi_a6)
  rm(spi_a12)
  spi_a[is.infinite(spi_a)]=NA
  
  dim(spi_a)   #[,, 264] 21 A?OS   2000 - 2020
  dim(spi_c)   #[,, 252] 20 A?OS   2001 - 2020
  dim(BA)
  
  #===============================================================================
  if (season == 'DJF' && first_month == 12) {

    num_iter = length((first_month - 26):(first_month - 14)) * 3 * 
      length((last_month - 3):(last_month)) * 3
  } else {
    
    num_iter = length((first_month - 14):(first_month - 2)) * 3 * 
      length((first_month - 1):last_month) * 3
  }
  #===============================================================================
  
  best_roc = array(NA, dim = c(ni, nj))
  best_sig = array(NA, dim = c(ni, nj))
  
  best_m_spi_a_fin = array(NA, dim = c(ni, nj))
  best_t_spi_a_fin = array(NA, dim = c(ni, nj))
  
  best_m_spi_c_fin = array(NA, dim = c(ni, nj))
  best_t_spi_c_fin = array(NA, dim = c(ni, nj))
  
  coef_spi_c = array(NA, dim = c(ni, nj, length(years)))
  coef_spi_a = array(NA, dim = c(ni, nj, length(years)))
  
  best_x1_fin = array(NA, dim = c(ni, nj, length(years)))
  best_x2_fin = array(NA, dim = c(ni, nj, length(years)))
  
  for (i in 1:length(lon)) {
    for (j in 1:length(lat)) {
      # for (i in 23:23) {
      #   for (j in 53:53) {
      
      if (mask[i, j] == 1) {
        roca = array(NA, dim = c(num_iter))
        sig = array(NA, dim = c(num_iter))
        
        best_m_spi_a = array(NA, dim = c(num_iter))
        best_t_spi_a = array(NA, dim = c(num_iter))
        
        best_m_spi_c = array(NA, dim = c(num_iter))
        best_t_spi_c = array(NA, dim = c(num_iter))
        
        best_x1_1 = array(NA, dim = c(num_iter, length(years)))
        best_x1_12 = array(NA, dim = c(num_iter, length(years)))
        best_x2_2 = array(NA, dim = c(num_iter, length(years)))
        best_x2_12 = array(NA, dim = c(num_iter, length(years)))
        
        best_x1 = array(NA, dim = c(num_iter, length(years)))
        best_x2 = array(NA, dim = c(num_iter, length(years)))
        
        
        # num_iter = length((first_month - 25):(first_month - 13)) * 3 * length((last_month - 3):(last_month)) * 3
        
        
        k = 0
        for ( im_spi_a in (
          # (first_month - 14):(first_month - 2)
          #===============================================================================
          if (season == 'DJF' && first_month == 12) {            
            (first_month - 26):(first_month - 14)
          } else {
            (first_month - 14):(first_month - 2)
          }
          #===============================================================================
        ))
        {   #-5 
          #antecedente
          
          for (isc_spi_a in 1:3) {
            # for (im_spi_c in ((first_month - 1):last_month)) {
            
            #===============================================================================
            for (im_spi_c in (   #11
              if (season == 'DJF' && first_month == 12) {
                (last_month - 3):(last_month)
                # (first_month - 1):(last_month+12)
              } else {
                (first_month - 1):last_month
              }
              #===============================================================================
            ))
            {
              
              #coincidente
              for (isc_spi_c in 1:3) {
                k = k + 1
                print(paste0(
                  'lon ',
                  i,
                  '/',
                  ni,
                  '; lat ',
                  j,
                  '/',
                  nj,
                  '; step ',
                  k,
                  '/',
                  num_iter
                ))
                
                spi_a_aux  = vector() #lenght 20
                spi_c_aux = vector()  #lenght 20
                
                
                if (im_spi_a <= -12) {
                  im_ok = 24 + im_spi_a
                  dum = spi_a[i, j, 1:(dim(spi_a)[3] - 24), isc_spi_a]  # [i,j,1:240]
                  spi_a_aux = dum[seq(im_ok, length(dum), 12)]
                } else if (im_spi_a > -12 & im_spi_a <= 0) {
                  im_ok = 12 + im_spi_a
                  dum = spi_a[i, j, 13:(dim(spi_a)[3] - 12), isc_spi_a] # [i,j,13:252]
                  spi_a_aux = dum[seq(im_ok, length(dum), 12)]
                } else {
                  im_ok = im_spi_a
                  dum = spi_a[i, j, 25:dim(spi_a)[3], isc_spi_a]        # [i,j,25:265]
                  spi_a_aux = dum[seq(im_ok, length(dum), 12)]
                }
                
                
                if (im_spi_c <= 0) {
                  im_ok_spi_c = 12 + im_spi_c
                  dum = spi_c[i, j, 1:(dim(spi_c)[3] - 12), isc_spi_c] # [i,j,1:240]
                  spi_c_aux = dum[seq(im_ok_spi_c, length(dum), 12)]
                } else {
                  im_ok_spi_c = im_spi_c
                  dum = spi_c[i, j, 13:dim(spi_c)[3], isc_spi_c]       # [i,j,13:252]
                  spi_c_aux = dum[seq(im_ok_spi_c, length(dum), 12)]
                }
                
                
                
                if (length(which(is.na(spi_c_aux))) == length(spi_c_aux)) {
                  next
                }
                
                if (length(which(is.na(spi_a_aux))) == length(spi_a_aux)) {
                  next
                }
                
                if (length(unique(spi_a_aux)) <= 2) {
                  next
                }
                
                if (length(unique(spi_c_aux)) <= 2) { #cpc has point with always the same values,GPCC has point with only 1 different value in the entire time series
                  next
                }
                
                # if (abs(spi_c_aux) == abs(spi_a_aux)) { #cpc has point with always the same values,GPCC has point with only 1 different value in the entire time series
                #   next
                # }
                
                
                
                #if (abs(round(spi_a_aux,4)) == abs(round(spi_c_aux,4))) { 
                 # next
                #}
                
                
                pre1 = vector()
                pre2 = vector()
                pre12 = vector()
                
                BAS_train = vector()
                for (iy in 1:length(years)) {
                  BAS_train = BAS[i, j,-iy]
                  BA_train <-
                    ifelse(BAS_train >= median(BAS_train, na.rm = TRUE),
                           1,
                           0)
                  spi_a_train = spi_a_aux[-iy] #lenght 19
                  spi_c_train = spi_c_aux[-iy] #lenght 19
                  
                  
                  
                  spi_a_test = spi_a_aux[iy]
                  spi_c_test = spi_c_aux[iy]
                  
                  
                  mydata_train = data.frame(
                    "y" = (BA_train),
                    "x1" = spi_c_train,
                    "x2" = spi_a_train
                  )
                  
                  
                  mydata_test = data.frame("x1" = (spi_c_test),
                                           "x2" = (spi_a_test))
                  
                  fit1 <-
                    glm(y ~ x1, data = mydata_train, family = binomial)
                  fit2 <-
                    glm(y ~ x2, data = mydata_train, family = binomial)
                  fit12 <-
                    glm(y ~ x1 + x2,
                        data = mydata_train,
                        family = binomial)
                  
                  # probabilities = predict(fit3, mydata_test, type="response")
                  # log(probabilities/(1-probabilities))
                  # predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
                  # predicted.classes
                  
                  pre1[iy] = fit1$coefficients[1] + fit1$coefficients[2] * mydata_test$x1
                  pre2[iy] = fit2$coefficients[1] + fit2$coefficients[2] * mydata_test$x2
                  pre12[iy] = fit12$coefficients[1] + fit12$coefficients[2] *
                    mydata_test$x1 + fit12$coefficients[3] * mydata_test$x2
                  
                  pre1[iy] =  1 / (1 + exp(-pre1[iy]))
                  
                  pre2[iy] =  1 / (1 + exp(-pre2[iy]))
                  
                  pre12[iy] =  1 / (1 + exp(-pre12[iy]))
                  
                  
                  best_x1_1[k, iy] = fit1$coefficients[2]
                  best_x1_12[k, iy] = fit1$coefficients[2]
                  
                  best_x2_2[k, iy] = fit2$coefficients[2]
                  best_x2_12[k, iy] = fit12$coefficients[3]
                  
                }
                BA[i, j, ] <-
                  ifelse(BAS[i, j, ] >= median(BAS[i, j, ], na.rm = TRUE), 1, 0)
                roc1 = roc.area(BA[i, j, ], pre1)
                roc2 = roc.area(BA[i, j, ], pre2)
                roc12 = roc.area(BA[i, j, ], pre12)
                
                ## only x1 and x2 positive
                if (roc1$A > roc2$A &
                    roc1$A > roc12$A &
                    min(best_x1_1[k, ], na.rm = TRUE) > 0) {
                  roca[k] = roc1$A
                  sig[k] = roc1$p.value
                  best_x1[k, ] = best_x1_1[k, ]
                  best_t_spi_c[k] = isc_spi_c
                  best_m_spi_c[k] = im_spi_c - last_month
                }
                if (roc2$A > roc1$A &
                    roc2$A > roc12$A &
                    min(best_x2_2[k, ], na.rm = TRUE) > 0) {
                  roca[k] = roc2$A
                  sig[k] = roc2$p.value
                  best_x2[k, ] = best_x2_2[k, ]
                  best_t_spi_a[k] = isc_spi_a
                  
                  
                  #===============================================================================
                  if (season == 'DJF' && first_month == 12) {
                    best_m_spi_a[k] = im_spi_a - (first_month - 12)
                  } else {
                    best_m_spi_a[k] = im_spi_a - first_month
                  }
                  
                  # best_m_spi_a[k] = im_spi_a - first_month
                  # best_m_spi_a[k] = im_spi_a - (first_month-12)
                  #===============================================================================
                  
                }
                if (roc12$A > roc1$A &
                    roc12$A > roc2$A &
                    min(best_x1_12[k, ], na.rm = TRUE) > 0 &
                    min(best_x2_12[k, ], na.rm = TRUE) > 0) {
                  roca[k] = roc12$A
                  sig[k] = roc12$p.value
                  best_x1[k, ] = best_x1_12[k, ]
                  best_x2[k, ] = best_x2_12[k, ]
                  best_t_spi_c[k] = isc_spi_c
                  best_m_spi_c[k] = im_spi_c - last_month
                  best_t_spi_a[k] = isc_spi_a
                  
                  #===============================================================================
                  if (season == 'DJF' && first_month == 12) {
                    best_m_spi_a[k] = im_spi_a - (first_month - 12)
                  } else {
                   best_m_spi_a[k] = im_spi_a - first_month
                  }
                  # best_m_spi_a[k] = im_spi_a -(first_month-12)
                  # best_m_spi_a[k] = im_spi_a -first_month
                  #===============================================================================
                }
              }
            }
          }
        }
        
        if (FDR==1) {
          sig = p.adjust(sig, method = "fdr")
        }
        
        # sig = p.adjust(sig, method = "fdr")
        # roca[sig > 0.05] = NA  #significancia del roc
        
        if (length(which(is.na(roca))) == length(roca)) {
          next
        }
        
        
        dum = max((roca), na.rm = TRUE)
        idx = which((roca) == dum, arr.ind = TRUE)
        idx = idx[1]
        best_roc[i, j] = roca[idx]
        best_sig[i, j] = sig[idx]
        best_m_spi_a_fin[i, j] = best_m_spi_a[idx]
        best_t_spi_a_fin[i, j] = best_t_spi_a[idx]
        best_m_spi_c_fin[i, j] = best_m_spi_c[idx]
        best_t_spi_c_fin[i, j] = best_t_spi_c[idx]
        
        #best_sig = sig[idx]
        
        
        best_x1_fin[i, j,] = best_x1[idx,]
        best_x2_fin[i, j,] = best_x2[idx,]
        
      }
    }
  }
  
  
  summary(best_x1_fin)
  
  
  length(which(is.na(best_roc))) / length(best_roc)
  
  image.plot(lon, lat, best_roc)
  plot(wrld_simpl, add = TRUE)
  
  best_x1_fin[i, j,]
  
  
  save(best_roc,
       file = paste0(dir_out, "roc_area_", version, "_", dataset, ".RData"))
  save(best_sig,
       file = paste0(dir_out, "sig_", version, "_", dataset, ".RData"))
  save(
    best_m_spi_a_fin,
    file = paste0(dir_out, "best_m_spi_a_fin_", version, "_", dataset, ".RData")
  )
  save(
    best_t_spi_a_fin,
    file = paste0(dir_out, "best_t_spi_a_fin_", version, "_", dataset, ".RData")
  )
  save(
    best_m_spi_c_fin,
    file = paste0(dir_out, "best_m_spi_c_fin_", version, "_", dataset, ".RData")
  )
  save(
    best_t_spi_c_fin,
    file = paste0(dir_out, "best_t_spi_c_fin_", version, "_", dataset, ".RData")
  )

  save(best_x1_fin,
       file = paste0(dir_out, "best_x1_", version, "_", dataset, ".RData"))
  save(best_x2_fin,
       file = paste0(dir_out, "best_x2_", version, "_", dataset, ".RData"))
  
  }
}

