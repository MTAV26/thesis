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

data(wrld_simpl)

## fix parameters
dir_f51 = '~/Chapter5-4FIRE/Data/'
dir_data = '~/Chapter5-4FIRE/Data/'
dir_out = '~/Chapter5-4FIRE/Data/'
dir_drop = '~/Chapter5-4FIRE/Data/'
dir_s5 = '~/Chapter5-4FIRE/Data/'

## fix parameters
years = 2001:2020 #antes en el original: 2001:2019
num_ens = 25
season = 'MAM'
FDR=1
if (FDR==1) {
  version = paste0(season,'_FDR')
} else {
  version = paste0(season)
}


if (season == 'JJA') {
  first_month = 6
  last_month = 8
} else if (season == 'MAM') {
  first_month = 3
  last_month = 5
} else if (season == 'SON') {
  first_month = 9
  last_month = 11
} else {
  print('no season, call terminator')
}


brk_prob <- seq(0.6, 1, length.out = 5)
col_prob <-(colorRampPalette(brewer.pal(length(brk_prob), "YlGn"))(length(brk_prob)-1))

## load SPI
fname <- file.path(dir_drop, 'DROP_3_1981_2020.nc')
obs.nc <- nc_open(fname)
obs.nc$dim$lon$vals -> lon
obs.nc$dim$lat$vals -> lat
ilat = which(lat > -60 & lat < 75)
ilon = which(lon > -165 & lon < 180)

## BA
load(paste0(dir_f51, "/BA_200101_202012_nat.RData"))

ni = dim(BA)[1]
nj = dim(BA)[2]

#monthly BA --> JJA BA
BAS = array(0, dim = c(ni, nj, length(years)))

for (i in 1:ni) {
  for (j in 1:nj) {
    for (iyear in 1:length(years)) {
      i1 = (iyear - 1) * 12 + first_month
      i2 = (iyear - 1) * 12 + last_month
      BAS[i, j, iyear] = sum(BA[i, j, i1:i2], na.rm = TRUE)
    }
  }
}

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
rm(BA)
BA = BAS * NA
## load observed spi
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
             'MSWEP'
)

# datasets = c('CHIRPS')

# ## create mask where we have a climate-fire model
mask_aux = array(NA, dim = c(ni, nj, length(datasets)))
for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  print(dataset)
  load(paste0(dir_data, "sig_", version, "_", dataset, ".RData"))
  best_sig[best_sig>0.05]=NA
  mask_aux[,,idata]=best_sig/best_sig
}

image.plot(lon, lat, mask_aux[,,1])

# #####################################################
mask_model = array(NA, dim = c(ni, nj))
num_model = array(NA, dim = c(ni, nj))
for (i in 1:ni) {
  for (j in 1:nj) {
    # length(which(is.na(mask_aux[11,61,])))
    num_model[i,j]=length(which(!is.na(mask_aux[i,j,])))
    if (length(which(is.na(mask_aux[i,j,])))<length(datasets)) {
      mask_model[i,j]=1
    }
  }
}


################################################################################
# MASCARA PARA CHIRPS PARA CALCULAR EL % DE CHIRPS
################################################################################
# load(file.path(paste0(dir_drop, "SPI3_CHIRPS_1981_2020.RData" )))
# mask_chirps = array(0, dim = c(ni, nj))
# for (i in 1:ni) {
#   for (j in 1:nj) {
#     # print(length(BA[i,j,]==0))
#     if (length(which(!is.na(spi3[i, j, ]))) && mask[i, j ] == 1 ) {
#       mask_chirps[i, j] = 1
#     }
#   }
# }
# image.plot(lon, lat, mask_chirps)
################################################################################


predBA = array(NA, dim = c(ni, nj, length(years), length(datasets)))
predBAobs = array(NA, dim = c(ni, nj, length(years), length(datasets)))

matx_roca_pred= array(NA, length(datasets))
matx_roca_obs =array(NA,length(datasets))

matx_bs_pred= array(NA, c(length(datasets),6))
matx_bs_obs =array(NA,c(length(datasets),6))

matx_bs_pred_pval= array(NA, c(length(datasets),6))
matx_bs_obs_pval=array(NA,c(length(datasets),6))

for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  print(dataset)
  
  load(file.path(paste0(dir_drop, "SPI3_", dataset, "_1981_2020.RData" )))
  load(file.path(paste0(dir_drop, "SPI6_", dataset, "_1981_2020.RData" )))
  load(file.path(paste0(dir_drop, "SPI12_", dataset, "_1981_2020.RData")))
  
  
  ## load spi_c
  years_spi_c = 1981:2020
  iok_spi_c = match((years[1] - 1):years[length(years)], years_spi_c) #we need also 2000
  #spi_c coef is suppose to be >0, while the spi effect is supposet to be <0. to have the same considtions below, i changes the sign of the spi
  spi_c3 = -spi3[, , (((iok_spi_c[1] - 1) * 12) + 1):(iok_spi_c[length(iok_spi_c)] * 12  )]
  spi_c6 = -spi6[, , (((iok_spi_c[1] - 1) * 12) + 1):(iok_spi_c[length(iok_spi_c)] * 12  )]
  spi_c12 = -spi12[, , (((iok_spi_c[1] - 1) * 12) + 1):(iok_spi_c[length(iok_spi_c)] * 12)]
  
  ## load SPEI
  years_spi_a = 1981:2020
  years = 2000:2020  #el area quemada solo tiene datos hasta 2019
  iok_spi_a = match((years[1] - 1):years[length(years)], years_spi_a) #we need also 1999
  
  spi_a3 = spi3[, , (((iok_spi_a[1] - 1) * 12) + 1):(iok_spi_a[length(iok_spi_a)] * 12  )]
  spi_a6 = spi6[, , (((iok_spi_a[1] - 1) * 12) + 1):(iok_spi_a[length(iok_spi_a)] * 12  )]
  spi_a12 = spi12[, , (((iok_spi_a[1] - 1) * 12) + 1):(iok_spi_a[length(iok_spi_a)] * 12)]
  dim(spi_a6)
  
  
  years = 2001:2020
  spi_c = array(NA, dim = c(ni, nj, dim(spi_c3)[3], 3))
  spi_c[, , , 1] = spi_c3
  spi_c[, , , 2] = spi_c6
  spi_c[, , , 3] = spi_c12
  
  rm(spi_c3, spi_c6, spi_c12)
  spi_c[is.infinite(spi_c)] = NA
  
  spi_a = array(NA, dim = c(ni, nj, dim(spi_a3)[3], 3))
  spi_a[, , , 1] = spi_a3
  spi_a[, , , 2] = spi_a6
  spi_a[, , , 3] = spi_a12
  
  rm(spi_a3, spi_a6, spi_a12)
  spi_a[is.infinite(spi_a)] = NA
  
  
  ## load seasonal forecast
  spipred = array(NA, dim = c(ni, nj, length(years), num_ens, 4, 3))
  
  load(paste0(dir_s5, paste0("SPI3SEAS5_02_MAY_",dataset,".RData")))
  spi3pred4[is.infinite(spi3pred4)] = NA
  aux = spi3pred4[, , 13:dim(spi3pred4)[3],]
  spipred[, , , , 4, 1] = aux[, , seq(last_month, dim(aux)[3], 12),]
  
  load(paste0(dir_s5, "SPI3SEAS5_02_APR_",dataset,".RData"))
  spi3pred3[is.infinite(spi3pred3)] = NA
  aux = spi3pred3[, , 13:dim(spi3pred3)[3],]
  spipred[, , , , 3, 1] = aux[, , seq(last_month - 1, dim(aux)[3], 12),]
  
  load(paste0(dir_s5, "SPI3SEAS5_02_MAR_",dataset,".RData"))
  spi3pred2[is.infinite(spi3pred2)] = NA
  aux = spi3pred2[, , 13:dim(spi3pred2)[3],]
  spipred[, , , , 2, 1] = aux[, , seq(last_month - 2, dim(aux)[3], 12),]
  
  load(paste0(dir_s5, "SPI3SEAS5_02_FEB_",dataset,".RData"))
  spi3pred1[is.infinite(spi3pred1)] = NA
  aux = spi3pred1[, , 13:dim(spi3pred1)[3],]
  spipred[, , , , 1, 1] = aux[, , seq(last_month - 3, dim(aux)[3], 12),]
  
  load(paste0(dir_s5, "SPI6SEAS5_02_MAY_",dataset,".RData"))
  spi6pred4[is.infinite(spi6pred4)] = NA
  aux = spi6pred4[, , 13:dim(spi6pred4)[3],]
  spipred[, , , , 4, 2] = aux[, , seq(last_month, dim(aux)[3], 12),]
  
  load(paste0(dir_s5, "SPI6SEAS5_02_APR_",dataset,".RData"))
  spi6pred3[is.infinite(spi6pred3)] = NA
  aux = spi6pred3[, , 13:dim(spi6pred3)[3],]
  spipred[, , , , 3, 2] = aux[, , seq(last_month - 1, dim(aux)[3], 12),]
  
  load(paste0(dir_s5, "SPI6SEAS5_02_MAR_",dataset,".RData"))
  spi6pred2[is.infinite(spi6pred2)] = NA
  aux = spi6pred2[, , 13:dim(spi6pred2)[3],]
  spipred[, , , , 2, 2] = aux[, , seq(last_month - 2, dim(aux)[3], 12),]
  
  load(paste0(dir_s5, "SPI6SEAS5_02_FEB_",dataset,".RData"))
  spi6pred1[is.infinite(spi6pred1)] = NA
  aux = spi6pred1[, , 13:dim(spi6pred1)[3],]
  spipred[, , , , 1, 2] = aux[, , seq(last_month - 3, dim(aux)[3], 12),]
  
  load(paste0(dir_s5, "SPI12SEAS5_02_MAY_",dataset,".RData"))
  spi12pred4[is.infinite(spi12pred4)] = NA
  aux = spi12pred4[, , 13:dim(spi12pred4)[3],]
  spipred[, , , , 4, 3] = aux[, , seq(last_month, dim(aux)[3], 12),]
  
  load(paste0(dir_s5, "SPI12SEAS5_02_APR_",dataset,".RData"))
  spi12pred3[is.infinite(spi12pred3)] = NA
  aux = spi12pred3[, , 13:dim(spi12pred3)[3],]
  spipred[, , , , 3, 3] = aux[, , seq(last_month - 1, dim(aux)[3], 12),]
  
  load(paste0(dir_s5, "SPI12SEAS5_02_MAR_",dataset,".RData"))
  spi12pred2[is.infinite(spi12pred2)] = NA
  aux = spi12pred2[, , 13:dim(spi12pred2)[3],]
  spipred[, , , , 2, 3] = aux[, , seq(last_month - 2, dim(aux)[3], 12),]
  
  load(paste0(dir_s5, "SPI12SEAS5_02_FEB_",dataset,".RData"))
  spi12pred1[is.infinite(spi12pred1)] = NA
  aux = spi12pred1[, , 13:dim(spi12pred1)[3],]
  spipred[, , , , 1, 3] = aux[, , seq(last_month - 3, dim(aux)[3], 12),]
  
  
  
  spipred = -spipred #mismo cambio de signo que spi_c
  
  
  ## load models parameters
  load(paste0(dir_data, "best_m_spi_a_fin_", version, "_", dataset, ".RData"))
  load(paste0(dir_data, "best_t_spi_a_fin_", version, "_", dataset, ".RData"))
  load(paste0(dir_data, "best_m_spi_c_fin_", version, "_", dataset, ".RData"))
  load(paste0(dir_data, "best_t_spi_c_fin_", version, "_", dataset, ".RData"))
  
  roca_pred = best_sig * NA
  sig_pred = roca_pred
  
  roca_obs = roca_pred
  sig_obs = roca_pred
  
  bs_pred = roca_pred
  bs_obs = roca_pred
  
  bs_pred_rel = roca_pred
  bs_obs_rel = roca_pred
  
  bs_pred_res = roca_pred
  bs_obs_res = roca_pred
  
  bs_pred_unc = roca_pred
  bs_obs_unc = roca_pred
  
  ## main loop
  for (i in 1:length(lon)) {
    for (j in 1:length(lat)) {
      
      if ((dataset == "JRA55") & (i == 84 & j == 42)) {
        next
      }
      
      if ((dataset == "PRECL") & (i == 78 & j == 42)) {
        next
      }
      #print(paste0('lon ', i, '/',ni,'; lat ', j,'/',nj))
      if (!is.na(best_m_spi_c_fin[i, j]) | !is.na(best_m_spi_a_fin[i, j]) 
          & !is.na(mask_model[i, j])
      ) {
        
        spi_a_aux  = rep(NA, length(years))
        spi_c_aux = rep(NA, length(years))
        
        if (!is.na(best_m_spi_c_fin[i, j])) {
          im_spi_c = best_m_spi_c_fin[i, j] + last_month  # last_month = 8
          
          if (im_spi_c <= 0) {
            im_ok_spi_c = 12 + im_spi_c  #20
            dum = spi_c[i, j, 1:(dim(spi_c)[3] - 12), best_t_spi_c_fin[i, j]]  #1:240
            spi_c_aux = dum[seq(im_ok_spi_c, length(dum), 12)]
          } else {
            im_ok_spi_c = im_spi_c   # 8
            dum = spi_c[i, j, 13:dim(spi_c)[3], best_t_spi_c_fin[i, j]]  #13:252
            spi_c_aux = dum[seq(im_ok_spi_c, length(dum), 12)]
          }
          
          spifor = array(NA, dim = c(length(years), num_ens))
          spifor = spipred[i, j, , , best_m_spi_c_fin[i, j] + 4, best_t_spi_c_fin[i, j]]
          
          if (length(which(is.na(spi_c_aux))) == length(spi_c_aux)) {
            next
          }
          
          # rho = cor.test(
          #   spi_c_aux,
          #   apply(spifor, c(1), mean, na.rm = T),
          #   use = "pairwise.complete.obs",
          #   alternative = "greater"
          # )
          # 
          # corr_s5[i, j] = rho$estimate
          # sig_s5[i, j] = rho$p.value
          
        }
        
        if (!is.na(best_m_spi_a_fin[i, j])) {
          im_spi_a = best_m_spi_a_fin[i, j] + first_month  # 6
          
          if (im_spi_a <= -12) {
            im_ok = 24 + im_spi_a   #24
            dum = spi_a[i, j, 1:(dim(spi_a)[3] - 24), best_t_spi_a_fin[i, j]]  #1:240
            spi_a_aux = dum[seq(im_ok, length(dum), 12)]  #mesi_12
            
          } else if (im_spi_a > -12 & im_spi_a <= 0) {
            im_ok = 12 + im_spi_a
            dum = spi_a[i, j, 13:(dim(spi_a)[3] - 12), best_t_spi_a_fin[i, j]] #13:252
            spi_a_aux = dum[seq(im_ok, length(dum), 12)]
            
          } else {
            im_ok = im_spi_a
            dum = spi_a[i, j, 25:dim(spi_a)[3], best_t_spi_a_fin[i, j]] #25:264
            spi_a_aux = dum[seq(im_ok, length(dum), 12)]
            
          }
          
          if (length(which(is.na(spi_a_aux))) == length(spi_a_aux)) {
            next
          }
          
        }
        
        pre_obs = vector()
        pre = vector()
        BAS_train = vector()
        
        for (iy in 1:length(years)) {
          BAS_train = BAS[i, j, -iy]
          BA_train <-ifelse(BAS_train >= median(BAS_train, na.rm = TRUE),1,0)
          
          spi_a_train = spi_a_aux[-iy]
          spi_c_train = spi_c_aux[-iy]
          
          spi_a_test = spi_a_aux[iy]
          spi_c_test = spi_c_aux[iy]
          
          mydata_train = data.frame("y" = (BA_train),
                                    "x1" = spi_c_train,
                                    "x2" = spi_a_train)
          
          if (is.na(best_m_spi_a_fin[i, j]) & !is.na(best_m_spi_c_fin[i, j])) {
            
            fit <- glm(y ~ x1, data = mydata_train, family = binomial)
            
            dum_pre = vector()
            
            for (iens in 1:num_ens) {
              dum_pre[iens] = fit$coefficients[1] + fit$coefficients[2] * spifor[iy, iens]
            }
            
            pre[iy] = mean(dum_pre, na.rm = TRUE)
            pre_obs[iy] = fit$coefficients[1] + fit$coefficients[2] * spi_c_test
            
          } else if (is.na(best_m_spi_c_fin[i, j]) & !is.na(best_m_spi_a_fin[i, j])) {
            
            fit <- glm(y ~ x2, data = mydata_train, family = binomial)
            
            pre[iy] = fit$coefficients[1] + fit$coefficients[2] * spi_a_test
            pre_obs[iy] = pre[iy]
            
          } else if (!is.na(best_m_spi_c_fin[i, j]) & !is.na(best_m_spi_a_fin[i, j])) {
            
            fit <- glm(y ~ x1 + x2, data = mydata_train, family = binomial)
            
            dum_pre = vector()
            
            for (iens in 1:num_ens) {
              dum_pre[iens] = fit$coefficients[1] + fit$coefficients[2] * spifor[iy, iens] + fit$coefficients[3] * spi_a_test
            }
            
            pre[iy] = mean(dum_pre, na.rm = TRUE)
            pre_obs[iy] = fit$coefficients[1] + fit$coefficients[2] * spi_c_test + fit$coefficients[3] * spi_a_test
            
          }
          
          pre[iy] =  1 / (1 + exp(-pre[iy]))
          pre_obs[iy] =  1 / (1 + exp(-pre_obs[iy]))
          
        }
        
        predBAobs[i,j,,idata]=pre_obs
        predBA[i,j,,idata]=pre
        
        BA[i, j,] <-  ifelse(BAS[i, j,] >= median(BAS[i, j,], na.rm = TRUE), 1, 0)
        
        roc_obs = roc.area(BA[i, j,], pre_obs)
        roc_pred = roc.area(BA[i, j,], pre)
        
        roca_obs[i, j] = roc_obs$A
        sig_obs[i, j] = roc_obs$p.value
        
        roca_pred[i, j] = roc_pred$A
        sig_pred[i, j] = roc_pred$p.value
        
        bs_obs_dum= brier(BA[i, j,], pre_obs)
        bs_pred_dum = brier(BA[i, j,], pre)
        
        bs_obs[i, j] = as.numeric(bs_obs_dum$bs)
        bs_pred[i, j] = as.numeric( bs_pred_dum$bs)
        
        bs_obs_rel[i, j] = as.numeric(bs_obs_dum$bs.reliability)
        bs_pred_rel[i, j] = as.numeric( bs_pred_dum$bs.reliability)
        
        bs_obs_res[i, j] = as.numeric(bs_obs_dum$bs.resol)
        bs_pred_res[i, j] = as.numeric( bs_pred_dum$bs.resol)
        
        bs_obs_unc[i, j] = as.numeric(bs_obs_dum$bs.uncert)
        bs_pred_unc[i, j] = as.numeric( bs_pred_dum$bs.uncert)
        
      }
    }
  }
  
  
  if (FDR==1) {
    sig_obs = p.adjust(sig_obs, method = "fdr")
    sig_pred = p.adjust(sig_pred, method = "fdr")
  }
  
  
  
  
  
  mask_dataset = array(0, dim = c(ni, nj))
  # mask_dataset = array(0, dim = c(ni, nj))
  for (i in 1:ni) {
    for (j in 1:nj) {
      # print(length(BA[i,j,]==0))
      if (length(which(!is.na(spi3[i, j, ]))) && mask[i, j ] == 1 ) {
        mask_dataset[i, j] = 1
      }
    }
  }
  image.plot(lon, lat, mask_dataset)
  
  
  
  
  
  
  
  
  best_roc_obs = roca_obs
  best_sig_obs = sig_obs
  best_roc_obs[best_sig_obs > 0.05] = NA # pasamos el filtro FDR al modelo de las 11 observaciones
  
  matx_bs_obs[idata,1]=round(mean(as.vector(bs_obs), na.rm=TRUE),2)
  matx_bs_obs[idata,2]=round(mean(as.vector(bs_obs_rel), na.rm=TRUE),4)
  matx_bs_obs[idata,3]=round(mean(as.vector(bs_obs_res), na.rm=TRUE),3)
  matx_bs_obs[idata,4]=round(mean(as.vector(best_roc_obs), na.rm=TRUE),2)
  matx_bs_obs[idata,5]=round(median(as.vector(best_roc_obs), na.rm=TRUE),2)
  
  # if(dataset == "CHIRPS"){
  matx_bs_obs[idata,6]=round(100 * length(which(!is.na(best_roc_obs))) / length(which(mask_dataset == 1)), 0)
  # } else {
  # matx_bs_obs[idata,6]=round(100 * length(which(!is.na(best_roc_obs))) / length(which(mask== 1)), 0)
  # }
  
  
  print(matx_bs_obs)
  
  bs_obs_pval=bs_obs
  bs_obs_pval[sig_obs > 0.05] = NA
  bs_obs_rel_pval=bs_obs_rel
  bs_obs_rel_pval[sig_obs > 0.05] = NA
  bs_obs_res_pval=bs_obs_res
  bs_obs_res_pval[sig_obs > 0.05] = NA
  bs_obs_unc_pval=bs_obs_unc
  bs_obs_unc_pval[sig_obs > 0.05] = NA
  
  matx_bs_obs_pval[idata,1]=round(mean(as.vector(bs_obs_pval), na.rm=TRUE),2)
  matx_bs_obs_pval[idata,2]=round(mean(as.vector(bs_obs_rel_pval), na.rm=TRUE),4)
  matx_bs_obs_pval[idata,3]=round(mean(as.vector(bs_obs_res_pval), na.rm=TRUE),3)
  matx_bs_obs_pval[idata,4]=round(mean(as.vector(best_roc_obs), na.rm=TRUE),2)
  matx_bs_obs_pval[idata,5]=round(median(as.vector(best_roc_obs), na.rm=TRUE),2)
  
  # if(dataset == "CHIRPS"){
  matx_bs_obs_pval[idata,6]=round(100 * length(which(!is.na(best_roc_obs))) / length(which(mask_dataset == 1)), 0)
  # } else {
  # matx_bs_obs_pval[idata,6]=round(100 * length(which(!is.na(best_roc_obs))) / length(which(mask == 1)), 0)
  # }
  
  print(matx_bs_obs_pval)
  
  best_roc_pred = roca_pred
  best_sig_pred = sig_pred
  best_roc_pred[best_sig_pred > 0.05] = NA # pasamos el filtro FDR a las 11 previsiones.
  
  matx_bs_pred[idata,1]=round(mean(as.vector(bs_pred), na.rm=TRUE),2)
  matx_bs_pred[idata,2]=round(mean(as.vector(bs_pred_rel), na.rm=TRUE),4)
  matx_bs_pred[idata,3]=round(mean(as.vector(bs_pred_res), na.rm=TRUE),3)
  matx_bs_pred[idata,4]=round(mean(as.vector(best_roc_pred), na.rm=TRUE),2)
  matx_bs_pred[idata,5]=round(median(as.vector(best_roc_pred), na.rm=TRUE),2)
  
  # if(dataset == "CHIRPS"){
  matx_bs_pred[idata,6]=round(100 * length(which(!is.na(best_roc_pred))) / length(which(mask_dataset == 1)),0)
  # }else{
  #  matx_bs_pred[idata,6]=round(100 * length(which(!is.na(best_roc_pred))) / length(which(mask == 1)),0)
  # }
  print(matx_bs_pred)
  
  bs_pred_pval=bs_pred
  bs_pred_pval[sig_pred > 0.05] = NA
  bs_pred_rel_pval=bs_pred_rel
  bs_pred_rel_pval[sig_pred > 0.05] = NA
  bs_pred_res_pval=bs_pred_res
  bs_pred_res_pval[sig_pred > 0.05] = NA
  bs_pred_unc_pval=bs_pred_unc
  bs_pred_unc_pval[sig_pred > 0.05] = NA
  
  matx_bs_pred_pval[idata,1]=round(mean(as.vector(bs_pred_pval), na.rm=TRUE),2)
  matx_bs_pred_pval[idata,2]=round(mean(as.vector(bs_pred_rel_pval), na.rm=TRUE),4)
  matx_bs_pred_pval[idata,3]=round(mean(as.vector(bs_pred_res_pval), na.rm=TRUE),3)
  matx_bs_pred_pval[idata,4]=round(mean(as.vector(best_roc_pred), na.rm=TRUE),2)
  matx_bs_pred_pval[idata,5]=round(median(as.vector(best_roc_pred), na.rm=TRUE),2)
  # if(dataset == "CHIRPS"){
  matx_bs_pred_pval[idata,6]=round(100 * length(which(!is.na(best_roc_pred))) / length(which(mask_dataset == 1)),0)
  # } else{
  # matx_bs_pred_pval[idata,6]=round(100 * length(which(!is.na(best_roc_pred))) / length(which(mask == 1)),0)
  # }
  
  print(matx_bs_pred_pval)
  rm(mask_dataset)
  # 
  save(best_roc_obs, file = paste0(dir_out_obs, "roc_obs_", version, "_", dataset, ".RData"))
  save(best_sig_obs, file = paste0(dir_out_obs, "sig_obs_", version, "_", dataset, ".RData"))
  save(best_roc_pred, file = paste0(dir_out_pre, "roc_4fire_", version, "_", dataset, ".RData"))
  save(best_sig_pred, file = paste0(dir_out_pre, "sig_4fire_", version, "_", dataset, ".RData"))
  
  save(bs_pred, file = paste0(dir_out_pre, "bs_pred_", version, "_", dataset, ".RData"))
  save(bs_obs, file = paste0(dir_out_obs, "bs_obs_", version, "_", dataset, ".RData"))
  save(bs_pred_rel, file = paste0(dir_out_pre, "bs_pred_rel_", version, "_", dataset, ".RData"))
  save(bs_obs_rel, file = paste0(dir_out_obs, "bs_obs_rel_", version, "_", dataset, ".RData"))
  save(bs_pred_res, file = paste0(dir_out_pre, "bs_pred_res_", version, "_", dataset, ".RData"))
  save(bs_obs_res, file = paste0(dir_out_obs, "bs_obs_res_", version, "_", dataset, ".RData"))
  save(bs_pred_unc, file = paste0(dir_out_pre, "bs_pred_unc_", version, "_", dataset, ".RData"))
  save(bs_obs_unc, file = paste0(dir_out_obs, "bs_obs_unc_", version, "_", dataset, ".RData"))
  
  save(bs_pred_pval, file = paste0(dir_out_pre, "bs_pred_", version, "_", dataset, "_pval.RData"))
  save(bs_obs_pval, file = paste0(dir_out_obs, "bs_obs_", version, "_", dataset, "_pval.RData"))
  save(bs_pred_rel_pval, file = paste0(dir_out_pre, "bs_pred_rel_", version, "_", dataset, "_pval.RData"))
  save(bs_obs_rel_pval, file = paste0(dir_out_obs, "bs_obs_rel_", version, "_", dataset, "_pval.RData"))
  save(bs_pred_res_pval, file = paste0(dir_out_pre, "bs_pred_res_", version, "_", dataset, "_pval.RData"))
  save(bs_obs_res_pval, file = paste0(dir_out_obs, "bs_obs_res_", version, "_", dataset, "_pval.RData"))
  save(bs_pred_unc_pval, file = paste0(dir_out_pre, "bs_pred_unc_", version, "_", dataset, "_pval.RData"))
  save(bs_obs_unc_pval, file = paste0(dir_out_obs, "bs_obs_unc_", version, "_", dataset, "_pval.RData"))
  
}


save(predBAobs,file = paste0(dir_out_obs, "preBAobs_drop_", version, ".RData"))
save(predBA,file = paste0(dir_out_pre, "preBApre_4fire_", version, ".RData"))

summary(as.vector(best_roc_obs))
summary(as.vector(best_roc_pred))

dev.off()
image.plot(lon, lat, mask_dataset)
image.plot(lon, lat, mask)
image.plot(lon, lat, best_roc_obs, col="purple", breaks = c(0,1), add=TRUE)
image.plot(lon, lat, best_roc_pred, col="green", breaks = c(0,1), add=TRUE)

image.plot(lon, lat, mask)
image.plot(lon, lat, bs_obs, col="purple", breaks = c(0,1), add=TRUE)
image.plot(lon, lat, bs_obs_pval, col="green", breaks = c(0,1), add=TRUE)

image.plot(lon, lat, mask)
image.plot(lon, lat, best_roc_obs, col="purple", breaks = c(0,1), add=TRUE)
image.plot(lon, lat, bs_obs_pval, col="green", breaks = c(0,1), add=TRUE)


################################################################################
# DROP and 4FIRE
################################################################################
load(paste0(dir_out_obs, "preBAobs_drop_", version, ".RData"))
load(paste0(dir_out_pre, "preBApre_4fire_", version, ".RData"))

# predBAobs1 <- ifelse(predBAobs >= median(predBAobs, na.rm = TRUE), 1, 0)  # binario
# predBApre2 <- ifelse(predBA >= median(predBA, na.rm = TRUE), 1, 0)  #binario

predBAobs1 = predBAobs #media de las probabilidades
predBApre1 = predBA  # media de las probabilidades


PredBAobs_ens=(apply(predBAobs1, c(1, 2, 3), mean, na.rm = TRUE))
PredBApre_ens=(apply(predBApre1, c(1, 2, 3), mean, na.rm = TRUE))


roca_drop = array(NA, dim = c(ni, nj))
roca_4fire = array(NA, dim = c(ni, nj))
sig_drop = array(NA, dim = c(ni, nj))
sig_4fire= array(NA, dim = c(ni, nj))

bs_drop = array(NA, dim = c(ni, nj))
bs_4fire= array(NA, dim = c(ni, nj))
bs_drop_rel = array(NA, dim = c(ni, nj))
bs_4fire_rel= array(NA, dim = c(ni, nj))
bs_drop_res = array(NA, dim = c(ni, nj))
bs_4fire_res= array(NA, dim = c(ni, nj))
bs_drop_unc = array(NA, dim = c(ni, nj))
bs_4fire_unc= array(NA, dim = c(ni, nj))

roc1 = vector()
roc2 = vector()

bs1 = vector()
bs2 = vector()

for (i in 1:ni) {
  for (j in 1:nj) {
    # if (!is.na(sum(PredBAobs_ens[i, j, ]))) {
    if (!is.na(mask_model[i, j])) {
      
      BA[i, j, ] <- ifelse(BAS[i, j, ] >= median(BAS[i, j, ], na.rm = TRUE), 1, 0)
      
      roc1 = roc.area(BA[i, j, ], PredBAobs_ens[i, j, ])
      roca_drop[i, j] = roc1$A
      sig_drop[i, j] = roc1$p.value
      
      roc2 = roc.area(BA[i, j, ], PredBApre_ens[i, j, ])
      roca_4fire[i, j] = roc2$A
      sig_4fire[i, j] = roc2$p.value
      
      bs1= brier(BA[i, j,], PredBAobs_ens[i, j, ])
      bs2 = brier(BA[i, j,], PredBApre_ens[i, j, ])
      
      bs_drop[i, j] = as.numeric(bs1$bs)
      bs_4fire[i, j] = as.numeric( bs2$bs)
      
      bs_drop_rel[i, j] = as.numeric(bs1$bs.reliability)
      bs_4fire_rel[i, j] = as.numeric( bs2$bs.reliability)
      
      bs_drop_res[i, j] = as.numeric(bs1$bs.resol)
      bs_4fire_res[i, j] = as.numeric( bs2$bs.resol)
      
      bs_drop_unc[i, j] = as.numeric(bs1$bs.uncert)
      bs_4fire_unc[i, j] = as.numeric( bs2$bs.uncert)
      
    }
  }
}

if (FDR==1) {
  sig_4fire = p.adjust(sig_4fire, method = "fdr")
  sig_drop = p.adjust(sig_drop, method = "fdr")
  
}

roca_drop[sig_drop > 0.05] = NA # pasamos el filtro FDR al ensemble de DROP
roca_4fire[sig_4fire > 0.05] = NA # pasasmos el filtro FDR al ensemble de 4FIRE

# dim(PredBAobs_ens)
# dev.off()
# image.plot(lon, lat, PredBAobs_ens[, , 19], add=TRUE)
# image.plot(lon, lat, mask,col=c( "white","purple"))

mask_dataset = array(0, dim = c(ni, nj))
# mask_dataset = array(0, dim = c(ni, nj))
for (i in 1:ni) {
  for (j in 1:nj) {
    # print(length(BA[i,j,]==0))
    if (length(which(!is.na(PredBAobs_ens[i, j, ]))) && mask[i, j ] == 1 ) {
      mask_dataset[i, j] = 1
    }
  }
}



datasets = c('CPC','GPCC','PRECL',  'ERA5',
             'JRA55',  'NCEP',  'MERRA2',  'CAMS_OPI',
             'CHIRPS',  'GPCP',  'MSWEP',   'DROP')

matx_drop= array(NA, c(length(datasets),6))
matx_drop_pval= array(NA, c(length(datasets),6))
# dim(matx_drp)
matx_drop[1:11,1:6]= matx_bs_obs
matx_drop



matx_drop[12,1]=round(mean(as.vector(bs_drop), na.rm=TRUE),2)
matx_drop[12,2]=round(mean(as.vector(bs_drop_rel), na.rm=TRUE),4)
matx_drop[12,3]=round(mean(as.vector(bs_drop_res), na.rm=TRUE),3)
matx_drop[12,4]=round(mean(as.vector(roca_drop), na.rm=TRUE),2)
matx_drop[12,5]=round(median(as.vector(roca_drop), na.rm=TRUE),2)
matx_drop[12,6]=round(100 * length(which(!is.na(roca_drop))) / length(which(mask_dataset == 1)), 0)
matx_drop

matx_drop_pval[1:11,1:6]=matx_bs_obs_pval
matx_drop_pval

bs_drop_pval=bs_drop
bs_drop_pval[sig_drop > 0.05] = NA
bs_drop_rel_pval=bs_drop_rel
bs_drop_rel_pval[sig_drop > 0.05] = NA
bs_drop_res_pval=bs_drop_res
bs_drop_res_pval[sig_drop > 0.05] = NA
bs_drop_unc_pval=bs_drop_unc
bs_drop_unc_pval[sig_drop > 0.05] = NA

matx_drop_pval[12,1]=round(mean(as.vector(bs_drop_pval), na.rm=TRUE),2)
matx_drop_pval[12,2]=round(mean(as.vector(bs_drop_rel_pval), na.rm=TRUE),4)
matx_drop_pval[12,3]=round(mean(as.vector(bs_drop_res_pval), na.rm=TRUE),3)
matx_drop_pval[12,4]=round(mean(as.vector(roca_drop), na.rm=TRUE),2)
matx_drop_pval[12,5]=round(median(as.vector(roca_drop), na.rm=TRUE),2)
matx_drop_pval[12,6]=round(100 * length(which(!is.na(roca_drop))) / length(which(mask_dataset == 1)), 0)
print(matx_drop_pval)


rm(mask_dataset)


mask_dataset = array(0, dim = c(ni, nj))
# mask_dataset = array(0, dim = c(ni, nj))
for (i in 1:ni) {
  for (j in 1:nj) {
    # print(length(BA[i,j,]==0))
    if (length(which(!is.na(PredBApre_ens[i, j, ]))) && mask[i, j ] == 1 ) {
      mask_dataset[i, j] = 1
    }
  }
}



datasets = c('CPC','GPCC','PRECL',  'ERA5',
             'JRA55',  'NCEP',  'MERRA2',  'CAMS_OPI',
             'CHIRPS',  'GPCP',  'MSWEP',   'DROP')

matx_4fire= array(NA, c(length(datasets),6))
matx_4fire_pval= array(NA, c(length(datasets),6))

matx_4fire[1:11,1:6]= matx_bs_pred
matx_4fire

matx_4fire[12,1]=round(mean(as.vector(bs_4fire), na.rm=TRUE),2)
matx_4fire[12,2]=round(mean(as.vector(bs_4fire_rel), na.rm=TRUE),4)
matx_4fire[12,3]=round(mean(as.vector(bs_4fire_res), na.rm=TRUE),3)
matx_4fire[12,4]=round(mean(as.vector(roca_4fire), na.rm=TRUE),2)
matx_4fire[12,5]=round(median(as.vector(roca_4fire), na.rm=TRUE),2)
matx_4fire[12,6]=round(100 * length(which(!is.na(roca_4fire))) / length(which(mask_dataset == 1)), 0)
matx_4fire

bs_4fire_pval=bs_4fire
bs_4fire_pval[sig_4fire > 0.05] = NA
bs_4fire_rel_pval=bs_4fire_rel
bs_4fire_rel_pval[sig_4fire > 0.05] = NA
bs_4fire_res_pval=bs_4fire_res
bs_4fire_res_pval[sig_4fire > 0.05] = NA
bs_4fire_unc_pval=bs_4fire_unc
bs_4fire_unc_pval[sig_4fire > 0.05] = NA

matx_4fire_pval[1:11,1:6]= matx_bs_pred_pval
matx_4fire_pval

matx_4fire_pval[12,1]=round(mean(as.vector(bs_4fire_pval), na.rm=TRUE),2)
matx_4fire_pval[12,2]=round(mean(as.vector(bs_4fire_rel_pval), na.rm=TRUE),4)
matx_4fire_pval[12,3]=round(mean(as.vector(bs_4fire_res_pval), na.rm=TRUE),3)
matx_4fire_pval[12,4]=round(mean(as.vector(roca_4fire), na.rm=TRUE),2)
matx_4fire_pval[12,5]=round(median(as.vector(roca_4fire), na.rm=TRUE),2)
matx_4fire_pval[12,6]=round(100 * length(which(!is.na(roca_4fire))) / length(which(mask_dataset == 1)), 0)
print(matx_4fire_pval)



write.table(matx_drop, file = paste0(dir_out_obs, "matrix_drop_", version, ".csv"), sep = ";",col.names = TRUE, row.names = F)
write.table(matx_drop_pval, file = paste0(dir_out_obs, "matrix_drop_pval_", version, ".csv"), sep = ";",col.names = TRUE, row.names = F)
write.table(matx_4fire, file = paste0(dir_out_pre, "matrix_4fire_", version, ".csv"), sep = ";",col.names = TRUE, row.names = F)
write.table(matx_4fire_pval, file = paste0(dir_out_pre, "matrix_4fire_pval_", version, ".csv"), sep = ";",col.names = TRUE, row.names = F)


save(roca_drop, file = paste0(dir_out_obs, "roc_drop_", version, ".RData"))
save(roca_4fire, file = paste0(dir_out_pre, "roc_4fire_", version, ".RData"))


save(bs_4fire, file = paste0(dir_out_pre, "bs_4fire_", version, ".RData"))
save(bs_drop, file = paste0(dir_out_obs, "bs_drop_", version, ".RData"))
save(bs_4fire_rel, file = paste0(dir_out_pre, "bs_4fire_rel_", version, ".RData"))
save(bs_drop_rel, file = paste0(dir_out_obs, "bs_drop_rel_", version, ".RData"))
save(bs_4fire_res, file = paste0(dir_out_pre, "bs_4fire_res_", version, ".RData"))
save(bs_drop_res, file = paste0(dir_out_obs, "bs_drop_res_", version, ".RData"))
save(bs_4fire_unc, file = paste0(dir_out_pre, "bs_4fire_unc_", version, ".RData"))
save(bs_drop_unc, file = paste0(dir_out_obs, "bs_drop_unc_", version, ".RData"))


save(bs_4fire_pval, file = paste0(dir_out_pre, "bs_4fire_", version, "_pval.RData"))
save(bs_drop_pval, file = paste0(dir_out_obs, "bs_drop_", version, "_pval.RData"))
save(bs_4fire_rel_pval, file = paste0(dir_out_pre, "bs_4fire_rel_", version, "_pval.RData"))
save(bs_drop_rel_pval, file = paste0(dir_out_obs, "bs_drop_rel_", version, "_pval.RData"))
save(bs_4fire_res_pval, file = paste0(dir_out_pre, "bs_4fire_res_", version, "_pval.RData"))
save(bs_drop_res_pval, file = paste0(dir_out_obs, "bs_drop_res_", version, "_pval.RData"))
save(bs_4fire_unc_pval, file = paste0(dir_out_pre, "bs_4fire_unc_", version, "_pval.RData"))
save(bs_drop_unc_pval, file = paste0(dir_out_obs, "bs_drop_unc_", version, "_pval.RData"))



# dev.off()
# image.plot(lon, lat, mask_dataset)
# image.plot(lon, lat, roca_drop, col="purple", breaks = c(0,1), add=TRUE)
# image.plot(lon, lat, roca_4fire, col="green", breaks = c(0,1), add=TRUE)

