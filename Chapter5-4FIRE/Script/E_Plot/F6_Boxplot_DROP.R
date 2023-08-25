rm(list = ls())
graphics.off()
gc()

# install.packages("StatDa")
# library(StatDA)


source("C:/Users/Usuario/Dropbox/4DROP/script/Common/CorrMIO.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/ColorBarM.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/mioplot_global.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot_stat.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot.R")

# dir_out= 'C:/Users/Usuario/Dropbox/4FIRE/data/model_obs_FDR/'
dir_out= 'C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs_FDR_mean1/'
dir_out1= 'C:/Users/Usuario/Desktop/RESULTADOS-4FIRE/'
# dir_out1= 'C:/Users/Usuario/Dropbox/4FIRE/Figuras/'
# seasons= c("DJF", "MAM", "JJA", "SON")
# seasons = c('JJA','SON', 'DJF', 'MAM')
bs=c("bs_obs")
seasons = c('JJA')

for (iseas in 1:length(seasons)) {
  season = seasons[iseas]
  print(season)
  
  load(paste(dir_out,bs,'_',season,'_FDR_CPC_pval.RData', sep = ""))
  corre_box = as.vector(bs_obs_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_GPCC_pval.RData', sep = ""))
  corre_box1 = as.vector(bs_obs_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_PRECL_pval.RData', sep = ""))
  corre_box2 = as.vector(bs_obs_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_ERA5_pval.RData', sep = ""))
  corre_box3 = as.vector(bs_obs_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_JRA55_pval.RData', sep = ""))
  corre_box4 = as.vector(bs_obs_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_NCEP_pval.RData', sep = ""))
  corre_box5 = as.vector(bs_obs_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_MERRA2_pval.RData', sep = ""))
  corre_box6 = as.vector(bs_obs_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_CAMS_OPI_pval.RData', sep = ""))
  corre_box7 = as.vector(bs_obs_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_CHIRPS_pval.RData', sep = ""))
  corre_box8 = as.vector(bs_obs_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_GPCP_pval.RData', sep = ""))
  corre_box9 = as.vector(bs_obs_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_MSWEP_pval.RData', sep = ""))
  corre_box10 = as.vector(bs_obs_pval)
  load(paste(dir_out,'bs_drop_',season,'_FDR_pval.RData', sep = ""))
  corre_box11 = as.vector(bs_drop_pval)
  
  
  plot_data <-
    data.frame(
      corre_box,
      corre_box1,
      corre_box2,
      corre_box3,
      corre_box4,
      corre_box5,
      corre_box6,
      corre_box7,
      corre_box8,
      corre_box9,
      corre_box10,
      corre_box11)
  #     corre_box[, 10]
  #   )
  
  
  setEPS()
  postscript(
    file.path(
      dir_out1,
      paste("BOX_BS_obs_pval_",season,".eps", sep = "")
      #paste("boxplot_corre_spi_", sc, "_ENS.eps", sep = "")
    ),
    horiz = FALSE,
    onefile = FALSE,
    width = 8.5,
    height = 5.5
  )
  par(oma = c(3, 1, 2, 1))
  my_boxplot(
    plot_data,
    quant = c(0.025, 0.975),
    outline = F,
    las = 2,
    ylim = c(0, 0.5),
    
    names =c('CPC', 'GPCC', 'PRECL', 'ERA5', 'JRA55',
             'NCEP', 'MERRA2', 'CAMS_OPI', 'CHIRPS', 
             'GPCP', 'MSWEP', "DROP"
    ),
    
    main=paste('Brier Score CLIBA (',season,')', sep=""),
    ylab='Brier Score', 
    cex.main = 1.5,
    cex.lab = 1.3,
    col = c("white","white","white","white","white","white",
            "white","white","white","white","white","dark green")
  )

  grid (NULL, NULL, lty = 6, col = "cornsilk2")
  dev.off()
  
}




#############################
#BRIER SCORE-RELIABILITY
############################

rm(list = ls())
graphics.off()
gc()

# install.packages("StatDa")
# library(StatDA)


source("C:/Users/Usuario/Dropbox/4DROP/script/Common/CorrMIO.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/ColorBarM.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/mioplot_global.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot_stat.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot.R")

# dir_out= 'C:/Users/Usuario/Dropbox/4FIRE/data/model_obs_FDR/'
dir_out= 'C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs_FDR_mean1/'
dir_out1= 'C:/Users/Usuario/Desktop/RESULTADOS-4FIRE/'
# dir_out1= 'C:/Users/Usuario/Dropbox/4FIRE/Figuras/'
# seasons= c("DJF", "MAM", "JJA", "SON")
seasons = c('JJA','SON', 'DJF', 'MAM')
bs=c("bs_obs_rel")
# seasons = c('JJA')

for (iseas in 1:length(seasons)) {
  season = seasons[iseas]
  print(season)
  
  load(paste(dir_out,bs,'_',season,'_FDR_CPC_pval.RData', sep = ""))
  corre_box = as.vector(bs_obs_rel_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_GPCC_pval.RData', sep = ""))
  corre_box1 = as.vector(bs_obs_rel_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_PRECL_pval.RData', sep = ""))
  corre_box2 = as.vector(bs_obs_rel_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_ERA5_pval.RData', sep = ""))
  corre_box3 = as.vector(bs_obs_rel_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_JRA55_pval.RData', sep = ""))
  corre_box4 = as.vector(bs_obs_rel_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_NCEP_pval.RData', sep = ""))
  corre_box5 = as.vector(bs_obs_rel_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_MERRA2_pval.RData', sep = ""))
  corre_box6 = as.vector(bs_obs_rel_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_CAMS_OPI_pval.RData', sep = ""))
  corre_box7 = as.vector(bs_obs_rel_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_CHIRPS_pval.RData', sep = ""))
  corre_box8 = as.vector(bs_obs_rel_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_GPCP_pval.RData', sep = ""))
  corre_box9 = as.vector(bs_obs_rel_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_MSWEP_pval.RData', sep = ""))
  corre_box10 = as.vector(bs_obs_rel_pval)
  load(paste(dir_out,'bs_drop_rel_',season,'_FDR_pval.RData', sep = ""))
  corre_box11 = as.vector(bs_drop_rel_pval)
  
  
  plot_data <-
    data.frame(
      corre_box,
      corre_box1,
      corre_box2,
      corre_box3,
      corre_box4,
      corre_box5,
      corre_box6,
      corre_box7,
      corre_box8,
      corre_box9,
      corre_box10,
      corre_box11)
  #     corre_box[, 10]
  #   )
  
  
  setEPS()
  postscript(
    file.path(
      dir_out1,
      paste("BOX_BS_obs_REL_pval_",season,".eps", sep = "")
      #paste("boxplot_corre_spi_", sc, "_ENS.eps", sep = "")
    ),
    horiz = FALSE,
    onefile = FALSE,
    width = 8.5,
    height = 5.5
  )
  par(oma = c(3, 1, 2, 1))
  my_boxplot(
    plot_data,
    quant = c(0.025, 0.975),
    outline = F,
    las = 2,
    ylim = c(0, 0.25),
    
    names =c('CPC', 'GPCC', 'PRECL', 'ERA5', 'JRA55',
             'NCEP', 'MERRA2', 'CAMS_OPI', 'CHIRPS', 
             'GPCP', 'MSWEP', "DROP"
    ),
    
    main=paste('Brier Score Reliability CLIBA (',season,')', sep=""),
    ylab='Reliability', 
    cex.main = 1.5,
    cex.lab = 1.3,
    col = c("white","white","white","white","white","white",
            "white","white","white","white","white","dark green")
  )
  
  grid (NULL, NULL, lty = 6, col = "cornsilk2")
  dev.off()
  
}



#############################
#BRIER SCORE-RESOLUTION
############################

rm(list = ls())
graphics.off()
gc()

source("C:/Users/Usuario/Dropbox/4DROP/script/Common/CorrMIO.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/ColorBarM.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/mioplot_global.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot_stat.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot.R")

# dir_out= 'C:/Users/Usuario/Dropbox/4FIRE/data/model_obs_FDR/'
dir_out= 'C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs_FDR_mean1/'
dir_out1= 'C:/Users/Usuario/Desktop/RESULTADOS-4FIRE/'
# dir_out1= 'C:/Users/Usuario/Dropbox/4FIRE/Figuras/'
# seasons= c("DJF", "MAM", "JJA", "SON")
seasons = c('JJA','SON', 'DJF', 'MAM')
bs=c("bs_obs_res")

for (iseas in 1:length(seasons)) {
  season = seasons[iseas]
  print(season)
  
  load(paste(dir_out,bs,'_',season,'_FDR_CPC_pval.RData', sep = ""))
  corre_box = as.vector(bs_obs_res_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_GPCC_pval.RData', sep = ""))
  corre_box1 = as.vector(bs_obs_res_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_PRECL_pval.RData', sep = ""))
  corre_box2 = as.vector(bs_obs_res_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_ERA5_pval.RData', sep = ""))
  corre_box3 = as.vector(bs_obs_res_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_JRA55_pval.RData', sep = ""))
  corre_box4 = as.vector(bs_obs_res_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_NCEP_pval.RData', sep = ""))
  corre_box5 = as.vector(bs_obs_res_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_MERRA2_pval.RData', sep = ""))
  corre_box6 = as.vector(bs_obs_res_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_CAMS_OPI_pval.RData', sep = ""))
  corre_box7 = as.vector(bs_obs_res_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_CHIRPS_pval.RData', sep = ""))
  corre_box8 = as.vector(bs_obs_res_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_GPCP_pval.RData', sep = ""))
  corre_box9 = as.vector(bs_obs_res_pval)
  load(paste(dir_out,bs,'_',season,'_FDR_MSWEP_pval.RData', sep = ""))
  corre_box10 = as.vector(bs_obs_res_pval)
  load(paste(dir_out,'bs_drop_res_',season,'_FDR_pval.RData', sep = ""))
  corre_box11 = as.vector(bs_drop_res_pval)
  
  
  plot_data <-
    data.frame(
      corre_box,
      corre_box1,
      corre_box2,
      corre_box3,
      corre_box4,
      corre_box5,
      corre_box6,
      corre_box7,
      corre_box8,
      corre_box9,
      corre_box10,
      corre_box11)
  #     corre_box[, 10]
  #   )
  
  
  setEPS()
  postscript(
    file.path(
      dir_out1,
      paste("BOX_BS_obs_RES_pval_",season,".eps", sep = "")
      #paste("boxplot_corre_spi_", sc, "_ENS.eps", sep = "")
    ),
    horiz = FALSE,
    onefile = FALSE,
    width = 8.5,
    height = 5.5
  )
  par(oma = c(3, 1, 2, 1))
  my_boxplot(
    plot_data,
    quant = c(0.025, 0.975),
    outline = F,
    las = 2,
    ylim = c(0, 0.25),
    
    names =c('CPC', 'GPCC', 'PRECL', 'ERA5', 'JRA55',
             'NCEP', 'MERRA2', 'CAMS_OPI', 'CHIRPS', 
             'GPCP', 'MSWEP', "DROP"
    ),
    
    main=paste('Brier Score Resolution CLIBA (',season,')', sep=""),
    ylab='Resolution', 
    cex.main = 1.5,
    cex.lab = 1.3,
    col = c("white","white","white","white","white","white",
            "white","white","white","white","white","dark green")
  )
  
  grid (NULL, NULL, lty = 6, col = "cornsilk2")
  dev.off()
  
}



#############################
#ROC AREA CLIMATOLOGY
############################

rm(list = ls())
graphics.off()
gc()

source("C:/Users/Usuario/Dropbox/4DROP/script/Common/CorrMIO.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/ColorBarM.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/mioplot_global.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot_stat.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot.R")

# dir_out= 'C:/Users/Usuario/Dropbox/4FIRE/data/model_obs_FDR/'
dir_out= 'C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs_FDR_mean1/'
dir_out1= 'C:/Users/Usuario/Desktop/RESULTADOS-4FIRE/'
# dir_out1= 'C:/Users/Usuario/Dropbox/4FIRE/Figuras/'
# seasons= c("DJF", "MAM", "JJA", "SON")
seasons = c('JJA','SON', 'DJF', 'MAM')
bs=c("roc_obs")

for (iseas in 1:length(seasons)) {
  season = seasons[iseas]
  print(season)
  
  load(paste(dir_out,bs,'_',season,'_FDR_CPC.RData', sep = ""))
  corre_box = as.vector(best_roc_obs)
  load(paste(dir_out,bs,'_',season,'_FDR_GPCC.RData', sep = ""))
  corre_box1 = as.vector(best_roc_obs)
  load(paste(dir_out,bs,'_',season,'_FDR_PRECL.RData', sep = ""))
  corre_box2 = as.vector(best_roc_obs)
  load(paste(dir_out,bs,'_',season,'_FDR_ERA5.RData', sep = ""))
  corre_box3 = as.vector(best_roc_obs)
  load(paste(dir_out,bs,'_',season,'_FDR_JRA55.RData', sep = ""))
  corre_box4 = as.vector(best_roc_obs)
  load(paste(dir_out,bs,'_',season,'_FDR_NCEP.RData', sep = ""))
  corre_box5 = as.vector(best_roc_obs)
  load(paste(dir_out,bs,'_',season,'_FDR_MERRA2.RData', sep = ""))
  corre_box6 = as.vector(best_roc_obs)
  load(paste(dir_out,bs,'_',season,'_FDR_CAMS_OPI.RData', sep = ""))
  corre_box7 = as.vector(best_roc_obs)
  load(paste(dir_out,bs,'_',season,'_FDR_CHIRPS.RData', sep = ""))
  corre_box8 = as.vector(best_roc_obs)
  load(paste(dir_out,bs,'_',season,'_FDR_GPCP.RData', sep = ""))
  corre_box9 = as.vector(best_roc_obs)
  load(paste(dir_out,bs,'_',season,'_FDR_MSWEP.RData', sep = ""))
  corre_box10 = as.vector(best_roc_obs)
  load(paste(dir_out,'roc_drop_',season,'_FDR.RData', sep = ""))
  corre_box11 = as.vector(roca_drop)
  
  
  plot_data <-
    data.frame(
      corre_box,
      corre_box1,
      corre_box2,
      corre_box3,
      corre_box4,
      corre_box5,
      corre_box6,
      corre_box7,
      corre_box8,
      corre_box9,
      corre_box10,
      corre_box11)
  #     corre_box[, 10]
  #   )
  
  
  setEPS()
  postscript(
    file.path(
      dir_out1,
      paste("BOX_ROC_obs_",season,".eps", sep = "")
      #paste("boxplot_corre_spi_", sc, "_ENS.eps", sep = "")
    ),
    horiz = FALSE,
    onefile = FALSE,
    width = 8.5,
    height = 5.5
  )
  par(oma = c(3, 1, 2, 1))
  my_boxplot(
    plot_data,
    quant = c(0.025, 0.975),
    outline = F,
    las = 2,
    ylim = c(0.5, 1),
    
    names =c('CPC', 'GPCC', 'PRECL', 'ERA5', 'JRA55',
             'NCEP', 'MERRA2', 'CAMS_OPI', 'CHIRPS', 
             'GPCP', 'MSWEP', "DROP"
    ),
    
    main=paste('ROC area CLIBA (',season,')', sep=""),
    ylab='ROC area', 
    cex.main = 1.5,
    cex.lab = 1.3,
    col = c("white","white","white","white","white","white",
            "white","white","white","white","white","dark green")
  )
  
  grid (NULL, NULL, lty = 6, col = "cornsilk2")
  dev.off()
  
}


