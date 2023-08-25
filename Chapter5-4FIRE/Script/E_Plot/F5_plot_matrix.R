rm(list = ls())
graphics.off()
gc()

library(pracma)
library(ncdf4)
library(fields)
library(maptools)
library(RColorBrewer)
library(viridis)
library(devtools)
library(plot.matrix)


dir_pdf="C:/Users/Usuario/Dropbox/4FIRE/"
# 
# #######################
# # DROP
# #######################
datasets = c(
  'CPC',
  'GPCC',
  'PRECL',
  'ERA5',
  'JRA55',
  'NCEP',
  'MERRA2',
  'CAMSOPI',
  'CHIRPS',
  'GPCP',
  'MSWEP',
  'DROP')
seasons =c("DJF", "MAM", "JJA", "SON")
# seasons =c("JJA")

dev.off()


pdf(paste(dir_pdf,"Figure4_new.pdf"), width=15, height=9)
set.panel() 
# split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
split.screen( rbind(c(0, .9,0,1), c(.9,1,0,1)))
split.screen(c(2,1), screen=1)-> ind
screen( ind[1])
# par(oma=c( 0,0,0,0),mar=rep(0, 4)) 

DJF<-read.csv("C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs_FDR_mean1/matrix_drop_pval_DJF_FDR.csv", sep = ";", header = F, skip = 1)
MAM<-read.csv("C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs_FDR_mean1/matrix_drop_pval_MAM_FDR.csv", sep = ";", header = F, skip = 1)
JJA<-read.csv("C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs_FDR_mean1/matrix_drop_pval_JJA_FDR.csv", sep = ";", header = F, skip = 1)
SON<-read.csv("C:/Users/Usuario/Dropbox/4FIRE/Results/model_obs_FDR_mean1/matrix_drop_pval_SON_FDR.csv", sep = ";", header = F, skip = 1)


matrix_model <-rbind(DJF$V6,MAM$V6,JJA$V6, SON$V6)
# matrix_model <-rbind(JJA)
colnames(matrix_model)<-datasets
rownames(matrix_model)<-seasons
matrix_model<-as.matrix(matrix_model)
gray_palette <- gray(seq(0, 1, length.out = 11))
plot(matrix_model, 
     digits=1,
     text.cell=list(cex=1),
     fmt.cell='%.0f', #quitamos el simbolo +
     col=gray_palette,
     main="(a) CLIBA model; Skilfull suface (%)",
      xlab="Datasets",
      ylab="Seasons",
     breaks=seq(from = 20, to = 80, by = 6)
     #,key=NULL
     )

#######################
# 4 FIRE
#######################

datasets1 = c(
  'CPC',
  'GPCC',
  'PRECL',
  'ERA5',
  'JRA55',
  'NCEP',
  'MERRA2',
  'CAMSOPI',
  'CHIRPS',
  'GPCP',
  'MSWEP',
  '4FIRE')

DJF<-read.csv("C:/Users/Usuario/Dropbox/4FIRE/Results/model_4fire_FDR_mean1/matrix_4fire_DJF_FDR.csv", sep = ";", header = F, skip = 1)
MAM<-read.csv("C:/Users/Usuario/Dropbox/4FIRE/Results/model_4fire_FDR_mean1/matrix_4fire_MAM_FDR.csv", sep = ";", header = F, skip = 1)
JJA<-read.csv("C:/Users/Usuario/Dropbox/4FIRE/Results/model_4fire_FDR_mean1/matrix_4fire_JJA_FDR.csv", sep = ";", header = F, skip = 1)
SON<-read.csv("C:/Users/Usuario/Dropbox/4FIRE/Results/model_4fire_FDR_mean1/matrix_4fire_SON_FDR.csv", sep = ";", header = F, skip = 1)


matrix_pred <-rbind(DJF$V6,MAM$V6,JJA$V6, SON$V6)
# matrix_pred <-rbind(JJA)
colnames(matrix_pred)<-datasets1
rownames(matrix_pred)<-seasons
matrix_pred<-as.matrix(matrix_pred)


screen( ind[2])
plot(matrix_pred, digits=1,text.cell=list(cex=1),
     fmt.cell='%.0f',
     col=gray_palette, main="(b) CLIBA (S5) model; Skilfull suface (%)",
     xlab="Datasets",
     ylab="Seasons",
     breaks=seq(from = 20, to = 80, by = 6)
     # ,key=NULL
     )

close.screen( all=TRUE)
dev.off()

