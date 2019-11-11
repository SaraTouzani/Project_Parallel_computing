## File:  06_KWF.R
## Description:  Experimentos2 - Modelo Kernel-Wavelet Functional
## Author: jc @ June 2017

#rm(list = ls())

##
## Instala y carga enercast library
##

#install.packages(pkgs='enercast/enercast_0.1.tar.gz',repos=NULL)
#packageDescription('enercast')$Built 

#library(devtools) ## https://github.com/cugliari/enercast
#install_github('cugliari/enercast', dependencies = FALSE)
library(enercast)
#library(help=enercast)

##
## Training and Test Data Set
##
load('./loadUY.rda')

days <- gsub("Tue|Wed|Thu", x = DAT$wday[seq(1, nrow(DAT), 24)], "TWT")
days <- ifelse(DAT$spec.day[seq(1, nrow(DAT), 24)] == "feriado", "HOL", days)
gr   <- paste(days, c(days[-1], "HOL"), sep = "_")
rm(days)

#TRAIN.SET <- DAT[1:61368,] ; TEST.SET  <- DAT[61369:70128,] ; rm(DAT)

#####
grids <- (61368 / 24) + 0:364 # these are the reference days
#h_Load <- h_k1 <- h_k2 <- h_k3 <- double(length(grids))
pred_Load <- matrix(NA_real_, ncol = 24, nrow = length(grids))
##
## Modelo y Prediccion
##
#pred_Load with for loop####
wkdata_loop <- function(wk_Load,grids){
for (q in seq_along(grids)) { 
  if ((q %/% 50) == (q / 50)) cat(paste("  iteration: ", q, "\n"))
  Q  <- grids[q]
  
  select_Load <- enercast:::select.wkdata(wk_Load, 1:Q)
  aux_Load  <- enercast::wavkerfun(obj = select_Load, EPS = .Machine$double.eps) 
  pred_Load[q,] <- enercast:::predict.wavkerfun(obj = aux_Load, cent = "DIFF")$X
}
}
#user  system elapsed 
#63.98    0.27   64.53 
#parallelized for pred_Load#####
wkdata_parallel_vers1 <- function(wk_Load,grids){
registerDoParallel(cores = 4)#has to be at 5 
pred_version1 <-foreach (i = 1:365, .combine = cbind) %dopar%{
    Q  <- grids[i]
    select_Load = enercast:::select.wkdata(wk_Load, 1:Q)
    aux_Load  = enercast::wavkerfun(obj = select_Load, EPS = .Machine$double.eps)
    pred_Load = enercast:::predict.wavkerfun(obj = aux_Load, cent = "DIFF")$X}
#dim(pred) = c(24,365)
#pred = t(pred)
}
#calcule du temps d'execution pour la boucle foreach parallel avec cbind en parametre
system.time(wkdata_parallel_vers1(wk_Load,grids))
#user  system elapsed 
#0.24    0.13   43.23 
wkdata_parallel_vers2 <- function(wk_Load,grids){
  registerDoParallel(cores = 4)
pred_version2 <- foreach (i = 1:365, .combine = c) %dopar%{
  Q  <- grids[i]
  select_Load = enercast:::select.wkdata(wk_Load, 1:Q)
  aux_Load  = enercast::wavkerfun(obj = select_Load, EPS = .Machine$double.eps)
  pred_Load = enercast:::predict.wavkerfun(obj = aux_Load, cent = "DIFF")$X}
dim(pred_version2) = c(24,365)
pred_version2 = t(pred_version2)
}
#calcule du temps d'execution pour la boucle foreach parallel avec transposer et c 
system.time(wkdata_parallel_vers2(wk_Load,grids))
#user  system elapsed 
#0.25    0.10   36.41 
#partie de comparaison avec microbenchmark et graphique de comparaison du temps d'execution####
library(doParallel)
library(microbenchmark)
library(ggplot2) 
wk_Load <- enercast::wkdata(X  = c(t(mat_Load)), gr = gr, p  = 24)
grids <- (61368 / 24) + 0:364
compare1 <- microbenchmark(wkdata_loop(wk_Load,grids),wkdata_parallel_vers2(wk_Load,grids)
                           ,wkdata_parallel_vers1(wk_Load,grids),times = 10L)
ggplot2::autoplot(compare1)
str(pred_Load)
str(pred)
head(pred)
head(pred_Load)
dim(pred_Load) #365,24
dim(pred) = c(24,365)
pred = t(pred)
identical(pred,pred_Load)#TRUE 
#   user  system elapsed 
#  0.23    0.04   33.95
#predictions/error ####
#pred_Load - mat_Load[grids + 1, ]
predicciones.kwf <- c(t(pred_Load))
mean(rowMeans(abs(pred_Load - mat_Load[grids + 1, ]) / mat_Load[grids + 1, ]))


#save(predicciones.kwf, 'resultados/06_KWF.RData')

##
##  ---- End Of File ---
##
