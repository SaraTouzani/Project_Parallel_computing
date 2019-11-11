## File:  04_SSM.R
## Description:  Experimentos2 - Modelo de Espacio de Estados
## Author: ac @ Sep 2016
library(RcppGSL)
rm(list=ls())

##
## Instala y carga enercast library
##
#install.packages("pathmapping")
library(pathmapping)
#install.packages(pkgs='enercast/enercast_0.1.tar.gz',repos=NULL)
packageDescription('enercast')$Built 
library(enercast)

##
## Cracion de Variables + Training and Test Data Set
##
setwd("C:/Users/User")
load('./loadUY.rda')

TRAIN.SET <- DAT[1:61368,] ; TEST.SET  <- DAT[61369:70128,]
# spline de temperatura
temp.gam <- data.frame(cte=1,enercast:::rcs(DAT$temp,nod=seq(9,29,4)))
  # spline de temp maxima y minima las ultimas 24 horas
system.time( for(i in 1:nrow(DAT)){
  DAT$tempM24[i] <- max(DAT$temp[ifelse(i-23<0,1,i-23):i])
  DAT$tempm24[i] <- min(DAT$temp[ifelse(i-23<0,1,i-23):i])}
)
#system.time pour le temps d'execution version R  
#user  system elapsed 
#15.73   39.78   56.02 

library(Rcpp)
system.time(
sourceCpp("min_max.cpp",rebuild = TRUE)#rebuild = TRUE pour recompiler la fonction
)
#system.time pour le temps d'execution version Rcpp 
#user  system elapsed 
#0.03    0.08    6.92 
sourceCpp("min_max.cpp")
temp_rcpp <- function(){sourceCpp("min_max.cpp",rebuild = TRUE)
  }
temp_for <- function(){
for(i in 1:nrow(DAT)){
  DAT$tempM24[i] <- max(DAT$temp[ifelse(i-23<0,1,i-23):i])
  DAT$tempm24[i] <- min(DAT$temp[ifelse(i-23<0,1,i-23):i])
}
}
identical(to_test[["tempM24"]],DAT$tempM24)#TRUE
identical(to_test[["tempm24"]],DAT$tempm24)#TRUE
library(microbenchmark)
library(ggplot2)
compare3 <- microbenchmark(temp_for(),temp_rcpp(),times = 10L)
autoplot(compare3)




#user  system elapsed 
#0.00    0.08    4.14 


#####


tM24 <- enercast:::rcs(DAT$tempM24,m=3); colnames(tM24) <- c('tM24_C0','tM24_C1')
tm24 <- enercast:::rcs(DAT$tempm24,m=3); colnames(tm24) <- c('tm24_C0','tm24_C1')

# trigonometricas - frecuencia semanal
sem <- enercast:::trigon(DAT,frecH=7); colnames(sem)<-paste('W',colnames(sem),sep='_')
# trigonometricas - frecuencia anual
anual <- enercast:::trigon(DAT,frecH=365)
tod <- ifelse(DAT$wday%in%c('Sat','Sun'),1,0) # partir en weekdays & weekends
anual.we <- anual*tod ;  anual.wd <- anual*(1-tod)
colnames(anual.we)<-paste('A.we',colnames(anual.we),sep='_')
colnames(anual.wd)<-paste('A.wd',colnames(anual.wd),sep='_')

supr <- which(names(DAT)%in%c('year','month','day','wday','temp','tempM24','tempm24')) 
DAT <- cbind(DAT[,-supr],temp.gam,tM24,tm24,sem,anual.we,anual.wd)


TRAIN.SET <- DAT[1:61368,] ; TEST.SET  <- DAT[61369:70128,] ; rm(DAT)

##
## Modelo y Prediccion (No tiene sentido reestimarlo TODOS los dias!!!)
##

week <- c(seq(1,nrow(TEST.SET),by=168)[-53],8761)
predicciones.ssm <- NULL #vector('list',n)
dat.L <- TRAIN.SET ; dat.T <- TEST.SET

TTT <- proc.time()[3]
for(w in 2:length(week)){
  TTTw <- proc.time()[3]
  MOD.SSM  <- enercast::ssm(formulaXa=load~.-1-spec.day,data=dat.L)
  sem <- (week[w]-week[w-1])
  dias7 <- dat.T[1:sem,]
  PRED.SSM <- enercast:::predict.ssm(MOD.SSM, dias7)
  dat.L <- rbind(dat.L, dias7)
  dat.T <- dat.T[-seq(sem), ]
  predicciones.ssm <- c(predicciones.ssm,PRED.SSM$pred)
 cat('Semana',w,'Demoro',proc.time()[3]-TTTw,'segundos','\n')
}
  cat('Demoro',proc.time()[3]-TTT,'segundos','\n')

dia <- seq(1,8760,24)
MAPE <- NULL
for (i in 1:365){
  MAPE <- c(MAPE,mape(TEST.SET$load[dia[i]:(dia[i]+23)],predicciones.ssm[dia[i]:(dia[i]+23)]))}

summary(round(MAPE*100,4))

#save.image('resultados/04_SSM.RData')

q('no')

##  ---- End Of File ---
##




