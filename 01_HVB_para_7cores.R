
#> 70127 - 61368
#[1] 8759
#> round(8759/24)
#[1] 365
load('./loadUY.rda')
require(compiler)

TRAIN.SET <- DAT[1:61368,] ; TEST.SET  <- DAT[61369:70128,] ; rm(DAT)
dat.L <- TRAIN.SET ; dat.T <- TEST.SET
n <- nrow(TEST.SET)/24
predicciones.hvb <- vector('list',n)
trend2test <- seq(61369,70128)
test<-cbind(dat.T,trend=trend2test[1:24*365])

########################################
Trait_hong <- function (cores = detectCores()-1) 
{ 
  c<-data.frame()
  cls <- makeCluster(cores)
  registerDoParallel(cls)
  clusterExport(cl=cls, varlist=c("dat.L", "test","predicciones.hvb"))    
  on.exit(stopCluster(cls))
  cmp_hong<-hong
  cmp_predict_hong<- predict.hong
  RES <- foreach(i=1:365) %dopar% 
    {
      dia <- test[((i-1)*24 +1 ):(24*i),]
      dat.L <- rbind(dat.L, dia[,1:8])
      MOD.HVB  <- cmp_hong(dat.L) 
      PRED.HVB <- cmp_predict_hong(MOD.HVB,dia)
    }
  return(RES)
}

 cmp_trait_hong <- cmpfun(Trait_hong)
 system.time(predicciones.hvb<-cmp_trait_hong(7))
# utilisateur     système      écoulé for n = 100 with 7
# 0.25        0.16      186.29 
 #utilisateur     système      écoulé for n = 365
 #0.89        0.52      685.74 

############################################"
dia <- seq(1,8760,24)
MAPE <- NULL
for (i in 1:2){
  MAPE <- c(MAPE,mape(TEST.SET$load[dia[i]:(dia[i]+23)],predicciones.hvb[[i]]))
}
#####
system.time(for(d in 1:365){
  TTTd <- proc.time()[3]
  MOD.HVB  <- hong(dat.L)
  dia <- dat.T[1:24,]
  PRED.HVB <- predict.hong(MOD.HVB,newdata=cbind(dia,trend=trend2test[1:24]))
  dat.L <- rbind(dat.L, dia)
  dat.T <- dat.T[-seq(24), ]
  trend2test <- trend2test[-seq(24)]
})
#utilisateur     système      écoulé for n = 100 with 7
#214.49        3.00      217.48
#utilisateur     système      écoulé for n = 365
#859.25       12.49      871.89 


save.image('./01_HVB.RData')
