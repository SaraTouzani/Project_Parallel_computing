## File : 7_GAM.R

#setwd("~/ownCloud/ANII_demanda/varios/EXPERIMENTOS2/datos/")
library(mgcv)
#library(enercast)
load("loadUY.rda")

mylag <- function(x, alpha) c(x[1], x[-1] * alpha + (1 - alpha) * x[-length(x)])

DAT$temp_s95 <- mylag(DAT$temp, .05)
DAT$load.24  <- lag(DAT$load, 24)
DAT$Posan    <- as.numeric(unlist(lapply(table(DAT$year), 
                                         function(x) 1:x[[1]] / x[[1]] )))
DAT$trend    <- 1:nrow(DAT)
DAT$hour     <- as.integer(DAT$hour)

TRAIN.SET <- DAT[1:61368,] ; TEST.SET <- DAT[61369:70128,] ; rm(DAT)

system.time(fit <- bam(load ~ wday + spec.day +
                         s(load.24,  bs = 'cr', k = 10) +
                         s(temp,     bs = 'cr', k = 20) +
                         #s(temp_s95, bs = "cr", k = 20) +  
                         s(hour,     bs = 'cr', k = 20) + 
                         s(trend,    bs = 'cr', k = 6)  +  
                         s(Posan,    bs = 'ad', k = 30) +
                         ti(load.24, hour, k = c(15, 15), bs = c("cr", "cr")) +
                         ti(temp, hour, k = c(10, 10), bs = c("cr", "cr")) +
                         #ti(temp_s95, hour, k = c(15, 15), bs = c("cr", "cr")) +
                         ti(Posan, hour, k = c(10, 15), bs = c("cr", "cr")),
                       data = TRAIN.SET, discrete = TRUE,nthreads = 4))

#fit <- gam(load ~ wday + spec.day +
#             s(load.24,  bs = 'cr', k = 10) +
#            #s(temp,     bs = 'cr', k = 20) +
#             s(temp_s95, bs = "cr", k = 20) +  
#             s(hour,     bs = 'cr', k = 20) + 
#            s(trend,    bs = 'cr', k = 6)  +  
#            s(Posan,    bs = 'ad', k = 30) +
#             ti(load.24, hour, k = c(15, 15), bs = c("cr", "cr")) +
#             ti(temp, hour, k = c(10, 10), bs = c("cr", "cr")) +
#            ti(temp_s95, hour, k = c(15, 15), bs = c("cr", "cr")) +
#            ti(Posan, hour, k = c(10, 15), bs = c("cr", "cr")),
#          data = TRAIN.SET)
registerDoParallel(4)
system.time(times(n) %do% (pred1 <- predict(fit, TEST.SET,discrete = TRUE,n.threads = 4))) 
system.time(times(n) %dopar% (pred2 <- predict(fit, TEST.SET,discrete = TRUE,n.threads = 4)))
stopImplicitCluster()
identical(pred2,pred1)
#user  system elapsed 
#103.56    0.23  107.75 
#user  system elapsed 
#3.05    0.55   29.14 
head(TEST.SET$load)




n <- nrow(TEST.SET) / 24
predicciones.gam <- matrix(NA, ncol = 365, nrow = 24)
prediccc = list()
dat.L <- TRAIN.SET ; dat.T <- TEST.SET

system.time( for (d in 1:n) {
  MOD.GAM <- fit <- bam(load ~ wday + spec.day +
                          s(load.24,  bs = 'cr', k = 10) +
                          s(temp,     bs = 'cr', k = 20) +
                          #s(temp_s95, bs = "cr", k = 20) +  
                          s(hour,     bs = 'cr', k = 20) + 
                          s(trend,    bs = 'cr', k = 6)  +  
                          s(Posan,    bs = 'ad', k = 30) +
                          ti(load.24, hour, k = c(15, 15), bs = c("cr", "cr")) +
                          ti(temp, hour, k = c(10, 10), bs = c("cr", "cr")) +
                          #ti(temp_s95, hour, k = c(15, 15), bs = c("cr", "cr")) +
                          ti(Posan, hour, k = c(10, 15), bs = c("cr", "cr")),
                        data = TRAIN.SET, discrete = TRUE, nthreads = 4)
  dat.L <- rbind(dat.L, dat.T[1:24,])
  dat.T <- dat.T[-seq(24), ]
  prediccc[[d]] <- MOD.GAM
  print(paste('termino DIA',d))
}
)
cls = makeCluster(4)
clusterExport(cls,list("dat.T","TEST.SET"))
system.time( stock <- parLapply(cls,prediccc,function(i){predict(i, 
                                                                 newdata = head(dat.T[, setdiff(names(TEST.SET), "load")], 24)
                                                                 ,discrete = TRUE,n.threads = 4)})
)
registerDoParallel(4)
to <- function(i){
  PRED.GAM <- predict(i, 
                      newdata = head(dat.T[, setdiff(names(TEST.SET), "load")], 24)
                      ,discrete = TRUE,n.threads = 4)
  dat.T <- dat.T[-seq(24), ]
}

#tail(pardat.L[nrow(pardat.L):(nrow(pardat.L)-24),])
#   user  system elapsed 
#3007.95   74.09 1962.09 
#user  system elapsed 
#2894.10   60.58 1956.06 


