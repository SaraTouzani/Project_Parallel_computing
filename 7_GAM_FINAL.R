## File : 7_GAM.R

setwd("~/home/toutou/Téléchargements/DATAmINING/project_parallel_comp/enercast")
library(mgcv)
library(enercast)
load("/home/toutou/Téléchargements/DATAmINING/project_parallel_comp/enercast/loadUY.rda")


mylag <- function(x, alpha) c(x[1], x[-1] * alpha + (1 - alpha) * x[-length(x)])

DAT$temp_s95 <- mylag(DAT$temp, .05)
DAT$load.24  <- lag(DAT$load, 24)
DAT$Posan    <- as.numeric(unlist(lapply(table(DAT$year), 
                                         function(x) 1:x[[1]] / x[[1]] )))
DAT$trend    <- 1:nrow(DAT)
DAT$hour     <- as.integer(DAT$hour)

TRAIN.SET <- DAT[1:61368,] ; TEST.SET <- DAT[61369:70128,] ; rm(DAT)

fit <- bam(load ~ wday + spec.day +
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
           data = TRAIN.SET, discrete = TRUE, nthreads = )

#fit <- gam(load ~ wday + spec.day +
#             s(load.24,  bs = 'cr', k = 10) +
#             #s(temp,     bs = 'cr', k = 20) +
#             s(temp_s95, bs = "cr", k = 20) +  
#             s(hour,     bs = 'cr', k = 20) + 
#             s(trend,    bs = 'cr', k = 6)  +  
#             s(Posan,    bs = 'ad', k = 30) +
#             ti(load.24, hour, k = c(15, 15), bs = c("cr", "cr")) +
#             ti(temp, hour, k = c(10, 10), bs = c("cr", "cr")) +
#             ti(temp_s95, hour, k = c(15, 15), bs = c("cr", "cr")) +
#             ti(Posan, hour, k = c(10, 15), bs = c("cr", "cr")),
#           data = TRAIN.SET)

pred <- predict(fit, TEST.SET)

head(TEST.SET$load)




n <- nrow(TEST.SET) / 24
predicciones.gam <- matrix(NA, ncol = 365, nrow = 24)
dat.L <- TRAIN.SET ; dat.T <- TEST.SET
test<-TEST.SET[, setdiff(names(TEST.SET), "load")]

for (d in 1:n) {
  MOD.GAM  <- fit <- bam(load ~ wday + spec.day +
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
                         data = dat.L, discrete = TRUE, nthreads = 4)
  PRED.GAM <- predict(MOD.GAM,  newdata = test[((i-1)*24 +1 ):(24*i),])
  dat.L <- rbind(dat.L, dat.T[1:24,])
  dat.T <- dat.T[-seq(24), ]
  predicciones.gam[, d] <- PRED.GAM
  print(paste('termino DIA',d))
}



###################################### tests

comparee<-microbenchmark( b_c_1_1_1thread= bam(load ~ wday + spec.day +
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
                                                 data = TRAIN.SET, discrete = TRUE, nthreads = c(1,1,1)),
                                                 
                        b_c_2_1thread=bam(load ~ wday + spec.day +
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
                                            data = TRAIN.SET, discrete = TRUE, nthreads = c(2,1)),
                                            
                        b_c_1_1thread_0clus=bam(load ~ wday + spec.day +
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
                                                  data = TRAIN.SET, discrete = TRUE, nthreads = c(1,1)),
                                                  
                        b_c_4thread=bam(load ~ wday + spec.day +
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
                                          data = TRAIN.SET, discrete = TRUE, nthreads = 4),
                        
                        b_c_4_1thread=bam(load ~ wday + spec.day +
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
                                            data = TRAIN.SET, discrete = TRUE, nthreads = 2), 
                        
                        b_c_1thread_0clust=bam(load ~ wday + spec.day +
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
                                                 data = TRAIN.SET, discrete = TRUE, nthreads = 1),
                        
                        b_c_1thread_clus=bam(load ~ wday + spec.day +
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
                                               data = TRAIN.SET, discrete = TRUE, cluster= cls ,nthreads = c(1,1,1))
                                      
                                
                                               
                                               ,times = 3)



################### fonction predict

for (d in 1:n) {
  MOD.GAM  <- fit <- bam(load ~ wday + spec.day +
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
                         data = dat.L, discrete = TRUE, nthreads = 4)
  PRED.GAM <- predict(MOD.GAM,  newdata = test[((i-1)*24 +1 ):(24*i),])
  #PRED.GAM <- predict(i, newdata =test[((i-1)*24 +1 ):(24*i),] ,discrete = TRUE,n.threads = 4)
  dat.L <- rbind(dat.L, dat.T[1:24,])
  dat.T <- dat.T[-seq(24), ]
  predicciones.gam[, d] <- PRED.GAM
  print(paste('termino DIA',d))
}
