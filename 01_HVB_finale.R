
#> 70127 - 61368
#[1] 8759
#> round(8759/24)
#[1] 365
packageDescription('enercast')$Built 
library(devtools) ## https://github.com/cugliari/enercast
install_github('cugliari/enercast',dependencies=FALSE)
library(enercast)
library(help=enercast)
library(compiler) 
library(doParallel)
library(compiler)
library(speedglm)
library(RcppGSL)

load("~/Téléchargements/DATAmINING/project_parallel_comp/enercast/loadUY.rda")

data<-DAT;rm(data)
DAT$hour <- as.character(DAT$hour)
DAT$month <- as.character(DAT$month)

TRAIN.SET <- DAT[1:61368,] ; TEST.SET  <- DAT[61369:70128,] ; rm(DAT)
n <- nrow(TEST.SET)/24
predicciones.hvb <- vector('list',n)
trend2test <- seq(61369,70128)
test<-cbind(TEST.SET,trend=trend2test[1:24*365])

######################################## TRaitement HONG

hong <- function(data){
  
  trend <- 1:nrow(data)

  hv <- l_m (load ~ trend + wday * hour + month + month * temp + 
                    month * (temp^2) + month * (temp^3) +  hour * temp +  
                    hour  * (temp^2) +  hour * (temp^3),
                  data = data)
  
  class(hv) <- c('lm', 'hong')
  return(hv)
}

l_m <-  function (formula, data,  ...) {
  #ret.x <- x
  #ret.y <- y
  # cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  # w <- as.vector(model.weights(mf)) NULL
  #offset <- model.offset(mf) NULL
  # mlm <- is.matrix(y) FALSE
  ny <- length(y)
  
  ##
  x <- model.matrix(mt, mf, NULL)
  z <-  lm.fit(x, y)
  ##    else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
  ##               ...)
  #######  
  class(z) <-  "lm"
  z$na.action <- attr(mf, "na.action")
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- match.call()
  z$terms <- mt
  z$model <- mf
  
  z
}
cmp_lm<-cmpfun(l_m)
cmp_hong<-cmpfun(hong)
cmp_predict_hong<- enercast:::predict.hong


Trait_hong_para <- function (cores = detectCores()-1) 
{ 
  c<-data.frame()
  cls <- makeCluster(cores)
  registerDoParallel(cls)
  clusterExport(cl=cls, varlist=c("TRAIN.SET", "TEST.SET","test","predicciones.hvb", "cmp_hong"))    
  on.exit(stopCluster(cls))
  RES <- foreach(i=1:365) %dopar% 
    {
      if(i>1){

        MOD.HVB  <- cmp_hong( rbind(TRAIN.SET,TEST.SET[( (i-2) *24 +1 ):(24*(i-1)),])) # BIND AVEC LIGNE DE TEST QUI PRECEDE
        PRED.HVB <- enercast:::predict.hong(MOD.HVB,test[((i-1)*24 +1 ):(24*i),])
      }
      
        MOD.HVB  <- cmp_hong(TRAIN.SET) 
        PRED.HVB <- enercast:::predict.hong(MOD.HVB,test[((i-1)*24 +1 ):(24*i),])
        
      }
  return(RES)
}

Trait_hong <- function (cores = detectCores()-1) 
{ 
  c<-data.frame()
  cls <- makeCluster(cores)
  registerDoParallel(cls)
  clusterExport(cl=cls, varlist=c("dat.L", "n" ,"test","predicciones.hvb"))    
  on.exit(stopCluster(cls))
  cmp_hong<-enercast::hong
  cmp_predict_hong<- enercast:::predict.hong
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
cmp_trait_hong_para<-cmpfun(Trait_hong_para)
system.time(cmp_trait_hong(3))
cmp_mape<-cmpfun(mape)

predicciones.hvb<-cmp_trait_hong(1)

############################################" MAPE 



dia <- seq(1,8760,24)
MAPE <- NULL

cls <- makeCluster(4)
registerDoParallel(cls)
clusterExport(cl=cls, varlist=c("TEST.SET", "dia","predicciones.hvb"))    
cmp_hong<-enercast::hong
cmp_predict_hong<- enercast:::predict.hong
RES <- foreach(i=1:365) %dopar% 
  {
    MAPE <- c(MAPE,cmp_mape(TEST.SET$load[dia[i]:(dia[i]+23)],predicciones.hvb[[i]]))
  }
stopCluster(cls)
save.image('./01_HVB.RData')
