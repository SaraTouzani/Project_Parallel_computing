## File:  02_TSB.R
## Description:  Experimentos2 - Modelo Time Series Benchmark
## Author: ac @ Sep 2016

rm(list=ls())

##
## Instala y carga enercast library
##

#install.packages(pkgs='enercast/enercast_0.1.tar.gz',repos=NULL)
packageDescription('enercast')$Built 
#library(devtools) ## https://github.com/cugliari/enercast
devtools::install_github('cugliari/enercast'); library(enercast)
#library(help=enercast)
library(compiler)

##
## Training and Test Data Set
##

load('./datos/loadUY.rda')

 
TRAIN.SET <- DAT[1:61368,] ; TEST.SET  <- DAT[61369:70128,] ; rm(DAT)

##
## Modelo y Prediccion
##

n <- nrow(TEST.SET)/24
 predicciones.tsb <- vector('list',n)
dat.L <- TRAIN.SET ; dat.T <- TEST.SET



################################### fonctions optimisé

tsb<-function (yt, p = 1, d = 1, q = 1, P1 = 1, D1 = 0, Q1 = 1, P2 = 0, 
               D2 = 1, Q2 = 1, s1 = 24, s2 = 168, k = 200) 
{
  ar = rep(0.1, p)
  AR1 = rep(0.1, P1)
  AR2 = NULL
  ma = rep(0.1, q)
  MA1 = rep(0.1, Q1)
  MA2 = rep(0.1, Q2)
  
  param <- c(ar, AR1, AR2, ma, MA1, MA2)
  
  par<-nlm(p = param, f = losss,steptol=1e-2,gradtol=1e-2, yt = yt, s = p, d = d,  q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, s1 = s1, s2 = s2, k = k)
  
  dif <- par$objective
  iter <- par$iterations
  par <- par$par
  
  ######
  dif<-par$minimum
  iter<-par$iterations
  par<-par$estimate
  
#on enlève tt les verification qu'on utilise pas vu que on utilise les param par default
    ar <- par[1:p]  #
    par <- par[-seq(p)] #
  
    AR1 <- par[1:P1] #
    par <- par[-seq(P1)] #
    
    AR2 <- NULL#

    ma <- par[1:q] #
    par <- par[-seq(q)]#

    MA1 <- par[1:Q1]#
    par <- par[-seq(Q1)]#

    MA2 <- par[1:Q2] #
    par <- par[-seq(Q2)]#

   ma <- enercast:::maInvert(ma)
   MA1 <- enercast:::maInvert(MA1)
   MA2 <- enercast:::maInvert(MA2)
  return(list(ar = list(reg = ar, est1 = AR1, est2 = AR2), 
              ma = list(reg = ma, est1 = MA1, est2 = MA2), d = c(d,  D1, D2), frec = c(s1, s2), dif = dif, iter = iter,  data = yt))
}

tsb_cmp<- cmpfun(tsb)

losss_parallel <- function(par, yt, s = 0 , d = 0, q = 0,
                 P1 = 0, D1 = 0, Q1 = 0, P2 = 0, D2 = 0, Q2 = 0,
                 s1 = 24, s2 = 168, k = 200){
  
  ytc <- yt
  
  if (s > 0) {
    ar <- par[1:s]
    par <- par[-seq(s)]
  } else ar <- NULL
  if (P1 > 0) {
    AR1 <- par[1:P1]
    par <- par[-seq(P1)]
  } else AR1 < -NULL
  if (P2 > 0) {
    AR2 <- par[1:P2]
    par <- par[-seq(P2)]
  } else AR2 <- NULL
  if (q > 0) {
    ma  <- par[1:q]
    par <- par[-seq(q)]
  } else ma <- NULL
  if (Q1 > 0) {
    MA1 <- par[1:Q1]
    par <- par[-seq(Q1)]
  } else MA1 <- NULL
  if (Q2 > 0) {
    MA2 <- par[1:Q2]
    par <- par[-seq(Q2)]
  } else MA2 <- NULL
  if (d > 0)  ytc <- diff(ytc, differences = d)
  if (D1 > 0) ytc <- diff(ytc, differences = D1, lag = s1)
  if (D2 > 0) ytc <- diff(ytc, differences = D2, lag = s2)
  cls <- makeCluster(detectCores()-1)
  registerDoParallel(cls)
  clusterExport(cl=cls, varlist=c("paralel_acf_acteo_cmp"))    
  on.exit(stopCluster(cls))
  
 res<- foreach::foreach(i = 0:1 ) %dopar%{
    paralel_acf_acteo_cmp(i,ytc , k,ar,AR1,AR2,ma,MA1,MA2,s1,s2)
  }

  
  return(sum((res[[1]] - res[[2]])^2))
}


losss <- function(par, yt, s = 0 , d = 0, q = 0,
                           P1 = 0, D1 = 0, Q1 = 0, P2 = 0, D2 = 0, Q2 = 0,
                           s1 = 24, s2 = 168, k = 200){
  
  ytc <- yt
  

    ar <- par[1:s]#
    par <- par[-seq(s)]#

    AR1 <- par[1:P1]#
    par <- par[-seq(P1)]#
     AR2 <- NULL#

    ma  <- par[1:q]#
    par <- par[-seq(q)]#

    MA1 <- par[1:Q1]#
    par <- par[-seq(Q1)]#

    MA2 <- par[1:Q2]#
    par <- par[-seq(Q2)]#

  if (d > 0)  ytc <- diff(ytc, differences = d)
  if (D1 > 0) ytc <- diff(ytc, differences = D1, lag = s1)
  if (D2 > 0) ytc <- diff(ytc, differences = D2, lag = s2)
  acf1 <- acf(ytc, lag.max = k, plot = FALSE)$acf[-1]
  acfteo <- enercast:::acft(ar = ar, AR1 = AR1, AR2 = AR2, ma = ma,  MA1 = MA1, MA2 = MA2, s1 = s1, s2 = s2, k = k)
                    
  
  
  return(sum((acf1-acfteo)^2))
}
losss_cmp<-cmpfun(losss)

paralel_acf_acteo<- function(x,ytc , k,ar,AR1,AR2,ma,MA1,MA2,s1,s2){
  output <- ifelse ((x) > 0,   (acf1 <- acf(ytc, lag.max = k, plot = FALSE)$acf[-1]),   (acfteo <- enercast:::acft(ar = ar, AR1 = AR1, AR2 = AR2, ma = ma,  MA1 = MA1, MA2 = MA2, s1 = s1, s2 = s2, k = k))
  )  
}
paralel_acf_acteo_cmp<-cmpfun(paralel_acf_acteo)
## la fonction loss utilise un argument p , la fonction qui la minimise nml dans tsb otimisé utilise un argument p aussi 


nlmin_b<- function (start, objective, gradient = NULL, hessian = NULL, 
                    ..., scale = 1, control = list(), lower = -Inf, upper = Inf) 
{
  par <- setNames(as.double(start), names(start))
  n <- length(par)
  iv <- integer(78 + 3 * n)
  v <- double(130 + (n * (n + 27))/2)
  .Call(C_port_ivset, 2, iv, v)
  
  if (length(control)) { ## lengh(controle) est à 0 pour première itération
    nms <- names(control)
    if (!is.list(control) || is.null(nms)) 
      stop("'control' argument must be a named list")
    pos <- pmatch(nms, names(port_cpos))
    if (any(nap <- is.na(pos))) {
      warning(sprintf(ngettext(length(nap), "unrecognized control element named %s ignored", 
                               "unrecognized control elements named %s ignored"), 
                      paste(sQuote(nms[nap]), collapse = ", ")), domain = NA)
      pos <- pos[!nap]
      control <- control[!nap]
    }
    ivpars <- pos <= 4
    vpars <- !ivpars
    if (any(ivpars)) 
      iv[port_cpos[pos[ivpars]]] <- as.integer(unlist(control[ivpars]))
    if (any(vpars)) 
      v[port_cpos[pos[vpars]]] <- as.double(unlist(control[vpars]))
  }
  ###
  obj <- quote(objective(.par, ...))
  rho <- new.env(parent = environment())    #Browse[3]> rho
                                            # <environment: 0x565466c1db80>
  assign(".par", par, envir = rho)
  grad <- hess <- low <- upp <- NULL
  
  if (!is.null(gradient)) {  # on entre pas dans le if pour la première itération 
    grad <- quote(gradient(.par, ...))
    if (!is.null(hessian)) {
      if (is.logical(hessian)) 
        stop("logical 'hessian' argument not allowed.  See documentation.")
      hess <- quote(hessian(.par, ...))
    }
  }
  ##########
  if (any(lower != -Inf) || any(upper != Inf)) {
    low <- rep_len(as.double(lower), length(par))
    upp <- rep_len(as.double(upper), length(par))
  }
  else low <- upp <- numeric() ## on a le else pas le if
  ##########
  .Call(C_port_nlminb, obj, grad, hess, rho, low, upp, d = rep_len(as.double(scale), 
                                                                   length(par)), iv, v)
  iv1 <- iv[1L]
  
  list(par = get(".par", envir = rho), objective = v[10L], 
       convergence = (if (iv1 %in% 3L:6L) 0L else 1L), iterations = iv[31L], 
       evaluations = c(`function` = iv[6L], gradient = iv[30L]), 
       message = if (19 <= iv1 && iv1 <= 43) {
         if (any(B <- iv1 == port_cpos)) sprintf("'control' component '%s' = %g, is out of range", 
                                                 names(port_cpos)[B], v[iv1]) else sprintf("V[IV[1]] = V[%d] = %g is out of range (see PORT docu.)", 
                                                                                           iv1, v[iv1])
       } else port_msg(iv1)) # on fait ça
}
TTT <- proc.time()[3]
for(d in 1:n){
  TTTd <- proc.time()[3]
  MOD.TSB  <- tsb(dat.L$load)
  modele<-proc.time()[3]
  ttm<-modele-TTTd
  PRED.TSB <- enercast:::predict.tsb(MOD.TSB, steps = 24)
  pred<-proc.time()[3]
  pred<-pred-modele
  dia <- dat.T[1:24,]
  dat.L <- rbind(dat.L, dia)
  dat.T <- dat.T[-seq(24), ]
  predicciones.tsb[[d]] <- PRED.TSB
  cat('Demoro',proc.time()[3]-TTTd,'segundos','\n')
  print(paste('termino DIA',d))
     }
cat('Demoro',proc.time()[3]-TTT,'segundos','\n')

Rprof("tsb.out")
replicate(5, tsb(dat.L$load))
Rprof(NULL)

Rprof("predict.tsb.out")
replicate(5, enercast:::predict.tsb(MOD.TSB, steps = 24))
Rprof(NULL)

summaryRprof("tsb.out")
summaryRprof("predict.tsb.out")



MAPE <- NULL
dia <- seq(1,8760,24)
cls <- makeCluster(4)
registerDoParallel(cls)
clusterExport(cl=cls, varlist=c( "dia","predicciones.tsb"))    
#cmp_hong<-enercast::hong
#cmp_predict_hong<- enercast:::predict.hong
MAPE <- foreach(i=1:365) %dopar% 
  {
    MAPE <- c(MAPE, enercast::performance(TEST.SET$load[dia[i]:(dia[i]+23)],predicciones.tsb[[i]] , measure = "mape"))
  }
    
stopCluster(cls)

MAPE <- NULL
for (i in 1:n){
}

summary(round(MAPE*100,4))


save.image('resultados/02_TSB.RData')


##
##  ---- End Of File ---
##
###optimisation de tsb

#system.time(replicate(10,nlminb(start = param, objective = loss, yt = yt, p = p, d = d, 
#                                +                     q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, 
#                                +                     s1 = s1, s2 = s2, k = k)))
#utilisateur     système      écoulé 
#110.172     140.084      65.536 

##system.time(replicate(10,nlm(p = param, f = loss, yt = yt, s = p, d = d, 
##+                    q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, 
##+                    s1 = s1, s2 = s2, k = k)))
##utilisateur     système      écoulé 
##127.780     159.212      78.644 

#Tried with the nlm() function already? Don't know if it's much faster, 
#but it does improve speed. Also check the options. optim uses a slow algorithm as the default.
# You can gain a > 5-fold speedup by using the Quasi-Newton algorithm (method="BFGS") instead of the default.
# If you're not concerned too much about the last digits, you can also set the tolerance levels higher of nlm()
# to gain extra speed. 

####### optim Method "SANN"
"optim(par = param , fn = loss ,yt = yt, s = p, d = d,   q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2,  s1 = s1, s2 = s2, k = k )
$par
[1] -0.01566162  0.78857370  0.28415144 -0.43362291 -0.34479398

$value
[1] 0.2022151

$counts
function gradient 
502       NA 

$convergence
[1] 1

$message
NULL

Browse[2]> system.time(optim(par = param , fn = loss ,yt = yt, s = p, d = d,   q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2,  s1 = s1, s2 = s2, k = k ))
utilisateur     système      écoulé 
34.004      43.120      20.197 "



###########################################################" optim method BFGs quasi-Newton method
#Browse[2]> optim(par = param , fn = loss , method = "BFGS" ,yt = yt, s = p, d = d,   q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2,  s1 = s1, s2 = s2, k = k )
"$par
[1] -0.006230769  0.862820860  0.240794057 -0.511999245 -0.520885356

$value
[1] 0.1958862

$counts
function gradient 
40       23 

$convergence
[1] 0

$message
NULL
"

##Browse[2]> system.time(optim(par = param , fn = loss , method = "BFGS" ,yt = yt, s = p, d = d,   q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2,  s1 = s1, s2 = s2, k = k ))
"utilisateur     système      écoulé 
19.464      23.928      11.990 "

##################################################" optim method L-BFGS-B ~ box constraints


#Browse[2]> optim(par = param , fn = loss , method = "L-BFGS-B" ,yt = yt, s = p, d = d,   q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2,  s1 = s1, s2 = s2, k = k )
"$par
[1] -0.006232907  0.862819629  0.240823883 -0.511985865 -0.520885716

$value
[1] 0.1958862

$counts
function gradient 
38       38 

$convergence
[1] 0
"
#$message
#[1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

#Browse[2]> system.time(optim(par = param , fn = loss , method = "L-BFGS-B" ,yt = yt, s = p, d = d,   q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2,  s1 = s1, s2 = s2, k = k ))
"utilisateur     système      écoulé 
#29.128      35.724      17.722 "
################################################### nlminb Unconstrained and box-constrained optimization 
"Browse[2]> nlminb(start = param, objective = loss, yt = yt, s = p, d = d, 
                  +                     q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, 
                  +                     s1 = s1, s2 = s2, k = k)
$par
[1] -0.006228863  0.862931870  0.240797973 -0.512104112 -0.521166316

$objective
[1] 0.1958862

$convergence
[1] 0

$iterations
[1] 21

$evaluations
function gradient 
28      131 
"
#$message
#[1] "relative convergence (4)"
########################################################## nlm
"Browse[2]> nlm(p = param, f = loss, yt = yt, s = p, d = d, 
               +                    q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, 
               +                    s1 = s1, s2 = s2, k = k)
$minimum
[1] 0.1958862

$estimate
[1] -0.006228907  0.862930634  0.240798269 -0.512102813 -0.521163113

$gradient
[1] -3.970589e-07 -1.270829e-07 -5.269576e-08 -8.469697e-08 -1.920492e-08

$code
[1] 1

$iterations
[1] 24

Browse[2]> system.time(nlm(p = param, f = loss, yt = yt, s = p, d = d, 
+                    q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, 
+                    s1 = s1, s2 = s2, k = k))
utilisateur     système      écoulé 
     12.336      15.560       7.449 
"





##### on aura soit nlminm soir nlm 


##nlm speed #### ############################################################
#avec une tolérence de 1e-4

#Browse[2]> system.time(nlm(p = param, f = losss,steptol=1e-4,gradtol=1e-4, yt = yt, s = p, d = d,  q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, s1 = s1, s2 = s2, k = k))
#utilisateur     système      écoulé 
#9.548      12.076       5.875 




#Browse[2]> system.time(replicate(10,nlm(p = param, f = losss_cmp,steptol=1e-4,gradtol=1e-4, yt = yt, s = p, d = d,  q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, s1 = s1, s2 = s2, k = k)))
#utilisateur     système      écoulé 
#98.864     124.768      64.130 

#Browse[2]> system.time(replicate(10,nlm(p = param, f = losss_cmp,steptol=1e-2,gradtol=1e-2, yt = yt, s = p, d = d,  q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, s1 = s1, s2 = s2, k = k)))
#utilisateur     système      écoulé 
#77.420      97.312      46.989 

## nlminb speed #### ########################################################

##Browse[2]> system.time(par<-nlminb(start = param, objective = losss, yt = yt, 
##                                  +                    s = p, d = d, q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, 
##                                +                    D2 = D2, Q2 = Q2, s1 = s1, s2 = s2, k = k))
##utilisateur     système      écoulé 
##12.100      14.588       8.506 

#Browse[2]> system.time(replicate(10,par<-nlminb(start = param, objective = losss, yt = yt,  s = p, d = d, q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, s1 = s1, s2 = s2, k= k)))
#utilisateur     système      écoulé 
#116.916     141.904      77.324 

## nlm results for 1e-2
#Browse[2]> par<-nlm(p = param, f = losss_cmp,steptol=1e-2,gradtol=1e-2, yt = yt, s = p, d = d,  q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, s1 = s1, s2 = s2, k = k)
#Browse[2]> par
#$minimum
#[1] 0.1959897

#$estimate
#[1] -0.006174339  0.851839480  0.238269451 -0.502510068 -0.491281699

#$gradient
#[1] -0.009414955 -0.013788206 -0.008868034 -0.008924223  0.004229187

#$code
#[1] 2

#$iterations
#[1] 17

##nlm results for tolerance de 1e-3 #### ################################

#Browse[2]> par<-nlm(p = param, f = losss_cmp,steptol=1e-3,gradtol=1e-3, yt = yt, s = p, d = d,  q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, s1 = s1, s2 = s2, k = k)
#Browse[2]> par
#$minimum
#[1] 0.1958862

#$estimate
#[1] -0.006231602  0.862911136  0.240823774 -0.512087692 -0.521093774

#$gradient
#[1] -4.599579e-05  2.398892e-05  2.963368e-05 -1.207678e-05  1.771996e-05

#$code
#[1] 1

#$iterations
#[1] 21

## nlminb results #### ##################################################"
#Browse[2]> par<-nlminb(start = param, objective = losss, yt = yt,  s = p, d = d, q = q, P1 = P1, D1 = D1, Q1 = Q1, P2 = P2, D2 = D2, Q2 = Q2, s1 = s1, s2 = s2, k= k)
#Browse[2]> 
 # debug: dif <- par$objective
#Browse[2]> par
#$par
#[1] -0.006228863  0.862931870  0.240797973 -0.512104112 -0.521166316

#$objective
#[1] 0.1958862

#$convergence
#[1] 0

#$iterations
#[1] 21

#$evaluations
#function gradient 
#28      131 

#$message
#[1] "relative convergence (4)"



"> system.time(  MOD.TSB  <- tsb(dat.L$load))
utilisateur     système      écoulé 
      7.832       9.848       4.817 
> system.time(  MOD.TSB  <- tsb(dat.L$load))
utilisateur     système      écoulé 
      7.396       9.212       4.405 
> system.time(  MOD.TSB  <- tsb(dat.L$load))
utilisateur     système      écoulé 
      7.688       9.744       4.631 
> system.time(replicate(10,  MOD.TSB  <- tsb(dat.L$load)))
utilisateur     système      écoulé 
     82.036      99.640      50.287 "
