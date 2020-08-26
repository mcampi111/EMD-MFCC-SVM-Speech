kkfunction_MFCC.k <- function(sigma_lp1, sigma_rbf2, sigma_rbf3, sigma_rbf4, w1,w2,w3,w4,w5){
  k <- function (x,y){  #y = NULL
    
    x1<- x[grep("x_syntspeak1_MLFC_IMF1",names(x))]
    y1<- y[grep("x_syntspeak1_MLFC_IMF1",names(y))]
    k1<- as.numeric(w1)*exp(-sigma_lp1*sqrt(-(round(2*crossprod(x1,y1) - crossprod(x1) - crossprod(y1),9))))  
    
    x2<- x[grep("x_syntspeak1_MLFC_IMF2",names(x))]
    y2<- y[grep("x_syntspeak1_MLFC_IMF2",names(y))] 
    k2<- as.numeric(w2)*exp( sigma_rbf2 * (2 * crossprod(x2, y2) - crossprod(x2) - crossprod(y2)) )
    
    x3<- x[grep("x_syntspeak1_MLFC_IMF3",names(x))]
    y3<- y[grep("x_syntspeak1_MLFC_IMF3",names(y))] 
    k3<- as.numeric(w3)*exp( sigma_rbf3 * (2 * crossprod(x3, y3) - crossprod(x3) - crossprod(y3)) )
    
    x4<- x[grep("x_syntspeak1_MLFC_IMF4",names(x))]
    y4<- y[grep("x_syntspeak1_MLFC_IMF4",names(y))] 
    k4<- as.numeric(w4)*exp( sigma_rbf4 * (2 * crossprod(x4, y4) - crossprod(x4) - crossprod(y4)) ) 
    
    x5<- x[grep("x_syntspeak1_MLFC_IMF5",names(x))]
    y5<- y[grep("x_syntspeak1_MLFC_IMF5",names(y))] 
    k5<- as.numeric(w5)*crossprod(x5,y5)  
    
    
    sum(k1, k2, k3, k4, k5)
    
  }
  
  class(k) <- "kernel"   #instead of kernel
  k}






#crossprod(x,y)                                                                               linear

#tanh(scale*crossprod(x*y)+offset)                                                            sigmoid

#(scale*crossprod(x,y)+offset)^(degree) )                                                     polynomial

#exp(sigma_rbf_lp * (2 * crossprod(x, y) - crossprod(x) - crossprod(y))                              rbf

#exp(-sigma*sqrt(-(round(2*crossprod(x,y) - crossprod(x) - crossprod(y),9))))                 laplace

#(((- besselJ(sigma*sqrt(-(2*crossprod(x,y) - crossprod(x) - crossprod(y)))  , order  ))* (((sigma*sqrt(-(2*crossprod(x,y) - crossprod(x) - crossprod(y)))))^(-order) ) )/(1/(gamma(order+1)*2^(order))) )^(degree)


# lim <- 1/(gamma(order+1)*2^(order))
# bkt <- sigma*sqrt(-(2*crossprod(x,y) - crossprod(x) - crossprod(y)))
# if(bkt < 10e-5)
#   res <- lim
# else
#   res <- besselJ(bkt,order)*(bkt^(-order))
# return((res/lim)^degree)
# 

#follow: https://www.r-bloggers.com/learning-kernels-svm/



# kkfunction.k <- function(scale, offset, degree,sigma_rbf_lp, order){
#   k <- function (x,y = NULL){
#     
#     x1<- x[grep("gg",names(x))]
#     y1<- y[grep("gg",names(y))]
#     k1<- exp(sigma_rbf_lp * (2 * crossprod(x1, y1) - crossprod(x1) - crossprod(y1)))
#     
#     x2<- x[grep("nn",names(x))]
#     y2<- y[grep("nn",names(y))] 
#     k2<- ((scale*crossprod(x2,y2)+offset)^(degree) )
#     
#     
#     sum(k1,k2)
#     
#   }
#   
#   class(k) <- "kernel"   #instead of kernel
#   k}
