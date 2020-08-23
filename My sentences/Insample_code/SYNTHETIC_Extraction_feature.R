library('fBasics')
library('entropy')
library('seewave')
library('zoo')
library('hht')
library('splines')
#EXTRACTION FEATURES OF SYNTHETIC VOICES
#We consider the first 3 IMFs and residual extracts mean, variance, sd, skewness, kurtosis, min, max

#Statistics on a window (No overlap - 10 windows length = 6000 --- GARETH)

#MEAN------------------------------------------------------------------------------------------------------------
feat_synt_mean<-list()

for(i in (1:m)){
  
  feat_synt_mean[[i]]<- rollapply(feat_IMF_data_synt[[i]],width = 6000, by =6000, FUN =mean, align = 'left')
  
}


#VARIANCE---------------------------------------------------------------------------------------------------------

feat_synt_var<-list()

for(i in (1:m)){
  
  feat_synt_var[[i]]<- rollapply(feat_IMF_data_synt[[i]],width = 6000, by =6000, FUN =var, align = 'left')
  
}



#SKEWNESS ---------------------------------------------------------------------------------------------------------

feat_synt_skew<-list()

for(i in (1:m)){
  
  feat_synt_skew[[i]]<- rollapply(feat_IMF_data_synt[[i]],width = 6000, by =6000, FUN =skewness, align = 'left')
  
}



#Kurtosis ---------------------------------------------------------------------------------------------------------

feat_synt_kurt<-list()

for(i in (1:m)){
  
  feat_synt_kurt[[i]]<- rollapply(feat_IMF_data_synt[[i]],width = 6000, by =6000, FUN =kurtosis, align = 'left')
  
}


#MIN ---------------------------------------------------------------------------------------------------------

feat_synt_min<-list()

for(i in (1:m)){
  
  feat_synt_min[[i]]<- rollapply(feat_IMF_data_synt[[i]],width = 6000, by =6000, FUN =min, align = 'left')
  
}



#MAX ---------------------------------------------------------------------------------------------------------

feat_synt_max<-list()

for(i in (1:m)){
  
  feat_synt_max[[i]]<- rollapply(feat_IMF_data_synt[[i]],width = 6000, by =6000, FUN =max, align = 'left')
  
}


#ROOT MEAN SQUARE ------------------------------------------------------------------------------------------------

feat_synt_rms<-list()

for(i in (1:m)){
  
  feat_synt_rms[[i]]<- rollapply(feat_IMF_data_synt[[i]],width = 6000, by =6000, FUN =rms, align = 'left')
  
}



#INSTANT FREQUENCY -------------------------------------------------------------------------------------------------

feat_synt_hht<- list()

for(i in (1:m)){
  
  feat_synt_hht[[i]]<- apply(feat_IMF_data_synt[[i]],2,HilbertTransform)
}

xx<- (0:60000)/44.1
xx<- xx[-length(xx)]

feat_synt_IF<- list()

for(i in (1:m)){
  
  feat_synt_IF[[i]]<- apply(feat_synt_hht[[i]],2,InstantaneousFrequency, tt= xx, lag = 1) #seq(1:(T1))
}




#SPLINE COEFFICIENTS -----------------------------------------------------------------------------------------------
t<- seq(0, 2.6, length = (T1))

feat_synt_splines1 <- vector(mode="list", m)
for (i in 1:m) {
  for (j in 1:5) {
    
    feat_synt_splines1[[i]]<- c( feat_synt_splines1[[i]], splinefun(t, feat_IMF_data_synt[[i]][,j]))
  }
}


feat_synt_coeff21<- c()

for (j in (1:m)) {
  for (i in (1:5)) {
    ls(envir = environment(feat_synt_splines1[[j]][[i]]))
    feat_synt_coeff21<- rbind(feat_synt_coeff21, 
                               as.data.frame(get("z", envir = environment(feat_synt_splines1[[j]][[i]]))))
    
  }
  
}





feat_synt_b<- matrix(data = feat_synt_coeff21$b, nrow = 100 )

feat_synt_c<- matrix(data = feat_synt_coeff21$c, nrow = 100 )

feat_synt_d<- matrix(data = feat_synt_coeff21$d, nrow = 100 )


feat_synt_coeff<- cbind(feat_synt_b, feat_synt_c, feat_synt_d)

rm(feat_synt_b, feat_synt_c, feat_synt_d, feat_synt_coeff21)

#UNIQUE VECTOR for statistics
#s
synt_vec1<- list()
synt_vec2<- list()

synt_vec<- lapply(1:m, function(k){
  
  synt_vec2[[k]]<- lapply(1:dim(feat_synt_mean[[k]])[2], function(i){
    
    synt_vec1[[i]]<- sapply(1:dim(feat_synt_mean[[k]])[1], function(j){
      
      cbind(feat_synt_mean[[k]][j,i], feat_synt_var[[k]][j,i], 
            feat_synt_skew[[k]][j,i], 
            feat_synt_kurt[[k]][j,i], feat_synt_min[[k]][j,i], feat_synt_max[[k]][j,i],  
            feat_synt_rms[[k]][j,i]) 
    })
    
  })
  
})

rm(feat_synt_mean, feat_synt_var, 
   feat_synt_skew, 
   feat_synt_kurt, feat_synt_min, feat_synt_max,  
    feat_synt_rms, synt_vec1, synt_vec2 )
