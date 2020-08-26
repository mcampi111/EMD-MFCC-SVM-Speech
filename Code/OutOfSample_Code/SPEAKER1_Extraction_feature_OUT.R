#EXTRACTION FEATURES OF SPEAKER 1 SIGNALS
#We consider the first 3 IMFs and residual extracts mean, variance, sd, skewness, kurtosis, min, max

library('fBasics')
library('entropy')
library('seewave')
library('zoo')
library('hht')
library('splines')

#Statistics on a window (No overlap - 10 windows length = 6000 --- GARETH)

#MEAN------------------------------------------------------------------------------------------------------------
feat_speak1_mean<-list()

for(i in (1:m)){
  
  feat_speak1_mean[[i]]<- rollapply(feat_IMF_data_speak1_new[[i]],width = 6000, by = 6000, FUN =mean, align = 'left')
  
}


#VARIANCE---------------------------------------------------------------------------------------------------------

feat_speak1_var<-list()

for(i in (1:m)){
  
  feat_speak1_var[[i]]<- rollapply(feat_IMF_data_speak1_new[[i]],width = 6000, by =6000, FUN =var, align = 'left')
  
}



#SKEWNESS ---------------------------------------------------------------------------------------------------------

feat_speak1_skew<-list()

for(i in (1:m)){
  
  feat_speak1_skew[[i]]<- rollapply(feat_IMF_data_speak1_new[[i]],width = 6000, by =6000, FUN =skewness, align = 'left')
  
}



#Kurtosis ---------------------------------------------------------------------------------------------------------

feat_speak1_kurt<-list()

for(i in (1:m)){
  
  feat_speak1_kurt[[i]]<- rollapply(feat_IMF_data_speak1_new[[i]],width = 6000, by =6000, FUN =kurtosis, align = 'left')
  
}


#MIN ---------------------------------------------------------------------------------------------------------

feat_speak1_min<-list()

for(i in (1:m)){
  
  feat_speak1_min[[i]]<- rollapply(feat_IMF_data_speak1_new[[i]],width = 6000, by =6000, FUN =min, align = 'left')
  
}



#MAX ---------------------------------------------------------------------------------------------------------

feat_speak1_max<-list()

for(i in (1:m)){
  
  feat_speak1_max[[i]]<- rollapply(feat_IMF_data_speak1_new[[i]],width = 6000, by =6000, FUN =max, align = 'left')
  
}


#ROOT MEAN SQUARE ------------------------------------------------------------------------------------------------

feat_speak1_rms<-list()

for(i in (1:m)){
  
  feat_speak1_rms[[i]]<- rollapply(feat_IMF_data_speak1_new[[i]],width = 6000, by =6000, FUN =rms, align = 'left')
  
}




#INSTANT FREQUENCY -------------------------------------------------------------------------------------------------

feat_speak1_hht<- list()

for(i in (1:m)){
  
  feat_speak1_hht[[i]]<- apply(feat_IMF_data_speak1_new[[i]],2,HilbertTransform)
}

xx<- (0:60000)/44.1
xx<- xx[-length(xx)]


feat_speak1_IF_new<- list()

for(i in (1:m)){
  
  feat_speak1_IF_new[[i]]<- apply(feat_speak1_hht[[i]],2,InstantaneousFrequency, tt= xx, lag = 1)
}




#SPLINE COEFFICIENTS -----------------------------------------------------------------------------------------------

t<- seq(0, 2.6, length = (T1))

feat_speak1_splines <- vector(mode="list", m)
for (i in 1:m) {
  for (j in 1:5) {
    
    feat_speak1_splines[[i]]<- c( feat_speak1_splines[[i]], splinefun(t, feat_IMF_data_speak1_new[[i]][,j]))
  }
}


feat_speak1_coeff_new2<- c()

for (j in (1:m)) {
  for (i in (1:5)) {
    ls(envir = environment(feat_speak1_splines[[j]][[i]]))
    feat_speak1_coeff_new2<- rbind(feat_speak1_coeff_new2, 
                              as.data.frame(get("z", envir = environment(feat_speak1_splines[[j]][[i]]))))
    
  }
  
}



feat_speak1_b<- matrix(data = feat_speak1_coeff_new2$b, nrow = 20 )

feat_speak1_c<- matrix(data = feat_speak1_coeff_new2$c, nrow = 20 )

feat_speak1_d<- matrix(data = feat_speak1_coeff_new2$d, nrow = 20 )


feat_speak1_coeff_new<- cbind(feat_speak1_b, feat_speak1_c, feat_speak1_d)

rm(feat_speak1_b, feat_speak1_c, feat_speak1_d, feat_speak1_coeff_new2)

#UNIQUE VECTOR for statistics
#s
speak1_vec1<- list()
speak1_vec2<- list()

speak1_vec_new<- lapply(1:m, function(k){
  
  speak1_vec2[[k]]<- lapply(1:dim(feat_speak1_mean[[k]])[2], function(i){
    
    speak1_vec1[[i]]<- sapply(1:dim(feat_speak1_mean[[k]])[1], function(j){
      
      cbind(feat_speak1_mean[[k]][j,i], feat_speak1_var[[k]][j,i], 
            feat_speak1_skew[[k]][j,i], 
            feat_speak1_kurt[[k]][j,i], feat_speak1_min[[k]][j,i], feat_speak1_max[[k]][j,i],  
            feat_speak1_rms[[k]][j,i]) 
    })
    
  })
  
})

rm(feat_speak1_mean, feat_speak1_var, 
   feat_speak1_skew, 
   feat_speak1_kurt, feat_speak1_min, feat_speak1_max,  
    feat_speak1_rms, speak1_vec1, speak1_vec2 )
