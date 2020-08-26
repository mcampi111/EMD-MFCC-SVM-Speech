#EXTRACTION FEATURES OF SPEAKER 1 SIGNALS
#We consider the first 3 IMFs and residual extracts mean, variance, sd, skewness, kurtosis, min, max
library('fBasics')
library('entropy')
library('seewave')
library('zoo')
library('hht')
library('splines')

#Statistics on a window (No overlap - 10 windows length = 6000 --- GARETH)

m = 20
data_speak1_new = data_speak1_new_PREC
data_synt_new = data_synt_new_PREC

#MEAN------------------------------------------------------------------------------------------------------------
feat_speak1_mean<-list()

for(i in (1:m)){
  
  feat_speak1_mean[[i]]<- rollapply(data_speak1_new[i,],width = 6000, by = 6000, FUN =mean, align = 'left')
  
}


#VARIANCE---------------------------------------------------------------------------------------------------------

feat_speak1_var<-list()

for(i in (1:m)){
  
  feat_speak1_var[[i]]<- rollapply(data_speak1_new[i,],width = 6000, by =6000, FUN =var, align = 'left')
  
}



#SKEWNESS ---------------------------------------------------------------------------------------------------------

feat_speak1_skew<-list()

for(i in (1:m)){
  
  feat_speak1_skew[[i]]<- rollapply(data_speak1_new[i,],width = 6000, by =6000, FUN =skewness, align = 'left')
  
}



#Kurtosis ---------------------------------------------------------------------------------------------------------

feat_speak1_kurt<-list()

for(i in (1:m)){
  
  feat_speak1_kurt[[i]]<- rollapply(data_speak1_new[i,],width = 6000, by =6000, FUN =kurtosis, align = 'left')
  
}


#MIN ---------------------------------------------------------------------------------------------------------

feat_speak1_min<-list()

for(i in (1:m)){
  
  feat_speak1_min[[i]]<- rollapply(data_speak1_new[i,],width = 6000, by =6000, FUN =min, align = 'left')
  
}



#MAX ---------------------------------------------------------------------------------------------------------

feat_speak1_max<-list()

for(i in (1:m)){
  
  feat_speak1_max[[i]]<- rollapply(data_speak1_new[i,],width = 6000, by =6000, FUN =max, align = 'left')
  
}


#ROOT MEAN SQUARE ------------------------------------------------------------------------------------------------

feat_speak1_rms<-list()

for(i in (1:m)){
  
  feat_speak1_rms[[i]]<- rollapply(data_speak1_new[i,],width = 6000, by =6000, FUN =rms, align = 'left')
  
}


#UNIQUE VECTOR for statistics
#s
speak1_vec2<- list()

speak1_vec<- lapply(1:m, function(k){
  
  speak1_vec2[[k]]<- sapply(1:length(feat_speak1_mean[[k]]), function(i){
    
    
      cbind(feat_speak1_mean[[k]][i], feat_speak1_var[[k]][i], 
            feat_speak1_skew[[k]][i], 
            feat_speak1_kurt[[k]][i], 
            feat_speak1_min[[k]][i], 
            feat_speak1_max[[k]][i], 
            feat_speak1_rms[[k]][i] ) 
    
  })
  
})

rm(feat_speak1_mean, feat_speak1_var, 
   feat_speak1_skew, 
   feat_speak1_kurt, feat_speak1_min, feat_speak1_max,  
    feat_speak1_rms, speak1_vec1, speak1_vec2 )
