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
  
  feat_synt_mean[[i]]<- rollapply(data_synt[i,],width = 6000, by =6000, FUN =mean, align = 'left')
  
}


#VARIANCE---------------------------------------------------------------------------------------------------------

feat_synt_var<-list()

for(i in (1:m)){
  
  feat_synt_var[[i]]<- rollapply(data_synt[i,],width = 6000, by =6000, FUN =var, align = 'left')
  
}



#SKEWNESS ---------------------------------------------------------------------------------------------------------

feat_synt_skew<-list()

for(i in (1:m)){
  
  feat_synt_skew[[i]]<- rollapply(data_synt[i,],width = 6000, by =6000, FUN =skewness, align = 'left')
  
}



#Kurtosis ---------------------------------------------------------------------------------------------------------

feat_synt_kurt<-list()

for(i in (1:m)){
  
  feat_synt_kurt[[i]]<- rollapply(data_synt[i,],width = 6000, by =6000, FUN =kurtosis, align = 'left')
  
}


#MIN ---------------------------------------------------------------------------------------------------------

feat_synt_min<-list()

for(i in (1:m)){
  
  feat_synt_min[[i]]<- rollapply(data_synt[i,],width = 6000, by =6000, FUN =min, align = 'left')
  
}



#MAX ---------------------------------------------------------------------------------------------------------

feat_synt_max<-list()

for(i in (1:m)){
  
  feat_synt_max[[i]]<- rollapply(data_synt[i,],width = 6000, by =6000, FUN =max, align = 'left')
  
}


#ROOT MEAN SQUARE ------------------------------------------------------------------------------------------------

feat_synt_rms<-list()

for(i in (1:m)){
  
  feat_synt_rms[[i]]<- rollapply(data_synt[i,],width = 6000, by =6000, FUN =rms, align = 'left')
  
}



#UNIQUE VECTOR for statistics
#s
synt_vec2<- list()

synt_vec<- lapply(1:m, function(k){
  
  synt_vec2[[k]]<- sapply(1:length(feat_synt_mean[[k]]), function(i){
    
      cbind(feat_synt_mean[[k]][i], feat_synt_var[[k]][i], 
            feat_synt_skew[[k]][i], 
            feat_synt_kurt[[k]][i], feat_synt_min[[k]][i], feat_synt_max[[k]][i],  
            feat_synt_rms[[k]][i]) 
    
  })
  
})

rm(feat_synt_mean, feat_synt_var, 
   feat_synt_skew, 
   feat_synt_kurt, feat_synt_min, feat_synt_max,  
    feat_synt_rms, synt_vec1, synt_vec2 )
