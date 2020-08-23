#DATA AND EMD ON SPEAKER 1 DATA
library(seewave)
library(audio)
library(tuneR)
library(EMD)
library(plyr)
library(zoo)
#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------

#import with units = c("sample")  GARETH

#Setting the directory
setwd("C:\\Users\\Marta\\Desktop\\Speech_data\\Speaker1")

mydir<- list.files(getwd())

lecture<- list()
for (i in 1: (length(mydir))) {
  
  lecture[[i]]<- readWave(mydir[[i]], units = c("seconds"))   #sample
}


data_speak1_2<- list()
for (i in 1:(length(mydir))) {
  
  data_speak1_2[[i]]<- lecture[[i]]@left
}


 # minimum1<- list()
 # for (i in 1:(length(mydir))) {
 #   
 #   minimum1[[i]]<- length(data_speak1_2[[i]])
 # }
 # 
 # minimum1<- as.numeric(minimum1)
 # 
 # TT<- min(minimum1)

T<- 132278 #from other script
m<- length(mydir)

#make all same length = T

data_speak1<- matrix(NA, nrow = length(mydir), ncol = T)

for (i in 1: (length(mydir))) {
  
  data_speak1[i,]<- data_speak1_2[[i]][1:T]
}


#I further cut them to remove silence    - GARETH!!!!
T1<- 60000
data_speak1<- matrix(NA, nrow = length(mydir), ncol = T1)

for (i in 1: (length(mydir))) {
  
  data_speak1[i,]<- data_speak1_2[[i]][1:T1]
}


#------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------

#RUM EMPIRICAL MODE DECOMPOSITION ON EACH MODEL - FOR LOOP depending on T            ????????
IMF_data_speak1<- lapply(1:m, function(x) matrix(NA, nrow=1, ncol=5))
IMF_data_speak11<- lapply(1:m, function(x) matrix(NA, nrow=1, ncol=5))
IMF_data_speak2<- list()
IMF<- list()
IIMF<- c()

IMF_data_speak1<- lapply(1:m, function(i){
  
  IMF_data_speak11[[i]]<- lapply(seq(0,T1, 5000)[-1], function(j){  
    
    IMF<- emd(data_speak1[i,(j-4999):j ])
    IIMF<- cbind(matrix(IMF$imf[,1:3],nrow = 5000),
    matrix(IMF$imf[,(dim(IMF$imf)[2])],nrow = 5000),  matrix(IMF$residue, nrow =5000))
    
    IMF_data_speak2[[i]]<- rbind(as.numeric(IMF_data_speak11[[i]]), IIMF)
      
      
      
      
    })
  
})



IMF1<- lapply(1:(length(IMF_data_speak1)), function(x) matrix(NA, nrow=1, ncol=1))
IMF2<- lapply(1:(length(IMF_data_speak1)), function(x) matrix(NA, nrow=1, ncol=1))
IMF3<- lapply(1:(length(IMF_data_speak1)), function(x) matrix(NA, nrow=1, ncol=1))
LAST<- lapply(1:(length(IMF_data_speak1)), function(x) matrix(NA, nrow=1, ncol=1))
RES<- lapply(1:(length(IMF_data_speak1)), function(x) matrix(NA, nrow=1, ncol=1))

for (j in 1: (length(IMF_data_speak1))) {

for (i in 1:(length(IMF_data_speak1[[1]]))) {
  
  IMF1[[j]]<- rbind(IMF1[[j]], matrix(unlist(IMF_data_speak1[[j]][[i]][,1])))
  IMF2[[j]]<- rbind(IMF2[[j]], matrix(unlist(IMF_data_speak1[[j]][[i]][,2])))
  IMF3[[j]]<- rbind(IMF3[[j]], matrix(unlist(IMF_data_speak1[[j]][[i]][,3])))
  LAST[[j]]<- rbind(LAST[[j]], matrix(unlist(IMF_data_speak1[[j]][[i]][,4])))
  RES[[j]]<- rbind(RES[[j]], matrix(unlist(IMF_data_speak1[[j]][[i]][,5])))
}
}

IMF1<- lapply(IMF1, function(x) x[!is.na(x)])
IMF2<- lapply(IMF2, function(x) x[!is.na(x)])
IMF3<- lapply(IMF3, function(x) x[!is.na(x)])
LAST<- lapply(LAST, function(x) x[!is.na(x)])
RES<- lapply(RES, function(x) x[!is.na(x)])

feat_IMF_data_speak1<- list()

for (j in 1:(length(IMF1)) ) {
  feat_IMF_data_speak1[[j]]<- cbind(IMF1[[j]], IMF2[[j]], IMF3[[j]], LAST[[j]], RES[[j]])
}

rm(IMF1,IMF2,IMF3,LAST,RES, IMF_data_speak11, IMF_data_speak2, IMF, IIMF)







