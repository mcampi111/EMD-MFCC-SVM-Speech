library(seewave)
library(audio)
library(tuneR)
library(EMD)
library(plyr)
library(zoo)


#Setting the directory
setwd("C:\\Users\\Marta\\Desktop\\Speech_OUT\\Synthetic_female_out")

mydir<- list.files(getwd())

lecture<- list()
for (i in 1: (length(mydir))) {
  
  lecture[[i]]<- readWave(mydir[[i]], units = c("sample")) 
}


data_synt_new_2<- list()
for (i in 1:(length(mydir))) {
  
  data_synt_new_2[[i]]<- lecture[[i]]@left
}


# minimum<- list()
# for (i in 1:(length(mydir))) {
# 
#   minimum[[i]]<- length(data_synt_new_2[[i]])
# }
# 
# minimum<- as.numeric(minimum)

T1<- 60000

data_synt_new<- matrix(NA, nrow = length(mydir), ncol = T1)

for (i in 1:(length(mydir))) {
  
  data_synt_new[i,]<- data_synt_new_2[[i]][2000:(T1+1999)]
  
}


#------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
m<- 20

#RUM EMPIRICAL MODE DECOMPOSITION ON EACH MODEL - FOR LOOP depending on T            ????????
IMF_data_synt_new1<- lapply(1:m, function(x) matrix(NA, nrow=1, ncol=5))
IMF_data_synt_new11<- lapply(1:m, function(x) matrix(NA, nrow=1, ncol=5))
IMF_data_synt_new2<- list()
IMF<- list()
IIMF<- c()

IMF_data_synt_new1<- lapply(1:m, function(i){
  
  IMF_data_synt_new11[[i]]<- lapply(seq(0,60000, 5000)[-1], function(j){  
    
    IMF<- emd(data_synt_new[i,(j-4999):j ])     #ERRORE
    IIMF<- cbind(matrix(IMF$imf[,1:3],nrow = 5000),
                 matrix(IMF$imf[,(dim(IMF$imf)[2])],nrow = 5000),  matrix(IMF$residue, nrow =5000))
    
    IMF_data_synt_new2[[i]]<- rbind(as.numeric(IMF_data_synt_new11[[i]]), IIMF)
    
    
    
    
  })
  
})



IMF1<- lapply(1:(length(IMF_data_synt_new1)), function(x) matrix(NA, nrow=1, ncol=1))
IMF2<- lapply(1:(length(IMF_data_synt_new1)), function(x) matrix(NA, nrow=1, ncol=1))
IMF3<- lapply(1:(length(IMF_data_synt_new1)), function(x) matrix(NA, nrow=1, ncol=1))
LAST<- lapply(1:(length(IMF_data_synt_new1)), function(x) matrix(NA, nrow=1, ncol=1))
RES<- lapply(1:(length(IMF_data_synt_new1)), function(x) matrix(NA, nrow=1, ncol=1))

for (j in 1: (length(IMF_data_synt_new1))) {
  
  for (i in 1:(length(IMF_data_synt_new1[[1]]))) {
    
    IMF1[[j]]<- rbind(IMF1[[j]], matrix(unlist(IMF_data_synt_new1[[j]][[i]][,1])))
    IMF2[[j]]<- rbind(IMF2[[j]], matrix(unlist(IMF_data_synt_new1[[j]][[i]][,2])))
    IMF3[[j]]<- rbind(IMF3[[j]], matrix(unlist(IMF_data_synt_new1[[j]][[i]][,3])))
    LAST[[j]]<- rbind(LAST[[j]], matrix(unlist(IMF_data_synt_new1[[j]][[i]][,4])))
    RES[[j]]<- rbind(RES[[j]], matrix(unlist(IMF_data_synt_new1[[j]][[i]][,5])))
  }
}

IMF1<- lapply(IMF1, function(x) x[!is.na(x)])
IMF2<- lapply(IMF2, function(x) x[!is.na(x)])
IMF3<- lapply(IMF3, function(x) x[!is.na(x)])
LAST<- lapply(LAST, function(x) x[!is.na(x)])
RES<- lapply(RES, function(x) x[!is.na(x)])

feat_IMF_data_synt_new<- list()

for (j in 1:(length(IMF1)) ) {
  feat_IMF_data_synt_new[[j]]<- cbind(IMF1[[j]], IMF2[[j]], IMF3[[j]], LAST[[j]], RES[[j]])
}

rm(IMF1,IMF2,IMF3,LAST,RES, IMF_data_synt_new11, IMF_data_synt_new2, IMF, IIMF)



