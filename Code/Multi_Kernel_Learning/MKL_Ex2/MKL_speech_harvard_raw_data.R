#MKL with MFCC on RAW DATA for Harvard Dataset
#PICK THE 5 THAT ARE BEST PERFORMING (all with RBF kernel)
library("ROCR")
library('ggplot2')
library('lattice')
library('caret')
library('kernlab')
library(e1071)

#(to check the following choice open SVM_in_out_MFCC_rawdata within RawData and check final_tbl performances)
#MFCCrd coeff number  7
#MFCCrd coeff number  8
#MFCCrd coeff number  9
#MFCCrd coeff number  10
#MFCCrd coeff number  11

m = 72

load("C:\\Users\\Marta\\Desktop\\Speech_harvard\\Rawdata\\CM_outofsample_MFCC_rawdata.RData")

delta<- 0.1 

diff_coeff = sapply(1:12, function(i) CM_out_of_sample[[i]]$overall[[1]] - delta  )

S = sum(diff_coeff)


w_coeff = diff_coeff/S


w_final = data.frame(w_coeff)


############################################################################################
#c(sapply(1:2, function(i) sapply(((3*i)-2):(3*i) ,function(j) paste("x_syntspeak1_MLFC_coeff", i, j , sep="_"))))
#colnames(x_syntspeak1_MLFC_new) <- paste("x_syntspeak1_MLFC", 1:ncol(x_syntspeak1_MLFC_new), sep="")


load("C:\\Users\\Marta\\Desktop\\Speech_harvard\\Rawdata\\feat_MLFC_speak1synt_rawdata_new.RData")

x_synt_MLFC_new<- matrix(unlist(mlfc_data_synt_new), byrow=TRUE, nrow=72 )
x_speak1_MLFC_new<- matrix(unlist(mlfc_data_speak1_new), byrow=TRUE, nrow=72 )
x_syntspeak1_MLFC_new<- rbind(x_synt_MLFC_new, x_speak1_MLFC_new)

colnames(x_syntspeak1_MLFC_new)<- c(sapply(1:12, function(i) sapply( ((89*i)-88):(89*i) ,function(j)
                                                              paste("x_syntspeak1_MLFC_coeff", i, j , sep="_"))))


load("C:\\Users\\Marta\\Desktop\\Speech_harvard\\Rawdata\\SVM_CM_insample_MFCC_rawdata.RData")

sigma1<- SVM_syntspeak1_MLFC[[7]]$bestTune$sigma
sigma2<- SVM_syntspeak1_MLFC[[8]]$bestTune$sigma
sigma3<- SVM_syntspeak1_MLFC[[9]]$bestTune$sigma
sigma4<- SVM_syntspeak1_MLFC[[10]]$bestTune$sigma
sigma5<-  SVM_syntspeak1_MLFC[[11]]$bestTune$sigma


#USE THESE ONLY FOR THE CASE OF ALL OF THEM
w1<- w_final[7,1]
w2<- w_final[8,1]
w3<- w_final[9,1]
w4<- w_final[10,1]
w5<- w_final[11,1]



X<- cbind(x_syntspeak1_MLFC_new[,535:623], #7    
          x_syntspeak1_MLFC_new[,624:712], # 8  
          x_syntspeak1_MLFC_new[,713:801], #9
          x_syntspeak1_MLFC_new[,802:890], #10
          x_syntspeak1_MLFC_new[,891:979]) #11


m<-72
y1f<- rep("uno",m)
y2f<- rep("zero",m)    
yf<- as.factor(c(y1f,y2f))

#run thisalong with weights_MKL_code and play with the weights below and inside the function
SVM_kernels_syntspeak1_MFCC<- ksvm(x = X, y = yf, kernel = kkfunction_MFCC_raw.k(sigma_rbf1 = sigma1,
                                                                             sigma_rbf2 = sigma2,
                                                                             sigma_rbf3 = sigma3,
                                                                             sigma_rbf4 = sigma4,
                                                                             sigma_rbf5 = sigma5,
                                                                             w1 = w1,
                                                                             w2 = w2,
                                                                             w3 = w3,
                                                                             w4 = w4,
                                                                             w5 = w5), type = "C-svc")


CM_kernels_syntspeak1_MFCC<- confusionMatrix(SVM_kernels_syntspeak1_MFCC@fitted, yf)
CM_kernels_syntspeak1_MFCC

save(SVM_kernels_syntspeak1_MFCC,CM_kernels_syntspeak1_MFCC, file ="C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL\\MKL_rawdata_5mfccs_SVM_CM.RData")










