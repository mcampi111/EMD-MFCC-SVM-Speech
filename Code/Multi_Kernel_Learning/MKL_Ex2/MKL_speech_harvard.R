library("ROCR")
library('ggplot2')
library('lattice')
library('caret')
library('kernlab')
library(e1071)



#MCC1 - RBF     -->  8
#MCC2 - RBF    -->  2
#MCC3 - RBF    -->  7
#MCC4 - RBF    --> 2
#MCC5 - RBF   -->  1

load("C:\\Users\\Marta\\Desktop\\Speech_harvard\\SVM_speak1_synth1_out\\CM_speak1_MFCC_IMF1.RData")
load("C:\\Users\\Marta\\Desktop\\Speech_harvard\\SVM_speak1_synth1_out\\CM_speak1_MFCC_IMF2.RData")
load("C:\\Users\\Marta\\Desktop\\Speech_harvard\\SVM_speak1_synth1_out\\CM_speak1_MFCC_IMF3.RData")
load("C:\\Users\\Marta\\Desktop\\Speech_harvard\\SVM_speak1_synth1_out\\CM_speak1_MFCC_IMF4.RData")
load("C:\\Users\\Marta\\Desktop\\Speech_harvard\\SVM_speak1_synth1_out\\CM_speak1_MFCC_IMF5.RData")

delta<- 0.1 

diff_IMF1 = sapply(1:12, function(i) get(paste("CM_syntspeak1_MLFC_IMF1_coeff", i, "_new", sep = ""))$overall[[1]] - delta  )

diff_IMF2 = sapply(1:12, function(i) get(paste("CM_syntspeak1_MLFC_IMF2_coeff", i, "_new", sep = ""))$overall[[1]] - delta  )

diff_IMF3 = sapply(1:12, function(i) get(paste("CM_syntspeak1_MLFC_IMF3_coeff", i, "_new", sep = ""))$overall[[1]] - delta  )

diff_IMF4 = sapply(1:12, function(i) get(paste("CM_syntspeak1_MLFC_IMF4_coeff", i, "_new", sep = ""))$overall[[1]] - delta  )

diff_IMF5 = sapply(1:12, function(i) get(paste("CM_syntspeak1_MLFC_IMF5_coeff", i, "_new", sep = ""))$overall[[1]] - delta  )


#case all the 5 MFCC-IMFs
S = sum(diff_IMF1,diff_IMF2, diff_IMF3, diff_IMF4, diff_IMF5)


w_IMF1 = diff_IMF1/S
w_IMF2 = diff_IMF2/S
w_IMF3 = diff_IMF3/S
w_IMF4 = diff_IMF4/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF1,w_IMF2,w_IMF3,w_IMF4,w_IMF5)



############################################################################################
############################################################################################


load("C:\\Users\\Marta\\Desktop\\Speech_harvard\\Data_speak1_out\\features_MLFC_speak1_noprec_new.RData")
load("C:\\Users\\Marta\\Desktop\\Speech_harvard\\Data_synth1_out\\features_MLFC_synt_noprec_new.RData")


x_synt_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_synt), byrow=TRUE, nrow=72 )
x_speak1_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_speak1), byrow=TRUE, nrow=72 )
x_syntspeak1_MLFC_IMF1_new<- rbind(x_synt_MLFC_IMF1, x_speak1_MLFC_IMF1)
colnames(x_syntspeak1_MLFC_IMF1_new) <- paste("x_syntspeak1_MLFC_IMF1", 1:ncol(x_syntspeak1_MLFC_IMF1_new), sep="")

x_synt_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_synt), byrow=TRUE, nrow=72 )
x_speak1_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_speak1), byrow=TRUE, nrow=72 )
x_syntspeak1_MLFC_IMF2_new<- rbind(x_synt_MLFC_IMF2, x_speak1_MLFC_IMF2)
colnames(x_syntspeak1_MLFC_IMF2_new) <- paste("x_syntspeak1_MLFC_IMF2", 1:ncol(x_syntspeak1_MLFC_IMF2_new), sep="")

x_synt_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_synt), byrow=TRUE, nrow=72 )
x_speak1_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_speak1), byrow=TRUE, nrow=72 )
x_syntspeak1_MLFC_IMF3_new<- rbind(x_synt_MLFC_IMF3, x_speak1_MLFC_IMF3)
colnames(x_syntspeak1_MLFC_IMF3_new) <- paste("x_syntspeak1_MLFC_IMF3", 1:ncol(x_syntspeak1_MLFC_IMF3_new), sep="")

x_synt_MLFC_IMF4<- matrix(unlist(mlfc_IMF4_data_synt), byrow=TRUE, nrow=72 )
x_speak1_MLFC_IMF4<- matrix(unlist(mlfc_IMF4_data_speak1), byrow=TRUE, nrow=72 )
x_syntspeak1_MLFC_IMF4_new<- rbind(x_synt_MLFC_IMF4, x_speak1_MLFC_IMF4)
colnames(x_syntspeak1_MLFC_IMF4_new) <- paste("x_syntspeak1_MLFC_IMF4", 1:ncol(x_syntspeak1_MLFC_IMF4_new), sep="")

x_synt_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_synt), byrow=TRUE, nrow=72 )
x_speak1_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_speak1), byrow=TRUE, nrow=72 )
x_syntspeak1_MLFC_IMF5_new<- rbind(x_synt_MLFC_IMF5, x_speak1_MLFC_IMF5)
colnames(x_syntspeak1_MLFC_IMF5_new) <- paste("x_syntspeak1_MLFC_IMF5", 1:ncol(x_syntspeak1_MLFC_IMF5_new), sep="")



load("C:\\Users\\Marta\\Desktop\\Speech_harvard\\SVM_speak1_synth1\\SVM_CM_speak1_MFCC_IMF1.RData")
load("C:/Users/Marta/Desktop/Speech_harvard\\SVM_speak1_synth1/SVM_CM_speak1_MFCC_IMF2.RData")
load("C:/Users/Marta/Desktop/Speech_harvard\\SVM_speak1_synth1/SVM_CM_speak1_MFCC_IMF3.RData")
load("C:/Users/Marta/Desktop/Speech_harvard\\SVM_speak1_synth1/SVM_CM_speak1_MFCC_IMF4.RData")
load("C:\\Users\\Marta\\Desktop\\Speech_harvard\\SVM_speak1_synth1\\SVM_CM_speak1_MFCC_IMF5.RData")

sigma1<- SVM_syntspeak1_MLFC_IMF1[[8]]$bestTune$sigma
sigma2<- SVM_syntspeak1_MLFC_IMF2[[2]]$bestTune$sigma
sigma3<- SVM_syntspeak1_MLFC_IMF3[[7]]$bestTune$sigma
sigma4<- SVM_syntspeak1_MLFC_IMF4[[2]]$bestTune$sigma
sigma5<-  SVM_syntspeak1_MLFC_IMF5[[1]]$bestTune$sigma


#USE THESE ONLY FOR THE CASE OF ALL OF THEM
w1<- w_final[8,1]
w2<- w_final[2,2]
w3<- w_final[7,3]
w4<- w_final[2,4]
w5<- w_final[1,5]


X<- cbind(x_syntspeak1_MLFC_IMF1_new[,624:712], #8
          x_syntspeak1_MLFC_IMF2_new[,90:178],  #2
          x_syntspeak1_MLFC_IMF3_new[,535:623], #7
          x_syntspeak1_MLFC_IMF4_new[,90:178],  #2
          x_syntspeak1_MLFC_IMF5_new[,1:89])    #1

m<-72
y1f<- rep("uno",m)
y2f<- rep("zero",m)    
yf<- as.factor(c(y1f,y2f))

#run this along with weights_MKL_code and play with the weights below and inside the function
SVM_kernels_syntspeak1_MFCC<- ksvm(x = X, y = yf, kernel = kkfunction_MFCC.k(sigma_rbf1 = sigma1,
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


save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_all_features_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_4_features_1_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_4_features_2_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_4_features_3_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_4_features_4_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_4_features_5_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_3_features_1_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_3_features_2_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_3_features_3_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_3_features_4_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_3_features_5_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_3_features_6_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_3_features_7_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_3_features_8_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_3_features_9_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_3_features_10_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_2_features_1_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_2_features_2_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_2_features_3_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_2_features_4_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_2_features_5_SVM_CM.RData")

save(SVM_kernels_syntspeak1_MFCC, CM_kernels_syntspeak1_MFCC, file = "C:\\Users\\Marta\\Desktop\\Speech_harvard\\MKL_2_features_7_SVM_CM.RData")

#I forgot to save some with only 2