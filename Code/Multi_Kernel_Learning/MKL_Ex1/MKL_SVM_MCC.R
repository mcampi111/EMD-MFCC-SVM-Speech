#MKL WITH EACH FEATURE (not divided between cepstral and not cepstral)
library("ROCR")
library('ggplot2')
library('lattice')
library('caret')
library('kernlab')



#MCC1 - LP     -->  7 - 8 - 9
#MCC2 - RBF    -->  8
#MCC3 - RBF    -->  5
#MCC4 - RBF    --> 10
#MCC5 - vann   -->  5 (9)


#MCC5 - LP   --> 8 (10)


#LP-----------------------------------------------------------------------------------------------------------
load("C:\\Users\\Marta\\Desktop\\RESULTS\\SVM_Speaker1\\LP\\SVM_CM_speak1_CEP_IMF1_LP.RData")

lp_syntspeak1_MLFC_IMF1_coeff7<- laplacedot(SVM_syntspeak1_MLFC_IMF1_lp[[7]]$bestTune$sigma)
x_lp_syntspeak1_MLFC_coeff7<- kernelMatrix(lp_syntspeak1_MLFC_IMF1_coeff7,as.matrix(x_syntspeak1_MLFC_IMF1_new[,805:938]))
rcond(x_lp_syntspeak1_MLFC_coeff7)

CM_syntspeak1_MLFC_IMF1_coeff7_new
prova<- ksvm(x = x_lp_syntspeak1_MLFC_coeff7, y = yf, kernel = "matrix", type = "C-svc")
provaCM<- confusionMatrix(prova@fitted,yf)
#RBF---------------------------------------------------------------------------------------------------------------
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/RBF/SVM_CM_speak1_CEP_IMF2_RBF.RData")

rbf_syntspeak1_MLFC_IMF2_coeff8<- rbfdot(SVM_syntspeak1_MLFC_IMF2[[8]]$bestTune$sigma)
x_rbf_syntspeak1_MLFC_coeff8<- kernelMatrix(rbf_syntspeak1_MLFC_IMF2_coeff8,as.matrix(x_syntspeak1_MLFC_IMF2_new[,939:1072]))
rcond(x_rbf_syntspeak1_MLFC_coeff8)

CM_syntspeak1_MLFC_IMF2_coeff8_new
prova2<- ksvm(x = x_rbf_syntspeak1_MLFC_coeff8, y = yf, kernel = "matrix", type = "C-svc")
provaCM2<- confusionMatrix(prova2@fitted,yf)
#RBF---------------------------------------------------------------------------------------------------------------
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/RBF/SVM_CM_speak1_CEP_IMF3_RBF.RData")

rbf_syntspeak1_MLFC_IMF3_coeff5<- rbfdot(SVM_syntspeak1_MLFC_IMF3[[5]]$bestTune$sigma)
x_rbf_syntspeak1_MLFC_coeff5<- kernelMatrix(rbf_syntspeak1_MLFC_IMF3_coeff5,as.matrix(x_syntspeak1_MLFC_IMF3_new[,537:670]))
rcond(x_rbf_syntspeak1_MLFC_coeff5)


#RBF---------------------------------------------------------------------------------------------------------------
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/RBF/SVM_CM_speak1_CEP_IMF4_RBF.RData")

rbf_syntspeak1_MLFC_IMF4_coeff10<- rbfdot(SVM_syntspeak1_MLFC_IMF4[[10]]$bestTune$sigma)
x_rbf_syntspeak1_MLFC_coeff10<- kernelMatrix(rbf_syntspeak1_MLFC_IMF4_coeff10,as.matrix(x_syntspeak1_MLFC_IMF4_new[,1207:1340]))
rcond(x_rbf_syntspeak1_MLFC_coeff10)


# #----------------------------------------------------------------------------
 load("C:\\Users\\Marta\\Desktop\\RESULTS\\SVM_Speaker1\\VANN\\SVM_CM_speak1_CEP_IMF5_VAN.RData")
 
 vann_syntspeak1_MLFC_IMF5_coeff5<- vanilladot()
 x_vann_syntspeak1_MLFC_coeff5<- kernelMatrix(vann_syntspeak1_MLFC_IMF5_coeff5,as.matrix(x_syntspeak1_MLFC_IMF5_new[,537:670]))
 rcond(x_vann_syntspeak1_MLFC_coeff5)
 
 
 vann_syntspeak1_MLFC_IMF5_coeff9<- vanilladot()
 x_vann_syntspeak1_MLFC_coeff9<- kernelMatrix(vann_syntspeak1_MLFC_IMF5_coeff9,as.matrix(x_syntspeak1_MLFC_IMF5_new[,1073:1206]))
 rcond(x_vann_syntspeak1_MLFC_coeff9)


#LP-----------------------------------------------------------------------------------------------------------
load("C:\\Users\\Marta\\Desktop\\RESULTS\\SVM_Speaker1\\LP\\SVM_CM_speak1_CEP_IMF5_LP.RData")

lp_syntspeak1_MLFC_IMF5_coeff8<- laplacedot(SVM_syntspeak1_MLFC_IMF5_lp[[8]]$bestTune$sigma)
x_lp_syntspeak1_MLFC_coeff8<- kernelMatrix(lp_syntspeak1_MLFC_IMF5_coeff8,as.matrix(x_syntspeak1_MLFC_IMF5_new[,939:1072]))
rcond(x_lp_syntspeak1_MLFC_coeff8)


# #KERNEL WEIGHTS------------------------------------------------------------------------------------------
# #KERNEL MATRIX WITH WEIGTHS------------------------------------------------------------------------------
# 
# #either better accuracy or, as second parameter, smaller rcond
# #
# 
# x_kernel<- ((w_LP_MLFC_IMF1[7]*x_lp_syntspeak1_MLFC_coeff7) + 
#               (w_RBF_MLFC_IMF2[8]*x_rbf_syntspeak1_MLFC_coeff8) +
#               (w_RBF_MLFC_IMF3[5]*x_rbf_syntspeak1_MLFC_coeff5) + 
#               (w_RBF_MLFC_IMF4[10]*x_rbf_syntspeak1_MLFC_coeff10) +
#               (w_LP_MLFC_IMF5[8]*x_lp_syntspeak1_MLFC_coeff8))
# 
# 
# 
# #SVM -------------------------------------------------------------------------------------------------------
# 
# m<-20
# y1f<- rep("uno",m)
# y2f<- rep("zero",m)    
# yf<- as.factor(c(y1f,y2f))
# 
# 
# SVM_kernels_syntspeak1<- ksvm(x = x_kernel, y = yf, kernel = "matrix", type = "C-svc")
# CM_kernels_syntspeak1<- confusionMatrix(SVM_kernels_syntspeak1@fitted, yf)
# CM_kernels_syntspeak1
# 
# save(SVM_kernels_syntspeak1,CM_kernels_syntspeak1,
#      file = "C:\\Users\\Marta\\Desktop\\Speech_out\\SVM_Speaker1_out\\CM_SVM_MKL_only_MCCC.RData")

#NEW------------------------------------------------------------------------
m<-50
y1f<- rep("uno",m)
y2f<- rep("zero",m)    
yf<- as.factor(c(y1f,y2f))

x_synt_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_synt_new_50), byrow=TRUE, nrow=50 )
x_speak1_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_speak1_new_50), byrow=TRUE, nrow=50 )
x_syntspeak1_MLFC_IMF1_new<- rbind(x_synt_MLFC_IMF1, x_speak1_MLFC_IMF1)
colnames(x_syntspeak1_MLFC_IMF1_new) <- paste("x_syntspeak1_MLFC_IMF1", 1:ncol(x_syntspeak1_MLFC_IMF1_new), sep="")

x_synt_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_synt_new_50), byrow=TRUE, nrow=50 )
x_speak1_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_speak1_new_50), byrow=TRUE, nrow=50 )
x_syntspeak1_MLFC_IMF2_new<- rbind(x_synt_MLFC_IMF2, x_speak1_MLFC_IMF2)
colnames(x_syntspeak1_MLFC_IMF2_new) <- paste("x_syntspeak1_MLFC_IMF2", 1:ncol(x_syntspeak1_MLFC_IMF2_new), sep="")

x_synt_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_synt_new_50), byrow=TRUE, nrow=50 )
x_speak1_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_speak1_new_50), byrow=TRUE, nrow=50 )
x_syntspeak1_MLFC_IMF3_new<- rbind(x_synt_MLFC_IMF3, x_speak1_MLFC_IMF3)
colnames(x_syntspeak1_MLFC_IMF3_new) <- paste("x_syntspeak1_MLFC_IMF3", 1:ncol(x_syntspeak1_MLFC_IMF3_new), sep="")

x_synt_MLFC_IMF4<- matrix(unlist(mlfc_IMF4_data_synt_new_50), byrow=TRUE, nrow=50 )
x_speak1_MLFC_IMF4<- matrix(unlist(mlfc_IMF4_data_speak1_new_50), byrow=TRUE, nrow=50 )
x_syntspeak1_MLFC_IMF4_new<- rbind(x_synt_MLFC_IMF4, x_speak1_MLFC_IMF4)
colnames(x_syntspeak1_MLFC_IMF4_new) <- paste("x_syntspeak1_MLFC_IMF4", 1:ncol(x_syntspeak1_MLFC_IMF4_new), sep="")

x_synt_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_synt_new_50), byrow=TRUE, nrow=50 )
x_speak1_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_speak1_new_50), byrow=TRUE, nrow=50 )
x_syntspeak1_MLFC_IMF5_new<- rbind(x_synt_MLFC_IMF5, x_speak1_MLFC_IMF5)
colnames(x_syntspeak1_MLFC_IMF5_new) <- paste("x_syntspeak1_MLFC_IMF5", 1:ncol(x_syntspeak1_MLFC_IMF5_new), sep="")

#BEST CASE FEATURES
load("C:\\Users\\Marta\\Desktop\\RESULTS\\SVM_Speaker1\\LP\\SVM_CM_speak1_CEP_IMF1_LP.RData")
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/RBF/SVM_CM_speak1_CEP_IMF2_RBF.RData")
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/RBF/SVM_CM_speak1_CEP_IMF3_RBF.RData")
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/RBF/SVM_CM_speak1_CEP_IMF4_RBF.RData")
load("C:\\Users\\Marta\\Desktop\\RESULTS\\SVM_Speaker1\\VANN\\SVM_CM_speak1_CEP_IMF5_VAN.RData")

sigma1<- SVM_syntspeak1_MLFC_IMF1_lp[[7]]$bestTune$sigma
sigma2<- SVM_syntspeak1_MLFC_IMF2[[8]]$bestTune$sigma
sigma3<- SVM_syntspeak1_MLFC_IMF3[[5]]$bestTune$sigma
sigma4<- SVM_syntspeak1_MLFC_IMF4[[10]]$bestTune$sigma

w1<- w_LP_MLFC_IMF1[7]
w2<- w_RBF_MLFC_IMF2[8]
w3<- w_RBF_MLFC_IMF3[5]
w4<- w_RBF_MLFC_IMF4[10]
w5<- w_vann_MLFC_IMF5[5]

X<- cbind(x_syntspeak1_MLFC_IMF1_new[,805:938],    #7
          x_syntspeak1_MLFC_IMF2_new[,939:1072],   #8
          x_syntspeak1_MLFC_IMF3_new[,537:670],    #5
          x_syntspeak1_MLFC_IMF4_new[,1207:1340],  #10
          x_syntspeak1_MLFC_IMF5_new[,537:670])    #5


SVM_kernels_syntspeak1_MFCC<- ksvm(x = X, y = yf, kernel = kkfunction_MFCC.k(sigma_lp1 = sigma1,
                                                                             sigma_rbf2 = sigma2,
                                                                             sigma_rbf3 = sigma3,
                                                                             sigma_rbf4 = sigma4,
                                                                             w1 = 0,
                                                                             w2 = 0,
                                                                             w3 = 0,
                                                                             w4 = w4,
                                                                             w5 = w5), type = "C-svc")


CM_kernels_syntspeak1_MFCC<- confusionMatrix(SVM_kernels_syntspeak1_MFCC@fitted, yf)
CM_kernels_syntspeak1_MFCC
 
#save(SVM_kernels_syntspeak1_MFCC,CM_kernels_syntspeak1_MFCC,
#    file = ".RData")


#WORST CASE SCENARIO - TO MAKE THINGS EASIER USE ONLY RBF
load("C:\\Users\\Marta\\Desktop\\RESULTS\\SVM_Speaker1\\RBF\\SVM_CM_speak1_CEP_IMF1_RBF.RData")
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/RBF/SVM_CM_speak1_CEP_IMF2_RBF.RData")
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/RBF/SVM_CM_speak1_CEP_IMF3_RBF.RData")
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/RBF/SVM_CM_speak1_CEP_IMF4_RBF.RData")
load("C:\\Users\\Marta\\Desktop\\RESULTS\\SVM_Speaker1\\RBF\\SVM_CM_speak1_CEP_IMF5_RBF.RData")


sigma1<- SVM_syntspeak1_MLFC_IMF1[[1]]$bestTune$sigma
sigma2<- SVM_syntspeak1_MLFC_IMF2[[1]]$bestTune$sigma
sigma3<- SVM_syntspeak1_MLFC_IMF3[[1]]$bestTune$sigma
sigma4<- SVM_syntspeak1_MLFC_IMF4[[1]]$bestTune$sigma
sigma5<- SVM_syntspeak1_MLFC_IMF5[[1]]$bestTune$sigma

w1<- w_RBF_MLFC_IMF1[1]
w2<- w_RBF_MLFC_IMF2[1]
w3<- w_RBF_MLFC_IMF3[1]
w4<- w_RBF_MLFC_IMF4[1]
w5<- w_RBF_MLFC_IMF5[1]

X<- cbind(x_syntspeak1_MLFC_IMF1_new[,1:134],    #1
          x_syntspeak1_MLFC_IMF2_new[,1:134],   #1
          x_syntspeak1_MLFC_IMF3_new[,1:134],    #1
          x_syntspeak1_MLFC_IMF4_new[,1:134],  #1
          x_syntspeak1_MLFC_IMF5_new[,1:134])    #1


SVM_kernels_syntspeak1_MFCC_worst<- ksvm(x = X, y = yf, kernel = kkfunction_MFCC.k_worst(sigma_rbf1 = sigma1,
                                                                             sigma_rbf2 =  sigma2,
                                                                             sigma_rbf3 =  sigma3,
                                                                             sigma_rbf4 =  sigma4,
                                                                             sigma_rbf5 =  sigma5,
                                                                             w1 = w1,
                                                                             w2 = w2,
                                                                             w3 = w3,
                                                                             w4 = w4,
                                                                             w5 = w5), type = "C-svc")

CM_kernels_syntspeak1_MFCC_worst<- confusionMatrix(SVM_kernels_syntspeak1_MFCC_worst@fitted, yf)
CM_kernels_syntspeak1_MFCC_worst



#WORST CASE SCENARIO - TO MAKE THINGS EASIER USE ONLY LP
load("C:\\Users\\Marta\\Desktop\\RESULTS\\SVM_Speaker1\\LP\\SVM_CM_speak1_CEP_IMF1_LP.RData")
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/LP/SVM_CM_speak1_CEP_IMF2_LP.RData")
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/LP/SVM_CM_speak1_CEP_IMF3_LP.RData")
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/LP/SVM_CM_speak1_CEP_IMF4_LP.RData")
load("C:\\Users\\Marta\\Desktop\\RESULTS\\SVM_Speaker1\\LP\\SVM_CM_speak1_CEP_IMF5_LP.RData")


sigma1<- SVM_syntspeak1_MLFC_IMF1_lp[[1]]$bestTune$sigma
sigma2<- SVM_syntspeak1_MLFC_IMF2_lp[[1]]$bestTune$sigma
sigma3<- SVM_syntspeak1_MLFC_IMF3_lp[[1]]$bestTune$sigma
sigma4<- SVM_syntspeak1_MLFC_IMF4_lp[[1]]$bestTune$sigma
sigma5<- SVM_syntspeak1_MLFC_IMF5_lp[[1]]$bestTune$sigma

w1<- w_LP_MLFC_IMF1[1]
w2<- w_LP_MLFC_IMF2[1]
w3<- w_LP_MLFC_IMF3[1]
w4<- w_LP_MLFC_IMF4[1]
w5<- w_LP_MLFC_IMF5[1]

X<- cbind(x_syntspeak1_MLFC_IMF1_new[,1:134],    #1
          x_syntspeak1_MLFC_IMF2_new[,1:134],   #1
          x_syntspeak1_MLFC_IMF3_new[,1:134],    #1
          x_syntspeak1_MLFC_IMF4_new[,1:134],  #1
          x_syntspeak1_MLFC_IMF5_new[,1:134])    #1


SVM_kernels_syntspeak1_MFCC_worst<- ksvm(x = X, y = yf, kernel = kkfunction_MFCC.k_worst_lp(sigma_lp1 = sigma1,
                                                                                         sigma_lp2 =  sigma2,
                                                                                         sigma_lp3 =  sigma3,
                                                                                         sigma_lp4 =  sigma4,
                                                                                         sigma_lp5 =  sigma5,
                                                                                         w1 = 0,
                                                                                         w2 = 0,
                                                                                         w3 = 0,
                                                                                         w4 = 0,
                                                                                         w5 = w5), type = "C-svc")

CM_kernels_syntspeak1_MFCC_worst<- confusionMatrix(SVM_kernels_syntspeak1_MFCC_worst@fitted, yf)
CM_kernels_syntspeak1_MFCC_worst



#WORST CASE SCENARIO BY PICKING THE WORST KERNEL AND THE WORKST FEATURE
#This corresponds to
#MFCC1 - LAPLACE - COEFF 4
#MFCC2 - SIGMOID - COEFF 5
#MFCC3 - VANILLA - COEFF 10
#MFCC4 - RBF - COEFF 1
#MFCC5 - RBF - COEFF 11

load("C:\\Users\\Marta\\Desktop\\RESULTS\\SVM_Speaker1\\LP\\SVM_CM_speak1_CEP_IMF1_LP.RData")
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/TAN/SVM_CM_speak1_CEP_IMF2_TAN.RData")
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/VANN/SVM_CM_speak1_CEP_IMF3_VAN.RData")
load("C:/Users/Marta/Desktop/RESULTS/SVM_Speaker1/RBF/SVM_CM_speak1_CEP_IMF4_RBF.RData")
load("C:\\Users\\Marta\\Desktop\\RESULTS\\SVM_Speaker1\\RBF\\SVM_CM_speak1_CEP_IMF5_RBF.RData")

sigma1<- SVM_syntspeak1_MLFC_IMF1_lp[[4]]$bestTune$sigma
scale2<- SVM_syntspeak1_MLFC_IMF2_tan[[5]]$bestTune$scale
offset2<- SVM_syntspeak1_MLFC_IMF2_tan[[10]]$bestTune$offset
sigma4<- SVM_syntspeak1_MLFC_IMF4[[1]]$bestTune$sigma
sigma5<- SVM_syntspeak1_MLFC_IMF5[[11]]$bestTune$sigma

w1<- w_LP_MLFC_IMF1[4]
w2<- w_tan_MLFC_IMF2[5]
w3<- w_vann_MLFC_IMF3[10]
w4<- w_RBF_MLFC_IMF4[1]
w5<- w_RBF_MLFC_IMF5[11]



X<- cbind(x_syntspeak1_MLFC_IMF1_new[,403:536],      #4
          x_syntspeak1_MLFC_IMF2_new[,537:670],      #5
          x_syntspeak1_MLFC_IMF3_new[,1073:1206],    #10
          x_syntspeak1_MLFC_IMF4_new[,1:134],        #1
          x_syntspeak1_MLFC_IMF5_new[,1207:1340])    #11


SVM_kernels_syntspeak1_MFCC_worst<- ksvm(x = X, y = yf, kernel = kkfunction_MFCC.k_worst_all(sigma_lp1 = sigma1,
                                                                                             scale_tan2 = scale2,
                                                                                             offset_tan2 = offset2,
                                                                                             sigma_rbf4 =  sigma4,
                                                                                             sigma_rbf5 =  sigma5,
                                                                                             w1 = 0,
                                                                                             w2 = 0,
                                                                                             w3 = 0,
                                                                                             w4 = w4,
                                                                                             w5 = w5), type = "C-svc")


CM_kernels_syntspeak1_MFCC_worst<- confusionMatrix(SVM_kernels_syntspeak1_MFCC_worst@fitted, yf)
CM_kernels_syntspeak1_MFCC_worst

#finisci con in sample data but out of sample weights


