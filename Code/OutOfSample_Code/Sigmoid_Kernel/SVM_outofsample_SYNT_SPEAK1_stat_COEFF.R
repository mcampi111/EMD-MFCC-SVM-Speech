#SVM OUT OF SAMPLE - SYNT vs SPEAKER1 ---  FEATURE: stat --- KERNEL: RBASIS  
library("ROCR")
#library("pROC")
library('ggplot2')
library('lattice')
library('caret')
library('kernlab')

m2<- 20
y11f<- rep("uno",m2)
y22f<- rep("zero",m2)
yff<- as.factor(c(y11f,y22f))

#-----------------------------------------------------------------------------------------------------
x_SYNT_stat_b<- matrix(unlist(synt_vec_melt_b), byrow=TRUE, nrow=20 )
x_SYNT_stat_c<- matrix(unlist(synt_vec_melt_c), byrow=TRUE, nrow=20 )
x_SYNT_stat_d<- matrix(unlist(synt_vec_melt_d), byrow=TRUE, nrow=20 )

x_synt_stat_coeff_new<- cbind(x_SYNT_stat_b,x_SYNT_stat_c,x_SYNT_stat_d)

x_speak1_stat_b<- matrix(unlist(speak1_vec_melt_b), byrow=TRUE, nrow=20 )
x_speak1_stat_c<- matrix(unlist(speak1_vec_melt_c), byrow=TRUE, nrow=20 )
x_speak1_stat_d<- matrix(unlist(speak1_vec_melt_d), byrow=TRUE, nrow=20 )

x_speak1_stat_coeff_new <- cbind(x_speak1_stat_b,x_speak1_stat_c,x_speak1_stat_d)

x_syntspeak1_stat_coeff_new<- rbind(x_synt_stat_coeff_new, x_speak1_stat_coeff_new)

colnames(x_syntspeak1_stat_coeff_new) <- paste("x_syntspeak1_stat_coeff", 1:ncol(x_syntspeak1_stat_coeff_new), sep="")

x_IMF1 <- cbind(x_syntspeak1_stat_coeff_new[,1:70],x_syntspeak1_stat_coeff_new[,351:420],x_syntspeak1_stat_coeff_new[,701:770] )
x_IMF2<- cbind(x_syntspeak1_stat_coeff_new[,71:140],x_syntspeak1_stat_coeff_new[,421:490],x_syntspeak1_stat_coeff_new[,771:840] )
x_IMF3<- cbind(x_syntspeak1_stat_coeff_new[,141:210],x_syntspeak1_stat_coeff_new[,491:560],x_syntspeak1_stat_coeff_new[,841:910] )
x_IMF4<- cbind(x_syntspeak1_stat_coeff_new[,211:280],x_syntspeak1_stat_coeff_new[,561:630],x_syntspeak1_stat_coeff_new[,911:980] )
x_IMF5<- cbind(x_syntspeak1_stat_coeff_new[,281:350],x_syntspeak1_stat_coeff_new[,631:700],x_syntspeak1_stat_coeff_new[,981:1050] )


#stat - FIRST stat

ypred_syntspeak1_stat1<- predict(SVM_syntspeak1_stat1_tan,newdata = x_IMF1)

CM_syntspeak1_stat1_new_tan<- confusionMatrix(ypred_syntspeak1_stat1, yff)
CM_syntspeak1_stat1_new_tan

#stat - SECOND stat

ypred_syntspeak1_stat2<- predict(SVM_syntspeak1_stat2_tan,newdata = x_IMF2)

CM_syntspeak1_stat2_new_tan<- confusionMatrix(ypred_syntspeak1_stat2, yff)
CM_syntspeak1_stat2_new_tan


#stat - THIRD stat

ypred_syntspeak1_stat3<- predict(SVM_syntspeak1_stat3_tan,newdata = x_IMF3)

CM_syntspeak1_stat3_new_tan<- confusionMatrix(ypred_syntspeak1_stat3, yff)
CM_syntspeak1_stat3_new_tan

#stat - FOURTH stat

ypred_syntspeak1_stat4<- predict(SVM_syntspeak1_stat4_tan,newdata = x_IMF4)

CM_syntspeak1_stat4_new_tan<- confusionMatrix(ypred_syntspeak1_stat4, yff)
CM_syntspeak1_stat4_new_tan

#stat - FstatTH stat

ypred_syntspeak1_stat5<- predict(SVM_syntspeak1_stat5_tan,newdata = x_IMF5)

CM_syntspeak1_stat5_new_tan<- confusionMatrix(ypred_syntspeak1_stat5, yff)
CM_syntspeak1_stat5_new_tan








