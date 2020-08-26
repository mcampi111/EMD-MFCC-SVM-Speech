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
x_synt_stat_new<- matrix(unlist(synt_vec), byrow=TRUE, nrow=20 )

x_speak1_stat_new<- matrix(unlist(speak1_vec), byrow=TRUE, nrow=20 )


x_syntspeak1_stat_new<- rbind(x_synt_stat_new, x_speak1_stat_new)

colnames(x_syntspeak1_stat_new) <- paste("x_syntspeak1_stat", 1:ncol(x_syntspeak1_stat_new), sep="")


#stat - RBF

ypred_syntspeak1_stat<- predict(SVM_syntspeak1_stat,newdata = x_syntspeak1_stat_new)

CM_syntspeak1_stat_new<- confusionMatrix(ypred_syntspeak1_stat, yff)
CM_syntspeak1_stat_new

#stat - LP

ypred_syntspeak1_stat_lp<- predict(SVM_syntspeak1_stat_lp,newdata = x_syntspeak1_stat_new)

CM_syntspeak1_stat_lp_new<- confusionMatrix(ypred_syntspeak1_stat_lp, yff)
CM_syntspeak1_stat_lp_new


#stat - POLY

ypred_syntspeak1_stat_poly<- predict(SVM_syntspeak1_stat_poly,newdata = x_syntspeak1_stat_new)

CM_syntspeak1_stat_poly_new<- confusionMatrix(ypred_syntspeak1_stat_poly, yff)
CM_syntspeak1_stat_poly_new



#stat - TAN

ypred_syntspeak1_stat_tan<- predict(SVM_syntspeak1_stat_tan,newdata = x_syntspeak1_stat_new)

CM_syntspeak1_stat_tan_new<- confusionMatrix(ypred_syntspeak1_stat_tan, yff)
CM_syntspeak1_stat_tan_new


#stat - BESS

ypred_syntspeak1_stat_bess<- predict(SVM_syntspeak1_stat_bess,newdata = x_syntspeak1_stat_new)

CM_syntspeak1_stat_bess_new<- confusionMatrix(ypred_syntspeak1_stat_bess, yff)
CM_syntspeak1_stat_bess_new


#stat - VANN


ypred_syntspeak1_stat_van<- predict(SVM_syntspeak1_stat_van,newdata = x_syntspeak1_stat_new)

CM_syntspeak1_stat_van_new<- confusionMatrix(ypred_syntspeak1_stat_van, yff)
CM_syntspeak1_stat_van_new

