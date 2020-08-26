library('ggplot2')
library('lattice')
library('caret')
library('kernlab')

m<- 20
y1f<- rep("uno",m)
y2f<- rep("zero",m)
yff<- as.factor(c(y1f,y2f))

#SVM IMF1 SPEAKER1 vs SYNTHETIC
#------------------------------------------------------------------

x_synt_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_synt_new), byrow=TRUE, nrow=20 )
 
x_speak1_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_speak1_new), byrow=TRUE, nrow=20 )
 
x_syntspeak1_MLFC_IMF1_new<- rbind(x_synt_MLFC_IMF1, x_speak1_MLFC_IMF1)
 
colnames(x_syntspeak1_MLFC_IMF1_new) <- paste("x_syntspeak1_MLFC_IMF1", 1:ncol(x_syntspeak1_MLFC_IMF1_new), sep="")


#IMF1------------------------------------------------
#----------------------------------------------------
#COEFF 1 --------------------------------------------


ypred_syntspeak1_MLFC_IMF1_coeff1<- predict(SVM_syntspeak1_MLFC_IMF1_poly[[1]],newdata = x_syntspeak1_MLFC_IMF1_new[,1:134])

CM_syntspeak1_MLFC_IMF1_coeff1_new<- confusionMatrix(ypred_syntspeak1_MLFC_IMF1_coeff1, yff)
CM_syntspeak1_MLFC_IMF1_coeff1_new


#COEFF 2 --------------------------------------------


ypred_syntspeak1_MLFC_IMF1_coeff2<- predict(SVM_syntspeak1_MLFC_IMF1_poly[[2]],newdata = x_syntspeak1_MLFC_IMF1_new[,135:268])

CM_syntspeak1_MLFC_IMF1_coeff2_new<- confusionMatrix(ypred_syntspeak1_MLFC_IMF1_coeff2, yff)
CM_syntspeak1_MLFC_IMF1_coeff2_new


#COEFF 3 --------------------------------------------


ypred_syntspeak1_MLFC_IMF1_coeff3<- predict(SVM_syntspeak1_MLFC_IMF1_poly[[3]],newdata = x_syntspeak1_MLFC_IMF1_new[,269:402])

CM_syntspeak1_MLFC_IMF1_coeff3_new<- confusionMatrix(ypred_syntspeak1_MLFC_IMF1_coeff3, yff)
CM_syntspeak1_MLFC_IMF1_coeff3_new


#COEFF 4 --------------------------------------------


ypred_syntspeak1_MLFC_IMF1_coeff4<- predict(SVM_syntspeak1_MLFC_IMF1_poly[[4]],newdata = x_syntspeak1_MLFC_IMF1_new[,403:536])

CM_syntspeak1_MLFC_IMF1_coeff4_new<- confusionMatrix(ypred_syntspeak1_MLFC_IMF1_coeff4, yff)
CM_syntspeak1_MLFC_IMF1_coeff4_new

#COEFF 5 --------------------------------------------


ypred_syntspeak1_MLFC_IMF1_coeff5<- predict(SVM_syntspeak1_MLFC_IMF1_poly[[5]],newdata = x_syntspeak1_MLFC_IMF1_new[,537:670])

CM_syntspeak1_MLFC_IMF1_coeff5_new<- confusionMatrix(ypred_syntspeak1_MLFC_IMF1_coeff5, yff)
CM_syntspeak1_MLFC_IMF1_coeff5_new


#COEFF 6 --------------------------------------------


ypred_syntspeak1_MLFC_IMF1_coeff6<- predict(SVM_syntspeak1_MLFC_IMF1_poly[[6]],newdata = x_syntspeak1_MLFC_IMF1_new[,671:804])

CM_syntspeak1_MLFC_IMF1_coeff6_new<- confusionMatrix(ypred_syntspeak1_MLFC_IMF1_coeff6, yff)
CM_syntspeak1_MLFC_IMF1_coeff6_new


#COEFF 7 --------------------------------------------

ypred_syntspeak1_MLFC_IMF1_coeff7<- predict(SVM_syntspeak1_MLFC_IMF1_poly[[7]],newdata = x_syntspeak1_MLFC_IMF1_new[,805:938])

CM_syntspeak1_MLFC_IMF1_coeff7_new<- confusionMatrix(ypred_syntspeak1_MLFC_IMF1_coeff7, yff)
CM_syntspeak1_MLFC_IMF1_coeff7_new

#COEFF 8 --------------------------------------------

ypred_syntspeak1_MLFC_IMF1_coeff8<- predict(SVM_syntspeak1_MLFC_IMF1_poly[[8]],newdata = x_syntspeak1_MLFC_IMF1_new[,939:1072])

CM_syntspeak1_MLFC_IMF1_coeff8_new<- confusionMatrix(ypred_syntspeak1_MLFC_IMF1_coeff8, yff)
CM_syntspeak1_MLFC_IMF1_coeff8_new


#COEFF 9 --------------------------------------------

ypred_syntspeak1_MLFC_IMF1_coeff9<- predict(SVM_syntspeak1_MLFC_IMF1_poly[[9]],newdata = x_syntspeak1_MLFC_IMF1_new[,1073:1206])

CM_syntspeak1_MLFC_IMF1_coeff9_new<- confusionMatrix(ypred_syntspeak1_MLFC_IMF1_coeff9, yff)
CM_syntspeak1_MLFC_IMF1_coeff9_new

#COEFF 10 --------------------------------------------

ypred_syntspeak1_MLFC_IMF1_coeff10<- predict(SVM_syntspeak1_MLFC_IMF1_poly[[10]],newdata = x_syntspeak1_MLFC_IMF1_new[,1207:1340])

CM_syntspeak1_MLFC_IMF1_coeff10_new<- confusionMatrix(ypred_syntspeak1_MLFC_IMF1_coeff10, yff)
CM_syntspeak1_MLFC_IMF1_coeff10_new

#COEFF 11 --------------------------------------------

ypred_syntspeak1_MLFC_IMF1_coeff11<- predict(SVM_syntspeak1_MLFC_IMF1_poly[[11]],newdata = x_syntspeak1_MLFC_IMF1_new[,1341:1474])

CM_syntspeak1_MLFC_IMF1_coeff11_new<- confusionMatrix(ypred_syntspeak1_MLFC_IMF1_coeff11, yff)
CM_syntspeak1_MLFC_IMF1_coeff11_new

#COEFF 12 --------------------------------------------

ypred_syntspeak1_MLFC_IMF1_coeff12<- predict(SVM_syntspeak1_MLFC_IMF1_poly[[12]],newdata = x_syntspeak1_MLFC_IMF1_new[,1475:1608])

CM_syntspeak1_MLFC_IMF1_coeff12_new<- confusionMatrix(ypred_syntspeak1_MLFC_IMF1_coeff12, yff)
CM_syntspeak1_MLFC_IMF1_coeff12_new

#----------------------------------------------------------------

save(CM_syntspeak1_MLFC_IMF1_coeff1_new,CM_syntspeak1_MLFC_IMF1_coeff10_new,CM_syntspeak1_MLFC_IMF1_coeff11_new,
     CM_syntspeak1_MLFC_IMF1_coeff12_new, CM_syntspeak1_MLFC_IMF1_coeff2_new,CM_syntspeak1_MLFC_IMF1_coeff3_new,
     CM_syntspeak1_MLFC_IMF1_coeff4_new,CM_syntspeak1_MLFC_IMF1_coeff5_new,CM_syntspeak1_MLFC_IMF1_coeff6_new,
     CM_syntspeak1_MLFC_IMF1_coeff7_new,CM_syntspeak1_MLFC_IMF1_coeff8_new,CM_syntspeak1_MLFC_IMF1_coeff9_new,
     file = "C:\\Users\\Marta\\Desktop\\Speech_OUT\\SVM_Speaker1_out\\POLY\\CM_speak1_CEP_IMF1_poly.RData")

