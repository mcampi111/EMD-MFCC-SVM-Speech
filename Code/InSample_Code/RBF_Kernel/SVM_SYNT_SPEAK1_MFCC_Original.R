library('ggplot2')
library('lattice')
library('caret')
library('kernlab')

m<- 100
y1f<- rep("uno",m)
y2f<- rep("zero",m)
yf<- as.factor(c(y1f,y2f))

train_control<- trainControl(method="repeatedcv", number=2, classProbs=TRUE,  summaryFunction = twoClassSummary,
                             savePredictions = TRUE, repeats = 1, search = 'grid')


#SVM SPEAKER1 vs SYNTHETIC
#------------------------------------------------------------------

x_synt_MLFC<- matrix(unlist(feat_synt_melffc), byrow=TRUE, nrow=100 )
 
x_speak1_MLFC<- matrix(unlist(feat_speak1_melffc), byrow=TRUE, nrow=100 )
 
x_syntspeak1_MLFC<- rbind(x_synt_MLFC, x_speak1_MLFC)
 
colnames(x_syntspeak1_MLFC) <- paste("x_syntspeak1_MLFC", 1:ncol(x_syntspeak1_MLFC), sep="")

#SVM FOR EACH COEFFICIENT (12) --> 12 SVMs 
#----------------------------------------------------

SVM_syntspeak1_MLFC<- vector(mode="list", 12)



for (i in 1:12){
                                                                  #(120*i)-119):(120*i)
  SVM_syntspeak1_MLFC[[i]]<- train(y = yf, x = x_syntspeak1_MLFC[,((90*i)-89):(90*i)],  trControl = train_control, 
                                   method = 'svmRadial',preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
  
}

names(SVM_syntspeak1_MLFC) <- paste("SVM_syntspeak1_MLFC_coeff", 1:12, sep = "") 

#----------------------------------------------------------------

CM_syntspeak1_MLFC_coeff1<- confusionMatrix(subset(SVM_syntspeak1_MLFC[[1]]$pred, 
                                                        SVM_syntspeak1_MLFC[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[1]]$bestTune[2]) )$pred[1:100],
                                               subset(SVM_syntspeak1_MLFC[[1]]$pred, 
                                                      SVM_syntspeak1_MLFC[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[1]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_coeff1


CM_syntspeak1_MLFC_coeff2<- confusionMatrix(subset(SVM_syntspeak1_MLFC[[2]]$pred, 
                                                        SVM_syntspeak1_MLFC[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[2]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC[[2]]$pred, 
                                                        SVM_syntspeak1_MLFC[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[2]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_coeff2

CM_syntspeak1_MLFC_coeff3<- confusionMatrix(subset(SVM_syntspeak1_MLFC[[3]]$pred, 
                                                        SVM_syntspeak1_MLFC[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[3]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC[[3]]$pred, 
                                                        SVM_syntspeak1_MLFC[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[3]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_coeff3


CM_syntspeak1_MLFC_coeff4<- confusionMatrix(subset(SVM_syntspeak1_MLFC[[4]]$pred, 
                                                        SVM_syntspeak1_MLFC[[4]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[4]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC[[4]]$pred, 
                                                        SVM_syntspeak1_MLFC[[4]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[4]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_coeff4


CM_syntspeak1_MLFC_coeff5<- confusionMatrix(subset(SVM_syntspeak1_MLFC[[5]]$pred, 
                                                        SVM_syntspeak1_MLFC[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[5]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC[[5]]$pred, 
                                                        SVM_syntspeak1_MLFC[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[5]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_coeff5


CM_syntspeak1_MLFC_coeff6<- confusionMatrix(subset(SVM_syntspeak1_MLFC[[6]]$pred, 
                                                        SVM_syntspeak1_MLFC[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[6]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC[[6]]$pred, 
                                                        SVM_syntspeak1_MLFC[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[6]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_coeff6


CM_syntspeak1_MLFC_coeff7<- confusionMatrix(subset(SVM_syntspeak1_MLFC[[7]]$pred, 
                                                        SVM_syntspeak1_MLFC[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[7]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC[[7]]$pred, 
                                                        SVM_syntspeak1_MLFC[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[7]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_coeff7


CM_syntspeak1_MLFC_coeff8<- confusionMatrix(subset(SVM_syntspeak1_MLFC[[8]]$pred, 
                                                        SVM_syntspeak1_MLFC[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[8]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC[[8]]$pred, 
                                                        SVM_syntspeak1_MLFC[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[8]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_coeff8


CM_syntspeak1_MLFC_coeff9<- confusionMatrix(subset(SVM_syntspeak1_MLFC[[9]]$pred, 
                                                        SVM_syntspeak1_MLFC[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[9]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC[[9]]$pred, 
                                                        SVM_syntspeak1_MLFC[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[9]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_coeff9


CM_syntspeak1_MLFC_coeff10<- confusionMatrix(subset(SVM_syntspeak1_MLFC[[10]]$pred, 
                                                        SVM_syntspeak1_MLFC[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[10]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC[[10]]$pred, 
                                                        SVM_syntspeak1_MLFC[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[10]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_coeff10


CM_syntspeak1_MLFC_coeff11<- confusionMatrix(subset(SVM_syntspeak1_MLFC[[11]]$pred, 
                                                         SVM_syntspeak1_MLFC[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[11]]$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC[[11]]$pred, 
                                                         SVM_syntspeak1_MLFC[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[11]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_coeff11


CM_syntspeak1_MLFC_coeff12<- confusionMatrix(subset(SVM_syntspeak1_MLFC[[12]]$pred, 
                                                         SVM_syntspeak1_MLFC[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[12]]$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC[[12]]$pred, 
                                                         SVM_syntspeak1_MLFC[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC[[12]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_coeff12



save(SVM_syntspeak1_MLFC,  CM_syntspeak1_MLFC_coeff1,CM_syntspeak1_MLFC_coeff10,CM_syntspeak1_MLFC_coeff11,CM_syntspeak1_MLFC_coeff11,
     CM_syntspeak1_MLFC_coeff12, CM_syntspeak1_MLFC_coeff2,CM_syntspeak1_MLFC_coeff3, CM_syntspeak1_MLFC_coeff4,
     CM_syntspeak1_MLFC_coeff5, CM_syntspeak1_MLFC_coeff6,CM_syntspeak1_MLFC_coeff7,CM_syntspeak1_MLFC_coeff8,
     CM_syntspeak1_MLFC_coeff9, file = "C:\\Users\\Marta\\Desktop\\Results\\SVM_Speaker1_fm_synt_fm\\RBF\\SVM_CM_speak1_MFCC_on_window_RBF_0035.RData")






