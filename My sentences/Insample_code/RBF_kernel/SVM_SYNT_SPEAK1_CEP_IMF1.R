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


#SVM IMF1 SPEAKER1 vs SYNTHETIC
#------------------------------------------------------------------

x_synt_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_synt), byrow=TRUE, nrow=100 )
 
x_speak1_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_speak1), byrow=TRUE, nrow=100 )
 
x_syntspeak1_MLFC_IMF1<- rbind(x_synt_MLFC_IMF1, x_speak1_MLFC_IMF1)
 
colnames(x_syntspeak1_MLFC_IMF1) <- paste("x_syntspeak1_MLFC_IMF1", 1:ncol(x_syntspeak1_MLFC_IMF1), sep="")

#SVM IMF1 FOR EACH COEFFICIENT (12) --> 12 SVMs for IMF1
#----------------------------------------------------

SVM_syntspeak1_MLFC_IMF1<- vector(mode="list", 12)



for (i in 1:12){

  SVM_syntspeak1_MLFC_IMF1[[i]]<- train(y = yf, x = x_syntspeak1_MLFC_IMF1[,((134*i)-133):(134*i)],  trControl = train_control, method = 'svmRadial',
              preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
  
}

names(SVM_syntspeak1_MLFC_IMF1) <- paste("SVM_syntspeak1_MLFC_IMF1_coeff", 1:12, sep = "") 

#----------------------------------------------------------------

CM_syntspeak1_MLFC_IMF1_coeff1<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1[[1]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[1]]$bestTune[2]) )$pred[1:100],
                                               subset(SVM_syntspeak1_MLFC_IMF1[[1]]$pred, 
                                                      SVM_syntspeak1_MLFC_IMF1[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[1]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff1


CM_syntspeak1_MLFC_IMF1_coeff2<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1[[2]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[2]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF1[[2]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[2]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff2

CM_syntspeak1_MLFC_IMF1_coeff3<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1[[3]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[3]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF1[[3]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[3]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff3


CM_syntspeak1_MLFC_IMF1_coeff4<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1[[4]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[4]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[4]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF1[[4]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[4]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[4]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff4


CM_syntspeak1_MLFC_IMF1_coeff5<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1[[5]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[5]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF1[[5]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[5]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff5


CM_syntspeak1_MLFC_IMF1_coeff6<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1[[6]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[6]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF1[[6]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[6]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff6


CM_syntspeak1_MLFC_IMF1_coeff7<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1[[7]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[7]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF1[[7]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[7]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff7


CM_syntspeak1_MLFC_IMF1_coeff8<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1[[8]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[8]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF1[[8]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[8]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff8


CM_syntspeak1_MLFC_IMF1_coeff9<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1[[9]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[9]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF1[[9]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[9]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff9


CM_syntspeak1_MLFC_IMF1_coeff10<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1[[10]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[10]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF1[[10]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[10]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff10


CM_syntspeak1_MLFC_IMF1_coeff11<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1[[11]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF1[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[11]]$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF1[[11]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF1[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[11]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff11


CM_syntspeak1_MLFC_IMF1_coeff12<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1[[12]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF1[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[12]]$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF1[[12]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF1[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1[[12]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff12










