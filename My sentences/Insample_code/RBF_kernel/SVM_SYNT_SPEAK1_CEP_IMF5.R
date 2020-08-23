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

#SVM IMF5 SPEAKER1 vs SYNTHETIC
#------------------------------------------------------------------

x_synt_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC_IMF5<- rbind(x_synt_MLFC_IMF5, x_speak1_MLFC_IMF5)


colnames(x_syntspeak1_MLFC_IMF5) <- paste("x_syntspeak1_MLFC_IMF5", 1:ncol(x_syntspeak1_MLFC_IMF5), sep="")




#SVM IMF5 FOR EACH COEFFICIENT (12) --> 12 SVMs for IMF5
#----------------------------------------------------

SVM_syntspeak1_MLFC_IMF5<- vector(mode="list", 12)



for (i in 1:12){
  
  SVM_syntspeak1_MLFC_IMF5[[i]]<- train(y = yf, x = x_syntspeak1_MLFC_IMF5[,((134*i)-133):(134*i)],  trControl = train_control, method = 'svmRadial',
                                        preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
  
}

names(SVM_syntspeak1_MLFC_IMF5) <- paste("SVM_syntspeak1_MLFC_IMF5_coeff", 1:12, sep = "") 

#----------------------------------------------------------------

CM_syntspeak1_MLFC_IMF5_coeff1<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5[[1]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[1]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF5[[1]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[1]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff1


CM_syntspeak1_MLFC_IMF5_coeff2<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5[[2]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[2]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF5[[2]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[2]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff2

CM_syntspeak1_MLFC_IMF5_coeff3<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5[[3]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[3]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF5[[3]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[3]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff3


CM_syntspeak1_MLFC_IMF5_coeff4<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5[[4]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[4]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[4]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF5[[4]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[4]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[4]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff4


CM_syntspeak1_MLFC_IMF5_coeff5<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5[[5]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[5]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF5[[5]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[5]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff5


CM_syntspeak1_MLFC_IMF5_coeff6<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5[[6]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[6]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF5[[6]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[6]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff6


CM_syntspeak1_MLFC_IMF5_coeff7<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5[[7]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[7]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF5[[7]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[7]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff7


CM_syntspeak1_MLFC_IMF5_coeff8<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5[[8]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[8]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF5[[8]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[8]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff8


CM_syntspeak1_MLFC_IMF5_coeff9<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5[[9]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[9]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF5[[9]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[9]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff9


CM_syntspeak1_MLFC_IMF5_coeff10<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5[[10]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF5[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[10]]$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF5[[10]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF5[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[10]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff10


CM_syntspeak1_MLFC_IMF5_coeff11<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5[[11]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF5[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[11]]$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF5[[11]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF5[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[11]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff11


CM_syntspeak1_MLFC_IMF5_coeff12<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5[[12]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF5[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[12]]$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF5[[12]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF5[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5[[12]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff12










