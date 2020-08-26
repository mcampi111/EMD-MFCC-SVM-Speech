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

#SVM IMF2 SPEAKER1 vs SYNTHETIC
#------------------------------------------------------------------

x_synt_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC_IMF2<- rbind(x_synt_MLFC_IMF2, x_speak1_MLFC_IMF2)


colnames(x_syntspeak1_MLFC_IMF2) <- paste("x_syntspeak1_MLFC_IMF2", 1:ncol(x_syntspeak1_MLFC_IMF2), sep="")




#SVM IMF2 FOR EACH COEFFICIENT (12) --> 12 SVMs for IMF2
#----------------------------------------------------

SVM_syntspeak1_MLFC_IMF2<- vector(mode="list", 12)



for (i in 1:12){
  
  SVM_syntspeak1_MLFC_IMF2[[i]]<- train(y = yf, x = x_syntspeak1_MLFC_IMF2[,((134*i)-133):(134*i)],  trControl = train_control, method = 'svmRadial',
                                        preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
  
}

names(SVM_syntspeak1_MLFC_IMF2) <- paste("SVM_syntspeak1_MLFC_IMF2_coeff", 1:12, sep = "") 

#----------------------------------------------------------------

CM_syntspeak1_MLFC_IMF2_coeff1<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2[[1]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[1]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF2[[1]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[1]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff1


CM_syntspeak1_MLFC_IMF2_coeff2<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2[[2]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[2]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF2[[2]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[2]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff2

CM_syntspeak1_MLFC_IMF2_coeff3<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2[[3]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[3]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF2[[3]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[3]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff3


CM_syntspeak1_MLFC_IMF2_coeff4<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2[[4]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[4]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[4]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF2[[4]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[4]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[4]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff4


CM_syntspeak1_MLFC_IMF2_coeff5<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2[[5]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[5]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF2[[5]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[5]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff5


CM_syntspeak1_MLFC_IMF2_coeff6<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2[[6]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[6]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF2[[6]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[6]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff6


CM_syntspeak1_MLFC_IMF2_coeff7<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2[[7]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[7]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF2[[7]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[7]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff7


CM_syntspeak1_MLFC_IMF2_coeff8<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2[[8]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[8]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF2[[8]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[8]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff8


CM_syntspeak1_MLFC_IMF2_coeff9<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2[[9]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[9]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF2[[9]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[9]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff9


CM_syntspeak1_MLFC_IMF2_coeff10<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2[[10]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF2[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[10]]$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF2[[10]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF2[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[10]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff10


CM_syntspeak1_MLFC_IMF2_coeff11<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2[[11]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF2[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[11]]$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF2[[11]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF2[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[11]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff11


CM_syntspeak1_MLFC_IMF2_coeff12<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2[[12]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF2[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[12]]$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF2[[12]]$pred, 
                                                         SVM_syntspeak1_MLFC_IMF2[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2[[12]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff12










