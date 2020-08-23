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

SVM_syntspeak1_MLFC_IMF5_lp<- vector(mode="list", 12)



for (i in 1:12){
  
  SVM_syntspeak1_MLFC_IMF5_lp[[i]]<- train(y = yf, x = x_syntspeak1_MLFC_IMF5[,((134*i)-133):(134*i)],  trControl = train_control, method = lpSVM,
                                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
  
}

names(SVM_syntspeak1_MLFC_IMF5_lp) <- paste("SVM_syntspeak1_MLFC_IMF5_lp_coeff", 1:12, sep = "") 

#----------------------------------------------------------------

CM_syntspeak1_MLFC_IMF5_coeff1_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_lp[[1]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[1]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[1]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF5_lp[[1]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[1]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[1]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff1_lp


CM_syntspeak1_MLFC_IMF5_coeff2_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_lp[[2]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[2]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[2]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF5_lp[[2]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[2]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[2]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff2_lp

CM_syntspeak1_MLFC_IMF5_coeff3_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_lp[[3]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[3]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[3]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF5_lp[[3]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[3]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[3]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff3_lp


CM_syntspeak1_MLFC_IMF5_coeff4_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_lp[[4]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[4]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[4]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF5_lp[[4]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[4]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[4]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff4_lp


CM_syntspeak1_MLFC_IMF5_coeff5_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_lp[[5]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[5]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[5]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF5_lp[[5]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[5]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[5]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff5_lp


CM_syntspeak1_MLFC_IMF5_coeff6_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_lp[[6]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[6]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[6]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF5_lp[[6]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[6]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[6]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff6_lp


CM_syntspeak1_MLFC_IMF5_coeff7_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_lp[[7]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[7]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[7]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF5_lp[[7]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[7]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[7]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff7_lp


CM_syntspeak1_MLFC_IMF5_coeff8_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_lp[[8]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[8]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[8]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF5_lp[[8]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[8]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[8]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff8_lp


CM_syntspeak1_MLFC_IMF5_coeff9_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_lp[[9]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[9]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[9]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF5_lp[[9]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF5_lp[[9]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[9]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff9_lp


CM_syntspeak1_MLFC_IMF5_coeff10_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_lp[[10]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF5_lp[[10]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[10]]$bestTune[2]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF5_lp[[10]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF5_lp[[10]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[10]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff10_lp


CM_syntspeak1_MLFC_IMF5_coeff11_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_lp[[11]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF5_lp[[11]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[11]]$bestTune[2]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF5_lp[[11]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF5_lp[[11]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[11]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff11_lp


CM_syntspeak1_MLFC_IMF5_coeff12_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_lp[[12]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF5_lp[[12]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[12]]$bestTune[2]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF5_lp[[12]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF5_lp[[12]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_lp[[12]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_coeff12_lp










