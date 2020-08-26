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

SVM_syntspeak1_MLFC_IMF2_lp<- vector(mode="list", 12)



for (i in 1:12){
  
  SVM_syntspeak1_MLFC_IMF2_lp[[i]]<- train(y = yf, x = x_syntspeak1_MLFC_IMF2[,((134*i)-133):(134*i)],  trControl = train_control, method = lpSVM,
                                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
  
}

names(SVM_syntspeak1_MLFC_IMF2_lp) <- paste("SVM_syntspeak1_MLFC_IMF2_lp_coeff", 1:12, sep = "") 

#----------------------------------------------------------------

CM_syntspeak1_MLFC_IMF2_coeff1_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_lp[[1]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[1]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[1]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF2_lp[[1]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[1]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[1]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff1_lp


CM_syntspeak1_MLFC_IMF2_coeff2_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_lp[[2]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[2]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[2]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF2_lp[[2]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[2]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[2]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff2_lp

CM_syntspeak1_MLFC_IMF2_coeff3_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_lp[[3]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[3]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[3]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF2_lp[[3]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[3]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[3]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff3_lp


CM_syntspeak1_MLFC_IMF2_coeff4_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_lp[[4]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[4]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[4]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF2_lp[[4]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[4]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[4]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff4_lp


CM_syntspeak1_MLFC_IMF2_coeff5_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_lp[[5]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[5]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[5]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF2_lp[[5]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[5]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[5]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff5_lp


CM_syntspeak1_MLFC_IMF2_coeff6_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_lp[[6]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[6]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[6]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF2_lp[[6]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[6]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[6]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff6_lp


CM_syntspeak1_MLFC_IMF2_coeff7_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_lp[[7]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[7]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[7]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF2_lp[[7]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[7]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[7]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff7_lp


CM_syntspeak1_MLFC_IMF2_coeff8_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_lp[[8]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[8]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[8]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF2_lp[[8]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[8]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[8]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff8_lp


CM_syntspeak1_MLFC_IMF2_coeff9_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_lp[[9]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[9]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[9]]$bestTune[2]) )$pred[1:100],
                                                    subset(SVM_syntspeak1_MLFC_IMF2_lp[[9]]$pred, 
                                                           SVM_syntspeak1_MLFC_IMF2_lp[[9]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[9]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff9_lp


CM_syntspeak1_MLFC_IMF2_coeff10_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_lp[[10]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF2_lp[[10]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[10]]$bestTune[2]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF2_lp[[10]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF2_lp[[10]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[10]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff10_lp


CM_syntspeak1_MLFC_IMF2_coeff11_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_lp[[11]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF2_lp[[11]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[11]]$bestTune[2]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF2_lp[[11]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF2_lp[[11]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[11]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff11_lp


CM_syntspeak1_MLFC_IMF2_coeff12_lp<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_lp[[12]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF2_lp[[12]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[12]]$bestTune[2]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF2_lp[[12]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF2_lp[[12]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_lp[[12]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff12_lp










