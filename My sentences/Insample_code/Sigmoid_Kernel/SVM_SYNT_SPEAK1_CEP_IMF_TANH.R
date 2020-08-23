#TANH KERNEL
library("seewave")
library("tuneR")
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

#SVM SYNTHETIC vs SPEAKER 1  ---------------------------------------------------------------------------------------
#HIGH COEFFICIENTS
#IMF1------------------
x_synt_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC_IMF1<- rbind(x_synt_MLFC_IMF1, x_speak1_MLFC_IMF1)


colnames(x_syntspeak1_MLFC_IMF1) <- paste("x_syntspeak1_MLFC_IMF1", 1:ncol(x_syntspeak1_MLFC_IMF1), sep="")

#MLFC - all data - HIGH COEFF
SVM_syntspeak1_MLFC_IMF1_HIGH_TAN<- train(y = yf, x = x_syntspeak1_MLFC_IMF1[,1:804], trControl = train_control, method = tanhSVM, 
                                         preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_MLFC_IMF1_HIGH_TAN$results
SVM_syntspeak1_MLFC_IMF1_HIGH_TAN$finalModel

CM_syntspeak1_MLFC_IMF1_HIGH_TAN<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_HIGH_TAN$pred, 
                                                         SVM_syntspeak1_MLFC_IMF1_HIGH_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_HIGH_TAN$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF1_HIGH_TAN$pred, 
                                                         SVM_syntspeak1_MLFC_IMF1_HIGH_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_HIGH_TAN$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_HIGH_TAN


#IMF2--------------------------
x_synt_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC_IMF2<- rbind(x_synt_MLFC_IMF2, x_speak1_MLFC_IMF2)


colnames(x_syntspeak1_MLFC_IMF2) <- paste("x_syntspeak1_MLFC_IMF2", 1:ncol(x_syntspeak1_MLFC_IMF2), sep="")

#MLFC - all data - HIGH COEFF
SVM_syntspeak1_MLFC_IMF2_HIGH_TAN<- train(y = yf, x = x_syntspeak1_MLFC_IMF2[,1:804], trControl = train_control, method = tanhSVM, 
                                         preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_MLFC_IMF2_HIGH_TAN$results
SVM_syntspeak1_MLFC_IMF2_HIGH_TAN$finalModel

CM_syntspeak1_MLFC_IMF2_HIGH_TAN<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_HIGH_TAN$pred, 
                                                         SVM_syntspeak1_MLFC_IMF2_HIGH_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_HIGH_TAN$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF2_HIGH_TAN$pred, 
                                                         SVM_syntspeak1_MLFC_IMF2_HIGH_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_HIGH_TAN$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_HIGH_TAN


#IMF3-------------------------------------------
x_synt_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC_IMF3<- rbind(x_synt_MLFC_IMF3, x_speak1_MLFC_IMF3)


colnames(x_syntspeak1_MLFC_IMF3) <- paste("x_syntspeak1_MLFC_IMF3", 1:ncol(x_syntspeak1_MLFC_IMF3), sep="")

#MLFC - all data - HIGH COEFF
SVM_syntspeak1_MLFC_IMF3_HIGH_TAN<- train(y = yf, x = x_syntspeak1_MLFC_IMF3[,1:804], trControl = train_control, method = tanhSVM, 
                                         preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_MLFC_IMF3_HIGH_TAN$results
SVM_syntspeak1_MLFC_IMF3_HIGH_TAN$finalModel

CM_syntspeak1_MLFC_IMF3_HIGH_TAN<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_HIGH_TAN$pred, 
                                                         SVM_syntspeak1_MLFC_IMF3_HIGH_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_HIGH_TAN$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF3_HIGH_TAN$pred, 
                                                         SVM_syntspeak1_MLFC_IMF3_HIGH_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_HIGH_TAN$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_HIGH_TAN



#IMF4-----------------------------------------------------------
x_synt_MLFC_IMF4<- matrix(unlist(mlfc_IMF4_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF4<- matrix(unlist(mlfc_IMF4_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC_IMF4<- rbind(x_synt_MLFC_IMF4, x_speak1_MLFC_IMF4)


colnames(x_syntspeak1_MLFC_IMF4) <- paste("x_syntspeak1_MLFC_IMF4", 1:ncol(x_syntspeak1_MLFC_IMF4), sep="")

#MLFC - all data - HIGH COEFF
SVM_syntspeak1_MLFC_IMF4_HIGH_TAN<- train(y = yf, x = x_syntspeak1_MLFC_IMF4[,1:804], trControl = train_control, method = tanhSVM, 
                                         preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_MLFC_IMF4_HIGH_TAN$results
SVM_syntspeak1_MLFC_IMF4_HIGH_TAN$finalModel

CM_syntspeak1_MLFC_IMF4_HIGH_TAN<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF4_HIGH_TAN$pred, 
                                                         SVM_syntspeak1_MLFC_IMF4_HIGH_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF4_HIGH_TAN$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF4_HIGH_TAN$pred, 
                                                         SVM_syntspeak1_MLFC_IMF4_HIGH_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF4_HIGH_TAN$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF4_HIGH_TAN



#IMF5------------------
x_synt_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC_IMF5<- rbind(x_synt_MLFC_IMF5, x_speak1_MLFC_IMF5)


colnames(x_syntspeak1_MLFC_IMF5) <- paste("x_syntspeak1_MLFC_IMF5", 1:ncol(x_syntspeak1_MLFC_IMF5), sep="")

#MLFC - all data - HIGH COEFF
SVM_syntspeak1_MLFC_IMF5_HIGH_TAN<- train(y = yf, x = x_syntspeak1_MLFC_IMF5[,1:804], trControl = train_control, method = tanhSVM, 
                                         preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_MLFC_IMF5_HIGH_TAN$results
SVM_syntspeak1_MLFC_IMF5_HIGH_TAN$finalModel

CM_syntspeak1_MLFC_IMF5_HIGH_TAN<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_HIGH_TAN$pred, 
                                                         SVM_syntspeak1_MLFC_IMF5_HIGH_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_HIGH_TAN$bestTune[2]) )$pred[1:100],
                                                  subset(SVM_syntspeak1_MLFC_IMF5_HIGH_TAN$pred, 
                                                         SVM_syntspeak1_MLFC_IMF5_HIGH_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_HIGH_TAN$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_HIGH_TAN


#SVM SYNTHETIC vs SPEAKER 1  ---------------------------------------------------------------------------------------
#LOW COEFFICIENTS
#IMF1------------------
x_synt_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC_IMF1<- rbind(x_synt_MLFC_IMF1, x_speak1_MLFC_IMF1)


colnames(x_syntspeak1_MLFC_IMF1) <- paste("x_syntspeak1_MLFC_IMF1", 1:ncol(x_syntspeak1_MLFC_IMF1), sep="")

#MLFC - all data - LOW COEFF
SVM_syntspeak1_MLFC_IMF1_LOW_TAN<- train(y = yf, x = x_syntspeak1_MLFC_IMF1[,1:804], trControl = train_control, method = tanhSVM, 
                                        preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_MLFC_IMF1_LOW_TAN$results
SVM_syntspeak1_MLFC_IMF1_LOW_TAN$finalModel

CM_syntspeak1_MLFC_IMF1_LOW_TAN<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_LOW_TAN$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1_LOW_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_LOW_TAN$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF1_LOW_TAN$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1_LOW_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_LOW_TAN$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_LOW_TAN


#IMF2--------------------------
x_synt_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC_IMF2<- rbind(x_synt_MLFC_IMF2, x_speak1_MLFC_IMF2)


colnames(x_syntspeak1_MLFC_IMF2) <- paste("x_syntspeak1_MLFC_IMF2", 1:ncol(x_syntspeak1_MLFC_IMF2), sep="")

#MLFC - all data - LOW COEFF
SVM_syntspeak1_MLFC_IMF2_LOW_TAN<- train(y = yf, x = x_syntspeak1_MLFC_IMF2[,1:804], trControl = train_control, method = tanhSVM, 
                                        preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_MLFC_IMF2_LOW_TAN$results
SVM_syntspeak1_MLFC_IMF2_LOW_TAN$finalModel

CM_syntspeak1_MLFC_IMF2_LOW_TAN<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_LOW_TAN$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2_LOW_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_LOW_TAN$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF2_LOW_TAN$pred, 
                                                        SVM_syntspeak1_MLFC_IMF2_LOW_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_LOW_TAN$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_LOW_TAN


#IMF3-------------------------------------------
x_synt_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC_IMF3<- rbind(x_synt_MLFC_IMF3, x_speak1_MLFC_IMF3)


colnames(x_syntspeak1_MLFC_IMF3) <- paste("x_syntspeak1_MLFC_IMF3", 1:ncol(x_syntspeak1_MLFC_IMF3), sep="")

#MLFC - all data - LOW COEFF
SVM_syntspeak1_MLFC_IMF3_LOW_TAN<- train(y = yf, x = x_syntspeak1_MLFC_IMF3[,1:804], trControl = train_control, method = tanhSVM, 
                                        preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_MLFC_IMF3_LOW_TAN$results
SVM_syntspeak1_MLFC_IMF3_LOW_TAN$finalModel

CM_syntspeak1_MLFC_IMF3_LOW_TAN<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_LOW_TAN$pred, 
                                                        SVM_syntspeak1_MLFC_IMF3_LOW_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_LOW_TAN$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF3_LOW_TAN$pred, 
                                                        SVM_syntspeak1_MLFC_IMF3_LOW_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_LOW_TAN$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_LOW_TAN



#IMF4-----------------------------------------------------------
x_synt_MLFC_IMF4<- matrix(unlist(mlfc_IMF4_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF4<- matrix(unlist(mlfc_IMF4_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC_IMF4<- rbind(x_synt_MLFC_IMF4, x_speak1_MLFC_IMF4)


colnames(x_syntspeak1_MLFC_IMF4) <- paste("x_syntspeak1_MLFC_IMF4", 1:ncol(x_syntspeak1_MLFC_IMF4), sep="")

#MLFC - all data - LOW COEFF
SVM_syntspeak1_MLFC_IMF4_LOW_TAN<- train(y = yf, x = x_syntspeak1_MLFC_IMF4[,1:804], trControl = train_control, method = tanhSVM, 
                                        preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_MLFC_IMF4_LOW_TAN$results
SVM_syntspeak1_MLFC_IMF4_LOW_TAN$finalModel

CM_syntspeak1_MLFC_IMF4_LOW_TAN<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF4_LOW_TAN$pred, 
                                                        SVM_syntspeak1_MLFC_IMF4_LOW_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF4_LOW_TAN$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF4_LOW_TAN$pred, 
                                                        SVM_syntspeak1_MLFC_IMF4_LOW_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF4_LOW_TAN$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF4_LOW_TAN



#IMF5------------------
x_synt_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC_IMF5<- rbind(x_synt_MLFC_IMF5, x_speak1_MLFC_IMF5)


colnames(x_syntspeak1_MLFC_IMF5) <- paste("x_syntspeak1_MLFC_IMF5", 1:ncol(x_syntspeak1_MLFC_IMF5), sep="")

#MLFC - all data - LOW COEFF
SVM_syntspeak1_MLFC_IMF5_LOW_TAN<- train(y = yf, x = x_syntspeak1_MLFC_IMF5[,1:804], trControl = train_control, method = tanhSVM, 
                                        preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_MLFC_IMF5_LOW_TAN$results
SVM_syntspeak1_MLFC_IMF5_LOW_TAN$finalModel

CM_syntspeak1_MLFC_IMF5_LOW_TAN<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_LOW_TAN$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5_LOW_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_LOW_TAN$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF5_LOW_TAN$pred, 
                                                        SVM_syntspeak1_MLFC_IMF5_LOW_TAN$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF5_LOW_TAN$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF5_LOW_TAN






















