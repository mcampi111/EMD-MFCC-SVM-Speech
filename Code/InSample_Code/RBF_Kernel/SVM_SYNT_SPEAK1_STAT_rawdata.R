#SVM ARIMA - GARCH - T = 132278, m = 100   ---- FEATURE: STATs --- KERNEL: RBASIS   - SCALE : TRUE
#CARET PACKAGE FOR SVM
library('ggplot2')
library('lattice')
library('caret')
library('kernlab')

m = 100
y1f<- rep("uno",m)
y2f<- rep("zero",m)    
yf<- as.factor(c(y1f,y2f))

train_control<- trainControl(method="repeatedcv", number=2, classProbs=TRUE,  summaryFunction = twoClassSummary, 
                             savePredictions = TRUE, repeats = 1, search = 'grid')



#SVM SYNTHETIC vs SPEAKER 1 ----------------------------------------------------------------------------------------

x_SYNT_stat<- matrix(unlist(synt_vec), byrow=TRUE, nrow=100 )

x_SPEAK1_stat<- matrix(unlist(speak1_vec), byrow=TRUE, nrow=100 )


x_syntspeak1_stat<- rbind(x_SYNT_stat, x_SPEAK1_stat)

colnames(x_syntspeak1_stat) <- paste("x_syntspeak1_stat", 1:ncol(x_syntspeak1_stat), sep="")


#Stat - RBF
SVM_syntspeak1_stat<- train(y = yf, x = x_syntspeak1_stat, trControl = train_control, method = 'svmRadial', 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat$results
SVM_syntspeak1_stat$finalModel

CM_syntspeak1_stat<- confusionMatrix(subset(SVM_syntspeak1_stat$pred, 
                                           SVM_syntspeak1_stat$pred[,7] == as.double(SVM_syntspeak1_stat$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_stat$pred, 
                                           SVM_syntspeak1_stat$pred[,7] == as.double(SVM_syntspeak1_stat$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_stat


#Stat - Laplace
SVM_syntspeak1_stat_lp<- train(y = yf, x = x_syntspeak1_stat, trControl = train_control, method = lpSVM, 
                                preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9) 

SVM_syntspeak1_stat_lp$results
SVM_syntspeak1_stat_lp$finalModel

CM_syntspeak1_stat_lp<- confusionMatrix(subset(SVM_syntspeak1_stat_lp$pred, 
                                                SVM_syntspeak1_stat_lp$pred[,7] == as.double(SVM_syntspeak1_stat_lp$bestTune[1]) &
                                                  SVM_syntspeak1_stat_lp$pred[,7] == as.double(SVM_syntspeak1_stat_lp$bestTune[1]) )$pred[1:100],
                                         subset(SVM_syntspeak1_stat_lp$pred, 
                                                SVM_syntspeak1_stat_lp$pred[,6] == as.double(SVM_syntspeak1_stat_lp$bestTune[2]) &
                                                  SVM_syntspeak1_stat_lp$pred[,6] == as.double(SVM_syntspeak1_stat_lp$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_stat_lp


#Stat - Poly
SVM_syntspeak1_stat_poly<- train(y = yf, x = x_syntspeak1_stat, trControl = train_control, method = polySVM, 
                                  preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat_poly$results
SVM_syntspeak1_stat_poly$finalModel

CM_syntspeak1_stat_poly<- confusionMatrix( subset(SVM_syntspeak1_stat_poly$pred, 
                                                   SVM_syntspeak1_stat_poly$pred[,6] == as.double(SVM_syntspeak1_stat_poly$bestTune[2]) & 
                                                     SVM_syntspeak1_stat_poly$pred[,7] == as.double(SVM_syntspeak1_stat_poly$bestTune[3]) & 
                                                     SVM_syntspeak1_stat_poly$pred[,8] == as.double(SVM_syntspeak1_stat_poly$bestTune[4]) & 
                                                     SVM_syntspeak1_stat_poly$pred[,9] == as.double(SVM_syntspeak1_stat_poly$bestTune[1]) )$pred[1:100], 
                                            subset(SVM_syntspeak1_stat_poly$pred, 
                                                   SVM_syntspeak1_stat_poly$pred[,6] == as.double(SVM_syntspeak1_stat_poly$bestTune[2]) &
                                                     SVM_syntspeak1_stat_poly$pred[,7] == as.double(SVM_syntspeak1_stat_poly$bestTune[3]) & 
                                                     SVM_syntspeak1_stat_poly$pred[,8] == as.double(SVM_syntspeak1_stat_poly$bestTune[4]) & 
                                                     SVM_syntspeak1_stat_poly$pred[,9] == as.double(SVM_syntspeak1_stat_poly$bestTune[1]) )$obs[1:100]
)

CM_syntspeak1_stat_poly


#Stat - TAN
SVM_syntspeak1_stat_tan<- train(y = yf, x = x_syntspeak1_stat, trControl = train_control, method = tanhSVM, 
                                 preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat_tan$results
SVM_syntspeak1_stat_tan$finalModel

CM_syntspeak1_stat_tan<- confusionMatrix( subset(SVM_syntspeak1_stat_tan$pred, 
                                                  SVM_syntspeak1_stat_tan$pred[,6] == as.double(SVM_syntspeak1_stat_tan$bestTune[2]) & 
                                                    SVM_syntspeak1_stat_tan$pred[,7] == as.double(SVM_syntspeak1_stat_tan$bestTune[3]) & 
                                                    SVM_syntspeak1_stat_tan$pred[,8] == as.double(SVM_syntspeak1_stat_tan$bestTune[1]) )$pred[1:100], 
                                           subset(SVM_syntspeak1_stat_tan$pred, 
                                                  SVM_syntspeak1_stat_tan$pred[,6] == as.double(SVM_syntspeak1_stat_tan$bestTune[2]) &
                                                    SVM_syntspeak1_stat_tan$pred[,7] == as.double(SVM_syntspeak1_stat_tan$bestTune[3]) & 
                                                    SVM_syntspeak1_stat_tan$pred[,8] == as.double(SVM_syntspeak1_stat_tan$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_stat_tan


#Stat BESS
SVM_syntspeak1_stat_bess<- train(y = yf, x = x_syntspeak1_stat, trControl = train_control, method = bessSVM, 
                                  preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat_bess$results
SVM_syntspeak1_stat_bess$finalModel

CM_syntspeak1_stat_bess<- confusionMatrix(subset(SVM_syntspeak1_stat_bess$pred, 
                                                  SVM_syntspeak1_stat_bess$pred[,6] == as.double(SVM_syntspeak1_stat_bess$bestTune[2]) &
                                                    SVM_syntspeak1_stat_bess$pred[,7] == as.double(SVM_syntspeak1_stat_bess$bestTune[4]) &
                                                    SVM_syntspeak1_stat_bess$pred[,8] == as.double(SVM_syntspeak1_stat_bess$bestTune[3]) &
                                                    SVM_syntspeak1_stat_bess$pred[,9] == as.double(SVM_syntspeak1_stat_bess$bestTune[1]))$pred[1:100],
                                           subset(SVM_syntspeak1_stat_bess$pred, 
                                                  SVM_syntspeak1_stat_bess$pred[,6] == as.double(SVM_syntspeak1_stat_bess$bestTune[2]) &
                                                    SVM_syntspeak1_stat_bess$pred[,7] == as.double(SVM_syntspeak1_stat_bess$bestTune[4]) &
                                                    SVM_syntspeak1_stat_bess$pred[,8] == as.double(SVM_syntspeak1_stat_bess$bestTune[3]) &
                                                    SVM_syntspeak1_stat_bess$pred[,9] == as.double(SVM_syntspeak1_stat_bess$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_stat_bess




#Stat VANN
SVM_syntspeak1_stat_van<- train(y = yf, x = x_syntspeak1_stat, trControl = train_control, method = vanSVM, 
                                 preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat_van$results
SVM_syntspeak1_stat_van$finalModel

CM_syntspeak1_stat_van<- confusionMatrix(subset(SVM_syntspeak1_stat_van$pred, 
                                                 SVM_syntspeak1_stat_van$pred[,6] == as.double(SVM_syntspeak1_stat_van$bestTune[1]) )$pred[1:100],
                                          subset(SVM_syntspeak1_stat_van$pred, 
                                                 SVM_syntspeak1_stat_van$pred[,6] == as.double(SVM_syntspeak1_stat_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_stat_van









