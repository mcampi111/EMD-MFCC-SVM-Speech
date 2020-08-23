#SVM ARIMA - GARCH - T = 132278, m = 100   ---- FEATURE: STATs --- KERNEL: RBASIS   - SCALE : TRUE
#CARET PACKAGE FOR SVM
library('ggplot2')
library('lattice')
library('caret')
library('kernlab')


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


#Stat - FIRST IMF
SVM_syntspeak1_stat1_poly<- train(y = yf, x = x_syntspeak1_stat[,1:70], trControl = train_control, method = polySVM, 
                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat1_poly$results
SVM_syntspeak1_stat1_poly$finalModel

CM_syntspeak1_stat1_poly<- confusionMatrix( subset(SVM_syntspeak1_stat1_poly$pred, 
                                                   SVM_syntspeak1_stat1_poly$pred[,6] == as.double(SVM_syntspeak1_stat1_poly$bestTune[2]) & 
                                                     SVM_syntspeak1_stat1_poly$pred[,7] == as.double(SVM_syntspeak1_stat1_poly$bestTune[3]) & 
                                                     SVM_syntspeak1_stat1_poly$pred[,8] == as.double(SVM_syntspeak1_stat1_poly$bestTune[4]) & 
                                                     SVM_syntspeak1_stat1_poly$pred[,9] == as.double(SVM_syntspeak1_stat1_poly$bestTune[1]) )$pred[1:100], 
                                            subset(SVM_syntspeak1_stat1_poly$pred, 
                                                   SVM_syntspeak1_stat1_poly$pred[,6] == as.double(SVM_syntspeak1_stat1_poly$bestTune[2]) &
                                                     SVM_syntspeak1_stat1_poly$pred[,7] == as.double(SVM_syntspeak1_stat1_poly$bestTune[3]) & 
                                                     SVM_syntspeak1_stat1_poly$pred[,8] == as.double(SVM_syntspeak1_stat1_poly$bestTune[4]) & 
                                                     SVM_syntspeak1_stat1_poly$pred[,9] == as.double(SVM_syntspeak1_stat1_poly$bestTune[1]) )$obs[1:100]
)

CM_syntspeak1_stat1_poly

#Stat - SECOND IMF
SVM_syntspeak1_stat2_poly<- train(y = yf, x = x_syntspeak1_stat[,71:140], trControl = train_control, method = polySVM, 
                                  preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat2_poly$results
SVM_syntspeak1_stat2_poly$finalModel

CM_syntspeak1_stat2_poly<- confusionMatrix( subset(SVM_syntspeak1_stat2_poly$pred, 
                                                   SVM_syntspeak1_stat2_poly$pred[,6] == as.double(SVM_syntspeak1_stat2_poly$bestTune[2]) & 
                                                     SVM_syntspeak1_stat2_poly$pred[,7] == as.double(SVM_syntspeak1_stat2_poly$bestTune[3]) & 
                                                     SVM_syntspeak1_stat2_poly$pred[,8] == as.double(SVM_syntspeak1_stat2_poly$bestTune[4]) & 
                                                     SVM_syntspeak1_stat2_poly$pred[,9] == as.double(SVM_syntspeak1_stat2_poly$bestTune[1]) )$pred[1:100], 
                                            subset(SVM_syntspeak1_stat2_poly$pred, 
                                                   SVM_syntspeak1_stat2_poly$pred[,6] == as.double(SVM_syntspeak1_stat2_poly$bestTune[2]) &
                                                     SVM_syntspeak1_stat2_poly$pred[,7] == as.double(SVM_syntspeak1_stat2_poly$bestTune[3]) & 
                                                     SVM_syntspeak1_stat2_poly$pred[,8] == as.double(SVM_syntspeak1_stat2_poly$bestTune[4]) & 
                                                     SVM_syntspeak1_stat2_poly$pred[,9] == as.double(SVM_syntspeak1_stat2_poly$bestTune[1]) )$obs[1:100]
)

CM_syntspeak1_stat2_poly


#Stat - THIRD IMF
SVM_syntspeak1_stat3_poly<- train(y = yf, x = x_syntspeak1_stat[,141:210], trControl = train_control, method = polySVM, 
                                  preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat3_poly$results
SVM_syntspeak1_stat3_poly$finalModel

CM_syntspeak1_stat3_poly<- confusionMatrix( subset(SVM_syntspeak1_stat3_poly$pred, 
                                                   SVM_syntspeak1_stat3_poly$pred[,6] == as.double(SVM_syntspeak1_stat3_poly$bestTune[2]) & 
                                                     SVM_syntspeak1_stat3_poly$pred[,7] == as.double(SVM_syntspeak1_stat3_poly$bestTune[3]) & 
                                                     SVM_syntspeak1_stat3_poly$pred[,8] == as.double(SVM_syntspeak1_stat3_poly$bestTune[4]) & 
                                                     SVM_syntspeak1_stat3_poly$pred[,9] == as.double(SVM_syntspeak1_stat3_poly$bestTune[1]) )$pred[1:100], 
                                            subset(SVM_syntspeak1_stat3_poly$pred, 
                                                   SVM_syntspeak1_stat3_poly$pred[,6] == as.double(SVM_syntspeak1_stat3_poly$bestTune[2]) &
                                                     SVM_syntspeak1_stat3_poly$pred[,7] == as.double(SVM_syntspeak1_stat3_poly$bestTune[3]) & 
                                                     SVM_syntspeak1_stat3_poly$pred[,8] == as.double(SVM_syntspeak1_stat3_poly$bestTune[4]) & 
                                                     SVM_syntspeak1_stat3_poly$pred[,9] == as.double(SVM_syntspeak1_stat3_poly$bestTune[1]) )$obs[1:100]
)

CM_syntspeak1_stat3_poly

#Stat - FOURTH IMF
SVM_syntspeak1_stat4_poly<- train(y = yf, x = x_syntspeak1_stat[,211:280], trControl = train_control, method = polySVM, 
                                  preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat4_poly$results
SVM_syntspeak1_stat4_poly$finalModel

CM_syntspeak1_stat4_poly<- confusionMatrix( subset(SVM_syntspeak1_stat4_poly$pred, 
                                                   SVM_syntspeak1_stat4_poly$pred[,6] == as.double(SVM_syntspeak1_stat4_poly$bestTune[2]) & 
                                                     SVM_syntspeak1_stat4_poly$pred[,7] == as.double(SVM_syntspeak1_stat4_poly$bestTune[3]) & 
                                                     SVM_syntspeak1_stat4_poly$pred[,8] == as.double(SVM_syntspeak1_stat4_poly$bestTune[4]) & 
                                                     SVM_syntspeak1_stat4_poly$pred[,9] == as.double(SVM_syntspeak1_stat4_poly$bestTune[1]) )$pred[1:100], 
                                            subset(SVM_syntspeak1_stat4_poly$pred, 
                                                   SVM_syntspeak1_stat4_poly$pred[,6] == as.double(SVM_syntspeak1_stat4_poly$bestTune[2]) &
                                                     SVM_syntspeak1_stat4_poly$pred[,7] == as.double(SVM_syntspeak1_stat4_poly$bestTune[3]) & 
                                                     SVM_syntspeak1_stat4_poly$pred[,8] == as.double(SVM_syntspeak1_stat4_poly$bestTune[4]) & 
                                                     SVM_syntspeak1_stat4_poly$pred[,9] == as.double(SVM_syntspeak1_stat4_poly$bestTune[1]) )$obs[1:100]
)

CM_syntspeak1_stat4_poly

#Stat - FOURTH IMF
SVM_syntspeak1_stat5_poly<- train(y = yf, x = x_syntspeak1_stat[,281:350], trControl = train_control, method = polySVM, 
                                  preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat5_poly$results
SVM_syntspeak1_stat5_poly$finalModel

CM_syntspeak1_stat5_poly<- confusionMatrix( subset(SVM_syntspeak1_stat5_poly$pred, 
                                                   SVM_syntspeak1_stat5_poly$pred[,6] == as.double(SVM_syntspeak1_stat5_poly$bestTune[2]) & 
                                                     SVM_syntspeak1_stat5_poly$pred[,7] == as.double(SVM_syntspeak1_stat5_poly$bestTune[3]) & 
                                                     SVM_syntspeak1_stat5_poly$pred[,8] == as.double(SVM_syntspeak1_stat5_poly$bestTune[4]) & 
                                                     SVM_syntspeak1_stat5_poly$pred[,9] == as.double(SVM_syntspeak1_stat5_poly$bestTune[1]) )$pred[1:100], 
                                            subset(SVM_syntspeak1_stat5_poly$pred, 
                                                   SVM_syntspeak1_stat5_poly$pred[,6] == as.double(SVM_syntspeak1_stat5_poly$bestTune[2]) &
                                                     SVM_syntspeak1_stat5_poly$pred[,7] == as.double(SVM_syntspeak1_stat5_poly$bestTune[3]) & 
                                                     SVM_syntspeak1_stat5_poly$pred[,8] == as.double(SVM_syntspeak1_stat5_poly$bestTune[4]) & 
                                                     SVM_syntspeak1_stat5_poly$pred[,9] == as.double(SVM_syntspeak1_stat5_poly$bestTune[1]) )$obs[1:100]
)

CM_syntspeak1_stat5_poly






