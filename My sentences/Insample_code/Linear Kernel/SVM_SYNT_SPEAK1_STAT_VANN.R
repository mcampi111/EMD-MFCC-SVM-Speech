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
SVM_syntspeak1_stat1_van<- train(y = yf, x = x_syntspeak1_stat[,1:70], trControl = train_control, method = vanSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat1_van$results
SVM_syntspeak1_stat1_van$finalModel

CM_syntspeak1_stat1_van<- confusionMatrix(subset(SVM_syntspeak1_stat1_van$pred, 
                                           SVM_syntspeak1_stat1_van$pred[,6] == as.double(SVM_syntspeak1_stat1_van$bestTune[1]) )$pred[1:100],
                                    subset(SVM_syntspeak1_stat1_van$pred, 
                                           SVM_syntspeak1_stat1_van$pred[,6] == as.double(SVM_syntspeak1_stat1_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_stat1_van

#Stat - SECOND IMF
SVM_syntspeak1_stat2_van<- train(y = yf, x = x_syntspeak1_stat[,71:140], trControl = train_control, method = vanSVM, 
                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat2_van$results
SVM_syntspeak1_stat2_van$finalModel

CM_syntspeak1_stat2_van<- confusionMatrix(subset(SVM_syntspeak1_stat2_van$pred, 
                                             SVM_syntspeak1_stat2_van$pred[,6] == as.double(SVM_syntspeak1_stat2_van$bestTune[1]) )$pred[1:100],
                                      subset(SVM_syntspeak1_stat2_van$pred, 
                                             SVM_syntspeak1_stat2_van$pred[,6] == as.double(SVM_syntspeak1_stat2_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_stat2_van


#Stat - THIRD IMF
SVM_syntspeak1_stat3_van<- train(y = yf, x = x_syntspeak1_stat[,141:210], trControl = train_control, method = vanSVM, 
                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat3_van$results
SVM_syntspeak1_stat3_van$finalModel

CM_syntspeak1_stat3_van<- confusionMatrix(subset(SVM_syntspeak1_stat3_van$pred, 
                                             SVM_syntspeak1_stat3_van$pred[,6] == as.double(SVM_syntspeak1_stat3_van$bestTune[1]) )$pred[1:100],
                                      subset(SVM_syntspeak1_stat3_van$pred, 
                                             SVM_syntspeak1_stat3_van$pred[,6] == as.double(SVM_syntspeak1_stat3_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_stat3_van

#Stat - FOURTH IMF
SVM_syntspeak1_stat4_van<- train(y = yf, x = x_syntspeak1_stat[,211:280], trControl = train_control, method = vanSVM, 
                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat4_van$results
SVM_syntspeak1_stat4_van$finalModel

CM_syntspeak1_stat4_van<- confusionMatrix(subset(SVM_syntspeak1_stat4_van$pred, 
                                             SVM_syntspeak1_stat4_van$pred[,6] == as.double(SVM_syntspeak1_stat4_van$bestTune[1]) )$pred[1:100],
                                      subset(SVM_syntspeak1_stat4_van$pred, 
                                             SVM_syntspeak1_stat4_van$pred[,6] == as.double(SVM_syntspeak1_stat4_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_stat4_van


#Stat - FIFTH IMF
SVM_syntspeak1_stat5_van<- train(y = yf, x = x_syntspeak1_stat[,281:350], trControl = train_control, method = vanSVM, 
                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat5_van$results
SVM_syntspeak1_stat5_van$finalModel

CM_syntspeak1_stat5_van<- confusionMatrix(subset(SVM_syntspeak1_stat5_van$pred, 
                                             SVM_syntspeak1_stat5_van$pred[,6] == as.double(SVM_syntspeak1_stat5_van$bestTune[1]) )$pred[1:100],
                                      subset(SVM_syntspeak1_stat5_van$pred, 
                                             SVM_syntspeak1_stat5_van$pred[,6] == as.double(SVM_syntspeak1_stat5_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_stat5_van





