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
SVM_syntspeak1_stat1<- train(y = yf, x = x_syntspeak1_stat[,1:70], trControl = train_control, method = 'svmRadial', 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat1$results
SVM_syntspeak1_stat1$finalModel

CM_syntspeak1_stat1<- confusionMatrix(subset(SVM_syntspeak1_stat1$pred, 
                                           SVM_syntspeak1_stat1$pred[,7] == as.double(SVM_syntspeak1_stat1$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_stat1$pred, 
                                           SVM_syntspeak1_stat1$pred[,7] == as.double(SVM_syntspeak1_stat1$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_stat1

#Stat - SECOND IMF
SVM_syntspeak1_stat2<- train(y = yf, x = x_syntspeak1_stat[,71:140], trControl = train_control, method = 'svmRadial', 
                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat2$results
SVM_syntspeak1_stat2$finalModel

CM_syntspeak1_stat2<- confusionMatrix(subset(SVM_syntspeak1_stat2$pred, 
                                             SVM_syntspeak1_stat2$pred[,7] == as.double(SVM_syntspeak1_stat2$bestTune[2]) )$pred[1:100],
                                      subset(SVM_syntspeak1_stat2$pred, 
                                             SVM_syntspeak1_stat2$pred[,7] == as.double(SVM_syntspeak1_stat2$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_stat2


#Stat - THIRD IMF
SVM_syntspeak1_stat3<- train(y = yf, x = x_syntspeak1_stat[,141:210], trControl = train_control, method = 'svmRadial', 
                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat3$results
SVM_syntspeak1_stat3$finalModel

CM_syntspeak1_stat3<- confusionMatrix(subset(SVM_syntspeak1_stat3$pred, 
                                             SVM_syntspeak1_stat3$pred[,7] == as.double(SVM_syntspeak1_stat3$bestTune[2]) )$pred[1:100],
                                      subset(SVM_syntspeak1_stat3$pred, 
                                             SVM_syntspeak1_stat3$pred[,7] == as.double(SVM_syntspeak1_stat3$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_stat3

#Stat - FOURTH IMF
SVM_syntspeak1_stat4<- train(y = yf, x = x_syntspeak1_stat[,211:280], trControl = train_control, method = 'svmRadial', 
                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat4$results
SVM_syntspeak1_stat4$finalModel

CM_syntspeak1_stat4<- confusionMatrix(subset(SVM_syntspeak1_stat4$pred, 
                                             SVM_syntspeak1_stat4$pred[,7] == as.double(SVM_syntspeak1_stat4$bestTune[2]) )$pred[1:100],
                                      subset(SVM_syntspeak1_stat4$pred, 
                                             SVM_syntspeak1_stat4$pred[,7] == as.double(SVM_syntspeak1_stat4$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_stat4


#Stat - FIFTH IMF
SVM_syntspeak1_stat5<- train(y = yf, x = x_syntspeak1_stat[,281:350], trControl = train_control, method = 'svmRadial', 
                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_stat5$results
SVM_syntspeak1_stat5$finalModel

CM_syntspeak1_stat5<- confusionMatrix(subset(SVM_syntspeak1_stat5$pred, 
                                             SVM_syntspeak1_stat5$pred[,7] == as.double(SVM_syntspeak1_stat5$bestTune[2]) )$pred[1:100],
                                      subset(SVM_syntspeak1_stat5$pred, 
                                             SVM_syntspeak1_stat5$pred[,7] == as.double(SVM_syntspeak1_stat5$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_stat5





