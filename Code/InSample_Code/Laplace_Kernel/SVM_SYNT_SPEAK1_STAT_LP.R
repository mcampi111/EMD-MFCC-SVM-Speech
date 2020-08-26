#SVM SYNT - SPEAKER1 - T = 132278, m = 100   ---- FEATURE: IMFs --- KERNEL: RBASIS   - SCALE : TRUE
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



#SVM SYNTHETIC vs SPEAKER 1  ---------------------------------------------------------------------------------------
x_SYNT_stat<- matrix(unlist(synt_vec), byrow=TRUE, nrow=100 )

x_SPEAK1_stat<- matrix(unlist(speak1_vec), byrow=TRUE, nrow=100 )


x_syntspeak1_stat<- rbind(x_SYNT_stat, x_SPEAK1_stat)

colnames(x_syntspeak1_stat) <- paste("x_syntspeak1_stat", 1:ncol(x_syntspeak1_stat), sep="")



#STAT - FIRST IMF
SVM_syntspeak1_stat1_lp<- train(y = yf, x = x_syntspeak1_stat[,1:70], trControl = train_control, method = lpSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9) 

SVM_syntspeak1_stat1_lp$results
SVM_syntspeak1_stat1_lp$finalModel

CM_syntspeak1_stat1_lp<- confusionMatrix(subset(SVM_syntspeak1_stat1_lp$pred, 
                                               SVM_syntspeak1_stat1_lp$pred[,7] == as.double(SVM_syntspeak1_stat1_lp$bestTune[1]) &
                                                 SVM_syntspeak1_stat1_lp$pred[,7] == as.double(SVM_syntspeak1_stat1_lp$bestTune[1]) )$pred[1:100],
                                    subset(SVM_syntspeak1_stat1_lp$pred, 
                                           SVM_syntspeak1_stat1_lp$pred[,6] == as.double(SVM_syntspeak1_stat1_lp$bestTune[2]) &
                                             SVM_syntspeak1_stat1_lp$pred[,6] == as.double(SVM_syntspeak1_stat1_lp$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_stat1_lp


#STAT - SECOND IMF
SVM_syntspeak1_stat2_lp<- train(y = yf, x = x_syntspeak1_stat[,71:140], trControl = train_control, method = lpSVM, 
                                preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9) 

SVM_syntspeak1_stat2_lp$results
SVM_syntspeak1_stat2_lp$finalModel

CM_syntspeak1_stat2_lp<- confusionMatrix(subset(SVM_syntspeak1_stat2_lp$pred, 
                                                SVM_syntspeak1_stat2_lp$pred[,7] == as.double(SVM_syntspeak1_stat2_lp$bestTune[1]) &
                                                  SVM_syntspeak1_stat2_lp$pred[,7] == as.double(SVM_syntspeak1_stat2_lp$bestTune[1]) )$pred[1:100],
                                         subset(SVM_syntspeak1_stat2_lp$pred, 
                                                SVM_syntspeak1_stat2_lp$pred[,6] == as.double(SVM_syntspeak1_stat2_lp$bestTune[2]) &
                                                  SVM_syntspeak1_stat2_lp$pred[,6] == as.double(SVM_syntspeak1_stat2_lp$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_stat2_lp

#STAT - THIRD IMF
SVM_syntspeak1_stat3_lp<- train(y = yf, x = x_syntspeak1_stat[,141:210], trControl = train_control, method = lpSVM, 
                                preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9) 

SVM_syntspeak1_stat3_lp$results
SVM_syntspeak1_stat3_lp$finalModel

CM_syntspeak1_stat3_lp<- confusionMatrix(subset(SVM_syntspeak1_stat3_lp$pred, 
                                                SVM_syntspeak1_stat3_lp$pred[,7] == as.double(SVM_syntspeak1_stat3_lp$bestTune[1]) &
                                                  SVM_syntspeak1_stat3_lp$pred[,7] == as.double(SVM_syntspeak1_stat3_lp$bestTune[1]) )$pred[1:100],
                                         subset(SVM_syntspeak1_stat3_lp$pred, 
                                                SVM_syntspeak1_stat3_lp$pred[,6] == as.double(SVM_syntspeak1_stat3_lp$bestTune[2]) &
                                                  SVM_syntspeak1_stat3_lp$pred[,6] == as.double(SVM_syntspeak1_stat3_lp$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_stat3_lp

#STAT - FOURTH IMF
SVM_syntspeak1_stat4_lp<- train(y = yf, x = x_syntspeak1_stat[,211:280], trControl = train_control, method = lpSVM, 
                                preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9) 

SVM_syntspeak1_stat4_lp$results
SVM_syntspeak1_stat4_lp$finalModel

CM_syntspeak1_stat4_lp<- confusionMatrix(subset(SVM_syntspeak1_stat4_lp$pred, 
                                                SVM_syntspeak1_stat4_lp$pred[,7] == as.double(SVM_syntspeak1_stat4_lp$bestTune[1]) &
                                                  SVM_syntspeak1_stat4_lp$pred[,7] == as.double(SVM_syntspeak1_stat4_lp$bestTune[1]) )$pred[1:100],
                                         subset(SVM_syntspeak1_stat4_lp$pred, 
                                                SVM_syntspeak1_stat4_lp$pred[,6] == as.double(SVM_syntspeak1_stat4_lp$bestTune[2]) &
                                                  SVM_syntspeak1_stat4_lp$pred[,6] == as.double(SVM_syntspeak1_stat4_lp$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_stat4_lp


#STAT - FIFTH IMF
SVM_syntspeak1_stat5_lp<- train(y = yf, x = x_syntspeak1_stat[,281:350], trControl = train_control, method = lpSVM, 
                                preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9) #

SVM_syntspeak1_stat5_lp$results
SVM_syntspeak1_stat5_lp$finalModel

CM_syntspeak1_stat5_lp<- confusionMatrix(subset(SVM_syntspeak1_stat5_lp$pred, 
                                                SVM_syntspeak1_stat5_lp$pred[,7] == as.double(SVM_syntspeak1_stat5_lp$bestTune[1]) &
                                                  SVM_syntspeak1_stat5_lp$pred[,7] == as.double(SVM_syntspeak1_stat5_lp$bestTune[1]) )$pred[1:100],
                                         subset(SVM_syntspeak1_stat5_lp$pred, 
                                                SVM_syntspeak1_stat5_lp$pred[,6] == as.double(SVM_syntspeak1_stat5_lp$bestTune[2]) &
                                                  SVM_syntspeak1_stat5_lp$pred[,6] == as.double(SVM_syntspeak1_stat5_lp$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_stat5_lp





