#SVM SYNT vs SPEAKER1 - T = 132278, m = 100   ---- FEATURE: IF --- KERNEL: RBASIS   - SCALE : TRUE
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



#SVM SYNT vs SPEAKER1   -----------------------------------------------------------------------------------------

x_synt_IF<- matrix(unlist(feat_synt_IF), byrow=TRUE, nrow=100 )


x_speak1_IF<- matrix(unlist(feat_speak1_IF), byrow=TRUE, nrow=100 )



x_syntspeak1_IF<- rbind(x_synt_IF, x_speak1_IF)

colnames(x_syntspeak1_IF) <- paste("x_syntspeak1_IF", 1:ncol(x_syntspeak1_IF), sep="")


#IF - FIRST IMF
SVM_syntspeak1_IF1_van<- train(y = yf, x = x_syntspeak1_IF[,1:60000], trControl = train_control, method = vanSVM, 
                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF1_van$results
SVM_syntspeak1_IF1_van$finalModel

CM_syntspeak1_IF1_van<- confusionMatrix(subset(SVM_syntspeak1_IF1_van$pred, 
                                           SVM_syntspeak1_IF1_van$pred[,6] == as.double(SVM_syntspeak1_IF1_van$bestTune[1]) )$pred[1:100],
                                      subset(SVM_syntspeak1_IF1_van$pred, 
                                             SVM_syntspeak1_IF1_van$pred[,6] == as.double(SVM_syntspeak1_IF1_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_IF1_van

#IF - SECOND IMF
SVM_syntspeak1_IF2_van<- train(y = yf, x = x_syntspeak1_IF[,60001:120000], trControl = train_control, method = vanSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF2_van$results
SVM_syntspeak1_IF2_van$finalModel

CM_syntspeak1_IF2_van<- confusionMatrix(subset(SVM_syntspeak1_IF2_van$pred, 
                                           SVM_syntspeak1_IF2_van$pred[,6] == as.double(SVM_syntspeak1_IF2_van$bestTune[1]) )$pred[1:100],
                                    subset(SVM_syntspeak1_IF2_van$pred, 
                                           SVM_syntspeak1_IF2_van$pred[,6] == as.double(SVM_syntspeak1_IF2_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_IF2_van

#IF - THIRD IMF
SVM_syntspeak1_IF3_van<- train(y = yf, x = x_syntspeak1_IF[,120001:180000], trControl = train_control, method = vanSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF3_van$results
SVM_syntspeak1_IF3_van$finalModel

CM_syntspeak1_IF3_van<- confusionMatrix(subset(SVM_syntspeak1_IF3_van$pred, 
                                           SVM_syntspeak1_IF3_van$pred[,6] == as.double(SVM_syntspeak1_IF3_van$bestTune[1]) )$pred[1:100],
                                    subset(SVM_syntspeak1_IF3_van$pred, 
                                           SVM_syntspeak1_IF3_van$pred[,6] == as.double(SVM_syntspeak1_IF3_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_IF3_van


#IF - FOURTH IMF
SVM_syntspeak1_IF4_van<- train(y = yf, x = x_syntspeak1_IF[,180001:240000], trControl = train_control, method = vanSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF4_van$results
SVM_syntspeak1_IF4_van$finalModel

CM_syntspeak1_IF4_van<- confusionMatrix(subset(SVM_syntspeak1_IF4_van$pred, 
                                           SVM_syntspeak1_IF4_van$pred[,6] == as.double(SVM_syntspeak1_IF4_van$bestTune[1]) )$pred[1:100],
                                    subset(SVM_syntspeak1_IF4_van$pred, 
                                           SVM_syntspeak1_IF4_van$pred[,6] == as.double(SVM_syntspeak1_IF4_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_IF4_van


#IF - FIFTH IMF
SVM_syntspeak1_IF5_van<- train(y = yf, x = x_syntspeak1_IF[,240001:300000], trControl = train_control, method = vanSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF5_van$results
SVM_syntspeak1_IF5_van$finalModel

CM_syntspeak1_IF5_van<- confusionMatrix(subset(SVM_syntspeak1_IF5_van$pred, 
                                           SVM_syntspeak1_IF5_van$pred[,6] == as.double(SVM_syntspeak1_IF5_van$bestTune[1]) )$pred[1:100],
                                    subset(SVM_syntspeak1_IF5_van$pred, 
                                           SVM_syntspeak1_IF5_van$pred[,6] == as.double(SVM_syntspeak1_IF5_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_IF5_van


