#SVM SYNT vs SPEAKER1 - T = 132278, m = 100   ---- FEATURE: IF --- KERNEL: RBASIS   - SCALE : TRUE
#CARET PACKAGE FOR SVM
library('ggplot2')
library('lattice')
library('caret')
library('kernlab')

m<-100
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
SVM_syntspeak1_IF1<- train(y = yf, x = x_syntspeak1_IF[,1:60000], trControl = train_control, method = 'svmRadial', 
                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF1$results
SVM_syntspeak1_IF1$finalModel

CM_syntspeak1_IF1<- confusionMatrix(subset(SVM_syntspeak1_IF1$pred, 
                              SVM_syntspeak1_IF1$pred[,7] == as.double(SVM_syntspeak1_IF1$bestTune[2]) )$pred[1:100],
                                      subset(SVM_syntspeak1_IF1$pred, 
                               SVM_syntspeak1_IF1$pred[,7] == as.double(SVM_syntspeak1_IF1$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_IF1

#IF - SECOND IMF
SVM_syntspeak1_IF2<- train(y = yf, x = x_syntspeak1_IF[,60001:120000], trControl = train_control, method = 'svmRadial', 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF2$results
SVM_syntspeak1_IF2$finalModel

CM_syntspeak1_IF2<- confusionMatrix(subset(SVM_syntspeak1_IF2$pred, 
                                           SVM_syntspeak1_IF2$pred[,7] == as.double(SVM_syntspeak1_IF2$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_IF2$pred, 
                                           SVM_syntspeak1_IF2$pred[,7] == as.double(SVM_syntspeak1_IF2$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_IF2

#IF - THIRD IMF
SVM_syntspeak1_IF3<- train(y = yf, x = x_syntspeak1_IF[,120001:180000], trControl = train_control, method = 'svmRadial', 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF3$results
SVM_syntspeak1_IF3$finalModel

CM_syntspeak1_IF3<- confusionMatrix(subset(SVM_syntspeak1_IF3$pred, 
                                           SVM_syntspeak1_IF3$pred[,7] == as.double(SVM_syntspeak1_IF3$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_IF3$pred, 
                                           SVM_syntspeak1_IF3$pred[,7] == as.double(SVM_syntspeak1_IF3$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_IF3


#IF - FOURTH IMF
SVM_syntspeak1_IF4<- train(y = yf, x = x_syntspeak1_IF[,180001:240000], trControl = train_control, method = 'svmRadial', 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF4$results
SVM_syntspeak1_IF4$finalModel

CM_syntspeak1_IF4<- confusionMatrix(subset(SVM_syntspeak1_IF4$pred, 
                                           SVM_syntspeak1_IF4$pred[,7] == as.double(SVM_syntspeak1_IF4$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_IF4$pred, 
                                           SVM_syntspeak1_IF4$pred[,7] == as.double(SVM_syntspeak1_IF4$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_IF4


#IF - FIFTH IMF
SVM_syntspeak1_IF5<- train(y = yf, x = x_syntspeak1_IF[,240001:300000], trControl = train_control, method = 'svmRadial', 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF5$results
SVM_syntspeak1_IF5$finalModel

CM_syntspeak1_IF5<- confusionMatrix(subset(SVM_syntspeak1_IF5$pred, 
                                           SVM_syntspeak1_IF5$pred[,7] == as.double(SVM_syntspeak1_IF5$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_IF5$pred, 
                                           SVM_syntspeak1_IF5$pred[,7] == as.double(SVM_syntspeak1_IF5$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_IF5


