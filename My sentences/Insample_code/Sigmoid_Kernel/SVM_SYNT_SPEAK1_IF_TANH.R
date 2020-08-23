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
SVM_syntspeak1_IF1_tan<- train(y = yf, x = x_syntspeak1_IF[,1:60000], trControl = train_control, method = tanhSVM, 
                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9) #, tuneLength = 9

SVM_syntspeak1_IF1_tan$results
SVM_syntspeak1_IF1_tan$finalModel

CM_syntspeak1_IF1_tan<- confusionMatrix( subset(SVM_syntspeak1_IF1_tan$pred, 
                                                SVM_syntspeak1_IF1_tan$pred[,6] == as.double(SVM_syntspeak1_IF1_tan$bestTune[2]) & 
                                                  SVM_syntspeak1_IF1_tan$pred[,7] == as.double(SVM_syntspeak1_IF1_tan$bestTune[3]) & 
                                                  SVM_syntspeak1_IF1_tan$pred[,8] == as.double(SVM_syntspeak1_IF1_tan$bestTune[1]) )$pred[1:100], 
                                         subset(SVM_syntspeak1_IF1_tan$pred, 
                                                SVM_syntspeak1_IF1_tan$pred[,6] == as.double(SVM_syntspeak1_IF1_tan$bestTune[2]) &
                                                  SVM_syntspeak1_IF1_tan$pred[,7] == as.double(SVM_syntspeak1_IF1_tan$bestTune[3]) & 
                                                  SVM_syntspeak1_IF1_tan$pred[,8] == as.double(SVM_syntspeak1_IF1_tan$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IF1_tan

#IF - SECOND IMF
SVM_syntspeak1_IF2_tan<- train(y = yf, x = x_syntspeak1_IF[,60001:120000], trControl = train_control, method = tanhSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC') #, tuneLength = 9

SVM_syntspeak1_IF2_tan$results
SVM_syntspeak1_IF2_tan$finalModel

CM_syntspeak1_IF2_tan<- confusionMatrix( subset(SVM_syntspeak1_IF2_tan$pred, 
                                                SVM_syntspeak1_IF2_tan$pred[,6] == as.double(SVM_syntspeak1_IF2_tan$bestTune[2]) & 
                                                  SVM_syntspeak1_IF2_tan$pred[,7] == as.double(SVM_syntspeak1_IF2_tan$bestTune[3]) & 
                                                  SVM_syntspeak1_IF2_tan$pred[,8] == as.double(SVM_syntspeak1_IF2_tan$bestTune[1]) )$pred[1:100], 
                                         subset(SVM_syntspeak1_IF2_tan$pred, 
                                                SVM_syntspeak1_IF2_tan$pred[,6] == as.double(SVM_syntspeak1_IF2_tan$bestTune[2]) &
                                                  SVM_syntspeak1_IF2_tan$pred[,7] == as.double(SVM_syntspeak1_IF2_tan$bestTune[3]) & 
                                                  SVM_syntspeak1_IF2_tan$pred[,8] == as.double(SVM_syntspeak1_IF2_tan$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IF2_tan

#IF - THIRD IMF
SVM_syntspeak1_IF3_tan<- train(y = yf, x = x_syntspeak1_IF[,120001:180000], trControl = train_control, method = tanhSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF3_tan$results
SVM_syntspeak1_IF3_tan$finalModel

CM_syntspeak1_IF3_tan<- confusionMatrix( subset(SVM_syntspeak1_IF3_tan$pred, 
                                                SVM_syntspeak1_IF3_tan$pred[,6] == as.double(SVM_syntspeak1_IF3_tan$bestTune[2]) & 
                                                  SVM_syntspeak1_IF3_tan$pred[,7] == as.double(SVM_syntspeak1_IF3_tan$bestTune[3]) & 
                                                  SVM_syntspeak1_IF3_tan$pred[,8] == as.double(SVM_syntspeak1_IF3_tan$bestTune[1]) )$pred[1:100], 
                                         subset(SVM_syntspeak1_IF3_tan$pred, 
                                                SVM_syntspeak1_IF3_tan$pred[,6] == as.double(SVM_syntspeak1_IF3_tan$bestTune[2]) &
                                                  SVM_syntspeak1_IF3_tan$pred[,7] == as.double(SVM_syntspeak1_IF3_tan$bestTune[3]) & 
                                                  SVM_syntspeak1_IF3_tan$pred[,8] == as.double(SVM_syntspeak1_IF3_tan$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IF3_tan


#IF - FOURTH IMF
SVM_syntspeak1_IF4_tan<- train(y = yf, x = x_syntspeak1_IF[,180001:240000], trControl = train_control, method = tanhSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF4_tan$results
SVM_syntspeak1_IF4_tan$finalModel

CM_syntspeak1_IF4_tan<- confusionMatrix( subset(SVM_syntspeak1_IF4_tan$pred, 
                                                SVM_syntspeak1_IF4_tan$pred[,6] == as.double(SVM_syntspeak1_IF4_tan$bestTune[2]) & 
                                                  SVM_syntspeak1_IF4_tan$pred[,7] == as.double(SVM_syntspeak1_IF4_tan$bestTune[3]) & 
                                                  SVM_syntspeak1_IF4_tan$pred[,8] == as.double(SVM_syntspeak1_IF4_tan$bestTune[1]) )$pred[1:100], 
                                         subset(SVM_syntspeak1_IF4_tan$pred, 
                                                SVM_syntspeak1_IF4_tan$pred[,6] == as.double(SVM_syntspeak1_IF4_tan$bestTune[2]) &
                                                  SVM_syntspeak1_IF4_tan$pred[,7] == as.double(SVM_syntspeak1_IF4_tan$bestTune[3]) & 
                                                  SVM_syntspeak1_IF4_tan$pred[,8] == as.double(SVM_syntspeak1_IF4_tan$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IF4_tan


#IF - FIFTH IMF
SVM_syntspeak1_IF5_tan<- train(y = yf, x = x_syntspeak1_IF[,240001:300000], trControl = train_control, method = tanhSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_IF5_tan$results
SVM_syntspeak1_IF5_tan$finalModel

CM_syntspeak1_IF5_tan<- confusionMatrix( subset(SVM_syntspeak1_IF5_tan$pred, 
                                                SVM_syntspeak1_IF5_tan$pred[,6] == as.double(SVM_syntspeak1_IF5_tan$bestTune[2]) & 
                                                  SVM_syntspeak1_IF5_tan$pred[,7] == as.double(SVM_syntspeak1_IF5_tan$bestTune[3]) & 
                                                  SVM_syntspeak1_IF5_tan$pred[,8] == as.double(SVM_syntspeak1_IF5_tan$bestTune[1]) )$pred[1:100], 
                                         subset(SVM_syntspeak1_IF5_tan$pred, 
                                                SVM_syntspeak1_IF5_tan$pred[,6] == as.double(SVM_syntspeak1_IF5_tan$bestTune[2]) &
                                                  SVM_syntspeak1_IF5_tan$pred[,7] == as.double(SVM_syntspeak1_IF5_tan$bestTune[3]) & 
                                                  SVM_syntspeak1_IF5_tan$pred[,8] == as.double(SVM_syntspeak1_IF5_tan$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IF5_tan


