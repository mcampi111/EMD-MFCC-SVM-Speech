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
SVM_syntspeak1_IF1_poly<- train(y = yf, x = x_syntspeak1_IF[,1:60000], trControl = train_control, method = polySVM, 
                             preProcess = c("center", "scale"), metric = 'ROC') # tunelength = 9

SVM_syntspeak1_IF1_poly$results
SVM_syntspeak1_IF1_poly$finalModel

CM_syntspeak1_IF1_poly<- confusionMatrix( subset(SVM_syntspeak1_IF1_poly$pred, 
                                SVM_syntspeak1_IF1_poly$pred[,6] == as.double(SVM_syntspeak1_IF1_poly$bestTune[2]) & 
                                SVM_syntspeak1_IF1_poly$pred[,7] == as.double(SVM_syntspeak1_IF1_poly$bestTune[3]) & 
                               SVM_syntspeak1_IF1_poly$pred[,8] == as.double(SVM_syntspeak1_IF1_poly$bestTune[4]) & 
                  SVM_syntspeak1_IF1_poly$pred[,9] == as.double(SVM_syntspeak1_IF1_poly$bestTune[1]) )$pred[1:100], 
                                      subset(SVM_syntspeak1_IF1_poly$pred, 
                                SVM_syntspeak1_IF1_poly$pred[,6] == as.double(SVM_syntspeak1_IF1_poly$bestTune[2]) &
                                SVM_syntspeak1_IF1_poly$pred[,7] == as.double(SVM_syntspeak1_IF1_poly$bestTune[3]) & 
                                SVM_syntspeak1_IF1_poly$pred[,8] == as.double(SVM_syntspeak1_IF1_poly$bestTune[4]) & 
                      SVM_syntspeak1_IF1_poly$pred[,9] == as.double(SVM_syntspeak1_IF1_poly$bestTune[1]) )$obs[1:100]
                                   )
CM_syntspeak1_IF1_poly

#IF - SECOND IMF
SVM_syntspeak1_IF2_poly<- train(y = yf, x = x_syntspeak1_IF[,60001:120000], trControl = train_control,method = polySVM, 
                           preProcess = c("center", "scale"), metric = 'ROC') #, tuneLength = 9

SVM_syntspeak1_IF2_poly$results
SVM_syntspeak1_IF2_poly$finalModel

CM_syntspeak1_IF2_poly<- confusionMatrix( subset(SVM_syntspeak1_IF2_poly$pred, 
                                                 SVM_syntspeak1_IF2_poly$pred[,6] == as.double(SVM_syntspeak1_IF2_poly$bestTune[2]) & 
                                                   SVM_syntspeak1_IF2_poly$pred[,7] == as.double(SVM_syntspeak1_IF2_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_IF2_poly$pred[,8] == as.double(SVM_syntspeak1_IF2_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_IF2_poly$pred[,9] == as.double(SVM_syntspeak1_IF2_poly$bestTune[1]) )$pred[1:100], 
                                          subset(SVM_syntspeak1_IF2_poly$pred, 
                                                 SVM_syntspeak1_IF2_poly$pred[,6] == as.double(SVM_syntspeak1_IF2_poly$bestTune[2]) &
                                                   SVM_syntspeak1_IF2_poly$pred[,7] == as.double(SVM_syntspeak1_IF2_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_IF2_poly$pred[,8] == as.double(SVM_syntspeak1_IF2_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_IF2_poly$pred[,9] == as.double(SVM_syntspeak1_IF2_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IF2_poly

#IF - THIRD IMF
SVM_syntspeak1_IF3_poly<- train(y = yf, x = x_syntspeak1_IF[,120001:180000], trControl = train_control,method = polySVM, 
                           preProcess = c("center", "scale"), metric = 'ROC') #, tuneLength = 9

SVM_syntspeak1_IF3_poly$results
SVM_syntspeak1_IF3_poly$finalModel

CM_syntspeak1_IF3_poly<- confusionMatrix( subset(SVM_syntspeak1_IF3_poly$pred, 
                                                 SVM_syntspeak1_IF3_poly$pred[,6] == as.double(SVM_syntspeak1_IF3_poly$bestTune[2]) & 
                                                   SVM_syntspeak1_IF3_poly$pred[,7] == as.double(SVM_syntspeak1_IF3_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_IF3_poly$pred[,8] == as.double(SVM_syntspeak1_IF3_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_IF3_poly$pred[,9] == as.double(SVM_syntspeak1_IF3_poly$bestTune[1]) )$pred[1:100], 
                                          subset(SVM_syntspeak1_IF3_poly$pred, 
                                                 SVM_syntspeak1_IF3_poly$pred[,6] == as.double(SVM_syntspeak1_IF3_poly$bestTune[2]) &
                                                   SVM_syntspeak1_IF3_poly$pred[,7] == as.double(SVM_syntspeak1_IF3_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_IF3_poly$pred[,8] == as.double(SVM_syntspeak1_IF3_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_IF3_poly$pred[,9] == as.double(SVM_syntspeak1_IF3_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IF3_poly


#IF - FOURTH IMF
SVM_syntspeak1_IF4_poly<- train(y = yf, x = x_syntspeak1_IF[,180001:240000], trControl = train_control, method = polySVM, 
                           preProcess = c("center", "scale"), metric = 'ROC') #, tuneLength = 9

SVM_syntspeak1_IF4_poly$results
SVM_syntspeak1_IF4_poly$finalModel

CM_syntspeak1_IF4_poly<- confusionMatrix( subset(SVM_syntspeak1_IF4_poly$pred, 
                              SVM_syntspeak1_IF4_poly$pred[,6] == as.double(SVM_syntspeak1_IF4_poly$bestTune[2]) & 
                               SVM_syntspeak1_IF4_poly$pred[,7] == as.double(SVM_syntspeak1_IF4_poly$bestTune[3]) &
                              SVM_syntspeak1_IF4_poly$pred[,8] == as.double(SVM_syntspeak1_IF4_poly$bestTune[4]) & 
                     SVM_syntspeak1_IF4_poly$pred[,9] == as.double(SVM_syntspeak1_IF4_poly$bestTune[1]) )$pred[1:100], 
                               subset(SVM_syntspeak1_IF4_poly$pred, 
                              SVM_syntspeak1_IF4_poly$pred[,6] == as.double(SVM_syntspeak1_IF4_poly$bestTune[2]) &
                              SVM_syntspeak1_IF4_poly$pred[,7] == as.double(SVM_syntspeak1_IF4_poly$bestTune[3]) & 
                              SVM_syntspeak1_IF4_poly$pred[,8] == as.double(SVM_syntspeak1_IF4_poly$bestTune[4]) & 
                      SVM_syntspeak1_IF4_poly$pred[,9] == as.double(SVM_syntspeak1_IF4_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IF4_poly


#IF - FIFTH IMF
SVM_syntspeak1_IF5_poly<- train(y = yf, x = x_syntspeak1_IF[,240001:300000], trControl = train_control, method = polySVM, 
                           preProcess = c("center", "scale"), metric = 'ROC') #, tuneLength = 9

SVM_syntspeak1_IF5_poly$results
SVM_syntspeak1_IF5_poly$finalModel

CM_syntspeak1_IF5_poly<- confusionMatrix( subset(SVM_syntspeak1_IF5_poly$pred, 
                                                 SVM_syntspeak1_IF5_poly$pred[,6] == as.double(SVM_syntspeak1_IF5_poly$bestTune[2]) & 
                                                   SVM_syntspeak1_IF5_poly$pred[,7] == as.double(SVM_syntspeak1_IF5_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_IF5_poly$pred[,8] == as.double(SVM_syntspeak1_IF5_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_IF5_poly$pred[,9] == as.double(SVM_syntspeak1_IF5_poly$bestTune[1]) )$pred[1:100], 
                                          subset(SVM_syntspeak1_IF5_poly$pred, 
                                                 SVM_syntspeak1_IF5_poly$pred[,6] == as.double(SVM_syntspeak1_IF5_poly$bestTune[2]) &
                                                   SVM_syntspeak1_IF5_poly$pred[,7] == as.double(SVM_syntspeak1_IF5_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_IF5_poly$pred[,8] == as.double(SVM_syntspeak1_IF5_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_IF5_poly$pred[,9] == as.double(SVM_syntspeak1_IF5_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IF5_poly


