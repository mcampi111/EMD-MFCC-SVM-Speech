#SVM SYNT - SPEAKER1 - T = 132278, m = 100   ---- FEATURE: IMFs --- KERNEL: RBASIS   - SCALE : TRUE
#CARET PACKAGE FOR SVM
library('ggplot2')
library('lattice')
library('caret')
library('kernlab')

feat_IMF_data_speak1 <- feat_IMF_data_speak1_PREC
feat_IMF_data_synt <- feat_IMF_data_synt_PREC
rm(feat_IMF_data_speak1_PREC, feat_IMF_data_synt_PREC)
m<- 100

y1f<- rep("uno",m)
y2f<- rep("zero",m)    
yf<- as.factor(c(y1f,y2f))

train_control<- trainControl(method="repeatedcv", number=2, classProbs=TRUE,  summaryFunction = twoClassSummary, 
                             savePredictions = TRUE, repeats = 1, search = 'grid')



#SVM SYNTHETIC vs SPEAKER 1  ---------------------------------------------------------------------------------------
x_synt_IMF<- matrix(unlist(feat_IMF_data_synt), byrow=TRUE, nrow=100 )

x_speak1_IMF<- matrix(unlist(feat_IMF_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_IMF<- rbind(x_synt_IMF, x_speak1_IMF)


colnames(x_syntspeak1_IMF) <- paste("x_syntspeak1_IMF", 1:ncol(x_syntspeak1_IMF), sep="")


#IMF - FIRST IMF
SVM_syntspeak1_IMF1<- train(y = yf, x = x_syntspeak1_IMF[,1:60000], trControl = train_control, method = 'svmRadial', 
                            metric = 'ROC', tuneLength = 9) #preProcess = c("center", "scale"),


SVM_syntspeak1_IMF1$results
SVM_syntspeak1_IMF1$finalModel

CM_syntspeak1_IMF1<- confusionMatrix(subset(SVM_syntspeak1_IMF1$pred, 
                          SVM_syntspeak1_IMF1$pred[,7] == as.double(SVM_syntspeak1_IMF1$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_IMF1$pred, 
                          SVM_syntspeak1_IMF1$pred[,7] == as.double(SVM_syntspeak1_IMF1$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_IMF1


#IMF - SECOND IMF
SVM_syntspeak1_IMF2<- train(y = yf, x = x_syntspeak1_IMF[,60001:120000], trControl = train_control, method = 'svmRadial', 
                           metric = 'ROC', tuneLength = 9) # preProcess = c("center", "scale"), 


SVM_syntspeak1_IMF2$results
SVM_syntspeak1_IMF2$finalModel

CM_syntspeak1_IMF2<- confusionMatrix(subset(SVM_syntspeak1_IMF2$pred, 
                     SVM_syntspeak1_IMF2$pred[,7] == as.double(SVM_syntspeak1_IMF2$bestTune[2]) )$pred[1:100],
                                     subset(SVM_syntspeak1_IMF2$pred, 
                    SVM_syntspeak1_IMF2$pred[,7] == as.double(SVM_syntspeak1_IMF2$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_IMF2



#IMF - THIRD IMF
SVM_syntspeak1_IMF3<- train(y = yf, x = x_syntspeak1_IMF[,120001:180000], trControl = train_control, method = 'svmRadial', 
                             metric = 'ROC', tuneLength = 9) # preProcess = c("center", "scale"),

SVM_syntspeak1_IMF3$results
SVM_syntspeak1_IMF3$finalModel

CM_syntspeak1_IMF3<- confusionMatrix(subset(SVM_syntspeak1_IMF3$pred, 
                            SVM_syntspeak1_IMF3$pred[,7] == as.double(SVM_syntspeak1_IMF3$bestTune[2]) )$pred[1:100],
                                     subset(SVM_syntspeak1_IMF3$pred, 
                          SVM_syntspeak1_IMF3$pred[,7] == as.double(SVM_syntspeak1_IMF3$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_IMF3



#IMF - FOURTH IMF
SVM_syntspeak1_IMF4<- train(y = yf, x = x_syntspeak1_IMF[,180001:240000], trControl = train_control, method = 'svmRadial', 
                             metric = 'ROC', tuneLength = 9) # preProcess = c("center", "scale"),

SVM_syntspeak1_IMF4$results
SVM_syntspeak1_IMF4$finalModel

CM_syntspeak1_IMF4<- confusionMatrix(subset(SVM_syntspeak1_IMF4$pred, 
                       SVM_syntspeak1_IMF4$pred[,7] == as.double(SVM_syntspeak1_IMF4$bestTune[2]) )$pred[1:100],
                                     subset(SVM_syntspeak1_IMF4$pred, 
                    SVM_syntspeak1_IMF4$pred[,7] == as.double(SVM_syntspeak1_IMF4$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_IMF4


#IMF - FIFTH IMF
SVM_syntspeak1_IMF5<- train(y = yf, x = x_syntspeak1_IMF[,240001:300000], trControl = train_control, method = 'svmRadial', 
                            metric = 'ROC', tuneLength = 9) # preProcess = c("center", "scale"), 

SVM_syntspeak1_IMF5$results
SVM_syntspeak1_IMF5$finalModel

CM_syntspeak1_IMF5<- confusionMatrix(subset(SVM_syntspeak1_IMF5$pred, 
                                            SVM_syntspeak1_IMF5$pred[,7] == as.double(SVM_syntspeak1_IMF5$bestTune[2]) )$pred[1:100],
                                     subset(SVM_syntspeak1_IMF5$pred, 
                                            SVM_syntspeak1_IMF5$pred[,7] == as.double(SVM_syntspeak1_IMF5$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_IMF5






