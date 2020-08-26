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
x_synt_IMF<- matrix(unlist(feat_IMF_data_synt), byrow=TRUE, nrow=100 )

x_speak1_IMF<- matrix(unlist(feat_IMF_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_IMF<- rbind(x_synt_IMF, x_speak1_IMF)


colnames(x_syntspeak1_IMF) <- paste("x_syntspeak1_IMF", 1:ncol(x_syntspeak1_IMF), sep="")


#IMF - FIRST IMF
SVM_syntspeak1_IMF1_poly<- train(y = yf, x = x_syntspeak1_IMF[,1:60000], trControl = train_control, method = polySVM, 
                           metric = 'ROC', tuneLength = 9) #preProcess = c("center", "scale"), 

SVM_syntspeak1_IMF1_poly$results
SVM_syntspeak1_IMF1_poly$finalModel

CM_syntspeak1_IMF1_poly<- confusionMatrix( subset(SVM_syntspeak1_IMF1_poly$pred, 
                                                  SVM_syntspeak1_IMF1_poly$pred[,6] == as.double(SVM_syntspeak1_IMF1_poly$bestTune[2]) & 
                                                    SVM_syntspeak1_IMF1_poly$pred[,7] == as.double(SVM_syntspeak1_IMF1_poly$bestTune[3]) & 
                                                    SVM_syntspeak1_IMF1_poly$pred[,8] == as.double(SVM_syntspeak1_IMF1_poly$bestTune[4]) & 
                                                    SVM_syntspeak1_IMF1_poly$pred[,9] == as.double(SVM_syntspeak1_IMF1_poly$bestTune[1]) )$pred[1:100], 
                                          subset(SVM_syntspeak1_IMF1_poly$pred, 
                                                 SVM_syntspeak1_IMF1_poly$pred[,6] == as.double(SVM_syntspeak1_IMF1_poly$bestTune[2]) &
                                                   SVM_syntspeak1_IMF1_poly$pred[,7] == as.double(SVM_syntspeak1_IMF1_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_IMF1_poly$pred[,8] == as.double(SVM_syntspeak1_IMF1_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_IMF1_poly$pred[,9] == as.double(SVM_syntspeak1_IMF1_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IMF1_poly


#IMF - SECOND IMF
SVM_syntspeak1_IMF2_poly<- train(y = yf, x = x_syntspeak1_IMF[,60001:120000], trControl = train_control, method = polySVM, 
                            metric = 'ROC', tuneLength = 9 ) #   preProcess = c("center", "scale"),

SVM_syntspeak1_IMF2_poly$results
SVM_syntspeak1_IMF2_poly$finalModel

CM_syntspeak1_IMF2_poly<- confusionMatrix( subset(SVM_syntspeak1_IMF2_poly$pred, 
                                                  SVM_syntspeak1_IMF2_poly$pred[,6] == as.double(SVM_syntspeak1_IMF2_poly$bestTune[2]) & 
                                                    SVM_syntspeak1_IMF2_poly$pred[,7] == as.double(SVM_syntspeak1_IMF2_poly$bestTune[3]) & 
                                                    SVM_syntspeak1_IMF2_poly$pred[,8] == as.double(SVM_syntspeak1_IMF2_poly$bestTune[4]) & 
                                                    SVM_syntspeak1_IMF2_poly$pred[,9] == as.double(SVM_syntspeak1_IMF2_poly$bestTune[1]) )$pred[1:100], 
                                           subset(SVM_syntspeak1_IMF2_poly$pred, 
                                                  SVM_syntspeak1_IMF2_poly$pred[,6] == as.double(SVM_syntspeak1_IMF2_poly$bestTune[2]) &
                                                    SVM_syntspeak1_IMF2_poly$pred[,7] == as.double(SVM_syntspeak1_IMF2_poly$bestTune[3]) & 
                                                    SVM_syntspeak1_IMF2_poly$pred[,8] == as.double(SVM_syntspeak1_IMF2_poly$bestTune[4]) & 
                                                    SVM_syntspeak1_IMF2_poly$pred[,9] == as.double(SVM_syntspeak1_IMF2_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IMF2_poly



#IMF - THIRD IMF
SVM_syntspeak1_IMF3_poly<- train(y = yf, x = x_syntspeak1_IMF[,120001:180000], trControl = train_control, method = polySVM, 
                             metric = 'ROC', tuneLength = 9 ) #    preProcess = c("center", "scale"),

SVM_syntspeak1_IMF3_poly$results
SVM_syntspeak1_IMF3_poly$finalModel

CM_syntspeak1_IMF3_poly<- confusionMatrix( subset(SVM_syntspeak1_IMF3_poly$pred, 
                                                  SVM_syntspeak1_IMF3_poly$pred[,6] == as.double(SVM_syntspeak1_IMF3_poly$bestTune[2]) & 
                                                    SVM_syntspeak1_IMF3_poly$pred[,7] == as.double(SVM_syntspeak1_IMF3_poly$bestTune[3]) & 
                                                    SVM_syntspeak1_IMF3_poly$pred[,8] == as.double(SVM_syntspeak1_IMF3_poly$bestTune[4]) & 
                                                    SVM_syntspeak1_IMF3_poly$pred[,9] == as.double(SVM_syntspeak1_IMF3_poly$bestTune[1]) )$pred[1:100], 
                                           subset(SVM_syntspeak1_IMF3_poly$pred, 
                                                  SVM_syntspeak1_IMF3_poly$pred[,6] == as.double(SVM_syntspeak1_IMF3_poly$bestTune[2]) &
                                                    SVM_syntspeak1_IMF3_poly$pred[,7] == as.double(SVM_syntspeak1_IMF3_poly$bestTune[3]) & 
                                                    SVM_syntspeak1_IMF3_poly$pred[,8] == as.double(SVM_syntspeak1_IMF3_poly$bestTune[4]) & 
                                                    SVM_syntspeak1_IMF3_poly$pred[,9] == as.double(SVM_syntspeak1_IMF3_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IMF3_poly



#IMF - FOURTH IMF
SVM_syntspeak1_IMF4_poly<- train(y = yf, x = x_syntspeak1_IMF[,180001:240000], trControl = train_control, method = polySVM, 
                             metric = 'ROC', tuneLength = 9 ) #  preProcess = c("center", "scale"),

SVM_syntspeak1_IMF4_poly$results
SVM_syntspeak1_IMF4_poly$finalModel

CM_syntspeak1_IMF4_poly<- confusionMatrix( subset(SVM_syntspeak1_IMF4_poly$pred, 
                                                  SVM_syntspeak1_IMF4_poly$pred[,6] == as.double(SVM_syntspeak1_IMF4_poly$bestTune[2]) & 
                                                    SVM_syntspeak1_IMF4_poly$pred[,7] == as.double(SVM_syntspeak1_IMF4_poly$bestTune[3]) & 
                                                    SVM_syntspeak1_IMF4_poly$pred[,8] == as.double(SVM_syntspeak1_IMF4_poly$bestTune[4]) & 
                                                    SVM_syntspeak1_IMF4_poly$pred[,9] == as.double(SVM_syntspeak1_IMF4_poly$bestTune[1]) )$pred[1:100], 
                                           subset(SVM_syntspeak1_IMF4_poly$pred, 
                                                  SVM_syntspeak1_IMF4_poly$pred[,6] == as.double(SVM_syntspeak1_IMF4_poly$bestTune[2]) &
                                                    SVM_syntspeak1_IMF4_poly$pred[,7] == as.double(SVM_syntspeak1_IMF4_poly$bestTune[3]) & 
                                                    SVM_syntspeak1_IMF4_poly$pred[,8] == as.double(SVM_syntspeak1_IMF4_poly$bestTune[4]) & 
                                                    SVM_syntspeak1_IMF4_poly$pred[,9] == as.double(SVM_syntspeak1_IMF4_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IMF4_poly


#IMF - FIFTH IMF
SVM_syntspeak1_IMF5_poly<- train(y = yf, x = x_syntspeak1_IMF[,240001:300000], trControl = train_control, method = polySVM, 
                             metric = 'ROC', tuneLength = 9) #   preProcess = c("center", "scale"),

SVM_syntspeak1_IMF5_poly$results
SVM_syntspeak1_IMF5_poly$finalModel

CM_syntspeak1_IMF5_poly<- confusionMatrix( subset(SVM_syntspeak1_IMF5_poly$pred, 
                                                  SVM_syntspeak1_IMF5_poly$pred[,6] == as.double(SVM_syntspeak1_IMF5_poly$bestTune[2]) & 
                                                    SVM_syntspeak1_IMF5_poly$pred[,7] == as.double(SVM_syntspeak1_IMF5_poly$bestTune[3]) & 
                                                    SVM_syntspeak1_IMF5_poly$pred[,8] == as.double(SVM_syntspeak1_IMF5_poly$bestTune[4]) & 
                                                    SVM_syntspeak1_IMF5_poly$pred[,9] == as.double(SVM_syntspeak1_IMF5_poly$bestTune[1]) )$pred[1:100], 
                                           subset(SVM_syntspeak1_IMF5_poly$pred, 
                                                  SVM_syntspeak1_IMF5_poly$pred[,6] == as.double(SVM_syntspeak1_IMF5_poly$bestTune[2]) &
                                                    SVM_syntspeak1_IMF5_poly$pred[,7] == as.double(SVM_syntspeak1_IMF5_poly$bestTune[3]) & 
                                                    SVM_syntspeak1_IMF5_poly$pred[,8] == as.double(SVM_syntspeak1_IMF5_poly$bestTune[4]) & 
                                                    SVM_syntspeak1_IMF5_poly$pred[,9] == as.double(SVM_syntspeak1_IMF5_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_IMF5_poly






