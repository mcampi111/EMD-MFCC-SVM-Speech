#SVM SYNT - SPEAK1 - T = 132278, m = 100   ---- FEATURE: SplineCoeff --- KERNEL: RBASIS   - SCALE : TRUE
#CARET PACKAGE FOR SVM
library('ggplot2')
library('lattice')
library('caret')
library('kernlab')

m<- 100
y1f<- rep("uno",m)
y2f<- rep("zero",m)    
yf<- as.factor(c(y1f,y2f))

train_control<- trainControl(method="repeatedcv", number=2,   summaryFunction = twoClassSummary, 
                             savePredictions = TRUE, repeats = 1, search = 'grid', classProbs=TRUE) 



#SVM SYNTHETIC vs SPEAKER 1 ------------------------------------------------------------------------------------------

x_SYNT_SC<- feat_synt_coeff

x_SPEAK1_SC<- feat_speak1_coeff



x_syntspeak1_SC<- rbind(x_SYNT_SC, x_SPEAK1_SC)

colnames(x_syntspeak1_SC) <- paste("x_syntspeak1_SC", 1:ncol(x_syntspeak1_SC), sep="")


#SC - FIRST IMF
x_sc1<- cbind(x_syntspeak1_SC[,1:60000],x_syntspeak1_SC[,300001:360000],  x_syntspeak1_SC[,600001:660000])

SVM_syntspeak1_SC1<- train(y = yf, x = x_sc1, trControl = train_control, method = 'svmRadial', 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)#

SVM_syntspeak1_SC1$results
SVM_syntspeak1_SC1$finalModel

CM_syntspeak1_SC1<- confusionMatrix(subset(SVM_syntspeak1_SC1$pred, 
                                           SVM_syntspeak1_SC1$pred[,7] == as.double(SVM_syntspeak1_SC1$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_SC1$pred, 
                                           SVM_syntspeak1_SC1$pred[,7] == as.double(SVM_syntspeak1_SC1$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_SC1

#SC - SECOND IMF
x_sc2<- cbind(x_syntspeak1_SC[,60001:120000],x_syntspeak1_SC[,360001:420000],  x_syntspeak1_SC[,660001:720000])

SVM_syntspeak1_SC2<- train(y = yf, x = x_sc2, trControl = train_control, method = 'svmRadial', 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC2$results
SVM_syntspeak1_SC2$finalModel

CM_syntspeak1_SC2<- confusionMatrix(subset(SVM_syntspeak1_SC2$pred, 
                                           SVM_syntspeak1_SC2$pred[,7] == as.double(SVM_syntspeak1_SC2$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_SC2$pred, 
                                           SVM_syntspeak1_SC2$pred[,7] == as.double(SVM_syntspeak1_SC2$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_SC2

#SC - THIRD IMF
x_sc3<- cbind(x_syntspeak1_SC[,120001:180000],x_syntspeak1_SC[,420001:480000],  x_syntspeak1_SC[,720001:780000])

SVM_syntspeak1_SC3<- train(y = yf, x = x_sc3, trControl = train_control, method = 'svmRadial', 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC3$results
SVM_syntspeak1_SC3$finalModel

CM_syntspeak1_SC3<- confusionMatrix(subset(SVM_syntspeak1_SC3$pred, 
                                           SVM_syntspeak1_SC3$pred[,7] == as.double(SVM_syntspeak1_SC3$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_SC3$pred, 
                                           SVM_syntspeak1_SC3$pred[,7] == as.double(SVM_syntspeak1_SC3$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_SC3


#SC - FOURTH IMF
x_sc4<- cbind(x_syntspeak1_SC[,180001:240000],x_syntspeak1_SC[,480001:540000],  x_syntspeak1_SC[,780001:840000])

SVM_syntspeak1_SC4<- train(y = yf, x = x_sc4, trControl = train_control, method = 'svmRadial', 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC4$results
SVM_syntspeak1_SC4$finalModel

CM_syntspeak1_SC4<- confusionMatrix(subset(SVM_syntspeak1_SC4$pred, 
                                           SVM_syntspeak1_SC4$pred[,7] == as.double(SVM_syntspeak1_SC4$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_SC4$pred, 
                                           SVM_syntspeak1_SC4$pred[,7] == as.double(SVM_syntspeak1_SC4$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_SC4


#SC - FIFTH IMF
x_sc5<- cbind(x_syntspeak1_SC[,240001:300000],x_syntspeak1_SC[,540001:600000],  x_syntspeak1_SC[,840001:900000])

SVM_syntspeak1_SC5<- train(y = yf, x = x_sc5, trControl = train_control, method = 'svmRadial', 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC5$results
SVM_syntspeak1_SC5$finalModel

CM_syntspeak1_SC5<- confusionMatrix(subset(SVM_syntspeak1_SC5$pred, 
                                           SVM_syntspeak1_SC5$pred[,7] == as.double(SVM_syntspeak1_SC5$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_SC5$pred, 
                                           SVM_syntspeak1_SC5$pred[,7] == as.double(SVM_syntspeak1_SC5$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_SC5


