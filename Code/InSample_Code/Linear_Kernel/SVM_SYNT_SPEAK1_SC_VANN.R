#SVM SYNT - SPEAK1 - T = 132278, m = 100   ---- FEATURE: SplineCoeff --- KERNEL: RBASIS   - SCALE : TRUE
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



#SVM SYNTHETIC vs SPEAKER 1 ------------------------------------------------------------------------------------------

x_SYNT_SC<- feat_synt_coeff

x_SPEAK1_SC<- feat_speak1_coeff



x_syntspeak1_SC<- rbind(x_SYNT_SC, x_SPEAK1_SC)

colnames(x_syntspeak1_SC) <- paste("x_syntspeak1_SC", 1:ncol(x_syntspeak1_SC), sep="")


#SC - FIRST IMF
x_sc1<- cbind(x_syntspeak1_SC[,1:60000],x_syntspeak1_SC[,300001:360000],  x_syntspeak1_SC[,600001:660000])

SVM_syntspeak1_SC1_van<- train(y = yf, x = x_sc1, trControl = train_control, method = vanSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC1_van$results
SVM_syntspeak1_SC1_van$finalModel

CM_syntspeak1_SC1_van<- confusionMatrix(subset(SVM_syntspeak1_SC1_van$pred, 
                                           SVM_syntspeak1_SC1_van$pred[,6] == as.double(SVM_syntspeak1_SC1_van$bestTune[1]) )$pred[1:100],
                                    subset(SVM_syntspeak1_SC1_van$pred, 
                                           SVM_syntspeak1_SC1_van$pred[,6] == as.double(SVM_syntspeak1_SC1_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_SC1_van

#SC - SECOND IMF
x_sc2<- cbind(x_syntspeak1_SC[,60001:120000],x_syntspeak1_SC[,360001:420000],  x_syntspeak1_SC[,660001:720000])

SVM_syntspeak1_SC2_van<- train(y = yf, x = x_sc2, trControl = train_control, method = vanSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC2_van$results
SVM_syntspeak1_SC2_van$finalModel

CM_syntspeak1_SC2_van<- confusionMatrix(subset(SVM_syntspeak1_SC2_van$pred, 
                                           SVM_syntspeak1_SC2_van$pred[,6] == as.double(SVM_syntspeak1_SC2_van$bestTune[1]) )$pred[1:100],
                                    subset(SVM_syntspeak1_SC2_van$pred, 
                                           SVM_syntspeak1_SC2_van$pred[,6] == as.double(SVM_syntspeak1_SC2_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_SC2_van

#SC - THIRD IMF
x_sc3<- cbind(x_syntspeak1_SC[,120001:180000],x_syntspeak1_SC[,420001:480000],  x_syntspeak1_SC[,720001:780000])

SVM_syntspeak1_SC3_van<- train(y = yf, x = x_sc3, trControl = train_control, method = vanSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC3_van$results
SVM_syntspeak1_SC3_van$finalModel

CM_syntspeak1_SC3_van<- confusionMatrix(subset(SVM_syntspeak1_SC3_van$pred, 
                                           SVM_syntspeak1_SC3_van$pred[,6] == as.double(SVM_syntspeak1_SC3_van$bestTune[1]) )$pred[1:100],
                                    subset(SVM_syntspeak1_SC3_van$pred, 
                                           SVM_syntspeak1_SC3_van$pred[,6] == as.double(SVM_syntspeak1_SC3_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_SC3_van


#SC - FOURTH IMF
x_sc4<- cbind(x_syntspeak1_SC[,180001:240000],x_syntspeak1_SC[,480001:540000],  x_syntspeak1_SC[,780001:840000])

SVM_syntspeak1_SC4_van<- train(y = yf, x = x_sc4, trControl = train_control, method = vanSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC4_van$results
SVM_syntspeak1_SC4_van$finalModel

CM_syntspeak1_SC4_van<- confusionMatrix(subset(SVM_syntspeak1_SC4_van$pred, 
                                           SVM_syntspeak1_SC4_van$pred[,6] == as.double(SVM_syntspeak1_SC4_van$bestTune[1]) )$pred[1:100],
                                    subset(SVM_syntspeak1_SC4_van$pred, 
                                           SVM_syntspeak1_SC4_van$pred[,6] == as.double(SVM_syntspeak1_SC4$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_SC4_van


#SC - FIFTH IMF
x_sc5<- cbind(x_syntspeak1_SC[,240001:300000],x_syntspeak1_SC[,540001:600000],  x_syntspeak1_SC[,840001:900000])

SVM_syntspeak1_SC5_van<- train(y = yf, x = x_sc5, trControl = train_control, method = vanSVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC5_van$results
SVM_syntspeak1_SC5_van$finalModel

CM_syntspeak1_SC5_van<- confusionMatrix(subset(SVM_syntspeak1_SC5_van$pred, 
                                           SVM_syntspeak1_SC5_van$pred[,6] == as.double(SVM_syntspeak1_SC5_van$bestTune[1]) )$pred[1:100],
                                    subset(SVM_syntspeak1_SC5_van$pred, 
                                           SVM_syntspeak1_SC5_van$pred[,6] == as.double(SVM_syntspeak1_SC5_van$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_SC5_van


