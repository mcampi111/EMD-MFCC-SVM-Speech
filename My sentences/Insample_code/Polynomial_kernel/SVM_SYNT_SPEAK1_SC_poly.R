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

SVM_syntspeak1_SC1_poly<- train(y = yf, x = x_sc1, trControl = train_control, method = polySVM, 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC1_poly$results
SVM_syntspeak1_SC1_poly$finalModel

CM_syntspeak1_SC1_poly<- confusionMatrix( subset(SVM_syntspeak1_SC1_poly$pred, 
                                            SVM_syntspeak1_SC1_poly$pred[,6] == as.double(SVM_syntspeak1_SC1_poly$bestTune[2]) & 
                                              SVM_syntspeak1_SC1_poly$pred[,7] == as.double(SVM_syntspeak1_SC1_poly$bestTune[3]) & 
                                              SVM_syntspeak1_SC1_poly$pred[,8] == as.double(SVM_syntspeak1_SC1_poly$bestTune[4]) & 
                                              SVM_syntspeak1_SC1_poly$pred[,9] == as.double(SVM_syntspeak1_SC1_poly$bestTune[1]) )$pred[1:100], 
                                     subset(SVM_syntspeak1_SC1_poly$pred, 
                                            SVM_syntspeak1_SC1_poly$pred[,6] == as.double(SVM_syntspeak1_SC1_poly$bestTune[2]) &
                                              SVM_syntspeak1_SC1_poly$pred[,7] == as.double(SVM_syntspeak1_SC1_poly$bestTune[3]) & 
                                              SVM_syntspeak1_SC1_poly$pred[,8] == as.double(SVM_syntspeak1_SC1_poly$bestTune[4]) & 
                                              SVM_syntspeak1_SC1_poly$pred[,9] == as.double(SVM_syntspeak1_SC1_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_SC1_poly


#SC - SECOND IMF
x_sc2<- cbind(x_syntspeak1_SC[,60001:120000],x_syntspeak1_SC[,360001:420000],  x_syntspeak1_SC[,660001:720000])

SVM_syntspeak1_SC2_poly<- train(y = yf, x = x_sc2, trControl = train_control, method = polySVM, 
                                preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC2_poly$results
SVM_syntspeak1_SC2_poly$finalModel

CM_syntspeak1_SC2_poly<- confusionMatrix( subset(SVM_syntspeak1_SC2_poly$pred, 
                                                 SVM_syntspeak1_SC2_poly$pred[,6] == as.double(SVM_syntspeak1_SC2_poly$bestTune[2]) & 
                                                   SVM_syntspeak1_SC2_poly$pred[,7] == as.double(SVM_syntspeak1_SC2_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_SC2_poly$pred[,8] == as.double(SVM_syntspeak1_SC2_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_SC2_poly$pred[,9] == as.double(SVM_syntspeak1_SC2_poly$bestTune[1]) )$pred[1:100], 
                                          subset(SVM_syntspeak1_SC2_poly$pred, 
                                                 SVM_syntspeak1_SC2_poly$pred[,6] == as.double(SVM_syntspeak1_SC2_poly$bestTune[2]) &
                                                   SVM_syntspeak1_SC2_poly$pred[,7] == as.double(SVM_syntspeak1_SC2_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_SC2_poly$pred[,8] == as.double(SVM_syntspeak1_SC2_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_SC2_poly$pred[,9] == as.double(SVM_syntspeak1_SC2_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_SC2_poly

#SC - THIRD IMF
x_sc3<- cbind(x_syntspeak1_SC[,120001:180000],x_syntspeak1_SC[,420001:480000],  x_syntspeak1_SC[,720001:780000])

SVM_syntspeak1_SC3_poly<- train(y = yf, x = x_sc3, trControl = train_control, method = polySVM, 
                                preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC3_poly$results
SVM_syntspeak1_SC3_poly$finalModel

CM_syntspeak1_SC3_poly<- confusionMatrix( subset(SVM_syntspeak1_SC3_poly$pred, 
                                                 SVM_syntspeak1_SC3_poly$pred[,6] == as.double(SVM_syntspeak1_SC3_poly$bestTune[2]) & 
                                                   SVM_syntspeak1_SC3_poly$pred[,7] == as.double(SVM_syntspeak1_SC3_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_SC3_poly$pred[,8] == as.double(SVM_syntspeak1_SC3_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_SC3_poly$pred[,9] == as.double(SVM_syntspeak1_SC3_poly$bestTune[1]) )$pred[1:100], 
                                          subset(SVM_syntspeak1_SC3_poly$pred, 
                                                 SVM_syntspeak1_SC3_poly$pred[,6] == as.double(SVM_syntspeak1_SC3_poly$bestTune[2]) &
                                                   SVM_syntspeak1_SC3_poly$pred[,7] == as.double(SVM_syntspeak1_SC3_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_SC3_poly$pred[,8] == as.double(SVM_syntspeak1_SC3_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_SC3_poly$pred[,9] == as.double(SVM_syntspeak1_SC3_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_SC3_poly



#SC - FOURTH IMF
x_sc4<- cbind(x_syntspeak1_SC[,180001:240000],x_syntspeak1_SC[,480001:540000],  x_syntspeak1_SC[,780001:840000])

SVM_syntspeak1_SC4_poly<- train(y = yf, x = x_sc4, trControl = train_control, method = polySVM, 
                                preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC4_poly$results
SVM_syntspeak1_SC4_poly$finalModel

CM_syntspeak1_SC4_poly<- confusionMatrix( subset(SVM_syntspeak1_SC4_poly$pred, 
                                                 SVM_syntspeak1_SC4_poly$pred[,6] == as.double(SVM_syntspeak1_SC4_poly$bestTune[2]) & 
                                                   SVM_syntspeak1_SC4_poly$pred[,7] == as.double(SVM_syntspeak1_SC4_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_SC4_poly$pred[,8] == as.double(SVM_syntspeak1_SC4_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_SC4_poly$pred[,9] == as.double(SVM_syntspeak1_SC4_poly$bestTune[1]) )$pred[1:100], 
                                          subset(SVM_syntspeak1_SC4_poly$pred, 
                                                 SVM_syntspeak1_SC4_poly$pred[,6] == as.double(SVM_syntspeak1_SC4_poly$bestTune[2]) &
                                                   SVM_syntspeak1_SC4_poly$pred[,7] == as.double(SVM_syntspeak1_SC4_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_SC4_poly$pred[,8] == as.double(SVM_syntspeak1_SC4_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_SC4_poly$pred[,9] == as.double(SVM_syntspeak1_SC4_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_SC4_poly



#SC - FIFTH IMF
x_sc5<- cbind(x_syntspeak1_SC[,240001:300000],x_syntspeak1_SC[,540001:600000],  x_syntspeak1_SC[,840001:900000])

SVM_syntspeak1_SC5_poly<- train(y = yf, x = x_sc5, trControl = train_control, method = polySVM, 
                                preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_SC5_poly$results
SVM_syntspeak1_SC5_poly$finalModel

CM_syntspeak1_SC5_poly<- confusionMatrix( subset(SVM_syntspeak1_SC5_poly$pred, 
                                                 SVM_syntspeak1_SC5_poly$pred[,6] == as.double(SVM_syntspeak1_SC5_poly$bestTune[2]) & 
                                                   SVM_syntspeak1_SC5_poly$pred[,7] == as.double(SVM_syntspeak1_SC5_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_SC5_poly$pred[,8] == as.double(SVM_syntspeak1_SC5_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_SC5_poly$pred[,9] == as.double(SVM_syntspeak1_SC5_poly$bestTune[1]) )$pred[1:100], 
                                          subset(SVM_syntspeak1_SC5_poly$pred, 
                                                 SVM_syntspeak1_SC5_poly$pred[,6] == as.double(SVM_syntspeak1_SC5_poly$bestTune[2]) &
                                                   SVM_syntspeak1_SC5_poly$pred[,7] == as.double(SVM_syntspeak1_SC5_poly$bestTune[3]) & 
                                                   SVM_syntspeak1_SC5_poly$pred[,8] == as.double(SVM_syntspeak1_SC5_poly$bestTune[4]) & 
                                                   SVM_syntspeak1_SC5_poly$pred[,9] == as.double(SVM_syntspeak1_SC5_poly$bestTune[1]) )$obs[1:100]
)
CM_syntspeak1_SC5_poly


