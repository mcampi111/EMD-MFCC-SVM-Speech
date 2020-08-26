#EXTRACTING MLFCC CEPSTRAL COEFFICIENTS - IMFs
library("seewave")
library("tuneR")
library('ggplot2')
library('lattice')
library('caret')
library('kernlab')

#MLFCC COEFF SPEAK1
#IMF1----------------------------------------------------------------------------------
feat_IMF1_data_speak1_wave<- list()

for (i in 1:m) {
  
  feat_IMF1_data_speak1_wave[[i]]<- Wave(feat_IMF_data_speak1[[i]][,1])
}


mlfc_IMF1_data_speak1<- list()

for (i in 1:m) {
  
  mlfc_IMF1_data_speak1[[i]]<- melfcc(feat_IMF1_data_speak1_wave[[i]])
}

#IMF2----------------------------------------------------------------------------------
feat_IMF2_data_speak1_wave<- list()

for (i in 1:m) {
  
  feat_IMF2_data_speak1_wave[[i]]<- Wave(feat_IMF_data_speak1[[i]][,2])
}


mlfc_IMF2_data_speak1<- list()

for (i in 1:m) {
  
  mlfc_IMF2_data_speak1[[i]]<- melfcc(feat_IMF2_data_speak1_wave[[i]])
}

#IMF3----------------------------------------------------------------------------------
feat_IMF3_data_speak1_wave<- list()

for (i in 1:m) {
  
  feat_IMF3_data_speak1_wave[[i]]<- Wave(feat_IMF_data_speak1[[i]][,3])
}


mlfc_IMF3_data_speak1<- list()

for (i in 1:m) {
  
  mlfc_IMF3_data_speak1[[i]]<- melfcc(feat_IMF3_data_speak1_wave[[i]])
}

#IMF4----------------------------------------------------------------------------------
feat_IMF4_data_speak1_wave<- list()

for (i in 1:m) {
  
  feat_IMF4_data_speak1_wave[[i]]<- Wave(feat_IMF_data_speak1[[i]][,4])
}


mlfc_IMF4_data_speak1<- list()

for (i in 1:m) {
  
  mlfc_IMF4_data_speak1[[i]]<- melfcc(feat_IMF4_data_speak1_wave[[i]])
}


#IMF5----------------------------------------------------------------------------------
feat_IMF5_data_speak1_wave<- list()

for (i in 1:m) {
  
  feat_IMF5_data_speak1_wave[[i]]<- Wave(feat_IMF_data_speak1[[i]][,5])
}


mlfc_IMF5_data_speak1<- list()

for (i in 1:m) {
  
  mlfc_IMF5_data_speak1[[i]]<- melfcc(feat_IMF5_data_speak1_wave[[i]])
}



#MLFCC COEFF synt
#IMF1----------------------------------------------------------------------------------
feat_IMF1_data_synt_wave<- list()

for (i in 1:m) {
  
  feat_IMF1_data_synt_wave[[i]]<- Wave(feat_IMF_data_synt[[i]][,1])
}


mlfc_IMF1_data_synt<- list()

for (i in 1:m) {
  
  mlfc_IMF1_data_synt[[i]]<- melfcc(feat_IMF1_data_synt_wave[[i]])
}

#IMF2----------------------------------------------------------------------------------
feat_IMF2_data_synt_wave<- list()

for (i in 1:m) {
  
  feat_IMF2_data_synt_wave[[i]]<- Wave(feat_IMF_data_synt[[i]][,2])
}


mlfc_IMF2_data_synt<- list()

for (i in 1:m) {
  
  mlfc_IMF2_data_synt[[i]]<- melfcc(feat_IMF2_data_synt_wave[[i]])
}

#IMF3----------------------------------------------------------------------------------
feat_IMF3_data_synt_wave<- list()

for (i in 1:m) {
  
  feat_IMF3_data_synt_wave[[i]]<- Wave(feat_IMF_data_synt[[i]][,3])
}


mlfc_IMF3_data_synt<- list()

for (i in 1:m) {
  
  mlfc_IMF3_data_synt[[i]]<- melfcc(feat_IMF3_data_synt_wave[[i]])
}

#IMF4----------------------------------------------------------------------------------
feat_IMF4_data_synt_wave<- list()

for (i in 1:m) {
  
  feat_IMF4_data_synt_wave[[i]]<- Wave(feat_IMF_data_synt[[i]][,4])
}


mlfc_IMF4_data_synt<- list()

for (i in 1:m) {
  
  mlfc_IMF4_data_synt[[i]]<- melfcc(feat_IMF4_data_synt_wave[[i]])
}


#IMF5----------------------------------------------------------------------------------
feat_IMF5_data_synt_wave<- list()

for (i in 1:m) {
  
  feat_IMF5_data_synt_wave[[i]]<- Wave(feat_IMF_data_synt[[i]][,5])
}


mlfc_IMF5_data_synt<- list()

for (i in 1:m) {
  
  mlfc_IMF5_data_synt[[i]]<- melfcc(feat_IMF5_data_synt_wave[[i]])
}





# #SVM BETWEEN THESE TWO FEATURES ----------------------------------------------------------
# m<- 100
# y1f<- rep("uno",m)
# y2f<- rep("zero",m)    
# yf<- as.factor(c(y1f,y2f))
# 
# train_control<- trainControl(method="repeatedcv", number=2, classProbs=TRUE,  summaryFunction = twoClassSummary, 
#                              savePredictions = TRUE, repeats = 1, search = 'grid')
# 
# #SVM SYNTHETIC vs SPEAKER 1  ---------------------------------------------------------------------------------------
# #HIGH COEFFICIENTS
# #IMF1------------------
# x_synt_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_synt), byrow=TRUE, nrow=100 )
# 
# x_speak1_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_speak1), byrow=TRUE, nrow=100 )
# 
# x_syntspeak1_MLFC_IMF1<- rbind(x_synt_MLFC_IMF1, x_speak1_MLFC_IMF1)
# 
# 
# colnames(x_syntspeak1_MLFC_IMF1) <- paste("x_syntspeak1_MLFC_IMF1", 1:ncol(x_syntspeak1_MLFC_IMF1), sep="")
# 
# #MLFC - all data - HIGH COEFF
# SVM_syntspeak1_MLFC_IMF1_HIGH<- train(y = yf, x = x_syntspeak1_MLFC_IMF1[,1474:1608], trControl = train_control, method = 'svmRadial', 
#                                       preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
# 
# SVM_syntspeak1_MLFC_IMF1_HIGH$results
# SVM_syntspeak1_MLFC_IMF1_HIGH$finalModel
# 
# CM_syntspeak1_MLFC_IMF1_HIGH<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_HIGH$pred, 
#                                                       SVM_syntspeak1_MLFC_IMF1_HIGH$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_HIGH$bestTune[2]) )$pred[1:100],
#                                                subset(SVM_syntspeak1_MLFC_IMF1_HIGH$pred, 
#                                                       SVM_syntspeak1_MLFC_IMF1_HIGH$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_HIGH$bestTune[2]) )$obs[1:100]  )
# CM_syntspeak1_MLFC_IMF1_HIGH
# 
# 
# #IMF2--------------------------
# x_synt_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_synt), byrow=TRUE, nrow=100 )
# 
# x_speak1_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_speak1), byrow=TRUE, nrow=100 )
# 
# 
# x_syntspeak1_MLFC_IMF2<- rbind(x_synt_MLFC_IMF2, x_speak1_MLFC_IMF2)
# 
# 
# colnames(x_syntspeak1_MLFC_IMF2) <- paste("x_syntspeak1_MLFC_IMF2", 1:ncol(x_syntspeak1_MLFC_IMF2), sep="")
# 
# #MLFC - all data - HIGH COEFF
# SVM_syntspeak1_MLFC_IMF2_HIGH<- train(y = yf, x = x_syntspeak1_MLFC_IMF2[,1474:1608], trControl = train_control, method = 'svmRadial', 
#                                       preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
# 
# SVM_syntspeak1_MLFC_IMF2_HIGH$results
# SVM_syntspeak1_MLFC_IMF2_HIGH$finalModel
# 
# CM_syntspeak1_MLFC_IMF2_HIGH<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_HIGH$pred, 
#                                                       SVM_syntspeak1_MLFC_IMF2_HIGH$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_HIGH$bestTune[2]) )$pred[1:100],
#                                                subset(SVM_syntspeak1_MLFC_IMF2_HIGH$pred, 
#                                                       SVM_syntspeak1_MLFC_IMF2_HIGH$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_HIGH$bestTune[2]) )$obs[1:100]  )
# CM_syntspeak1_MLFC_IMF2_HIGH
# 
# 
# #IMF3-------------------------------------------
# x_synt_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_synt), byrow=TRUE, nrow=100 )
# 
# x_speak1_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_speak1), byrow=TRUE, nrow=100 )
# 
# 
# x_syntspeak1_MLFC_IMF3<- rbind(x_synt_MLFC_IMF3, x_speak1_MLFC_IMF3)
# 
# 
# colnames(x_syntspeak1_MLFC_IMF3) <- paste("x_syntspeak1_MLFC_IMF3", 1:ncol(x_syntspeak1_MLFC_IMF3), sep="")
# 
# #MLFC - all data - HIGH COEFF
# SVM_syntspeak1_MLFC_IMF3_HIGH<- train(y = yf, x = x_syntspeak1_MLFC_IMF3[,1474:1608], trControl = train_control, method = 'svmRadial', 
#                                       preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
# 
# SVM_syntspeak1_MLFC_IMF3_HIGH$results
# SVM_syntspeak1_MLFC_IMF3_HIGH$finalModel
# 
# CM_syntspeak1_MLFC_IMF3_HIGH<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_HIGH$pred, 
#                                                       SVM_syntspeak1_MLFC_IMF3_HIGH$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_HIGH$bestTune[2]) )$pred[1:100],
#                                                subset(SVM_syntspeak1_MLFC_IMF3_HIGH$pred, 
#                                                       SVM_syntspeak1_MLFC_IMF3_HIGH$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_HIGH$bestTune[2]) )$obs[1:100]  )
# CM_syntspeak1_MLFC_IMF3_HIGH
# 
# 
# 
# #IMF4-----------------------------------------------------------
# x_synt_MLFC_IMF4<- matrix(unlist(mlfc_IMF4_data_synt), byrow=TRUE, nrow=100 )
# 
# x_speak1_MLFC_IMF4<- matrix(unlist(mlfc_IMF4_data_speak1), byrow=TRUE, nrow=100 )
# 
# 
# x_syntspeak1_MLFC_IMF4<- rbind(x_synt_MLFC_IMF4, x_speak1_MLFC_IMF4)
# 
# 
# colnames(x_syntspeak1_MLFC_IMF4) <- paste("x_syntspeak1_MLFC_IMF4", 1:ncol(x_syntspeak1_MLFC_IMF4), sep="")
# 
# #MLFC - all data - HIGH COEFF
# SVM_syntspeak1_MLFC_IMF4_HIGH<- train(y = yf, x = x_syntspeak1_MLFC_IMF4[,805:1608], trControl = train_control, method = 'svmRadial', 
#                                       preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
# 
# SVM_syntspeak1_MLFC_IMF4_HIGH$results
# SVM_syntspeak1_MLFC_IMF4_HIGH$finalModel
# 
# CM_syntspeak1_MLFC_IMF4_HIGH<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF4_HIGH$pred, 
#                                                       SVM_syntspeak1_MLFC_IMF4_HIGH$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF4_HIGH$bestTune[2]) )$pred[1:100],
#                                                subset(SVM_syntspeak1_MLFC_IMF4_HIGH$pred, 
#                                                       SVM_syntspeak1_MLFC_IMF4_HIGH$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF4_HIGH$bestTune[2]) )$obs[1:100]  )
# CM_syntspeak1_MLFC_IMF4_HIGH
# 
# 
# 
# #IMF5------------------
# x_synt_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_synt), byrow=TRUE, nrow=100 )
# 
# x_speak1_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_speak1), byrow=TRUE, nrow=100 )
# 
# 
# x_syntspeak1_MLFC_IMF5<- rbind(x_synt_MLFC_IMF5, x_speak1_MLFC_IMF5)
# 
# 
# colnames(x_syntspeak1_MLFC_IMF5) <- paste("x_syntspeak1_MLFC_IMF5", 1:ncol(x_syntspeak1_MLFC_IMF5), sep="")
# 
# #MLFC - all data - HIGH COEFF
# SVM_syntspeak1_MLFC_IMF5_HIGH<- train(y = yf, x = x_syntspeak1_MLFC_IMF5[,1474:1608], trControl = train_control, method = 'svmRadial', 
#                                       preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
# 
# SVM_syntspeak1_MLFC_IMF5_HIGH$results
# SVM_syntspeak1_MLFC_IMF5_HIGH$finalModel
# 
# CM_syntspeak1_MLFC_IMF5_HIGH<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_HIGH$pred, 
#                                                       SVM_syntspeak1_MLFC_IMF5_HIGH$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5_HIGH$bestTune[2]) )$pred[1:100],
#                                                subset(SVM_syntspeak1_MLFC_IMF5_HIGH$pred, 
#                                                       SVM_syntspeak1_MLFC_IMF5_HIGH$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5_HIGH$bestTune[2]) )$obs[1:100]  )
# CM_syntspeak1_MLFC_IMF5_HIGH
# 
# 
# #SVM SYNTHETIC vs SPEAKER 1  ---------------------------------------------------------------------------------------
# #LOW COEFFICIENTS
# #IMF1------------------
# x_synt_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_synt), byrow=TRUE, nrow=100 )
# 
# x_speak1_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_speak1), byrow=TRUE, nrow=100 )
# 
# 
# x_syntspeak1_MLFC_IMF1<- rbind(x_synt_MLFC_IMF1, x_speak1_MLFC_IMF1)
# 
# 
# colnames(x_syntspeak1_MLFC_IMF1) <- paste("x_syntspeak1_MLFC_IMF1", 1:ncol(x_syntspeak1_MLFC_IMF1), sep="")
# 
# #MLFC - all data - LOW COEFF
# SVM_syntspeak1_MLFC_IMF1_LOW<- train(y = yf, x = x_syntspeak1_MLFC_IMF1[,1:804], trControl = train_control, method = 'svmRadial', 
#                                      preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
# 
# SVM_syntspeak1_MLFC_IMF1_LOW$results
# SVM_syntspeak1_MLFC_IMF1_LOW$finalModel
# 
# CM_syntspeak1_MLFC_IMF1_LOW<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_LOW$pred, 
#                                                      SVM_syntspeak1_MLFC_IMF1_LOW$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_LOW$bestTune[2]) )$pred[1:100],
#                                               subset(SVM_syntspeak1_MLFC_IMF1_LOW$pred, 
#                                                      SVM_syntspeak1_MLFC_IMF1_LOW$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_LOW$bestTune[2]) )$obs[1:100]  )
# CM_syntspeak1_MLFC_IMF1_LOW
# 
# 
# #IMF2--------------------------
# x_synt_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_synt), byrow=TRUE, nrow=100 )
# 
# x_speak1_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_speak1), byrow=TRUE, nrow=100 )
# 
# 
# x_syntspeak1_MLFC_IMF2<- rbind(x_synt_MLFC_IMF2, x_speak1_MLFC_IMF2)
# 
# 
# colnames(x_syntspeak1_MLFC_IMF2) <- paste("x_syntspeak1_MLFC_IMF2", 1:ncol(x_syntspeak1_MLFC_IMF2), sep="")
# 
# #MLFC - all data - LOW COEFF
# SVM_syntspeak1_MLFC_IMF2_LOW<- train(y = yf, x = x_syntspeak1_MLFC_IMF2[,1:804], trControl = train_control, method = 'svmRadial', 
#                                      preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
# 
# SVM_syntspeak1_MLFC_IMF2_LOW$results
# SVM_syntspeak1_MLFC_IMF2_LOW$finalModel
# 
# CM_syntspeak1_MLFC_IMF2_LOW<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_LOW$pred, 
#                                                      SVM_syntspeak1_MLFC_IMF2_LOW$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_LOW$bestTune[2]) )$pred[1:100],
#                                               subset(SVM_syntspeak1_MLFC_IMF2_LOW$pred, 
#                                                      SVM_syntspeak1_MLFC_IMF2_LOW$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_LOW$bestTune[2]) )$obs[1:100]  )
# CM_syntspeak1_MLFC_IMF2_LOW
# 
# 
# #IMF3-------------------------------------------
# x_synt_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_synt), byrow=TRUE, nrow=100 )
# 
# x_speak1_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_speak1), byrow=TRUE, nrow=100 )
# 
# 
# x_syntspeak1_MLFC_IMF3<- rbind(x_synt_MLFC_IMF3, x_speak1_MLFC_IMF3)
# 
# 
# colnames(x_syntspeak1_MLFC_IMF3) <- paste("x_syntspeak1_MLFC_IMF3", 1:ncol(x_syntspeak1_MLFC_IMF3), sep="")
# 
# #MLFC - all data - LOW COEFF
# SVM_syntspeak1_MLFC_IMF3_LOW<- train(y = yf, x = x_syntspeak1_MLFC_IMF3[,1:804], trControl = train_control, method = 'svmRadial', 
#                                      preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
# 
# SVM_syntspeak1_MLFC_IMF3_LOW$results
# SVM_syntspeak1_MLFC_IMF3_LOW$finalModel
# 
# CM_syntspeak1_MLFC_IMF3_LOW<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_LOW$pred, 
#                                                      SVM_syntspeak1_MLFC_IMF3_LOW$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_LOW$bestTune[2]) )$pred[1:100],
#                                               subset(SVM_syntspeak1_MLFC_IMF3_LOW$pred, 
#                                                      SVM_syntspeak1_MLFC_IMF3_LOW$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_LOW$bestTune[2]) )$obs[1:100]  )
# CM_syntspeak1_MLFC_IMF3_LOW
# 
# 
# 
# #IMF4-----------------------------------------------------------
# x_synt_MLFC_IMF4<- matrix(unlist(mlfc_IMF4_data_synt), byrow=TRUE, nrow=100 )
# 
# x_speak1_MLFC_IMF4<- matrix(unlist(mlfc_IMF4_data_speak1), byrow=TRUE, nrow=100 )
# 
# 
# x_syntspeak1_MLFC_IMF4<- rbind(x_synt_MLFC_IMF4, x_speak1_MLFC_IMF4)
# 
# 
# colnames(x_syntspeak1_MLFC_IMF4) <- paste("x_syntspeak1_MLFC_IMF4", 1:ncol(x_syntspeak1_MLFC_IMF4), sep="")
# 
# #MLFC - all data - LOW COEFF
# SVM_syntspeak1_MLFC_IMF4_LOW<- train(y = yf, x = x_syntspeak1_MLFC_IMF4[,1:804], trControl = train_control, method = 'svmRadial', 
#                                      preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
# 
# SVM_syntspeak1_MLFC_IMF4_LOW$results
# SVM_syntspeak1_MLFC_IMF4_LOW$finalModel
# 
# CM_syntspeak1_MLFC_IMF4_LOW<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF4_LOW$pred, 
#                                                      SVM_syntspeak1_MLFC_IMF4_LOW$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF4_LOW$bestTune[2]) )$pred[1:100],
#                                               subset(SVM_syntspeak1_MLFC_IMF4_LOW$pred, 
#                                                      SVM_syntspeak1_MLFC_IMF4_LOW$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF4_LOW$bestTune[2]) )$obs[1:100]  )
# CM_syntspeak1_MLFC_IMF4_LOW
# 
# 
# 
# #IMF5------------------
# x_synt_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_synt), byrow=TRUE, nrow=100 )
# 
# x_speak1_MLFC_IMF5<- matrix(unlist(mlfc_IMF5_data_speak1), byrow=TRUE, nrow=100 )
# 
# 
# x_syntspeak1_MLFC_IMF5<- rbind(x_synt_MLFC_IMF5, x_speak1_MLFC_IMF5)
# 
# 
# colnames(x_syntspeak1_MLFC_IMF5) <- paste("x_syntspeak1_MLFC_IMF5", 1:ncol(x_syntspeak1_MLFC_IMF5), sep="")
# 
# #MLFC - all data - LOW COEFF
# SVM_syntspeak1_MLFC_IMF5_LOW<- train(y = yf, x = x_syntspeak1_MLFC_IMF5[,1:804], trControl = train_control, method = 'svmRadial', 
#                                      preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
# 
# SVM_syntspeak1_MLFC_IMF5_LOW$results
# SVM_syntspeak1_MLFC_IMF5_LOW$finalModel
# 
# CM_syntspeak1_MLFC_IMF5_LOW<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF5_LOW$pred, 
#                                                      SVM_syntspeak1_MLFC_IMF5_LOW$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5_LOW$bestTune[2]) )$pred[1:100],
#                                               subset(SVM_syntspeak1_MLFC_IMF5_LOW$pred, 
#                                                      SVM_syntspeak1_MLFC_IMF5_LOW$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF5_LOW$bestTune[2]) )$obs[1:100]  )
# CM_syntspeak1_MLFC_IMF5_LOW
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
