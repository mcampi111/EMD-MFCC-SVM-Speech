#EXTRACTING MLFCC CEPSTRAL COEFFICIENTS
library("seewave")
library("tuneR")
library('ggplot2')
library('lattice')
library('caret')
library('kernlab')

#MLFCC COEFF SPEAK1
data_speak1_wave<- list()

for (i in 1:m) {
  
  data_speak1_wave[[i]]<- Wave(data_speak1[i,])
}


mlfc_data_speak1<- list()

for (i in 1:m) {
  
  mlfc_data_speak1[[i]]<- melfcc(data_speak1_wave[[i]])
}


#MLFCC COEFF SYNT
data_synt_wave<- list()

for (i in 1:m) {
  
  data_synt_wave[[i]]<- Wave(data_synt[i,])
}


mlfc_data_synt<- list()

for (i in 1:m) {
  
  mlfc_data_synt[[i]]<- melfcc(data_synt_wave[[i]])
}


#SVM BETWEEN THESE TWO FEATURES ----------------------------------------------------------
m<- 100
y1f<- rep("uno",m)
y2f<- rep("zero",m)    
yf<- as.factor(c(y1f,y2f))

train_control<- trainControl(method="repeatedcv", number=2, classProbs=TRUE,  summaryFunction = twoClassSummary, 
                             savePredictions = TRUE, repeats = 1, search = 'grid')

#SVM SYNTHETIC vs SPEAKER 1  ---------------------------------------------------------------------------------------
#HIGH COEFFICIENTS
x_synt_MLFC<- matrix(unlist(mlfc_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC<- matrix(unlist(mlfc_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_MLFC<- rbind(x_synt_MLFC, x_speak1_MLFC)


colnames(x_syntspeak1_MLFC) <- paste("x_syntspeak1_MLFC", 1:ncol(x_syntspeak1_MLFC), sep="")

#MLFC - all data - HIGH COEFF
SVM_syntspeak1_MLFC_DATA_HIGH<- train(y = yf, x = x_syntspeak1_MLFC[,1:804], trControl = train_control, method = 'svmRadial', 
                           preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_MLFC_DATA_HIGH$results
SVM_syntspeak1_MLFC_DATA_HIGH$finalModel

CM_syntspeak1_MLFC_DATA_HIGH<- confusionMatrix(subset(SVM_syntspeak1_MLFC_DATA_HIGH$pred, 
                                                 SVM_syntspeak1_MLFC_DATA_HIGH$pred[,7] == as.double(SVM_syntspeak1_MLFC_DATA_HIGH$bestTune[2]) )$pred[1:100],
                                    subset(SVM_syntspeak1_MLFC_DATA_HIGH$pred, 
                                           SVM_syntspeak1_MLFC_DATA_HIGH$pred[,7] == as.double(SVM_syntspeak1_MLFC_DATA_HIGH$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_DATA_HIGH


#MLFC - all data - LOW COEFF
SVM_syntspeak1_MLFC_DATA_LOW<- train(y = yf, x = x_syntspeak1_MLFC[,805:1608], trControl = train_control, method = 'svmRadial', 
                                     preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)

SVM_syntspeak1_MLFC_DATA_LOW$results
SVM_syntspeak1_MLFC_DATA_LOW$finalModel

CM_syntspeak1_MLFC_DATA_LOW<- confusionMatrix(subset(SVM_syntspeak1_MLFC_DATA_LOW$pred, 
                                                     SVM_syntspeak1_MLFC_DATA_LOW$pred[,7] == as.double(SVM_syntspeak1_MLFC_DATA_LOW$bestTune[2]) )$pred[1:100],
                                              subset(SVM_syntspeak1_MLFC_DATA_LOW$pred, 
                                                     SVM_syntspeak1_MLFC_DATA_LOW$pred[,7] == as.double(SVM_syntspeak1_MLFC_DATA_LOW$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_DATA_LOW






