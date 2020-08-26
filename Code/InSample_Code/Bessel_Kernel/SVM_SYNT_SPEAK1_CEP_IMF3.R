library('ggplot2')
library('lattice')
library('caret')
library('kernlab')

m<- 100
y1f<- rep("uno",m)
y2f<- rep("zero",m)
yf<- as.factor(c(y1f,y2f))

train_control<- trainControl(method="repeatedcv", number=2, classProbs=TRUE,  summaryFunction = twoClassSummary,
                             savePredictions = TRUE, repeats = 1, search = 'grid')


#SVM IMF3 SPEAKER1 vs SYNTHETIC
#------------------------------------------------------------------

x_synt_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF3<- matrix(unlist(mlfc_IMF3_data_speak1), byrow=TRUE, nrow=100 )

x_syntspeak1_MLFC_IMF3<- rbind(x_synt_MLFC_IMF3, x_speak1_MLFC_IMF3)

colnames(x_syntspeak1_MLFC_IMF3) <- paste("x_syntspeak1_MLFC_IMF3", 1:ncol(x_syntspeak1_MLFC_IMF3), sep="")

#SVM IMF3 FOR EACH COEFFICIENT (12) --> 12 SVMs for IMF3
#----------------------------------------------------

SVM_syntspeak1_MLFC_IMF3_bess<- vector(mode="list", 12)



for (i in 1:12){
  
  SVM_syntspeak1_MLFC_IMF3_bess[[i]]<- train(y = yf, x = x_syntspeak1_MLFC_IMF3[,((134*i)-133):(134*i)],  trControl = train_control, method = bessSVM,
                                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
  
}

names(SVM_syntspeak1_MLFC_IMF3_bess) <- paste("SVM_syntspeak1_MLFC_IMF3_bess_coeff", 1:12, sep = "") 

#----------------------------------------------------------------
CM_syntspeak1_MLFC_IMF3_coeff1_bess<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_bess[[1]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[1]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[1]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[1]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[1]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[1]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[1]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[1]]$bestTune[1]))$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF3_bess[[1]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[1]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[1]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[1]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[1]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[1]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[1]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[1]]$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_coeff1_bess


CM_syntspeak1_MLFC_IMF3_coeff2_bess<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_bess[[2]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[2]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[2]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[2]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[2]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[2]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[2]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[2]]$bestTune[1]))$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF3_bess[[2]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[2]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[2]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[2]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[2]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[2]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[2]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[2]]$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_coeff2_bess

CM_syntspeak1_MLFC_IMF3_coeff3_bess<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_bess[[3]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[3]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[3]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[3]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[3]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[3]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[3]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[3]]$bestTune[1]))$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF3_bess[[3]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[3]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[3]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[3]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[3]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[3]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[3]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[3]]$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_coeff3_bess


CM_syntspeak1_MLFC_IMF3_coeff4_bess<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_bess[[4]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[4]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[4]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[4]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[4]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[4]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[4]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[4]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[4]]$bestTune[1]))$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF3_bess[[4]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[4]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[4]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[4]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[4]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[4]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[4]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[4]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[4]]$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_coeff4_bess


CM_syntspeak1_MLFC_IMF3_coeff5_bess<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_bess[[5]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[5]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[5]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[5]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[5]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[5]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[5]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[5]]$bestTune[1]))$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF3_bess[[5]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[5]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[5]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[5]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[5]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[5]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[5]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[5]]$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_coeff5_bess


CM_syntspeak1_MLFC_IMF3_coeff6_bess<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_bess[[6]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[6]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[6]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[6]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[6]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[6]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[6]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[6]]$bestTune[1]))$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF3_bess[[6]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[6]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[6]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[6]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[6]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[6]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[6]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[6]]$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_coeff6_bess


CM_syntspeak1_MLFC_IMF3_coeff7_bess<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_bess[[7]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[7]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[7]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[7]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[7]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[7]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[7]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[7]]$bestTune[1]))$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF3_bess[[7]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[7]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[7]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[7]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[7]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[7]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[7]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[7]]$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_coeff7_bess


CM_syntspeak1_MLFC_IMF3_coeff8_bess<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_bess[[8]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[8]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[8]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[8]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[8]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[8]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[8]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[8]]$bestTune[1]))$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF3_bess[[8]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[8]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[8]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[8]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[8]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[8]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[8]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[8]]$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_coeff8_bess


CM_syntspeak1_MLFC_IMF3_coeff9_bess<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_bess[[9]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[9]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[9]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[9]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[9]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[9]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[9]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[9]]$bestTune[1]))$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF3_bess[[9]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF3_bess[[9]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[9]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[9]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[9]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[9]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF3_bess[[9]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[9]]$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_coeff9_bess


CM_syntspeak1_MLFC_IMF3_coeff10_bess<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_bess[[10]]$pred, 
                                                              SVM_syntspeak1_MLFC_IMF3_bess[[10]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[10]]$bestTune[2]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[10]]$bestTune[4]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[10]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[10]]$bestTune[3]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[10]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[10]]$bestTune[1]))$pred[1:100],
                                                       subset(SVM_syntspeak1_MLFC_IMF3_bess[[10]]$pred, 
                                                              SVM_syntspeak1_MLFC_IMF3_bess[[10]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[10]]$bestTune[2]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[10]]$bestTune[4]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[10]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[10]]$bestTune[3]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[10]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[10]]$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_coeff10_bess


CM_syntspeak1_MLFC_IMF3_coeff11_bess<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_bess[[11]]$pred, 
                                                              SVM_syntspeak1_MLFC_IMF3_bess[[11]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[11]]$bestTune[2]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[11]]$bestTune[4]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[11]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[11]]$bestTune[3]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[11]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[11]]$bestTune[1]))$pred[1:100],
                                                       subset(SVM_syntspeak1_MLFC_IMF3_bess[[11]]$pred, 
                                                              SVM_syntspeak1_MLFC_IMF3_bess[[11]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[11]]$bestTune[2]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[11]]$bestTune[4]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[11]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[11]]$bestTune[3]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[11]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[11]]$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_coeff11_bess


CM_syntspeak1_MLFC_IMF3_coeff12_bess<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF3_bess[[12]]$pred, 
                                                              SVM_syntspeak1_MLFC_IMF3_bess[[12]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[12]]$bestTune[2]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[12]]$bestTune[4]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[12]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[12]]$bestTune[3]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[12]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[12]]$bestTune[1]))$pred[1:100],
                                                       subset(SVM_syntspeak1_MLFC_IMF3_bess[[12]]$pred, 
                                                              SVM_syntspeak1_MLFC_IMF3_bess[[12]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[12]]$bestTune[2]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[12]]$bestTune[4]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[12]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[12]]$bestTune[3]) &
                                                                SVM_syntspeak1_MLFC_IMF3_bess[[12]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF3_bess[[12]]$bestTune[1]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF3_coeff12_bess










