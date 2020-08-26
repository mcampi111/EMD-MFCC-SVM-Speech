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


#SVM IMF2 SPEAKER1 vs SYNTHETIC
#------------------------------------------------------------------

x_synt_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_synt), byrow=TRUE, nrow=100 )

x_speak1_MLFC_IMF2<- matrix(unlist(mlfc_IMF2_data_speak1), byrow=TRUE, nrow=100 )

x_syntspeak1_MLFC_IMF2<- rbind(x_synt_MLFC_IMF2, x_speak1_MLFC_IMF2)

colnames(x_syntspeak1_MLFC_IMF2) <- paste("x_syntspeak1_MLFC_IMF2", 1:ncol(x_syntspeak1_MLFC_IMF2), sep="")

#SVM IMF2 FOR EACH COEFFICIENT (12) --> 12 SVMs for IMF2
#----------------------------------------------------

SVM_syntspeak1_MLFC_IMF2_poly<- vector(mode="list", 12)



for (i in 1:12){
  
  SVM_syntspeak1_MLFC_IMF2_poly[[i]]<- train(y = yf, x = x_syntspeak1_MLFC_IMF2[,((134*i)-133):(134*i)],  trControl = train_control, method = polySVM,
                                             preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
  
}

names(SVM_syntspeak1_MLFC_IMF2_poly) <- paste("SVM_syntspeak1_MLFC_IMF2_poly_coeff", 1:12, sep = "") 

#----------------------------------------------------------------

CM_syntspeak1_MLFC_IMF2_coeff1_poly<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_poly[[1]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[1]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[1]]$bestTune[2]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[1]]$bestTune[3]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[1]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[1]]$bestTune[4])& 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[1]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[1]]$bestTune[1])  )$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF2_poly[[1]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[1]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[1]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[1]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[1]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[1]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[1]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[1]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff1_poly


CM_syntspeak1_MLFC_IMF2_coeff2_poly<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_poly[[2]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[2]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[2]]$bestTune[2]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[2]]$bestTune[3]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[2]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[2]]$bestTune[4])& 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[2]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[2]]$bestTune[1])  )$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF2_poly[[2]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[2]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[2]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[2]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[2]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[2]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[2]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[2]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff2_poly


CM_syntspeak1_MLFC_IMF2_coeff3_poly<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_poly[[3]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[3]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[3]]$bestTune[2]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[3]]$bestTune[3]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[3]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[3]]$bestTune[4])& 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[3]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[3]]$bestTune[1])  )$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF2_poly[[3]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[3]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[3]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[3]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[3]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[3]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[3]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[3]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff3_poly



CM_syntspeak1_MLFC_IMF2_coeff4_poly<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_poly[[4]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[4]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[4]]$bestTune[2]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[4]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[4]]$bestTune[3]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[4]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[4]]$bestTune[4])& 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[4]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[4]]$bestTune[1])  )$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF2_poly[[4]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[4]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[4]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[4]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[4]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[4]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[4]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[4]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[4]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff4_poly


CM_syntspeak1_MLFC_IMF2_coeff5_poly<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_poly[[5]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[5]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[5]]$bestTune[2]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[5]]$bestTune[3]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[5]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[5]]$bestTune[4])& 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[5]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[5]]$bestTune[1])  )$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF2_poly[[5]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[5]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[5]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[5]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[5]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[5]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[5]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[5]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff5_poly

CM_syntspeak1_MLFC_IMF2_coeff6_poly<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_poly[[6]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[6]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[6]]$bestTune[2]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[6]]$bestTune[3]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[6]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[6]]$bestTune[4])& 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[6]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[6]]$bestTune[1])  )$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF2_poly[[6]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[6]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[6]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[6]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[6]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[6]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[6]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[6]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff6_poly

CM_syntspeak1_MLFC_IMF2_coeff7_poly<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_poly[[7]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[7]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[7]]$bestTune[2]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[7]]$bestTune[3]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[7]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[7]]$bestTune[4])& 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[7]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[7]]$bestTune[1])  )$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF2_poly[[7]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[7]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[7]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[7]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[7]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[7]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[7]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[7]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff7_poly

CM_syntspeak1_MLFC_IMF2_coeff8_poly<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_poly[[8]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[8]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[8]]$bestTune[2]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[8]]$bestTune[3]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[8]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[8]]$bestTune[4])& 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[8]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[8]]$bestTune[1])  )$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF2_poly[[8]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[8]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[8]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[8]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[8]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[8]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[8]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[8]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff8_poly

CM_syntspeak1_MLFC_IMF2_coeff9_poly<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_poly[[9]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[9]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[9]]$bestTune[2]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[9]]$bestTune[3]) & 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[9]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[9]]$bestTune[4])& 
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[9]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[9]]$bestTune[1])  )$pred[1:100],
                                                      subset(SVM_syntspeak1_MLFC_IMF2_poly[[9]]$pred, 
                                                             SVM_syntspeak1_MLFC_IMF2_poly[[9]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[9]]$bestTune[2]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[9]]$bestTune[3]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[9]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[9]]$bestTune[4]) &
                                                               SVM_syntspeak1_MLFC_IMF2_poly[[9]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[9]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff9_poly


CM_syntspeak1_MLFC_IMF2_coeff10_poly<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_poly[[10]]$pred, 
                                                              SVM_syntspeak1_MLFC_IMF2_poly[[10]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[10]]$bestTune[2]) & 
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[10]]$bestTune[3]) & 
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[10]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[10]]$bestTune[4])& 
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[10]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[10]]$bestTune[1])  )$pred[1:100],
                                                       subset(SVM_syntspeak1_MLFC_IMF2_poly[[10]]$pred, 
                                                              SVM_syntspeak1_MLFC_IMF2_poly[[10]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[10]]$bestTune[2]) &
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[10]]$bestTune[3]) &
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[10]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[10]]$bestTune[4]) &
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[10]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[10]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff10_poly

CM_syntspeak1_MLFC_IMF2_coeff11_poly<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_poly[[11]]$pred, 
                                                              SVM_syntspeak1_MLFC_IMF2_poly[[11]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[11]]$bestTune[2]) & 
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[11]]$bestTune[3]) & 
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[11]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[11]]$bestTune[4])& 
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[11]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[11]]$bestTune[1])  )$pred[1:100],
                                                       subset(SVM_syntspeak1_MLFC_IMF2_poly[[11]]$pred, 
                                                              SVM_syntspeak1_MLFC_IMF2_poly[[11]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[11]]$bestTune[2]) &
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[11]]$bestTune[3]) &
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[11]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[11]]$bestTune[4]) &
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[11]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[11]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff11_poly


CM_syntspeak1_MLFC_IMF2_coeff12_poly<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF2_poly[[12]]$pred, 
                                                              SVM_syntspeak1_MLFC_IMF2_poly[[12]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[12]]$bestTune[2]) & 
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[12]]$bestTune[3]) & 
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[12]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[12]]$bestTune[4])& 
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[12]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[12]]$bestTune[1])  )$pred[1:100],
                                                       subset(SVM_syntspeak1_MLFC_IMF2_poly[[12]]$pred, 
                                                              SVM_syntspeak1_MLFC_IMF2_poly[[12]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[12]]$bestTune[2]) &
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[12]]$bestTune[3]) &
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[12]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[12]]$bestTune[4]) &
                                                                SVM_syntspeak1_MLFC_IMF2_poly[[12]]$pred[,9] == as.double(SVM_syntspeak1_MLFC_IMF2_poly[[12]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF2_coeff12_poly









