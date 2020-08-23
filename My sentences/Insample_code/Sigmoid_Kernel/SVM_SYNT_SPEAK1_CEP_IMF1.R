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


#SVM IMF1 SPEAKER1 vs SYNTHETIC
#------------------------------------------------------------------

x_synt_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_synt), byrow=TRUE, nrow=100 )
 
x_speak1_MLFC_IMF1<- matrix(unlist(mlfc_IMF1_data_speak1), byrow=TRUE, nrow=100 )
 
x_syntspeak1_MLFC_IMF1<- rbind(x_synt_MLFC_IMF1, x_speak1_MLFC_IMF1)
 
colnames(x_syntspeak1_MLFC_IMF1) <- paste("x_syntspeak1_MLFC_IMF1", 1:ncol(x_syntspeak1_MLFC_IMF1), sep="")

#SVM IMF1 FOR EACH COEFFICIENT (12) --> 12 SVMs for IMF1
#----------------------------------------------------

SVM_syntspeak1_MLFC_IMF1_tan<- vector(mode="list", 12)



for (i in 1:12){

  SVM_syntspeak1_MLFC_IMF1_tan[[i]]<- train(y = yf, x = x_syntspeak1_MLFC_IMF1[,((134*i)-133):(134*i)],  trControl = train_control, method = tanhSVM,
              preProcess = c("center", "scale"), metric = 'ROC', tuneLength = 9)
  
}

names(SVM_syntspeak1_MLFC_IMF1_tan) <- paste("SVM_syntspeak1_MLFC_IMF1_tan_coeff", 1:12, sep = "") 

#----------------------------------------------------------------

CM_syntspeak1_MLFC_IMF1_coeff1_tan<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_tan[[1]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1_tan[[1]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[1]]$bestTune[2]) & 
                                                          SVM_syntspeak1_MLFC_IMF1_tan[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[1]]$bestTune[3]) & 
                                                          SVM_syntspeak1_MLFC_IMF1_tan[[1]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[1]]$bestTune[1]) )$pred[1:100],
                                               subset(SVM_syntspeak1_MLFC_IMF1_tan[[1]]$pred, 
                                                      SVM_syntspeak1_MLFC_IMF1_tan[[1]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[1]]$bestTune[2]) &
                                                        SVM_syntspeak1_MLFC_IMF1_tan[[1]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[1]]$bestTune[3]) &
                                                        SVM_syntspeak1_MLFC_IMF1_tan[[1]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[1]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff1_tan


CM_syntspeak1_MLFC_IMF1_coeff2_tan<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_tan[[2]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[2]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[2]]$bestTune[2]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[2]]$bestTune[3]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[2]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[2]]$bestTune[1]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF1_tan[[2]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[2]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[2]]$bestTune[2]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[2]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[2]]$bestTune[3]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[2]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[2]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff2_tan


CM_syntspeak1_MLFC_IMF1_coeff3_tan<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_tan[[3]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[3]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[3]]$bestTune[2]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[3]]$bestTune[3]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[3]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[3]]$bestTune[1]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF1_tan[[3]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[3]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[3]]$bestTune[2]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[3]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[3]]$bestTune[3]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[3]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[3]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff3_tan


CM_syntspeak1_MLFC_IMF1_coeff4_tan<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_tan[[4]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1_tan[[4]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[4]]$bestTune[2]) )$pred[1:100],
                                                 subset(SVM_syntspeak1_MLFC_IMF1_tan[[4]]$pred, 
                                                        SVM_syntspeak1_MLFC_IMF1_tan[[4]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[4]]$bestTune[2]) )$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff4_tan


CM_syntspeak1_MLFC_IMF1_coeff5_tan<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_tan[[5]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[5]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[5]]$bestTune[2]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[5]]$bestTune[3]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[5]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[5]]$bestTune[1]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF1_tan[[5]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[5]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[5]]$bestTune[2]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[5]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[5]]$bestTune[3]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[5]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[5]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff5_tan


CM_syntspeak1_MLFC_IMF1_coeff6_tan<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_tan[[6]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[6]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[6]]$bestTune[2]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[6]]$bestTune[3]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[6]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[6]]$bestTune[1]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF1_tan[[6]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[6]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[6]]$bestTune[2]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[6]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[6]]$bestTune[3]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[6]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[6]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff6_tan


CM_syntspeak1_MLFC_IMF1_coeff7_tan<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_tan[[7]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[7]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[7]]$bestTune[2]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[7]]$bestTune[3]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[7]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[7]]$bestTune[1]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF1_tan[[7]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[7]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[7]]$bestTune[2]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[7]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[7]]$bestTune[3]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[7]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[7]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff7_tan

CM_syntspeak1_MLFC_IMF1_coeff8_tan<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_tan[[8]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[8]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[8]]$bestTune[2]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[8]]$bestTune[3]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[8]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[8]]$bestTune[1]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF1_tan[[8]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[8]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[8]]$bestTune[2]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[8]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[8]]$bestTune[3]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[8]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[8]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff8_tan


CM_syntspeak1_MLFC_IMF1_coeff9_tan<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_tan[[9]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[9]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[9]]$bestTune[2]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[9]]$bestTune[3]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[9]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[9]]$bestTune[1]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF1_tan[[9]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[9]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[9]]$bestTune[2]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[9]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[9]]$bestTune[3]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[9]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[9]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff9_tan


CM_syntspeak1_MLFC_IMF1_coeff10_tan<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_tan[[10]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[10]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[10]]$bestTune[2]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[10]]$bestTune[3]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[10]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[10]]$bestTune[1]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF1_tan[[10]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[10]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[10]]$bestTune[2]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[10]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[10]]$bestTune[3]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[10]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[10]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff10_tan


CM_syntspeak1_MLFC_IMF1_coeff11_tan<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_tan[[11]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[11]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[11]]$bestTune[2]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[11]]$bestTune[3]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[11]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[11]]$bestTune[1]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF1_tan[[11]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[11]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[11]]$bestTune[2]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[11]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[11]]$bestTune[3]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[11]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[11]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff11_tan


CM_syntspeak1_MLFC_IMF1_coeff12_tan<- confusionMatrix(subset(SVM_syntspeak1_MLFC_IMF1_tan[[12]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[12]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[12]]$bestTune[2]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[12]]$bestTune[3]) & 
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[12]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[12]]$bestTune[1]) )$pred[1:100],
                                                     subset(SVM_syntspeak1_MLFC_IMF1_tan[[12]]$pred, 
                                                            SVM_syntspeak1_MLFC_IMF1_tan[[12]]$pred[,6] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[12]]$bestTune[2]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[12]]$pred[,7] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[12]]$bestTune[3]) &
                                                              SVM_syntspeak1_MLFC_IMF1_tan[[12]]$pred[,8] == as.double(SVM_syntspeak1_MLFC_IMF1_tan[[12]]$bestTune[1]))$obs[1:100]  )
CM_syntspeak1_MLFC_IMF1_coeff12_tan










