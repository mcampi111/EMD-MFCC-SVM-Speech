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

train_control<- trainControl(method="repeatedcv", number=2, classProbs=TRUE,  summaryFunction = twoClassSummary, 
                             savePredictions = TRUE, repeats = 1, search = 'grid')



#SVM SYNTHETIC vs SPEAKER 1 ------------------------------------------------------------------------------------------

x_SYNT_SC<- feat_synt_coeff

x_SPEAK1_SC<- feat_speak1_coeff



x_syntspeak1_SC<- rbind(x_SYNT_SC, x_SPEAK1_SC)

colnames(x_syntspeak1_SC) <- paste("x_syntspeak1_SC", 1:ncol(x_syntspeak1_SC), sep="")


#SC - FIRST IMF
x_sc1<- cbind(x_syntspeak1_SC[,1:60000],x_syntspeak1_SC[,300001:360000],  x_syntspeak1_SC[,600001:660000])

svm<- list()
param<- c()

for (i in seq(6000, 180000, by=6000)) {
  
  svm<- train(y = yf, x = x_sc1[,(i-5999):i], trControl = train_control, method = bessSVM, 
              preProcess = c("center", "scale"), metric = 'ROC') 
  
  param<- rbind(param,svm$bestTune)
  
  
  rm(svm)
}


xmat<- matrix(0,200,200)
xx<- c()
k<- list()
d<- 0

for (j in (0:(dim(param)[1]-1))) {
  
  
  k<- c(k,laplacedot(param$sigma[j+1]))
  
}


for (i in seq(6000, 180000, by=6000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_sc1[,(i-5999):i]))
  xmat<- xmat+xx
  
  
}

xmat2<- xmat
xmat2[is.na(xmat2)]<- 0

SVM_syntspeak1_SC1_bess<- ksvm(x = xmat2, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_SC1_bess

CM_syntspeak1_SC1_bess<- confusionMatrix(SVM_syntspeak1_SC1_bess@fitted, yf )
CM_syntspeak1_SC1_bess

rm(xx, xmat, param, d, k,i,j, xmat2)


#SC - SECOND IMF
x_sc2<- cbind(x_syntspeak1_SC[,60001:120000],x_syntspeak1_SC[,360001:420000],  x_syntspeak1_SC[,660001:720000])

svm<- list()
param<- c()

for (i in seq(6000, 180000, by=6000)) {
  
  svm<- train(y = yf, x = x_sc2[,(i-5999):i], trControl = train_control, method = bessSVM, 
              preProcess = c("center", "scale"), metric = 'ROC') 
  
  param<- rbind(param,svm$bestTune)
  
  
  rm(svm)
}


xmat<- matrix(0,200,200)
xx<- c()
k<- list()
d<- 0

for (j in (0:(dim(param)[1]-1))) {
  
  
  k<- c(k,laplacedot(param$sigma[j+1]))
  
}


for (i in seq(6000, 180000, by=6000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_sc2[,(i-5999):i]))
  xmat<- xmat+xx
  
  
}

xmat2<- xmat
xmat2[is.na(xmat2)]<- 0

SVM_syntspeak1_SC2_bess<- ksvm(x = xmat2, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_SC2_bess

CM_syntspeak1_SC2_bess<- confusionMatrix(SVM_syntspeak1_SC2_bess@fitted, yf )
CM_syntspeak1_SC2_bess

rm(xx, xmat, param, d, k,i,j,xmat2)

#SC - THIRD IMF
x_sc3<- cbind(x_syntspeak1_SC[,120001:180000],x_syntspeak1_SC[,420001:480000],  x_syntspeak1_SC[,720001:780000])

svm<- list()
param<- c()

for (i in seq(6000, 180000, by=6000)) {
  
  svm<- train(y = yf, x = x_sc3[,(i-5999):i], trControl = train_control, method = bessSVM, 
              preProcess = c("center", "scale"), metric = 'ROC') 
  
  param<- rbind(param,svm$bestTune)
  
  
  rm(svm)
}


xmat<- matrix(0,200,200)
xx<- c()
k<- list()
d<- 0

for (j in (0:(dim(param)[1]-1))) {
  
  
  k<- c(k,laplacedot(param$sigma[j+1]))
  
}


for (i in seq(6000, 180000, by=6000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_sc3[,(i-5999):i]))
  xmat<- xmat+xx
  
  
}

xmat2<- xmat
xmat2[is.na(xmat2)]<- 0

SVM_syntspeak1_SC3_bess<- ksvm(x = xmat2, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_SC3_bess

CM_syntspeak1_SC3_bess<- confusionMatrix(SVM_syntspeak1_SC3_bess@fitted, yf )
CM_syntspeak1_SC3_bess

rm(xx, xmat, param, d, k,i,j,xmat2)


#SC - FOURTH IMF
x_sc4<- cbind(x_syntspeak1_SC[,180001:240000],x_syntspeak1_SC[,480001:540000],  x_syntspeak1_SC[,780001:840000])

svm<- list()
param<- c()

for (i in seq(6000, 180000, by=6000)) {
  
  svm<- train(y = yf, x = x_sc4[,(i-5999):i], trControl = train_control, method = bessSVM, 
              preProcess = c("center", "scale"), metric = 'ROC') 
  
  param<- rbind(param,svm$bestTune)
  
  
  rm(svm)
}


xmat<- matrix(0,200,200)
xx<- c()
k<- list()
d<- 0

for (j in (0:(dim(param)[1]-1))) {
  
  
  k<- c(k,laplacedot(param$sigma[j+1]))
  
}


for (i in seq(6000, 180000, by=6000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_sc4[,(i-5999):i]))
  xmat<- xmat+xx
  
  
}

xmat2<- xmat
xmat2[is.na(xmat2)]<- 0

SVM_syntspeak1_SC4_bess<- ksvm(x = xmat2, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_SC4_bess

CM_syntspeak1_SC4_bess<- confusionMatrix(SVM_syntspeak1_SC4_bess@fitted, yf )
CM_syntspeak1_SC4_bess

rm(xx, xmat, param, d, k,i,j,xmat2)

#SC - FIFTH IMF
x_sc5<- cbind(x_syntspeak1_SC[,240001:300000],x_syntspeak1_SC[,540001:600000],  x_syntspeak1_SC[,840001:900000])

svm<- list()
param<- c()

for (i in seq(6000, 180000, by=6000)) {
  
  svm<- train(y = yf, x = x_sc5[,(i-5999):i], trControl = train_control, method = bessSVM, 
              preProcess = c("center", "scale"), metric = 'ROC') 
  
  param<- rbind(param,svm$bestTune)
  
  
  rm(svm)
}


xmat<- matrix(0,200,200)
xx<- c()
k<- list()
d<- 0

for (j in (0:(dim(param)[1]-1))) {
  
  
  k<- c(k,laplacedot(param$sigma[j+1]))
  
}


for (i in seq(6000, 180000, by=6000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_sc5[,(i-5999):i]))
  xmat<- xmat+xx
  
  
}

xmat2<- xmat
xmat2[is.na(xmat2)]<- 0

SVM_syntspeak1_SC5_bess<- ksvm(x = xmat2, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_SC5_bess

CM_syntspeak1_SC5_bess<- confusionMatrix(SVM_syntspeak1_SC5_bess@fitted, yf )
CM_syntspeak1_SC5_bess

rm(xx, xmat, param, d, k,i,j,xmat2)


