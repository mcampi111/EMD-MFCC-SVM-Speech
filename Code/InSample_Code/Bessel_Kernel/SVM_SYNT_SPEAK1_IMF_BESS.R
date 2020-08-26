#SVM SYNT - SPEAKER1 - T = 132278, m = 100   ---- FEATURE: IMFs --- KERNEL: RBASIS   - SCALE : TRUE
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



#SVM SYNTHETIC vs SPEAKER 1  ---------------------------------------------------------------------------------------
x_synt_IMF<- matrix(unlist(feat_IMF_data_synt), byrow=TRUE, nrow=100 )

x_speak1_IMF<- matrix(unlist(feat_IMF_data_speak1), byrow=TRUE, nrow=100 )


x_syntspeak1_IMF<- rbind(x_synt_IMF, x_speak1_IMF)


colnames(x_syntspeak1_IMF) <- paste("x_syntspeak1_IMF", 1:ncol(x_syntspeak1_IMF), sep="")


#IMF - FIRST IMF - bess-----------------------------
svm<- list()
param<- c()

for (i in seq(4000, 60000, by=4000)) {
  
  svm<- train(y = yf, x = x_syntspeak1_IMF[,(i-4999):i], trControl = train_control, method = bessSVM, 
              metric = 'ROC') # preProcess = c("center", "scale"),
  
  param<- rbind(param,svm$bestTune)
  
  
  rm(svm)
}


xmat<- matrix(0,200,200)
xx<- c()
k<- list()
d<- 0

for (j in (0:(dim(param)[1]-1))) {
  
  
  k<- c(k,besseldot(param$sigma[j+1], param$order[j+1], param$degree[j+1]))
  
}


for (i in seq(4000, 60000, by=4000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_syntspeak1_IMF[,(i-3999):i]))
  xmat<- xmat+xx
  
  
}

# xmat2<- xmat
# xmat2[is.na(xmat2)]<- 0

SVM_syntspeak1_IMF1_bess<- ksvm(x = xmat, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_IMF1_bess

CM_syntspeak1_IMF1_bess<- confusionMatrix(SVM_syntspeak1_IMF1_bess@fitted, yf )
CM_syntspeak1_IMF1_bess

rm(xx, xmat, param, d, k,i,j, xmat2)


#IMF - SECOND IMF - bess -----------------

svm<- list()
param<- c()

for (i in seq(64000, 120000, by=4000)) {
  
  svm<- train(y = yf, x = x_syntspeak1_IMF[,(i-3999):i], trControl = train_control, method = bessSVM, 
               metric = 'ROC') #preProcess = c("center", "scale"),
  
  param<- rbind(param,svm$bestTune)
  
  
  rm(svm)
}


xmat<- matrix(0,200,200)
xx<- c()
k<- list()
d<- 0

for (j in (0:(dim(param)[1]-1))) {
  
  
  k<- c(k,besseldot(param$sigma[j+1], param$order[j+1], param$degree[j+1]))
  
}



for (i in seq(64000, 120000, by=4000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_syntspeak1_IMF[,(i-3999):i]))
  xmat<- xmat+xx
  
  
}

# xmat2<- xmat
# xmat2[is.na(xmat2)]<- 0


SVM_syntspeak1_IMF2_bess<- ksvm(x = xmat, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_IMF2_bess

CM_syntspeak1_IMF2_bess<- confusionMatrix(SVM_syntspeak1_IMF2_bess@fitted, yf )
CM_syntspeak1_IMF2_bess

rm(xx, xmat, param, d, k,i,j, xmat2)

#IMF - THIRD IMF - bess -----------------

svm<- list()
param<- c()

for (i in seq(124000, 180000, by=4000)) {
  
  svm<- train(y = yf, x = x_syntspeak1_IMF[,(i-3999):i], trControl = train_control, method = bessSVM, 
             metric = 'ROC') # preProcess = c("center", "scale"), 
  
  param<- rbind(param,svm$bestTune)
  
  
  rm(svm)
}


xmat<- matrix(0,200,200)
xx<- c()
k<- list()
d<- 0

for (j in (0:(dim(param)[1]-1))) {
  
  
  k<- c(k,besseldot(param$sigma[j+1], param$order[j+1], param$degree[j+1]))
  
}



for (i in seq(124000, 180000, by=4000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_syntspeak1_IMF[,(i-3999):i]))
  xmat<- xmat+xx
  
  
}

# xmat2<- xmat
# xmat2[is.na(xmat2)]<- 0

SVM_syntspeak1_IMF3_bess<- ksvm(x = xmat, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_IMF3_bess

CM_syntspeak1_IMF3_bess<- confusionMatrix(SVM_syntspeak1_IMF3_bess@fitted, yf )
CM_syntspeak1_IMF3_bess

rm(xx, xmat, param, d, k,i,j, xmat2)


#IMF - FOURTH IMF - bess -----------------

svm<- list()
param<- c()

for (i in seq(180500, 240000, by=500)) {
  
  svm<- train(y = yf, x = x_syntspeak1_IMF[,(i-499):i], trControl = train_control, method = bessSVM, 
               metric = 'ROC')  #preProcess = c("center", "scale"),
  
  param<- rbind(param,svm$bestTune)
  
  
  rm(svm)
}


xmat<- matrix(0,200,200)
xx<- c()
k<- list()
d<- 0

for (j in (0:(dim(param)[1]-1))) {
  
  
  k<- c(k,besseldot(param$sigma[j+1], param$order[j+1], param$degree[j+1]))
  
}



for (i in seq(184000, 240000, by=4000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_syntspeak1_IMF[,(i-3999):i]))
  xmat<- xmat+xx
  
  
}

 xmat2<- xmat
 xmat2[is.na(xmat2)]<- 0


SVM_syntspeak1_IMF4_bess<- ksvm(x = xmat2, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_IMF4_bess

CM_syntspeak1_IMF4_bess<- confusionMatrix(SVM_syntspeak1_IMF4_bess@fitted, yf )
CM_syntspeak1_IMF4_bess

rm(xx, xmat, param, d, k,i,j, xmat2)

#IMF - FIMFTH IMF - bess -----------------

svm<- list()
param<- c()

for (i in seq(244000, 300000, by=4000)) {
  
  svm<- train(y = yf, x = x_syntspeak1_IMF[,(i-3999):i], trControl = train_control, method = bessSVM, 
               metric = 'ROC') #preProcess = c("center", "scale"),
  
  param<- rbind(param,svm$bestTune)
  
  
  rm(svm)
}


xmat<- matrix(0,200,200)
xx<- c()
k<- list()
d<- 0

for (j in (0:(dim(param)[1]-1))) {
  
  
  k<- c(k,besseldot(param$sigma[j+1], param$order[j+1], param$degree[j+1]))
  
}



for (i in seq(244000, 300000, by=4000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_syntspeak1_IMF[,(i-3999):i]))
  xmat<- xmat+xx
  
  
}

# xmat2<- xmat
# xmat2[is.na(xmat2)]<- 0

SVM_syntspeak1_IMF5_bess<- ksvm(x = xmat, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_IMF5_bess

CM_syntspeak1_IMF5_bess<- confusionMatrix(SVM_syntspeak1_IMF5_bess@fitted, yf )
CM_syntspeak1_IMF5_bess

rm(xx, xmat, param, d, k,i,j, xmat2)
