#SVM SYNT vs SPEAKER1 - T = 132278, m = 100   ---- FEATURE: IF --- KERNEL: RBASIS   - SCALE : TRUE
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



#SVM SYNT vs SPEAKER1   -----------------------------------------------------------------------------------------

x_synt_IF<- matrix(unlist(feat_synt_IF), byrow=TRUE, nrow=100 )


x_speak1_IF<- matrix(unlist(feat_speak1_IF), byrow=TRUE, nrow=100 )



x_syntspeak1_IF<- rbind(x_synt_IF, x_speak1_IF)

colnames(x_syntspeak1_IF) <- paste("x_syntspeak1_IF", 1:ncol(x_syntspeak1_IF), sep="")


#IF - FIRST IMF - LP-----------------------------
svm<- list()
param<- c()
ss<- list()

for (i in seq(3000, 60000, by=3000)) {
  
  svm<- train(y = yf, x = x_syntspeak1_IF[,(i-2999):i], trControl = train_control, method = lpSVM, 
               metric = 'ROC') #preProcess = c("center", "scale"),
  
  param<- rbind(param,svm$bestTune)
#  ss<- list(ss,svm)
  
  rm(svm)
}


xmat<- matrix(0,200,200)
xx<- c()
k<- list()
d<- 0

for (j in (0:(dim(param)[1]-1))) {
  
  
  k<- c(k,laplacedot(param$sigma[j+1]))
  
}


for (i in seq(3000, 60000, by=3000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_syntspeak1_IF[,(i-2999):i]))
  xmat<- xmat+xx
  
  
}


SVM_syntspeak1_IF1_lp<- ksvm(x = xmat, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_IF1_lp

CM_syntspeak1_IF1_lp<- confusionMatrix(SVM_syntspeak1_IF1_lp@fitted, yf )
CM_syntspeak1_IF1_lp

rm(xx, xmat, param, d, k,i,j)


#IF - SECOND IMF - LP -----------------

svm<- list()
param<- c()

for (i in seq(63000, 120000, by=3000)) {
  
  svm<- train(y = yf, x = x_syntspeak1_IF[,(i-2999):i], trControl = train_control, method = lpSVM, 
               metric = 'ROC') #preProcess = c("center", "scale"),
  
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



for (i in seq(63000, 120000, by=3000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_syntspeak1_IF[,(i-2999):i]))
  xmat<- xmat+xx
  
  
}


SVM_syntspeak1_IF2_lp<- ksvm(x = xmat, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_IF2_lp

CM_syntspeak1_IF2_lp<- confusionMatrix(SVM_syntspeak1_IF2_lp@fitted, yf )
CM_syntspeak1_IF2_lp

rm(xx, xmat, param, d, k,i,j)

#IF - THIRD IMF - LP -----------------

svm<- list()
param<- c()

for (i in seq(123000, 180000, by=3000)) {
  
  svm<- train(y = yf, x = x_syntspeak1_IF[,(i-2999):i], trControl = train_control, method = lpSVM, 
               metric = 'ROC') #preProcess = c("center", "scale"),
  
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



for (i in seq(123000, 180000, by=3000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_syntspeak1_IF[,(i-2999):i]))
  xmat<- xmat+xx
  
  
}


SVM_syntspeak1_IF3_lp<- ksvm(x = xmat, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_IF3_lp

CM_syntspeak1_IF3_lp<- confusionMatrix(SVM_syntspeak1_IF3_lp@fitted, yf )
CM_syntspeak1_IF3_lp

rm(xx, xmat, param, d, k,i,j)


#IF - FOURTH IMF - LP -----------------

svm<- list()
param<- c()

for (i in seq(183000, 240000, by=3000)) {
  
  svm<- train(y = yf, x = x_syntspeak1_IF[,(i-2999):i], trControl = train_control, method = lpSVM, 
               metric = 'ROC')  #preProcess = c("center", "scale"),
  
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



for (i in seq(183000, 240000, by=3000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_syntspeak1_IF[,(i-2999):i]))
  xmat<- xmat+xx
  
  
}


SVM_syntspeak1_IF4_lp<- ksvm(x = xmat, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_IF4_lp

CM_syntspeak1_IF4_lp<- confusionMatrix(SVM_syntspeak1_IF4_lp@fitted, yf )
CM_syntspeak1_IF4_lp

rm(xx, xmat, param, d, k,i,j)

#IF - FIFTH IMF - LP -----------------

svm<- list()
param<- c()

for (i in seq(243000, 300000, by=3000)) {
  
  svm<- train(y = yf, x = x_syntspeak1_IF[,(i-2999):i], trControl = train_control, method = lpSVM, 
              metric = 'ROC') #preProcess = c("center", "scale"), 
  
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



for (i in seq(243000, 300000, by=3000)) {
  
  d<- d+1
  xx<- kernelMatrix(k[[d]],as.matrix(x_syntspeak1_IF[,(i-2999):i]))
  xmat<- xmat+xx
  
  
}


SVM_syntspeak1_IF5_lp<- ksvm(x = xmat, y = yf, kernel = "matrix", type = "C-svc")
SVM_syntspeak1_IF5_lp

CM_syntspeak1_IF5_lp<- confusionMatrix(SVM_syntspeak1_IF5_lp@fitted, yf )
CM_syntspeak1_IF5_lp

rm(xx, xmat, param, d, k,i,j)
