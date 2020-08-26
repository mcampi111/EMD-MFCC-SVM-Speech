#SVM OUT OF SAMPLE - SYNT vs SPEAKER1 ---  FEATURE: IF --- KERNEL: RBASIS  
library("ROCR")
#library("pROC")
library('ggplot2')
library('lattice')
library('caret')
library('kernlab')

m2<- 20
y11f<- rep("uno",m2)
y22f<- rep("zero",m2)
yff<- as.factor(c(y11f,y22f))


#-----------------------------------------------------------------------------------------------------
x_synt_IF_new<- matrix(unlist(feat_synt_IF_new), byrow=TRUE, nrow=20 )


x_speak1_IF_new<- matrix(unlist(feat_speak1_IF_new), byrow=TRUE, nrow=20 )


x_syntspeak1_IF_new<- rbind(x_synt_IF_new, x_speak1_IF_new)

colnames(x_syntspeak1_IF_new) <- paste("x_syntspeak1_IF", 1:ncol(x_syntspeak1_IF_new), sep="")


#IF - FIRST IMF

ypred_syntspeak1_IF1<- predict(SVM_syntspeak1_IF1_vann,newdata = x_syntspeak1_IF_new[,1:60000])

CM_syntspeak1_IF1_new_vann<- confusionMatrix(ypred_syntspeak1_IF1, yff)
CM_syntspeak1_IF1_new_vann

predvec <- ifelse(ypred_syntspeak1_IF1=="uno", 1, 0)
realvec <- ifelse(yff =="uno", 1, 0)
pr1 <- prediction(predvec, realvec)
prf1 <- performance(pr1, measure="tpr",  x.measure="fpr")
auc1 <- performance(pr1,"auc")
auc1 <- unlist(slot(auc1, "y.values"))
auct1 <- paste(c("AUC  = "), auc1,sep="")
plot(prf1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     main = "ROC - IMF1  ")
abline(a=0, b= 1, lty = 3, col = "black")
mtext(auct1, side = 1, line = -12, col = "red")
rm(predvec,realvec)

#IF - SECOND IMF

ypred_syntspeak1_IF2<- predict(SVM_syntspeak1_IF2_vann,newdata = x_syntspeak1_IF_new[,60001:120000])

CM_syntspeak1_IF2_new_vann<- confusionMatrix(ypred_syntspeak1_IF2, yff)
CM_syntspeak1_IF2_new_vann

predvec <- ifelse(ypred_syntspeak1_IF2=="uno", 1, 0)
realvec <- ifelse(yff =="uno", 1, 0)
pr2 <- prediction(predvec, realvec)
prf2 <- performance(pr2, measure="tpr",  x.measure="fpr")
auc2 <- performance(pr2,"auc")
auc2 <- unlist(slot(auc2, "y.values"))
auct2 <- paste(c("AUC  = "), auc2,sep="")
plot(prf2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     main = "ROC - IMF2 ")
abline(a=0, b= 1, lty = 3, col = "black")
mtext(auct2, side = 1, line = -12, col = "red")
rm(predvec,realvec)

#IF - THIRD IMF

ypred_syntspeak1_IF3<- predict(SVM_syntspeak1_IF3_vann,newdata = x_syntspeak1_IF_new[,120001:180000])

CM_syntspeak1_IF3_new_vann<- confusionMatrix(ypred_syntspeak1_IF3, yff)
CM_syntspeak1_IF3_new_vann

predvec <- ifelse(ypred_syntspeak1_IF3=="uno", 1, 0)
realvec <- ifelse(yff =="uno", 1, 0)
pr3 <- prediction(predvec, realvec)
prf3 <- performance(pr3, measure="tpr",  x.measure="fpr")
auc3 <- performance(pr3,"auc")
auc3 <- unlist(slot(auc3, "y.values"))
auct3 <- paste(c("AUC  = "), auc3,sep="")
plot(prf3, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     main = "ROC - IMF3  ")
abline(a=0, b= 1, lty = 3, col = "black")
mtext(auct3, side = 1, line = -12, col = "red")
rm(predvec,realvec)


#IF - FOURTH IMF

ypred_syntspeak1_IF4<- predict(SVM_syntspeak1_IF4_vann,newdata = x_syntspeak1_IF_new[,180001:240000])

CM_syntspeak1_IF4_new_vann<- confusionMatrix(ypred_syntspeak1_IF4, yff)
CM_syntspeak1_IF4_new_vann

predvec <- ifelse(ypred_syntspeak1_IF4=="uno", 1, 0)
realvec <- ifelse(yff =="uno", 1, 0)
pr4 <- prediction(predvec, realvec)
prf4 <- performance(pr4, measure="tpr",  x.measure="fpr")
auc4 <- performance(pr4,"auc")
auc4 <- unlist(slot(auc4, "y.values"))
auct4 <- paste(c("AUC  = "), auc4,sep="")
plot(prf4, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     main = "ROC - IMF3 - Instantaneous Frequency ")
abline(a=0, b= 1, lty = 3, col = "black")
mtext(auct4, side = 1, line = -12, col = "red")
rm(predvec,realvec)

#IF - FIFTH IMF

ypred_syntspeak1_IF5<- predict(SVM_syntspeak1_IF5_vann,newdata = x_syntspeak1_IF_new[,240001:300000])

CM_syntspeak1_IF5_new_vann<- confusionMatrix(ypred_syntspeak1_IF5, yff)
CM_syntspeak1_IF5_new_vann


predvec <- ifelse(ypred_syntspeak1_IF5=="uno", 1, 0)
realvec <- ifelse(yff =="uno", 1, 0)
pr5 <- prediction(predvec, realvec)
prf5 <- performance(pr5, measure="tpr",  x.measure="fpr")
auc5 <- performance(pr5,"auc")
auc5 <- unlist(slot(auc5, "y.values"))
auct5 <- paste(c("AUC  = "), auc5,sep="")
plot(prf5, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     main = "ROC - IMF5  ")
abline(a=0, b= 1, lty = 3, col = "black")
mtext(auct5, side = 1, line = -12, col = "red")
rm(predvec,realvec)


xx<- c(0,0,1,1)
yy<- c(1,0,0,1)

attach(mtcars)
par(mfrow=c(3,2),oma = c(0, 0, 2, 0))

plot(prf1, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
      lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf1, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "IF1", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct1, side = 3, line = -4, col = "blue", cex = 0.7)


plot(prf2, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
      lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf2, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "IF2", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct2, side = 3, line = -4, col = "blue", cex = 0.7)


plot(prf3, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
      lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf3, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "IF3", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct3, side = 3, line = -4, col = "blue", cex = 0.7)

plot(prf4, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
      lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf4, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "IF4", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct4, side = 3, line = -4, col = "blue", cex = 0.7)

plot(prf5, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf5, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "IF5", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct5, side = 3, line = -4, col = "blue", cex = 0.7)

mtext("ROC SYNT vs SPEAK1 ", outer = TRUE, cex = 1.5)

dev.off()


rm(auc4,auct4,predvec,realvec, pr1,pr2,pr3,pr4, prf1,prf2,prf3,prf4, predvec,realvec)







