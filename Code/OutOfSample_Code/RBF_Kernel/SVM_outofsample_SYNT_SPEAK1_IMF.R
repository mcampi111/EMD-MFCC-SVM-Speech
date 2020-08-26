#SVM OUT OF SAMPLE - SYNT vs SPEAKER1 ---  FEATURE: IMF --- KERNEL: RBASIS  
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
x_synt_IMF_new<- matrix(unlist(feat_IMF_data_synt_new), byrow=TRUE, nrow=20 )


x_speak1_IMF_new<- matrix(unlist(feat_IMF_data_speak1_new), byrow=TRUE, nrow=20 )


x_syntspeak1_IMF_new<- rbind(x_synt_IMF_new, x_speak1_IMF_new)

colnames(x_syntspeak1_IMF_new) <- paste("x_syntspeak1_IMF", 1:ncol(x_syntspeak1_IMF_new), sep="")


#IMF - FIRST IMF

ypred_syntspeak1_IMF1<- predict(SVM_syntspeak1_IMF1,newdata = x_syntspeak1_IMF_new[,1:60000])

CM_syntspeak1_IMF1_new<- confusionMatrix(ypred_syntspeak1_IMF1, yff)
CM_syntspeak1_IMF1_new

predvec <- ifelse(ypred_syntspeak1_IMF1=="uno", 1, 0)
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

#IMF - SECOND IMF

ypred_syntspeak1_IMF2<- predict(SVM_syntspeak1_IMF2,newdata = x_syntspeak1_IMF_new[,60001:120000])

CM_syntspeak1_IMF2_new<- confusionMatrix(ypred_syntspeak1_IMF2, yff)
CM_syntspeak1_IMF2_new

predvec <- ifelse(ypred_syntspeak1_IMF2=="uno", 1, 0)
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

#IMF - THIRD IMF

ypred_syntspeak1_IMF3<- predict(SVM_syntspeak1_IMF3,newdata = x_syntspeak1_IMF_new[,120001:180000])

CM_syntspeak1_IMF3_new<- confusionMatrix(ypred_syntspeak1_IMF3, yff)
CM_syntspeak1_IMF3_new

predvec <- ifelse(ypred_syntspeak1_IMF3=="uno", 1, 0)
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


#IMF - FOURTH IMF

ypred_syntspeak1_IMF4<- predict(SVM_syntspeak1_IMF4,newdata = x_syntspeak1_IMF_new[,180001:240000])

CM_syntspeak1_IMF4_new<- confusionMatrix(ypred_syntspeak1_IMF4, yff)
CM_syntspeak1_IMF4_new

predvec <- ifelse(ypred_syntspeak1_IMF4=="uno", 1, 0)
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

#IMF - FIMFTH IMF

ypred_syntspeak1_IMF5<- predict(SVM_syntspeak1_IMF5,newdata = x_syntspeak1_IMF_new[,240001:300000])

CM_syntspeak1_IMF5_new<- confusionMatrix(ypred_syntspeak1_IMF5, yff)
CM_syntspeak1_IMF5_new


predvec <- ifelse(ypred_syntspeak1_IMF5=="uno", 1, 0)
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
     main = "IMF1", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct1, side = 3, line = -4, col = "blue", cex = 0.7)


plot(prf2, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf2, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "IMF2", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct2, side = 3, line = -4, col = "blue", cex = 0.7)


plot(prf3, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf3, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "IMF3", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct3, side = 3, line = -4, col = "blue", cex = 0.7)

plot(prf4, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf4, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "IMF4", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct4, side = 3, line = -4, col = "blue", cex = 0.7)

plot(prf5, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf5, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "IMF5", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct5, side = 3, line = -4, col = "blue", cex = 0.7)

mtext("ROC SYNT vs SPEAK1 ", outer = TRUE, cex = 1.5)

dev.off()


rm(auc4,auct4,predvec,realvec, pr1,pr2,pr3,pr4, prf1,prf2,prf3,prf4, predvec,realvec)







