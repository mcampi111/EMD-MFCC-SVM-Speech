#SVM OUT OF SAMPLE - SYNT vs SPEAKER1 ---  FEATURE: stat --- KERNEL: RBASIS  
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
x_synt_stat_new<- matrix(unlist(synt_vec_new), byrow=TRUE, nrow=20 )


x_speak1_stat_new<- matrix(unlist(speak1_vec_new), byrow=TRUE, nrow=20 )


x_syntspeak1_stat_new<- rbind(x_synt_stat_new, x_speak1_stat_new)

colnames(x_syntspeak1_stat_new) <- paste("x_syntspeak1_stat", 1:ncol(x_syntspeak1_stat_new), sep="")


#stat - FIRST stat

ypred_syntspeak1_stat1<- predict(SVM_syntspeak1_stat1_lp,newdata = x_syntspeak1_stat_new[,1:70])

CM_syntspeak1_stat1_new<- confusionMatrix(ypred_syntspeak1_stat1, yff)
CM_syntspeak1_stat1_new

predvec <- ifelse(ypred_syntspeak1_stat1=="uno", 1, 0)
realvec <- ifelse(yff =="uno", 1, 0)
pr1 <- prediction(predvec, realvec)
prf1 <- performance(pr1, measure="tpr",  x.measure="fpr")
auc1 <- performance(pr1,"auc")
auc1 <- unlist(slot(auc1, "y.values"))
auct1 <- paste(c("AUC  = "), auc1,sep="")
plot(prf1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     main = "ROC - stat1  ")
abline(a=0, b= 1, lty = 3, col = "black")
mtext(auct1, side = 1, line = -12, col = "red")
rm(predvec,realvec)

#stat - SECOND stat

ypred_syntspeak1_stat2<- predict(SVM_syntspeak1_stat2_lp,newdata = x_syntspeak1_stat_new[,71:140])

CM_syntspeak1_stat2_new<- confusionMatrix(ypred_syntspeak1_stat2, yff)
CM_syntspeak1_stat2_new

predvec <- ifelse(ypred_syntspeak1_stat2=="uno", 1, 0)
realvec <- ifelse(yff =="uno", 1, 0)
pr2 <- prediction(predvec, realvec)
prf2 <- performance(pr2, measure="tpr",  x.measure="fpr")
auc2 <- performance(pr2,"auc")
auc2 <- unlist(slot(auc2, "y.values"))
auct2 <- paste(c("AUC  = "), auc2,sep="")
plot(prf2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     main = "ROC - stat2 ")
abline(a=0, b= 1, lty = 3, col = "black")
mtext(auct2, side = 1, line = -12, col = "red")
rm(predvec,realvec)

#stat - THIRD stat

ypred_syntspeak1_stat3<- predict(SVM_syntspeak1_stat3_lp,newdata = x_syntspeak1_stat_new[,141:210])

CM_syntspeak1_stat3_new<- confusionMatrix(ypred_syntspeak1_stat3, yff)
CM_syntspeak1_stat3_new

predvec <- ifelse(ypred_syntspeak1_stat3=="uno", 1, 0)
realvec <- ifelse(yff =="uno", 1, 0)
pr3 <- prediction(predvec, realvec)
prf3 <- performance(pr3, measure="tpr",  x.measure="fpr")
auc3 <- performance(pr3,"auc")
auc3 <- unlist(slot(auc3, "y.values"))
auct3 <- paste(c("AUC  = "), auc3,sep="")
plot(prf3, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     main = "ROC - stat3  ")
abline(a=0, b= 1, lty = 3, col = "black")
mtext(auct3, side = 1, line = -12, col = "red")
rm(predvec,realvec)


#stat - FOURTH stat

ypred_syntspeak1_stat4<- predict(SVM_syntspeak1_stat4_lp,newdata = x_syntspeak1_stat_new[,211:280])

CM_syntspeak1_stat4_new<- confusionMatrix(ypred_syntspeak1_stat4, yff)
CM_syntspeak1_stat4_new

predvec <- ifelse(ypred_syntspeak1_stat4=="uno", 1, 0)
realvec <- ifelse(yff =="uno", 1, 0)
pr4 <- prediction(predvec, realvec)
prf4 <- performance(pr4, measure="tpr",  x.measure="fpr")
auc4 <- performance(pr4,"auc")
auc4 <- unlist(slot(auc4, "y.values"))
auct4 <- paste(c("AUC  = "), auc4,sep="")
plot(prf4, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     main = "ROC - stat3 - Instantaneous Frequency ")
abline(a=0, b= 1, lty = 3, col = "black")
mtext(auct4, side = 1, line = -12, col = "red")
rm(predvec,realvec)

#stat - FstatTH stat

ypred_syntspeak1_stat5<- predict(SVM_syntspeak1_stat5_lp,newdata = x_syntspeak1_stat_new[,281:350])

CM_syntspeak1_stat5_new<- confusionMatrix(ypred_syntspeak1_stat5, yff)
CM_syntspeak1_stat5_new


predvec <- ifelse(ypred_syntspeak1_stat5=="uno", 1, 0)
realvec <- ifelse(yff =="uno", 1, 0)
pr5 <- prediction(predvec, realvec)
prf5 <- performance(pr5, measure="tpr",  x.measure="fpr")
auc5 <- performance(pr5,"auc")
auc5 <- unlist(slot(auc5, "y.values"))
auct5 <- paste(c("AUC  = "), auc5,sep="")
plot(prf5, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     main = "ROC - stat5  ")
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
     main = "stat1", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct1, side = 3, line = -4, col = "blue", cex = 0.7)


plot(prf2, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf2, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "stat2", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct2, side = 3, line = -4, col = "blue", cex = 0.7)


plot(prf3, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf3, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "stat3", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct3, side = 3, line = -4, col = "blue", cex = 0.7)

plot(prf4, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf4, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "stat4", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct4, side = 3, line = -4, col = "blue", cex = 0.7)

plot(prf5, col = "blue", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),
     lwd = 2)
par(new = TRUE)
polygon(xx, yy, col = "gray", border = "black", density = 20)
par(new=TRUE)
plot(prf5, col = "blue", print.cutoffs.at = seq(0,1,by = 0.1), text.adj = c(-0.2,1.7),
     main = "stat5", lwd = 2)
abline(a=0, b= 1, lty = 3, col = "red", lwd = 1.5)
mtext(auct5, side = 3, line = -4, col = "blue", cex = 0.7)

mtext("ROC SYNT vs SPEAK1 ", outer = TRUE, cex = 1.5)

dev.off()


rm(auc4,auct4,predvec,realvec, pr1,pr2,pr3,pr4, prf1,prf2,prf3,prf4, predvec,realvec)







