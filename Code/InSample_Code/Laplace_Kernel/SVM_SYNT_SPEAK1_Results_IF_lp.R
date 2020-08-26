#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - IF

#ACCURACY

acc_IF1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF1_lp$overall[1])
acc_IF2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF2_lp$overall[1])
acc_IF3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF3_lp$overall[1])
acc_IF4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF4_lp$overall[1])
acc_IF5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF5_lp$overall[1])


acc_IF_SYNTSPEAK1_lp<- c(acc_IF1_SYNTSPEAK1_lp,acc_IF2_SYNTSPEAK1_lp,acc_IF3_SYNTSPEAK1_lp, 
                           acc_IF4_SYNTSPEAK1_lp, 
                           acc_IF5_SYNTSPEAK1_lp)

#F1 score

F1_IF1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF1_lp$byClass[7])
F1_IF2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF2_lp$byClass[7])
F1_IF3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF3_lp$byClass[7])
F1_IF4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF4_lp$byClass[7])
F1_IF5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF5_lp$byClass[7])


F1_IF_SYNTSPEAK1_lp<- c(F1_IF1_SYNTSPEAK1_lp,F1_IF2_SYNTSPEAK1_lp,F1_IF3_SYNTSPEAK1_lp,
                          F1_IF4_SYNTSPEAK1_lp, F1_IF5_SYNTSPEAK1_lp)

#Kappa

# Kappa_IF1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF1_lp$overall[2])
# Kappa_IF2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF2_lp$overall[2])
# Kappa_IF3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF3_lp$overall[2])
# Kappa_IF4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF4_lp$overall[2])
# Kappa_IF5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF5_lp$overall[2])
# 
# 
# Kappa_IF_SYNTSPEAK1_lp<- c(Kappa_IF1_SYNTSPEAK1_lp,Kappa_IF2_SYNTSPEAK1_lp,Kappa_IF3_SYNTSPEAK1_lp,
#                              Kappa_IF4_SYNTSPEAK1_lp, 
#                              Kappa_IF5_SYNTSPEAK1_lp)
# 
#Precision

Precision_IF1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF1_lp$byClass[5])
Precision_IF2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF2_lp$byClass[5])
Precision_IF3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF3_lp$byClass[5])
Precision_IF4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF4_lp$byClass[5])
Precision_IF5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF5_lp$byClass[5])

Precision_IF_SYNTSPEAK1_lp<- c(Precision_IF1_SYNTSPEAK1_lp,Precision_IF2_SYNTSPEAK1_lp,
                                 Precision_IF3_SYNTSPEAK1_lp,
                                 Precision_IF4_SYNTSPEAK1_lp, Precision_IF5_SYNTSPEAK1_lp)

#Sensitivity

Sens_IF1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF1_lp$byClass[1])
Sens_IF2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF2_lp$byClass[1])
Sens_IF3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF3_lp$byClass[1])
Sens_IF4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF4_lp$byClass[1])
Sens_IF5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF5_lp$byClass[1])

Sens_IF_SYNTSPEAK1_lp<- c(Sens_IF1_SYNTSPEAK1_lp,Sens_IF2_SYNTSPEAK1_lp,Sens_IF3_SYNTSPEAK1_lp, 
                            Sens_IF4_SYNTSPEAK1_lp,
                            Sens_IF5_SYNTSPEAK1_lp)

#SpecIFicity 

Spec_IF1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF1_lp$byClass[2])
Spec_IF2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF2_lp$byClass[2])
Spec_IF3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF3_lp$byClass[2])
Spec_IF4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF4_lp$byClass[2])
Spec_IF5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IF5_lp$byClass[2])

Spec_IF_SYNTSPEAK1_lp<- c(Spec_IF1_SYNTSPEAK1_lp,Spec_IF2_SYNTSPEAK1_lp,Spec_IF3_SYNTSPEAK1_lp, 
                            Spec_IF4_SYNTSPEAK1_lp,
                            Spec_IF5_SYNTSPEAK1_lp)

#PRINT RESULTS ON A TABLE
nsvm<- c('IF1', 'IF2', 'IF3', 'IF4','IF5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score', 'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_lp<- data.frame(nsvm, acc_IF_SYNTSPEAK1_lp, F1_IF_SYNTSPEAK1_lp,
                               Precision_IF_SYNTSPEAK1_lp, 
                               Sens_IF_SYNTSPEAK1_lp, Spec_IF_SYNTSPEAK1_lp)
names(tbl_SYNTSPEAK1_lp)<- cnames_1

ltabl_SYNTSPEAK1_lp<-kable(tbl_SYNTSPEAK1_lp, format = "latex", longtable = T, caption = "Results of SYNT vs 
                           SPEAK1 SVM
                           with Instantaneous Frequencies of each IFs as features - Kernel: lpnomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_IF_SYNTSPEAK1_lp, F1_IF_SYNTSPEAK1_lp, Kappa_IF_SYNTSPEAK1_lp, Precision_IF_SYNTSPEAK1_lp, 
   Sens_IF_SYNTSPEAK1_lp, Spec_IF_SYNTSPEAK1_lp)







