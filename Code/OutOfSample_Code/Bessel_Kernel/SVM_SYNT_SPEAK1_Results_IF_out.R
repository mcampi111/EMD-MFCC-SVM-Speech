#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - IF

#ACCURACY

acc_IF1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF1_new_bess$overall[1])
acc_IF2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF2_new_bess$overall[1])
acc_IF3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF3_new_bess$overall[1])
acc_IF4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF4_new_bess$overall[1])
acc_IF5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF5_new_bess$overall[1])

acc_IF_SYNTSPEAK1_bess<- c(acc_IF1_SYNTSPEAK1,acc_IF2_SYNTSPEAK1,acc_IF3_SYNTSPEAK1, acc_IF4_SYNTSPEAK1, acc_IF5_SYNTSPEAK1)

acc_IF_SYNTSPEAK1<- c(acc_IF1_SYNTSPEAK1,acc_IF2_SYNTSPEAK1,acc_IF3_SYNTSPEAK1, acc_IF4_SYNTSPEAK1, acc_IF5_SYNTSPEAK1)

#F1 score

F1_IF1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF1_new_bess$byClass[7])
F1_IF2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF2_new_bess$byClass[7])
F1_IF3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF3_new_bess$byClass[7])
F1_IF4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF4_new_bess$byClass[7])
F1_IF5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF5_new_bess$byClass[7])


F1_IF_SYNTSPEAK1<- c(F1_IF1_SYNTSPEAK1,F1_IF2_SYNTSPEAK1,F1_IF3_SYNTSPEAK1, F1_IF4_SYNTSPEAK1, F1_IF5_SYNTSPEAK1)

#Kappa

# Kappa_IF1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF1_new_bess$overall[2])
# Kappa_IF2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF2_new_bess$overall[2])
# Kappa_IF3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF3_new_bess$overall[2])
# Kappa_IF4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF4_new_bess$overall[2])
# Kappa_IF5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF5_new_bess$overall[2])
# 
# 
# Kappa_IF_SYNTSPEAK1<- c(Kappa_IF1_SYNTSPEAK1,Kappa_IF2_SYNTSPEAK1,Kappa_IF3_SYNTSPEAK1, Kappa_IF4_SYNTSPEAK1, 
#                      Kappa_IF5_SYNTSPEAK1)

#Precision

Precision_IF1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF1_new_bess$byClass[5])
Precision_IF2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF2_new_bess$byClass[5])
Precision_IF3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF3_new_bess$byClass[5])
Precision_IF4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF4_new_bess$byClass[5])
Precision_IF5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF5_new_bess$byClass[5])

Precision_IF_SYNTSPEAK1<- c(Precision_IF1_SYNTSPEAK1,Precision_IF2_SYNTSPEAK1,Precision_IF3_SYNTSPEAK1,
                            Precision_IF4_SYNTSPEAK1, Precision_IF5_SYNTSPEAK1)

#Sensitivity

Sens_IF1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF1_new_bess$byClass[1])
Sens_IF2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF2_new_bess$byClass[1])
Sens_IF3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF3_new_bess$byClass[1])
Sens_IF4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF4_new_bess$byClass[1])
Sens_IF5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF5_new_bess$byClass[1])

Sens_IF_SYNTSPEAK1<- c(Sens_IF1_SYNTSPEAK1,Sens_IF2_SYNTSPEAK1,Sens_IF3_SYNTSPEAK1, Sens_IF4_SYNTSPEAK1,
                       Sens_IF5_SYNTSPEAK1)

#Specificity 

Spec_IF1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF1_new_bess$byClass[2])
Spec_IF2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF2_new_bess$byClass[2])
Spec_IF3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF3_new_bess$byClass[2])
Spec_IF4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF4_new_bess$byClass[2])
Spec_IF5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IF5_new_bess$byClass[2])

Spec_IF_SYNTSPEAK1<- c(Spec_IF1_SYNTSPEAK1,Spec_IF2_SYNTSPEAK1,Spec_IF3_SYNTSPEAK1, Spec_IF4_SYNTSPEAK1,
                       Spec_IF5_SYNTSPEAK1)

#PRINT RESULTS ON A TABLE
nsvm<- c('IF1', 'IF2', 'IF3', 'IF4','IF5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score', 'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1<- data.frame(nsvm, acc_IF_SYNTSPEAK1, F1_IF_SYNTSPEAK1,  Precision_IF_SYNTSPEAK1, 
                            Sens_IF_SYNTSPEAK1, Spec_IF_SYNTSPEAK1)
names(tbl_SYNTSPEAK1)<- cnames_1

ltabl_SYNTSPEAK1<-kable(tbl_SYNTSPEAK1, format = "latex", longtable = T, caption = "Results of SYNT vs SPEAK1 SVM
                        with Instantaneous Frequencies of each IMFs as features - Kernel:RBF.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_IF_SYNTSPEAK1, F1_IF_SYNTSPEAK1, Kappa_IF_SYNTSPEAK1, Precision_IF_SYNTSPEAK1, 
   Sens_IF_SYNTSPEAK1, Spec_IF_SYNTSPEAK1)







