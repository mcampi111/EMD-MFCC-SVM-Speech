#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - IF

#ACCURACY

acc_IF1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF1_bess$overall[1])
acc_IF2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF2_bess$overall[1])
acc_IF3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF3_bess$overall[1])
acc_IF4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF4_bess$overall[1])
acc_IF5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF5_bess$overall[1])


acc_IF_SYNTSPEAK1_bess<- c(acc_IF1_SYNTSPEAK1_bess,acc_IF2_SYNTSPEAK1_bess,acc_IF3_SYNTSPEAK1_bess, 
                             acc_IF4_SYNTSPEAK1_bess, 
                             acc_IF5_SYNTSPEAK1_bess)

#F1 score

F1_IF1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF1_bess$byClass[7])
F1_IF2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF2_bess$byClass[7])
F1_IF3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF3_bess$byClass[7])
F1_IF4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF4_bess$byClass[7])
F1_IF5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF5_bess$byClass[7])


F1_IF_SYNTSPEAK1_bess<- c(F1_IF1_SYNTSPEAK1_bess,F1_IF2_SYNTSPEAK1_bess,F1_IF3_SYNTSPEAK1_bess,
                            F1_IF4_SYNTSPEAK1_bess, F1_IF5_SYNTSPEAK1_bess)

#Kappa

Kappa_IF1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF1_bess$overall[2])
Kappa_IF2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF2_bess$overall[2])
Kappa_IF3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF3_bess$overall[2])
Kappa_IF4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF4_bess$overall[2])
Kappa_IF5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF5_bess$overall[2])


Kappa_IF_SYNTSPEAK1_bess<- c(Kappa_IF1_SYNTSPEAK1_bess,Kappa_IF2_SYNTSPEAK1_bess,Kappa_IF3_SYNTSPEAK1_bess,
                               Kappa_IF4_SYNTSPEAK1_bess, 
                               Kappa_IF5_SYNTSPEAK1_bess)

#Precision

Precision_IF1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF1_bess$byClass[5])
Precision_IF2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF2_bess$byClass[5])
Precision_IF3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF3_bess$byClass[5])
Precision_IF4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF4_bess$byClass[5])
Precision_IF5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF5_bess$byClass[5])

Precision_IF_SYNTSPEAK1_bess<- c(Precision_IF1_SYNTSPEAK1_bess,Precision_IF2_SYNTSPEAK1_bess,
                                   Precision_IF3_SYNTSPEAK1_bess,
                                   Precision_IF4_SYNTSPEAK1_bess, Precision_IF5_SYNTSPEAK1_bess)

#Sensitivity

Sens_IF1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF1_bess$byClass[1])
Sens_IF2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF2_bess$byClass[1])
Sens_IF3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF3_bess$byClass[1])
Sens_IF4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF4_bess$byClass[1])
Sens_IF5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF5_bess$byClass[1])

Sens_IF_SYNTSPEAK1_bess<- c(Sens_IF1_SYNTSPEAK1_bess,Sens_IF2_SYNTSPEAK1_bess,Sens_IF3_SYNTSPEAK1_bess, 
                              Sens_IF4_SYNTSPEAK1_bess,
                              Sens_IF5_SYNTSPEAK1_bess)

#SpecIFicity 

Spec_IF1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF1_bess$byClass[2])
Spec_IF2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF2_bess$byClass[2])
Spec_IF3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF3_bess$byClass[2])
Spec_IF4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF4_bess$byClass[2])
Spec_IF5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IF5_bess$byClass[2])

Spec_IF_SYNTSPEAK1_bess<- c(Spec_IF1_SYNTSPEAK1_bess,Spec_IF2_SYNTSPEAK1_bess,Spec_IF3_SYNTSPEAK1_bess, 
                              Spec_IF4_SYNTSPEAK1_bess,
                              Spec_IF5_SYNTSPEAK1_bess)

#PRINT RESULTS ON A TABLE
nsvm<- c('IF1', 'IF2', 'IF3', 'IF4','IF5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score',  'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_bess<- data.frame(nsvm, acc_IF_SYNTSPEAK1_bess, F1_IF_SYNTSPEAK1_bess, 
                                 Precision_IF_SYNTSPEAK1_bess, 
                                 Sens_IF_SYNTSPEAK1_bess, Spec_IF_SYNTSPEAK1_bess)
names(tbl_SYNTSPEAK1_bess)<- cnames_1

ltabl_SYNTSPEAK1_bess<-kable(tbl_SYNTSPEAK1_bess, format = "latex", longtable = T, caption = "Results of SYNT vs 
                             SPEAK1 SVM
                             with Instantaneous Frequencies of each IFs as features - Kernel: bessnomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_IF_SYNTSPEAK1_bess, F1_IF_SYNTSPEAK1_bess, Kappa_IF_SYNTSPEAK1_bess, Precision_IF_SYNTSPEAK1_bess, 
   Sens_IF_SYNTSPEAK1_bess, Spec_IF_SYNTSPEAK1_bess)







