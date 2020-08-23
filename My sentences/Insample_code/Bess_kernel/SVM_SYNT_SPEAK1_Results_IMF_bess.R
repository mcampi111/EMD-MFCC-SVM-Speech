#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - IMF

#ACCURACY

acc_IMF1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF1_bess$overall[1])
acc_IMF2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF2_bess$overall[1])
acc_IMF3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF3_bess$overall[1])
acc_IMF4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF4_bess$overall[1])
acc_IMF5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF5_bess$overall[1])


acc_IMF_SYNTSPEAK1_bess<- c(acc_IMF1_SYNTSPEAK1_bess,acc_IMF2_SYNTSPEAK1_bess,acc_IMF3_SYNTSPEAK1_bess, 
                             acc_IMF4_SYNTSPEAK1_bess, 
                             acc_IMF5_SYNTSPEAK1_bess)

#F1 score

F1_IMF1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF1_bess$byClass[7])
F1_IMF2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF2_bess$byClass[7])
F1_IMF3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF3_bess$byClass[7])
F1_IMF4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF4_bess$byClass[7])
F1_IMF5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF5_bess$byClass[7])


F1_IMF_SYNTSPEAK1_bess<- c(F1_IMF1_SYNTSPEAK1_bess,F1_IMF2_SYNTSPEAK1_bess,F1_IMF3_SYNTSPEAK1_bess,
                            F1_IMF4_SYNTSPEAK1_bess, F1_IMF5_SYNTSPEAK1_bess)

#Kappa

# Kappa_IMF1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF1_bess$overall[2])
# Kappa_IMF2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF2_bess$overall[2])
# Kappa_IMF3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF3_bess$overall[2])
# Kappa_IMF4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF4_bess$overall[2])
# Kappa_IMF5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF5_bess$overall[2])
# 
# 
# Kappa_IMF_SYNTSPEAK1_bess<- c(Kappa_IMF1_SYNTSPEAK1_bess,Kappa_IMF2_SYNTSPEAK1_bess,Kappa_IMF3_SYNTSPEAK1_bess,
#                                Kappa_IMF4_SYNTSPEAK1_bess, 
#                                Kappa_IMF5_SYNTSPEAK1_bess)

#Precision

Precision_IMF1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF1_bess$byClass[5])
Precision_IMF2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF2_bess$byClass[5])
Precision_IMF3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF3_bess$byClass[5])
Precision_IMF4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF4_bess$byClass[5])
Precision_IMF5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF5_bess$byClass[5])

Precision_IMF_SYNTSPEAK1_bess<- c(Precision_IMF1_SYNTSPEAK1_bess,Precision_IMF2_SYNTSPEAK1_bess,
                                   Precision_IMF3_SYNTSPEAK1_bess,
                                   Precision_IMF4_SYNTSPEAK1_bess, Precision_IMF5_SYNTSPEAK1_bess)

#Sensitivity

Sens_IMF1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF1_bess$byClass[1])
Sens_IMF2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF2_bess$byClass[1])
Sens_IMF3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF3_bess$byClass[1])
Sens_IMF4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF4_bess$byClass[1])
Sens_IMF5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF5_bess$byClass[1])

Sens_IMF_SYNTSPEAK1_bess<- c(Sens_IMF1_SYNTSPEAK1_bess,Sens_IMF2_SYNTSPEAK1_bess,Sens_IMF3_SYNTSPEAK1_bess, 
                              Sens_IMF4_SYNTSPEAK1_bess,
                              Sens_IMF5_SYNTSPEAK1_bess)

#SpecIMFicity 

Spec_IMF1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF1_bess$byClass[2])
Spec_IMF2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF2_bess$byClass[2])
Spec_IMF3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF3_bess$byClass[2])
Spec_IMF4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF4_bess$byClass[2])
Spec_IMF5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_IMF5_bess$byClass[2])

Spec_IMF_SYNTSPEAK1_bess<- c(Spec_IMF1_SYNTSPEAK1_bess,Spec_IMF2_SYNTSPEAK1_bess,Spec_IMF3_SYNTSPEAK1_bess, 
                              Spec_IMF4_SYNTSPEAK1_bess,
                              Spec_IMF5_SYNTSPEAK1_bess)

#PRINT RESULTS ON A TABLE
nsvm<- c('IMF1', 'IMF2', 'IMF3', 'IMF4','IMF5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score',  'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_bess<- data.frame(nsvm, acc_IMF_SYNTSPEAK1_bess, F1_IMF_SYNTSPEAK1_bess, 
                                 Precision_IMF_SYNTSPEAK1_bess, 
                                 Sens_IMF_SYNTSPEAK1_bess, Spec_IMF_SYNTSPEAK1_bess)
names(tbl_SYNTSPEAK1_bess)<- cnames_1

ltabl_SYNTSPEAK1_bess<-kable(tbl_SYNTSPEAK1_bess, format = "latex", longtable = T, caption = "Results of SYNT vs 
                             SPEAK1 SVM
                             with Instantaneous Frequencies of each IMFs as features - Kernel: bessnomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_IMF_SYNTSPEAK1_bess, F1_IMF_SYNTSPEAK1_bess, Kappa_IMF_SYNTSPEAK1_bess, Precision_IMF_SYNTSPEAK1_bess, 
   Sens_IMF_SYNTSPEAK1_bess, Spec_IMF_SYNTSPEAK1_bess)







