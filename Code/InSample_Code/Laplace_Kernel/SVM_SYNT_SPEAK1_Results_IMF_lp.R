#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - IMF

#ACCURACY

acc_IMF1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF1_lp$overall[1])
acc_IMF2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF2_lp$overall[1])
acc_IMF3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF3_lp$overall[1])
acc_IMF4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF4_lp$overall[1])
acc_IMF5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF5_lp$overall[1])


acc_IMF_SYNTSPEAK1_lp<- c(acc_IMF1_SYNTSPEAK1_lp,acc_IMF2_SYNTSPEAK1_lp,acc_IMF3_SYNTSPEAK1_lp, 
                           acc_IMF4_SYNTSPEAK1_lp, 
                           acc_IMF5_SYNTSPEAK1_lp)

#F1 score

F1_IMF1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF1_lp$byClass[7])
F1_IMF2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF2_lp$byClass[7])
F1_IMF3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF3_lp$byClass[7])
F1_IMF4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF4_lp$byClass[7])
F1_IMF5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF5_lp$byClass[7])


F1_IMF_SYNTSPEAK1_lp<- c(F1_IMF1_SYNTSPEAK1_lp,F1_IMF2_SYNTSPEAK1_lp,F1_IMF3_SYNTSPEAK1_lp,
                          F1_IMF4_SYNTSPEAK1_lp, F1_IMF5_SYNTSPEAK1_lp)

#Kappa

# Kappa_IMF1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF1_lp$overall[2])
# Kappa_IMF2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF2_lp$overall[2])
# Kappa_IMF3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF3_lp$overall[2])
# Kappa_IMF4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF4_lp$overall[2])
# Kappa_IMF5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF5_lp$overall[2])
# 
# 
# Kappa_IMF_SYNTSPEAK1_lp<- c(Kappa_IMF1_SYNTSPEAK1_lp,Kappa_IMF2_SYNTSPEAK1_lp,Kappa_IMF3_SYNTSPEAK1_lp,
#                              Kappa_IMF4_SYNTSPEAK1_lp, 
#                              Kappa_IMF5_SYNTSPEAK1_lp)
# 
#Precision

Precision_IMF1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF1_lp$byClass[5])
Precision_IMF2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF2_lp$byClass[5])
Precision_IMF3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF3_lp$byClass[5])
Precision_IMF4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF4_lp$byClass[5])
Precision_IMF5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF5_lp$byClass[5])

Precision_IMF_SYNTSPEAK1_lp<- c(Precision_IMF1_SYNTSPEAK1_lp,Precision_IMF2_SYNTSPEAK1_lp,
                                 Precision_IMF3_SYNTSPEAK1_lp,
                                 Precision_IMF4_SYNTSPEAK1_lp, Precision_IMF5_SYNTSPEAK1_lp)

#Sensitivity

Sens_IMF1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF1_lp$byClass[1])
Sens_IMF2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF2_lp$byClass[1])
Sens_IMF3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF3_lp$byClass[1])
Sens_IMF4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF4_lp$byClass[1])
Sens_IMF5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF5_lp$byClass[1])

Sens_IMF_SYNTSPEAK1_lp<- c(Sens_IMF1_SYNTSPEAK1_lp,Sens_IMF2_SYNTSPEAK1_lp,Sens_IMF3_SYNTSPEAK1_lp, 
                            Sens_IMF4_SYNTSPEAK1_lp,
                            Sens_IMF5_SYNTSPEAK1_lp)

#SpecIMFicity 

Spec_IMF1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF1_lp$byClass[2])
Spec_IMF2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF2_lp$byClass[2])
Spec_IMF3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF3_lp$byClass[2])
Spec_IMF4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF4_lp$byClass[2])
Spec_IMF5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_IMF5_lp$byClass[2])

Spec_IMF_SYNTSPEAK1_lp<- c(Spec_IMF1_SYNTSPEAK1_lp,Spec_IMF2_SYNTSPEAK1_lp,Spec_IMF3_SYNTSPEAK1_lp, 
                            Spec_IMF4_SYNTSPEAK1_lp,
                            Spec_IMF5_SYNTSPEAK1_lp)

#PRINT RESULTS ON A TABLE
nsvm<- c('IMF1', 'IMF2', 'IMF3', 'IMF4','IMF5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score', 'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_lp<- data.frame(nsvm, acc_IMF_SYNTSPEAK1_lp, F1_IMF_SYNTSPEAK1_lp,
                               Precision_IMF_SYNTSPEAK1_lp, 
                               Sens_IMF_SYNTSPEAK1_lp, Spec_IMF_SYNTSPEAK1_lp)
names(tbl_SYNTSPEAK1_lp)<- cnames_1

ltabl_SYNTSPEAK1_lp<-kable(tbl_SYNTSPEAK1_lp, format = "latex", longtable = T, caption = "Results of SYNT vs 
                           SPEAK1 SVM
                           with Instantaneous Frequencies of each IMFs as features - Kernel: lpnomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_IMF_SYNTSPEAK1_lp, F1_IMF_SYNTSPEAK1_lp, Kappa_IMF_SYNTSPEAK1_lp, Precision_IMF_SYNTSPEAK1_lp, 
   Sens_IMF_SYNTSPEAK1_lp, Spec_IMF_SYNTSPEAK1_lp)







