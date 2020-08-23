#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - IMF

#ACCURACY

acc_IMF1_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF1_van$overall[1])
acc_IMF2_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF2_van$overall[1])
acc_IMF3_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF3_van$overall[1])
acc_IMF4_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF4_van$overall[1])
acc_IMF5_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF5_van$overall[1])


acc_IMF_SYNTSPEAK1_van<- c(acc_IMF1_van_SYNTSPEAK1,acc_IMF2_van_SYNTSPEAK1,acc_IMF3_van_SYNTSPEAK1, 
                       acc_IMF4_van_SYNTSPEAK1, acc_IMF5_van_SYNTSPEAK1)

acc_IMF_SYNTSPEAK1<- c(acc_IMF1_van_SYNTSPEAK1,acc_IMF2_van_SYNTSPEAK1,acc_IMF3_van_SYNTSPEAK1, 
                       acc_IMF4_van_SYNTSPEAK1, acc_IMF5_van_SYNTSPEAK1)

#F1 score

F1_IMF1_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF1_van$byClass[7])
F1_IMF2_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF2_van$byClass[7])
F1_IMF3_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF3_van$byClass[7])
F1_IMF4_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF4_van$byClass[7])
F1_IMF5_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF5_van$byClass[7])


F1_IMF_SYNTSPEAK1<- c(F1_IMF1_van_SYNTSPEAK1,F1_IMF2_van_SYNTSPEAK1,F1_IMF3_van_SYNTSPEAK1, F1_IMF4_van_SYNTSPEAK1, F1_IMF5_van_SYNTSPEAK1)

#Kappa

# Kappa_IMF1_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF1_van$overall[2])
# Kappa_IMF2_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF2_van$overall[2])
# Kappa_IMF3_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF3_van$overall[2])
# Kappa_IMF4_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF4_van$overall[2])
# Kappa_IMF5_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF5_van$overall[2])
# 
# 
# Kappa_IMF_SYNTSPEAK1<- c(Kappa_IMF1_van_SYNTSPEAK1,Kappa_IMF2_van_SYNTSPEAK1,Kappa_IMF3_van_SYNTSPEAK1, Kappa_IMF4_van_SYNTSPEAK1, 
#                          Kappa_IMF5_van_SYNTSPEAK1)

#Precision

Precision_IMF1_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF1_van$byClass[5])
Precision_IMF2_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF2_van$byClass[5])
Precision_IMF3_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF3_van$byClass[5])
Precision_IMF4_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF4_van$byClass[5])
Precision_IMF5_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF5_van$byClass[5])

Precision_IMF_SYNTSPEAK1<- c(Precision_IMF1_van_SYNTSPEAK1,Precision_IMF2_van_SYNTSPEAK1,Precision_IMF3_van_SYNTSPEAK1,
                             Precision_IMF4_van_SYNTSPEAK1, Precision_IMF5_van_SYNTSPEAK1)

#Sensitivity

Sens_IMF1_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF1_van$byClass[1])
Sens_IMF2_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF2_van$byClass[1])
Sens_IMF3_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF3_van$byClass[1])
Sens_IMF4_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF4_van$byClass[1])
Sens_IMF5_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF5_van$byClass[1])

Sens_IMF_SYNTSPEAK1<- c(Sens_IMF1_van_SYNTSPEAK1,Sens_IMF2_van_SYNTSPEAK1,Sens_IMF3_van_SYNTSPEAK1, Sens_IMF4_van_SYNTSPEAK1,
                        Sens_IMF5_van_SYNTSPEAK1)

#SpecIMFicity 

Spec_IMF1_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF1_van$byClass[2])
Spec_IMF2_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF2_van$byClass[2])
Spec_IMF3_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF3_van$byClass[2])
Spec_IMF4_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF4_van$byClass[2])
Spec_IMF5_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_IMF5_van$byClass[2])

Spec_IMF_SYNTSPEAK1<- c(Spec_IMF1_van_SYNTSPEAK1,Spec_IMF2_van_SYNTSPEAK1,Spec_IMF3_van_SYNTSPEAK1, Spec_IMF4_van_SYNTSPEAK1,
                        Spec_IMF5_van_SYNTSPEAK1)

#PRINT RESULTS ON A TABLE
nsvm<- c('IMF1_van', 'IMF2_van', 'IMF3_van', 'IMF4_van','IMF5_van')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score', 'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1<- data.frame(nsvm, acc_IMF_SYNTSPEAK1, F1_IMF_SYNTSPEAK1,  
                            Precision_IMF_SYNTSPEAK1, 
                            Sens_IMF_SYNTSPEAK1, Spec_IMF_SYNTSPEAK1)
names(tbl_SYNTSPEAK1)<- cnames_1

ltabl_SYNTSPEAK1<-kable(tbl_SYNTSPEAK1, format = "latex", longtable = T, caption = "Results of SYNT vs SPEAK1 SVM
                        with Instantaneous Frequencies of each IMFs as features - Kernel:RBF.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_IMF_SYNTSPEAK1, F1_IMF_SYNTSPEAK1, Kappa_IMF_SYNTSPEAK1, Precision_IMF_SYNTSPEAK1, 
   Sens_IMF_SYNTSPEAK1, Spec_IMF_SYNTSPEAK1)







