#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - SC

#ACCURACY

acc_SC1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC1_new_lp$overall[1])
acc_SC2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC2_new_lp$overall[1])
acc_SC3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC3_new_lp$overall[1])
acc_SC4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC4_new_lp$overall[1])
acc_SC5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC5_new_lp$overall[1])

acc_SC_SYNTSPEAK1_lp<- c(acc_SC1_SYNTSPEAK1,acc_SC2_SYNTSPEAK1,acc_SC3_SYNTSPEAK1, acc_SC4_SYNTSPEAK1, acc_SC5_SYNTSPEAK1)

acc_SC_SYNTSPEAK1<- c(acc_SC1_SYNTSPEAK1,acc_SC2_SYNTSPEAK1,acc_SC3_SYNTSPEAK1, acc_SC4_SYNTSPEAK1, acc_SC5_SYNTSPEAK1)

#F1 score

F1_SC1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC1_new_lp$byClass[7])
F1_SC2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC2_new_lp$byClass[7])
F1_SC3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC3_new_lp$byClass[7])
F1_SC4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC4_new_lp$byClass[7])
F1_SC5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC5_new_lp$byClass[7])


F1_SC_SYNTSPEAK1<- c(F1_SC1_SYNTSPEAK1,F1_SC2_SYNTSPEAK1,F1_SC3_SYNTSPEAK1, F1_SC4_SYNTSPEAK1, F1_SC5_SYNTSPEAK1)

#Kappa

# Kappa_SC1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC1_new_lp$overall[2])
# Kappa_SC2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC2_new_lp$overall[2])
# Kappa_SC3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC3_new_lp$overall[2])
# Kappa_SC4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC4_new_lp$overall[2])
# Kappa_SC5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC5_new_lp$overall[2])
# 
# 
# Kappa_SC_SYNTSPEAK1<- c(Kappa_SC1_SYNTSPEAK1,Kappa_SC2_SYNTSPEAK1,Kappa_SC3_SYNTSPEAK1, Kappa_SC4_SYNTSPEAK1, 
#                      Kappa_SC5_SYNTSPEAK1)

#Precision

Precision_SC1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC1_new_lp$byClass[5])
Precision_SC2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC2_new_lp$byClass[5])
Precision_SC3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC3_new_lp$byClass[5])
Precision_SC4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC4_new_lp$byClass[5])
Precision_SC5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC5_new_lp$byClass[5])

Precision_SC_SYNTSPEAK1<- c(Precision_SC1_SYNTSPEAK1,Precision_SC2_SYNTSPEAK1,Precision_SC3_SYNTSPEAK1,
                            Precision_SC4_SYNTSPEAK1, Precision_SC5_SYNTSPEAK1)

#Sensitivity

Sens_SC1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC1_new_lp$byClass[1])
Sens_SC2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC2_new_lp$byClass[1])
Sens_SC3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC3_new_lp$byClass[1])
Sens_SC4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC4_new_lp$byClass[1])
Sens_SC5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC5_new_lp$byClass[1])

Sens_SC_SYNTSPEAK1<- c(Sens_SC1_SYNTSPEAK1,Sens_SC2_SYNTSPEAK1,Sens_SC3_SYNTSPEAK1, Sens_SC4_SYNTSPEAK1,
                       Sens_SC5_SYNTSPEAK1)

#SpecSCicity 

Spec_SC1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC1_new_lp$byClass[2])
Spec_SC2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC2_new_lp$byClass[2])
Spec_SC3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC3_new_lp$byClass[2])
Spec_SC4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC4_new_lp$byClass[2])
Spec_SC5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_SC5_new_lp$byClass[2])

Spec_SC_SYNTSPEAK1<- c(Spec_SC1_SYNTSPEAK1,Spec_SC2_SYNTSPEAK1,Spec_SC3_SYNTSPEAK1, Spec_SC4_SYNTSPEAK1,
                       Spec_SC5_SYNTSPEAK1)

#PRINT RESULTS ON A TABLE
nsvm<- c('SC1', 'SC2', 'SC3', 'SC4','SC5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score', 'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1<- data.frame(nsvm, acc_SC_SYNTSPEAK1, F1_SC_SYNTSPEAK1,  Precision_SC_SYNTSPEAK1, 
                            Sens_SC_SYNTSPEAK1, Spec_SC_SYNTSPEAK1)
names(tbl_SYNTSPEAK1)<- cnames_1

ltabl_SYNTSPEAK1<-kable(tbl_SYNTSPEAK1, format = "latex", longtable = T, caption = "Results of SYNT vs SPEAK1 SVM
                        with Instantaneous Frequencies of each IMFs as features - Kernel:RBF.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_SC_SYNTSPEAK1, F1_SC_SYNTSPEAK1, Kappa_SC_SYNTSPEAK1, Precision_SC_SYNTSPEAK1, 
   Sens_SC_SYNTSPEAK1, Spec_SC_SYNTSPEAK1)







