#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - SC

#ACCURACY

acc_SC1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC1_bess$overall[1])
acc_SC2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC2_bess$overall[1])
acc_SC3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC3_bess$overall[1])
acc_SC4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC4_bess$overall[1])
acc_SC5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC5_bess$overall[1])


acc_SC_SYNTSPEAK1_bess<- c(acc_SC1_SYNTSPEAK1_bess,acc_SC2_SYNTSPEAK1_bess,acc_SC3_SYNTSPEAK1_bess, 
                             acc_SC4_SYNTSPEAK1_bess, 
                             acc_SC5_SYNTSPEAK1_bess)

#F1 score

F1_SC1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC1_bess$byClass[7])
F1_SC2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC2_bess$byClass[7])
F1_SC3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC3_bess$byClass[7])
F1_SC4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC4_bess$byClass[7])
F1_SC5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC5_bess$byClass[7])


F1_SC_SYNTSPEAK1_bess<- c(F1_SC1_SYNTSPEAK1_bess,F1_SC2_SYNTSPEAK1_bess,F1_SC3_SYNTSPEAK1_bess,
                            F1_SC4_SYNTSPEAK1_bess, F1_SC5_SYNTSPEAK1_bess)

#Kappa

# Kappa_SC1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC1_bess$overall[2])
# Kappa_SC2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC2_bess$overall[2])
# Kappa_SC3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC3_bess$overall[2])
# Kappa_SC4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC4_bess$overall[2])
# Kappa_SC5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC5_bess$overall[2])
# 
# 
# Kappa_SC_SYNTSPEAK1_bess<- c(Kappa_SC1_SYNTSPEAK1_bess,Kappa_SC2_SYNTSPEAK1_bess,Kappa_SC3_SYNTSPEAK1_bess,
#                                Kappa_SC4_SYNTSPEAK1_bess, 
#                                Kappa_SC5_SYNTSPEAK1_bess)
# 
#Precision

Precision_SC1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC1_bess$byClass[5])
Precision_SC2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC2_bess$byClass[5])
Precision_SC3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC3_bess$byClass[5])
Precision_SC4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC4_bess$byClass[5])
Precision_SC5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC5_bess$byClass[5])

Precision_SC_SYNTSPEAK1_bess<- c(Precision_SC1_SYNTSPEAK1_bess,Precision_SC2_SYNTSPEAK1_bess,
                                   Precision_SC3_SYNTSPEAK1_bess,
                                   Precision_SC4_SYNTSPEAK1_bess, Precision_SC5_SYNTSPEAK1_bess)

#Sensitivity

Sens_SC1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC1_bess$byClass[1])
Sens_SC2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC2_bess$byClass[1])
Sens_SC3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC3_bess$byClass[1])
Sens_SC4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC4_bess$byClass[1])
Sens_SC5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC5_bess$byClass[1])

Sens_SC_SYNTSPEAK1_bess<- c(Sens_SC1_SYNTSPEAK1_bess,Sens_SC2_SYNTSPEAK1_bess,Sens_SC3_SYNTSPEAK1_bess, 
                              Sens_SC4_SYNTSPEAK1_bess,
                              Sens_SC5_SYNTSPEAK1_bess)

#SpecSCicity 

Spec_SC1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC1_bess$byClass[2])
Spec_SC2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC2_bess$byClass[2])
Spec_SC3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC3_bess$byClass[2])
Spec_SC4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC4_bess$byClass[2])
Spec_SC5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_SC5_bess$byClass[2])

Spec_SC_SYNTSPEAK1_bess<- c(Spec_SC1_SYNTSPEAK1_bess,Spec_SC2_SYNTSPEAK1_bess,Spec_SC3_SYNTSPEAK1_bess, 
                              Spec_SC4_SYNTSPEAK1_bess,
                              Spec_SC5_SYNTSPEAK1_bess)

#PRINT RESULTS ON A TABLE
nsvm<- c('SC1', 'SC2', 'SC3', 'SC4','SC5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score',  'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_bess<- data.frame(nsvm, acc_SC_SYNTSPEAK1_bess, F1_SC_SYNTSPEAK1_bess, 
                                 Precision_SC_SYNTSPEAK1_bess, 
                                 Sens_SC_SYNTSPEAK1_bess, Spec_SC_SYNTSPEAK1_bess)
names(tbl_SYNTSPEAK1_bess)<- cnames_1

ltabl_SYNTSPEAK1_bess<-kable(tbl_SYNTSPEAK1_bess, format = "latex", longtable = T, caption = "Results of SYNT vs 
                             SPEAK1 SVM
                             with Instantaneous Frequencies of each SCs as features - Kernel: bessnomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_SC_SYNTSPEAK1_bess, F1_SC_SYNTSPEAK1_bess, Kappa_SC_SYNTSPEAK1_bess, Precision_SC_SYNTSPEAK1_bess, 
   Sens_SC_SYNTSPEAK1_bess, Spec_SC_SYNTSPEAK1_bess)







