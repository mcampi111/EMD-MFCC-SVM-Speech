#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - SC

#ACCURACY

acc_SC1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC1_lp$overall[1])
acc_SC2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC2_lp$overall[1])
acc_SC3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC3_lp$overall[1])
acc_SC4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC4_lp$overall[1])
acc_SC5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC5_lp$overall[1])


acc_SC_SYNTSPEAK1_lp<- c(acc_SC1_SYNTSPEAK1_lp,acc_SC2_SYNTSPEAK1_lp,acc_SC3_SYNTSPEAK1_lp, 
                           acc_SC4_SYNTSPEAK1_lp, 
                           acc_SC5_SYNTSPEAK1_lp)

#F1 score

F1_SC1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC1_lp$byClass[7])
F1_SC2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC2_lp$byClass[7])
F1_SC3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC3_lp$byClass[7])
F1_SC4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC4_lp$byClass[7])
F1_SC5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC5_lp$byClass[7])


F1_SC_SYNTSPEAK1_lp<- c(F1_SC1_SYNTSPEAK1_lp,F1_SC2_SYNTSPEAK1_lp,F1_SC3_SYNTSPEAK1_lp,
                          F1_SC4_SYNTSPEAK1_lp, F1_SC5_SYNTSPEAK1_lp)

#Kappa

# Kappa_SC1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC1_lp$overall[2])
# Kappa_SC2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC2_lp$overall[2])
# Kappa_SC3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC3_lp$overall[2])
# Kappa_SC4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC4_lp$overall[2])
# Kappa_SC5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC5_lp$overall[2])
# 
# 
# Kappa_SC_SYNTSPEAK1_lp<- c(Kappa_SC1_SYNTSPEAK1_lp,Kappa_SC2_SYNTSPEAK1_lp,Kappa_SC3_SYNTSPEAK1_lp,
#                              Kappa_SC4_SYNTSPEAK1_lp, 
     #                        Kappa_SC5_SYNTSPEAK1_lp)

#Precision

Precision_SC1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC1_lp$byClass[5])
Precision_SC2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC2_lp$byClass[5])
Precision_SC3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC3_lp$byClass[5])
Precision_SC4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC4_lp$byClass[5])
Precision_SC5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC5_lp$byClass[5])

Precision_SC_SYNTSPEAK1_lp<- c(Precision_SC1_SYNTSPEAK1_lp,Precision_SC2_SYNTSPEAK1_lp,
                                 Precision_SC3_SYNTSPEAK1_lp,
                                 Precision_SC4_SYNTSPEAK1_lp, Precision_SC5_SYNTSPEAK1_lp)

#Sensitivity

Sens_SC1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC1_lp$byClass[1])
Sens_SC2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC2_lp$byClass[1])
Sens_SC3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC3_lp$byClass[1])
Sens_SC4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC4_lp$byClass[1])
Sens_SC5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC5_lp$byClass[1])

Sens_SC_SYNTSPEAK1_lp<- c(Sens_SC1_SYNTSPEAK1_lp,Sens_SC2_SYNTSPEAK1_lp,Sens_SC3_SYNTSPEAK1_lp, 
                            Sens_SC4_SYNTSPEAK1_lp,
                            Sens_SC5_SYNTSPEAK1_lp)

#SpecSCicity 

Spec_SC1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC1_lp$byClass[2])
Spec_SC2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC2_lp$byClass[2])
Spec_SC3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC3_lp$byClass[2])
Spec_SC4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC4_lp$byClass[2])
Spec_SC5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_SC5_lp$byClass[2])

Spec_SC_SYNTSPEAK1_lp<- c(Spec_SC1_SYNTSPEAK1_lp,Spec_SC2_SYNTSPEAK1_lp,Spec_SC3_SYNTSPEAK1_lp, 
                            Spec_SC4_SYNTSPEAK1_lp,
                            Spec_SC5_SYNTSPEAK1_lp)

#PRINT RESULTS ON A TABLE
nsvm<- c('SC1', 'SC2', 'SC3', 'SC4','SC5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score',  'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_lp<- data.frame(nsvm, acc_SC_SYNTSPEAK1_lp, F1_SC_SYNTSPEAK1_lp, 
                               Precision_SC_SYNTSPEAK1_lp, 
                               Sens_SC_SYNTSPEAK1_lp, Spec_SC_SYNTSPEAK1_lp)
names(tbl_SYNTSPEAK1_lp)<- cnames_1

ltabl_SYNTSPEAK1_lp<-kable(tbl_SYNTSPEAK1_lp, format = "latex", longtable = T, caption = "Results of SYNT vs 
                           SPEAK1 SVM
                           with Instantaneous Frequencies of each SCs as features - Kernel: lpnomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_SC_SYNTSPEAK1_lp, F1_SC_SYNTSPEAK1_lp, Kappa_SC_SYNTSPEAK1_lp, Precision_SC_SYNTSPEAK1_lp, 
   Sens_SC_SYNTSPEAK1_lp, Spec_SC_SYNTSPEAK1_lp)







