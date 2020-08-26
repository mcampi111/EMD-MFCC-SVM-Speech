#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - SC

#ACCURACY

acc_SC1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC1_poly$overall[1])
acc_SC2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC2_poly$overall[1])
acc_SC3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC3_poly$overall[1])
acc_SC4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC4_poly$overall[1])
acc_SC5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC5_poly$overall[1])


acc_SC_SYNTSPEAK1_poly<- c(acc_SC1_SYNTSPEAK1_poly,acc_SC2_SYNTSPEAK1_poly,acc_SC3_SYNTSPEAK1_poly, 
                             acc_SC4_SYNTSPEAK1_poly, 
                             acc_SC5_SYNTSPEAK1_poly)

#F1 score

F1_SC1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC1_poly$byClass[7])
F1_SC2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC2_poly$byClass[7])
F1_SC3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC3_poly$byClass[7])
F1_SC4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC4_poly$byClass[7])
F1_SC5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC5_poly$byClass[7])


F1_SC_SYNTSPEAK1_poly<- c(F1_SC1_SYNTSPEAK1_poly,F1_SC2_SYNTSPEAK1_poly,F1_SC3_SYNTSPEAK1_poly,
                            F1_SC4_SYNTSPEAK1_poly, F1_SC5_SYNTSPEAK1_poly)

#Kappa

# Kappa_SC1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC1_poly$overall[2])
# Kappa_SC2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC2_poly$overall[2])
# Kappa_SC3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC3_poly$overall[2])
# Kappa_SC4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC4_poly$overall[2])
# Kappa_SC5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC5_poly$overall[2])
# 
# 
# Kappa_SC_SYNTSPEAK1_poly<- c(Kappa_SC1_SYNTSPEAK1_poly,Kappa_SC2_SYNTSPEAK1_poly,Kappa_SC3_SYNTSPEAK1_poly,
#                                Kappa_SC4_SYNTSPEAK1_poly, 
#                                Kappa_SC5_SYNTSPEAK1_poly)

#Precision

Precision_SC1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC1_poly$byClass[5])
Precision_SC2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC2_poly$byClass[5])
Precision_SC3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC3_poly$byClass[5])
Precision_SC4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC4_poly$byClass[5])
Precision_SC5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC5_poly$byClass[5])

Precision_SC_SYNTSPEAK1_poly<- c(Precision_SC1_SYNTSPEAK1_poly,Precision_SC2_SYNTSPEAK1_poly,
                                   Precision_SC3_SYNTSPEAK1_poly,
                                   Precision_SC4_SYNTSPEAK1_poly, Precision_SC5_SYNTSPEAK1_poly)

#Sensitivity

Sens_SC1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC1_poly$byClass[1])
Sens_SC2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC2_poly$byClass[1])
Sens_SC3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC3_poly$byClass[1])
Sens_SC4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC4_poly$byClass[1])
Sens_SC5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC5_poly$byClass[1])

Sens_SC_SYNTSPEAK1_poly<- c(Sens_SC1_SYNTSPEAK1_poly,Sens_SC2_SYNTSPEAK1_poly,Sens_SC3_SYNTSPEAK1_poly, 
                              Sens_SC4_SYNTSPEAK1_poly,
                              Sens_SC5_SYNTSPEAK1_poly)

#SpecSCicity 

Spec_SC1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC1_poly$byClass[2])
Spec_SC2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC2_poly$byClass[2])
Spec_SC3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC3_poly$byClass[2])
Spec_SC4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC4_poly$byClass[2])
Spec_SC5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_SC5_poly$byClass[2])

Spec_SC_SYNTSPEAK1_poly<- c(Spec_SC1_SYNTSPEAK1_poly,Spec_SC2_SYNTSPEAK1_poly,Spec_SC3_SYNTSPEAK1_poly, 
                              Spec_SC4_SYNTSPEAK1_poly,
                              Spec_SC5_SYNTSPEAK1_poly)

#PRINT RESULTS ON A TABLE
nsvm<- c('SC1', 'SC2', 'SC3', 'SC4','SC5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score',  'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_poly<- data.frame(nsvm, acc_SC_SYNTSPEAK1_poly, F1_SC_SYNTSPEAK1_poly, 
                                 Precision_SC_SYNTSPEAK1_poly, 
                                 Sens_SC_SYNTSPEAK1_poly, Spec_SC_SYNTSPEAK1_poly)
names(tbl_SYNTSPEAK1_poly)<- cnames_1

ltabl_SYNTSPEAK1_poly<-kable(tbl_SYNTSPEAK1_poly, format = "latex", longtable = T, caption = "Results of SYNT vs 
                             SPEAK1 SVM
                             with Instantaneous Frequencies of each SCs as features - Kernel: Polynomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_SC_SYNTSPEAK1_poly, F1_SC_SYNTSPEAK1_poly, Kappa_SC_SYNTSPEAK1_poly, Precision_SC_SYNTSPEAK1_poly, 
   Sens_SC_SYNTSPEAK1_poly, Spec_SC_SYNTSPEAK1_poly)







