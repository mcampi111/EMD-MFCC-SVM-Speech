#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - SC

#ACCURACY

acc_SC1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC1_tan$overall[1])
acc_SC2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC2_tan$overall[1])
acc_SC3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC3_tan$overall[1])
acc_SC4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC4_tan$overall[1])
acc_SC5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC5_tan$overall[1])


acc_SC_SYNTSPEAK1_tan<- c(acc_SC1_SYNTSPEAK1_tan,acc_SC2_SYNTSPEAK1_tan,acc_SC3_SYNTSPEAK1_tan, 
                            acc_SC4_SYNTSPEAK1_tan, 
                            acc_SC5_SYNTSPEAK1_tan)

#F1 score

F1_SC1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC1_tan$byClass[7])
F1_SC2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC2_tan$byClass[7])
F1_SC3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC3_tan$byClass[7])
F1_SC4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC4_tan$byClass[7])
F1_SC5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC5_tan$byClass[7])


F1_SC_SYNTSPEAK1_tan<- c(F1_SC1_SYNTSPEAK1_tan,F1_SC2_SYNTSPEAK1_tan,F1_SC3_SYNTSPEAK1_tan,
                           F1_SC4_SYNTSPEAK1_tan, F1_SC5_SYNTSPEAK1_tan)

#Kappa

# Kappa_SC1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC1_tan$overall[2])
# Kappa_SC2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC2_tan$overall[2])
# Kappa_SC3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC3_tan$overall[2])
# Kappa_SC4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC4_tan$overall[2])
# Kappa_SC5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC5_tan$overall[2])
# 
# 
# Kappa_SC_SYNTSPEAK1_tan<- c(Kappa_SC1_SYNTSPEAK1_tan,Kappa_SC2_SYNTSPEAK1_tan,Kappa_SC3_SYNTSPEAK1_tan,
#                               Kappa_SC4_SYNTSPEAK1_tan, 
#                               Kappa_SC5_SYNTSPEAK1_tan)

#Precision

Precision_SC1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC1_tan$byClass[5])
Precision_SC2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC2_tan$byClass[5])
Precision_SC3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC3_tan$byClass[5])
Precision_SC4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC4_tan$byClass[5])
Precision_SC5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC5_tan$byClass[5])

Precision_SC_SYNTSPEAK1_tan<- c(Precision_SC1_SYNTSPEAK1_tan,Precision_SC2_SYNTSPEAK1_tan,
                                  Precision_SC3_SYNTSPEAK1_tan,
                                  Precision_SC4_SYNTSPEAK1_tan, Precision_SC5_SYNTSPEAK1_tan)

#Sensitivity

Sens_SC1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC1_tan$byClass[1])
Sens_SC2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC2_tan$byClass[1])
Sens_SC3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC3_tan$byClass[1])
Sens_SC4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC4_tan$byClass[1])
Sens_SC5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC5_tan$byClass[1])

Sens_SC_SYNTSPEAK1_tan<- c(Sens_SC1_SYNTSPEAK1_tan,Sens_SC2_SYNTSPEAK1_tan,Sens_SC3_SYNTSPEAK1_tan, 
                             Sens_SC4_SYNTSPEAK1_tan,
                             Sens_SC5_SYNTSPEAK1_tan)

#SpecSCicity 

Spec_SC1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC1_tan$byClass[2])
Spec_SC2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC2_tan$byClass[2])
Spec_SC3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC3_tan$byClass[2])
Spec_SC4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC4_tan$byClass[2])
Spec_SC5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_SC5_tan$byClass[2])

Spec_SC_SYNTSPEAK1_tan<- c(Spec_SC1_SYNTSPEAK1_tan,Spec_SC2_SYNTSPEAK1_tan,Spec_SC3_SYNTSPEAK1_tan, 
                             Spec_SC4_SYNTSPEAK1_tan,
                             Spec_SC5_SYNTSPEAK1_tan)

#PRINT RESULTS ON A TABLE
nsvm<- c('SC1', 'SC2', 'SC3', 'SC4','SC5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score',  'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_tan<- data.frame(nsvm, acc_SC_SYNTSPEAK1_tan, F1_SC_SYNTSPEAK1_tan, 
                                Precision_SC_SYNTSPEAK1_tan, 
                                Sens_SC_SYNTSPEAK1_tan, Spec_SC_SYNTSPEAK1_tan)
names(tbl_SYNTSPEAK1_tan)<- cnames_1

ltabl_SYNTSPEAK1_tan<-kable(tbl_SYNTSPEAK1_tan, format = "latex", longtable = T, caption = "Results of SYNT vs 
                            SPEAK1 SVM
                            with Instantaneous Frequencies of each SCs as features - Kernel: tannomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_SC_SYNTSPEAK1_tan, F1_SC_SYNTSPEAK1_tan, Kappa_SC_SYNTSPEAK1_tan, Precision_SC_SYNTSPEAK1_tan, 
   Sens_SC_SYNTSPEAK1_tan, Spec_SC_SYNTSPEAK1_tan)








