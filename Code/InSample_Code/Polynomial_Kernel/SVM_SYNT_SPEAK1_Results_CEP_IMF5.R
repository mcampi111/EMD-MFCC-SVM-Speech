#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#ACCURACY

acc_coeff1_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff1_poly$overall[1])
acc_coeff2_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff2_poly$overall[1])
acc_coeff3_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff3_poly$overall[1])
acc_coeff4_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff4_poly$overall[1])
acc_coeff5_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff5_poly$overall[1])
acc_coeff6_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff6_poly$overall[1])
acc_coeff7_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff7_poly$overall[1])
acc_coeff8_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff8_poly$overall[1])
acc_coeff9_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff9_poly$overall[1])
acc_coeff10_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff10_poly$overall[1])
acc_coeff11_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff11_poly$overall[1])
acc_coeff12_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff12_poly$overall[1])

acc_coeff_SYNTSPEAK1<- c(acc_coeff1_poly_SYNTSPEAK1,acc_coeff2_poly_SYNTSPEAK1,acc_coeff3_poly_SYNTSPEAK1, acc_coeff4_poly_SYNTSPEAK1, 
                         acc_coeff5_poly_SYNTSPEAK1,acc_coeff6_poly_SYNTSPEAK1,acc_coeff7_poly_SYNTSPEAK1,acc_coeff8_poly_SYNTSPEAK1,
                         acc_coeff9_poly_SYNTSPEAK1,acc_coeff10_poly_SYNTSPEAK1,acc_coeff11_poly_SYNTSPEAK1,acc_coeff12_poly_SYNTSPEAK1)

#F1 score

F1_coeff1_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff1_poly$byClass[7])
F1_coeff2_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff2_poly$byClass[7])
F1_coeff3_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff3_poly$byClass[7])
F1_coeff4_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff4_poly$byClass[7])
F1_coeff5_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff5_poly$byClass[7])
F1_coeff6_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff6_poly$byClass[7])
F1_coeff7_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff7_poly$byClass[7])
F1_coeff8_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff8_poly$byClass[7])
F1_coeff9_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff9_poly$byClass[7])
F1_coeff10_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff10_poly$byClass[7])
F1_coeff11_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff11_poly$byClass[7])
F1_coeff12_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff12_poly$byClass[7])

F1_coeff_SYNTSPEAK1<- c(F1_coeff1_poly_SYNTSPEAK1,F1_coeff2_poly_SYNTSPEAK1,F1_coeff3_poly_SYNTSPEAK1, F1_coeff4_poly_SYNTSPEAK1, 
                        F1_coeff5_poly_SYNTSPEAK1,F1_coeff6_poly_SYNTSPEAK1,F1_coeff7_poly_SYNTSPEAK1,F1_coeff8_poly_SYNTSPEAK1,
                        F1_coeff9_poly_SYNTSPEAK1,F1_coeff10_poly_SYNTSPEAK1,F1_coeff11_poly_SYNTSPEAK1,F1_coeff12_poly_SYNTSPEAK1)

#Precision

Precision_coeff1_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff1_poly$byClass[5])
Precision_coeff2_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff2_poly$byClass[5])
Precision_coeff3_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff3_poly$byClass[5])
Precision_coeff4_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff4_poly$byClass[5])
Precision_coeff5_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff5_poly$byClass[5])
Precision_coeff6_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff6_poly$byClass[5])
Precision_coeff7_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff7_poly$byClass[5])
Precision_coeff8_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff8_poly$byClass[5])
Precision_coeff9_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff9_poly$byClass[5])
Precision_coeff10_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff10_poly$byClass[5])
Precision_coeff11_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff11_poly$byClass[5])
Precision_coeff12_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff12_poly$byClass[5])

Precision_coeff_SYNTSPEAK1<- c(Precision_coeff1_poly_SYNTSPEAK1,Precision_coeff2_poly_SYNTSPEAK1,Precision_coeff3_poly_SYNTSPEAK1, Precision_coeff4_poly_SYNTSPEAK1, 
                               Precision_coeff5_poly_SYNTSPEAK1,Precision_coeff6_poly_SYNTSPEAK1,Precision_coeff7_poly_SYNTSPEAK1,Precision_coeff8_poly_SYNTSPEAK1,
                               Precision_coeff9_poly_SYNTSPEAK1,Precision_coeff10_poly_SYNTSPEAK1,Precision_coeff11_poly_SYNTSPEAK1,Precision_coeff12_poly_SYNTSPEAK1)

#Sensitivity

Sens_coeff1_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff1_poly$byClass[1])
Sens_coeff2_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff2_poly$byClass[1])
Sens_coeff3_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff3_poly$byClass[1])
Sens_coeff4_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff4_poly$byClass[1])
Sens_coeff5_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff5_poly$byClass[1])
Sens_coeff6_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff6_poly$byClass[1])
Sens_coeff7_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff7_poly$byClass[1])
Sens_coeff8_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff8_poly$byClass[1])
Sens_coeff9_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff9_poly$byClass[1])
Sens_coeff10_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff10_poly$byClass[1])
Sens_coeff11_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff11_poly$byClass[1])
Sens_coeff12_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff12_poly$byClass[1])

Sens_coeff_SYNTSPEAK1<- c(Sens_coeff1_poly_SYNTSPEAK1,Sens_coeff2_poly_SYNTSPEAK1,Sens_coeff3_poly_SYNTSPEAK1, Sens_coeff4_poly_SYNTSPEAK1, 
                          Sens_coeff5_poly_SYNTSPEAK1,Sens_coeff6_poly_SYNTSPEAK1,Sens_coeff7_poly_SYNTSPEAK1,Sens_coeff8_poly_SYNTSPEAK1,
                          Sens_coeff9_poly_SYNTSPEAK1,Sens_coeff10_poly_SYNTSPEAK1,Sens_coeff11_poly_SYNTSPEAK1,Sens_coeff12_poly_SYNTSPEAK1)
#Specificity 

Spec_coeff1_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff1_poly$byClass[2])
Spec_coeff2_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff2_poly$byClass[2])
Spec_coeff3_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff3_poly$byClass[2])
Spec_coeff4_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff4_poly$byClass[2])
Spec_coeff5_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff5_poly$byClass[2])
Spec_coeff6_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff6_poly$byClass[2])
Spec_coeff7_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff7_poly$byClass[2])
Spec_coeff8_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff8_poly$byClass[2])
Spec_coeff9_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff9_poly$byClass[2])
Spec_coeff10_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff10_poly$byClass[2])
Spec_coeff11_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff11_poly$byClass[2])
Spec_coeff12_poly_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF5_coeff12_poly$byClass[2])

Spec_coeff_SYNTSPEAK1<- c(Spec_coeff1_poly_SYNTSPEAK1,Spec_coeff2_poly_SYNTSPEAK1,Spec_coeff3_poly_SYNTSPEAK1, Spec_coeff4_poly_SYNTSPEAK1, 
                          Spec_coeff5_poly_SYNTSPEAK1,Spec_coeff6_poly_SYNTSPEAK1,Spec_coeff7_poly_SYNTSPEAK1,Spec_coeff8_poly_SYNTSPEAK1,
                          Spec_coeff9_poly_SYNTSPEAK1,Spec_coeff10_poly_SYNTSPEAK1,Spec_coeff11_poly_SYNTSPEAK1,Spec_coeff12_poly_SYNTSPEAK1)

#PRINT RESULTS ON A TABLE
nsvm<- c('coeff1_poly', 'coeff2_poly', 'coeff3_poly', 'coeff4_poly','coeff5_poly', 'coeff6_poly', 'coeff7_poly', 'coeff8_poly', 'coeff9_poly', 'coeff10_poly', 
         'coeff11_poly', 'coeff12_poly')

cnames_1<- list('SYNT vs SPEAK1 - IMF5 ', 'Accuracy', 'F1-score', 'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1<- data.frame(nsvm, acc_coeff_SYNTSPEAK1, F1_coeff_SYNTSPEAK1,  Precision_coeff_SYNTSPEAK1, 
                            Sens_coeff_SYNTSPEAK1, Spec_coeff_SYNTSPEAK1)
names(tbl_SYNTSPEAK1)<- cnames_1

ltabl_SYNTSPEAK1<-kable(tbl_SYNTSPEAK1, format = "latex", longtable = T, caption = "Results of SYNT vs SPEAK1 SVM
                        with Instantaneous Frequencies of each IMFs as features - Kernel:RBF.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_coeff_SYNTSPEAK1, F1_coeff_SYNTSPEAK1, Kappa_coeff_SYNTSPEAK1, Precision_coeff_SYNTSPEAK1, 
   Sens_coeff_SYNTSPEAK1, Spec_coeff_SYNTSPEAK1)







