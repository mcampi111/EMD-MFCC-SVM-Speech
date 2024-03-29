#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#ACCURACY

acc_coeff1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff1_new$overall[1])
acc_coeff2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff2_new$overall[1])
acc_coeff3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff3_new$overall[1])
acc_coeff4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff4_new$overall[1])
acc_coeff5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff5_new$overall[1])
acc_coeff6_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff6_new$overall[1])
acc_coeff7_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff7_new$overall[1])
acc_coeff8_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff8_new$overall[1])
acc_coeff9_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff9_new$overall[1])
acc_coeff10_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff10_new$overall[1])
acc_coeff11_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff11_new$overall[1])
acc_coeff12_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff12_new$overall[1])

acc_coeff_SYNTSPEAK1<- c(acc_coeff1_SYNTSPEAK1,acc_coeff2_SYNTSPEAK1,acc_coeff3_SYNTSPEAK1, acc_coeff4_SYNTSPEAK1, 
                      acc_coeff5_SYNTSPEAK1,acc_coeff6_SYNTSPEAK1,acc_coeff7_SYNTSPEAK1,acc_coeff8_SYNTSPEAK1,
                      acc_coeff9_SYNTSPEAK1,acc_coeff10_SYNTSPEAK1,acc_coeff11_SYNTSPEAK1,acc_coeff12_SYNTSPEAK1)

#F1 score

F1_coeff1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff1_new$byClass[7])
F1_coeff2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff2_new$byClass[7])
F1_coeff3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff3_new$byClass[7])
F1_coeff4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff4_new$byClass[7])
F1_coeff5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff5_new$byClass[7])
F1_coeff6_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff6_new$byClass[7])
F1_coeff7_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff7_new$byClass[7])
F1_coeff8_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff8_new$byClass[7])
F1_coeff9_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff9_new$byClass[7])
F1_coeff10_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff10_new$byClass[7])
F1_coeff11_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff11_new$byClass[7])
F1_coeff12_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff12_new$byClass[7])

F1_coeff_SYNTSPEAK1<- c(F1_coeff1_SYNTSPEAK1,F1_coeff2_SYNTSPEAK1,F1_coeff3_SYNTSPEAK1, F1_coeff4_SYNTSPEAK1, 
                        F1_coeff5_SYNTSPEAK1,F1_coeff6_SYNTSPEAK1,F1_coeff7_SYNTSPEAK1,F1_coeff8_SYNTSPEAK1,
                        F1_coeff9_SYNTSPEAK1,F1_coeff10_SYNTSPEAK1,F1_coeff11_SYNTSPEAK1,F1_coeff12_SYNTSPEAK1)

#Precision

Precision_coeff1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff1_new$byClass[5])
Precision_coeff2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff2_new$byClass[5])
Precision_coeff3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff3_new$byClass[5])
Precision_coeff4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff4_new$byClass[5])
Precision_coeff5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff5_new$byClass[5])
Precision_coeff6_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff6_new$byClass[5])
Precision_coeff7_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff7_new$byClass[5])
Precision_coeff8_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff8_new$byClass[5])
Precision_coeff9_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff9_new$byClass[5])
Precision_coeff10_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff10_new$byClass[5])
Precision_coeff11_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff11_new$byClass[5])
Precision_coeff12_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff12_new$byClass[5])

Precision_coeff_SYNTSPEAK1<- c(Precision_coeff1_SYNTSPEAK1,Precision_coeff2_SYNTSPEAK1,Precision_coeff3_SYNTSPEAK1, Precision_coeff4_SYNTSPEAK1, 
                               Precision_coeff5_SYNTSPEAK1,Precision_coeff6_SYNTSPEAK1,Precision_coeff7_SYNTSPEAK1,Precision_coeff8_SYNTSPEAK1,
                               Precision_coeff9_SYNTSPEAK1,Precision_coeff10_SYNTSPEAK1,Precision_coeff11_SYNTSPEAK1,Precision_coeff12_SYNTSPEAK1)

#Sensitivity

Sens_coeff1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff1_new$byClass[1])
Sens_coeff2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff2_new$byClass[1])
Sens_coeff3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff3_new$byClass[1])
Sens_coeff4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff4_new$byClass[1])
Sens_coeff5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff5_new$byClass[1])
Sens_coeff6_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff6_new$byClass[1])
Sens_coeff7_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff7_new$byClass[1])
Sens_coeff8_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff8_new$byClass[1])
Sens_coeff9_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff9_new$byClass[1])
Sens_coeff10_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff10_new$byClass[1])
Sens_coeff11_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff11_new$byClass[1])
Sens_coeff12_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff12_new$byClass[1])

Sens_coeff_SYNTSPEAK1<- c(Sens_coeff1_SYNTSPEAK1,Sens_coeff2_SYNTSPEAK1,Sens_coeff3_SYNTSPEAK1, Sens_coeff4_SYNTSPEAK1, 
                          Sens_coeff5_SYNTSPEAK1,Sens_coeff6_SYNTSPEAK1,Sens_coeff7_SYNTSPEAK1,Sens_coeff8_SYNTSPEAK1,
                          Sens_coeff9_SYNTSPEAK1,Sens_coeff10_SYNTSPEAK1,Sens_coeff11_SYNTSPEAK1,Sens_coeff12_SYNTSPEAK1)
#Specificity 

Spec_coeff1_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff1_new$byClass[2])
Spec_coeff2_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff2_new$byClass[2])
Spec_coeff3_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff3_new$byClass[2])
Spec_coeff4_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff4_new$byClass[2])
Spec_coeff5_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff5_new$byClass[2])
Spec_coeff6_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff6_new$byClass[2])
Spec_coeff7_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff7_new$byClass[2])
Spec_coeff8_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff8_new$byClass[2])
Spec_coeff9_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff9_new$byClass[2])
Spec_coeff10_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff10_new$byClass[2])
Spec_coeff11_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff11_new$byClass[2])
Spec_coeff12_SYNTSPEAK1<- as.numeric(CM_syntspeak1_MLFC_IMF1_coeff12_new$byClass[2])

Spec_coeff_SYNTSPEAK1<- c(Spec_coeff1_SYNTSPEAK1,Spec_coeff2_SYNTSPEAK1,Spec_coeff3_SYNTSPEAK1, Spec_coeff4_SYNTSPEAK1, 
                          Spec_coeff5_SYNTSPEAK1,Spec_coeff6_SYNTSPEAK1,Spec_coeff7_SYNTSPEAK1,Spec_coeff8_SYNTSPEAK1,
                          Spec_coeff9_SYNTSPEAK1,Spec_coeff10_SYNTSPEAK1,Spec_coeff11_SYNTSPEAK1,Spec_coeff12_SYNTSPEAK1)

#PRINT RESULTS ON A TABLE
nsvm<- c('coeff1', 'coeff2', 'coeff3', 'coeff4','coeff5', 'coeff6', 'coeff7', 'coeff8', 'coeff9', 'coeff10', 
         'coeff11', 'coeff12')

cnames_1<- list('SYNT vs SPEAK1 - IMF1 ', 'Accuracy', 'F1-score', 'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1<- data.frame(nsvm, acc_coeff_SYNTSPEAK1, F1_coeff_SYNTSPEAK1,  Precision_coeff_SYNTSPEAK1, 
                            Sens_coeff_SYNTSPEAK1, Spec_coeff_SYNTSPEAK1)
names(tbl_SYNTSPEAK1)<- cnames_1

ltabl_SYNTSPEAK1<-kable(tbl_SYNTSPEAK1, format = "latex", longtable = T, caption = "Results of SYNT vs SPEAK1 SVM
                        with Instantaneous Frequencies of each IMFs as features - Kernel:RBF.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_coeff_SYNTSPEAK1, F1_coeff_SYNTSPEAK1, Kappa_coeff_SYNTSPEAK1, Precision_coeff_SYNTSPEAK1, 
   Sens_coeff_SYNTSPEAK1, Spec_coeff_SYNTSPEAK1)


save(ltabl_SYNTSPEAK1, file = "C:\\Users\\Marta\\Desktop\\Speech_OUT\\SVM_Speaker1_out\\TAN\\table_speak1_tan_CEP_IMF1_out.RData")




