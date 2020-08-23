#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - IMF

#ACCURACY

acc_IMF1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF1_poly$overall[1])
acc_IMF2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF2_poly$overall[1])
acc_IMF3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF3_poly$overall[1])
acc_IMF4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF4_poly$overall[1])
acc_IMF5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF5_poly$overall[1])


acc_IMF_SYNTSPEAK1_poly<- c(acc_IMF1_SYNTSPEAK1_poly,acc_IMF2_SYNTSPEAK1_poly,acc_IMF3_SYNTSPEAK1_poly, 
                            acc_IMF4_SYNTSPEAK1_poly, 
                            acc_IMF5_SYNTSPEAK1_poly)

#F1 score

F1_IMF1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF1_poly$byClass[7])
F1_IMF2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF2_poly$byClass[7])
F1_IMF3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF3_poly$byClass[7])
F1_IMF4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF4_poly$byClass[7])
F1_IMF5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF5_poly$byClass[7])


F1_IMF_SYNTSPEAK1_poly<- c(F1_IMF1_SYNTSPEAK1_poly,F1_IMF2_SYNTSPEAK1_poly,F1_IMF3_SYNTSPEAK1_poly,
                           F1_IMF4_SYNTSPEAK1_poly, F1_IMF5_SYNTSPEAK1_poly)

#Kappa

# Kappa_IMF1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF1_poly$overall[2])
# Kappa_IMF2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF2_poly$overall[2])
# Kappa_IMF3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF3_poly$overall[2])
# Kappa_IMF4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF4_poly$overall[2])
# Kappa_IMF5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF5_poly$overall[2])
# 
# 
# Kappa_IMF_SYNTSPEAK1_poly<- c(Kappa_IMF1_SYNTSPEAK1_poly,Kappa_IMF2_SYNTSPEAK1_poly,Kappa_IMF3_SYNTSPEAK1_poly,
#                               Kappa_IMF4_SYNTSPEAK1_poly, 
#                               Kappa_IMF5_SYNTSPEAK1_poly)

#Precision

Precision_IMF1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF1_poly$byClass[5])
Precision_IMF2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF2_poly$byClass[5])
Precision_IMF3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF3_poly$byClass[5])
Precision_IMF4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF4_poly$byClass[5])
Precision_IMF5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF5_poly$byClass[5])

Precision_IMF_SYNTSPEAK1_poly<- c(Precision_IMF1_SYNTSPEAK1_poly,Precision_IMF2_SYNTSPEAK1_poly,
                                  Precision_IMF3_SYNTSPEAK1_poly,
                                  Precision_IMF4_SYNTSPEAK1_poly, Precision_IMF5_SYNTSPEAK1_poly)

#Sensitivity

Sens_IMF1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF1_poly$byClass[1])
Sens_IMF2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF2_poly$byClass[1])
Sens_IMF3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF3_poly$byClass[1])
Sens_IMF4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF4_poly$byClass[1])
Sens_IMF5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF5_poly$byClass[1])

Sens_IMF_SYNTSPEAK1_poly<- c(Sens_IMF1_SYNTSPEAK1_poly,Sens_IMF2_SYNTSPEAK1_poly,Sens_IMF3_SYNTSPEAK1_poly, 
                             Sens_IMF4_SYNTSPEAK1_poly,
                             Sens_IMF5_SYNTSPEAK1_poly)

#SpecIMFicity 

Spec_IMF1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF1_poly$byClass[2])
Spec_IMF2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF2_poly$byClass[2])
Spec_IMF3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF3_poly$byClass[2])
Spec_IMF4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF4_poly$byClass[2])
Spec_IMF5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IMF5_poly$byClass[2])

Spec_IMF_SYNTSPEAK1_poly<- c(Spec_IMF1_SYNTSPEAK1_poly,Spec_IMF2_SYNTSPEAK1_poly,Spec_IMF3_SYNTSPEAK1_poly, 
                             Spec_IMF4_SYNTSPEAK1_poly,
                             Spec_IMF5_SYNTSPEAK1_poly)

#PRINT RESULTS ON A TABLE
nsvm<- c('IMF1', 'IMF2', 'IMF3', 'IMF4','IMF5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score', 'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_poly<- data.frame(nsvm, acc_IMF_SYNTSPEAK1_poly, F1_IMF_SYNTSPEAK1_poly, 
                                 Precision_IMF_SYNTSPEAK1_poly, 
                                 Sens_IMF_SYNTSPEAK1_poly, Spec_IMF_SYNTSPEAK1_poly)
names(tbl_SYNTSPEAK1_poly)<- cnames_1

ltabl_SYNTSPEAK1_poly<-kable(tbl_SYNTSPEAK1_poly, format = "latex", longtable = T, caption = "Results of SYNT vs 
                             SPEAK1 SVM
                             with Instantaneous Frequencies of each IMFs as features - Kernel: Polynomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_IMF_SYNTSPEAK1_poly, F1_IMF_SYNTSPEAK1_poly, Kappa_IMF_SYNTSPEAK1_poly, Precision_IMF_SYNTSPEAK1_poly, 
   Sens_IMF_SYNTSPEAK1_poly, Spec_IMF_SYNTSPEAK1_poly)







