#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - IMF

#ACCURACY

acc_IMF1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF1_tan$overall[1])
acc_IMF2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF2_tan$overall[1])
acc_IMF3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF3_tan$overall[1])
acc_IMF4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF4_tan$overall[1])
acc_IMF5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF5_tan$overall[1])


acc_IMF_SYNTSPEAK1_tan<- c(acc_IMF1_SYNTSPEAK1_tan,acc_IMF2_SYNTSPEAK1_tan,acc_IMF3_SYNTSPEAK1_tan, 
                           acc_IMF4_SYNTSPEAK1_tan, 
                           acc_IMF5_SYNTSPEAK1_tan)

#F1 score

F1_IMF1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF1_tan$byClass[7])
F1_IMF2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF2_tan$byClass[7])
F1_IMF3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF3_tan$byClass[7])
F1_IMF4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF4_tan$byClass[7])
F1_IMF5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF5_tan$byClass[7])


F1_IMF_SYNTSPEAK1_tan<- c(F1_IMF1_SYNTSPEAK1_tan,F1_IMF2_SYNTSPEAK1_tan,F1_IMF3_SYNTSPEAK1_tan,
                          F1_IMF4_SYNTSPEAK1_tan, F1_IMF5_SYNTSPEAK1_tan)

#Kappa
# 
# Kappa_IMF1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF1_tan$overall[2])
# Kappa_IMF2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF2_tan$overall[2])
# Kappa_IMF3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF3_tan$overall[2])
# Kappa_IMF4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF4_tan$overall[2])
# Kappa_IMF5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF5_tan$overall[2])
# 
# 
# Kappa_IMF_SYNTSPEAK1_tan<- c(Kappa_IMF1_SYNTSPEAK1_tan,Kappa_IMF2_SYNTSPEAK1_tan,Kappa_IMF3_SYNTSPEAK1_tan,
#                              Kappa_IMF4_SYNTSPEAK1_tan, 
#                              Kappa_IMF5_SYNTSPEAK1_tan)
# 
#Precision

Precision_IMF1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF1_tan$byClass[5])
Precision_IMF2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF2_tan$byClass[5])
Precision_IMF3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF3_tan$byClass[5])
Precision_IMF4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF4_tan$byClass[5])
Precision_IMF5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF5_tan$byClass[5])

Precision_IMF_SYNTSPEAK1_tan<- c(Precision_IMF1_SYNTSPEAK1_tan,Precision_IMF2_SYNTSPEAK1_tan,
                                 Precision_IMF3_SYNTSPEAK1_tan,
                                 Precision_IMF4_SYNTSPEAK1_tan, Precision_IMF5_SYNTSPEAK1_tan)

#Sensitivity

Sens_IMF1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF1_tan$byClass[1])
Sens_IMF2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF2_tan$byClass[1])
Sens_IMF3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF3_tan$byClass[1])
Sens_IMF4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF4_tan$byClass[1])
Sens_IMF5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF5_tan$byClass[1])

Sens_IMF_SYNTSPEAK1_tan<- c(Sens_IMF1_SYNTSPEAK1_tan,Sens_IMF2_SYNTSPEAK1_tan,Sens_IMF3_SYNTSPEAK1_tan, 
                            Sens_IMF4_SYNTSPEAK1_tan,
                            Sens_IMF5_SYNTSPEAK1_tan)

#SpecIMFicity 

Spec_IMF1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF1_tan$byClass[2])
Spec_IMF2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF2_tan$byClass[2])
Spec_IMF3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF3_tan$byClass[2])
Spec_IMF4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF4_tan$byClass[2])
Spec_IMF5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IMF5_tan$byClass[2])

Spec_IMF_SYNTSPEAK1_tan<- c(Spec_IMF1_SYNTSPEAK1_tan,Spec_IMF2_SYNTSPEAK1_tan,Spec_IMF3_SYNTSPEAK1_tan, 
                            Spec_IMF4_SYNTSPEAK1_tan,
                            Spec_IMF5_SYNTSPEAK1_tan)

#PRINT RESULTS ON A TABLE
nsvm<- c('IMF1', 'IMF2', 'IMF3', 'IMF4','IMF5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score',  'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_tan<- data.frame(nsvm, acc_IMF_SYNTSPEAK1_tan, F1_IMF_SYNTSPEAK1_tan, 
                                Precision_IMF_SYNTSPEAK1_tan, 
                                Sens_IMF_SYNTSPEAK1_tan, Spec_IMF_SYNTSPEAK1_tan)
names(tbl_SYNTSPEAK1_tan)<- cnames_1

ltabl_SYNTSPEAK1_tan<-kable(tbl_SYNTSPEAK1_tan, format = "latex", longtable = T, caption = "Results of SYNT vs 
                            SPEAK1 SVM
                            with Instantaneous Frequencies of each IMFs as features - Kernel: tannomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_IMF_SYNTSPEAK1_tan, F1_IMF_SYNTSPEAK1_tan, Kappa_IMF_SYNTSPEAK1_tan, Precision_IMF_SYNTSPEAK1_tan, 
   Sens_IMF_SYNTSPEAK1_tan, Spec_IMF_SYNTSPEAK1_tan)







