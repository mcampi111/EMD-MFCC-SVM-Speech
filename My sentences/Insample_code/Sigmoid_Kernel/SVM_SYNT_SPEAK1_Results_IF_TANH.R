#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - IF

#ACCURACY

acc_IF1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF1_tan$overall[1])
acc_IF2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF2_tan$overall[1])
acc_IF3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF3_tan$overall[1])
acc_IF4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF4_tan$overall[1])
acc_IF5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF5_tan$overall[1])


acc_IF_SYNTSPEAK1_tan<- c(acc_IF1_SYNTSPEAK1_tan,acc_IF2_SYNTSPEAK1_tan,acc_IF3_SYNTSPEAK1_tan, 
                          acc_IF4_SYNTSPEAK1_tan, 
                          acc_IF5_SYNTSPEAK1_tan)

#F1 score

F1_IF1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF1_tan$byClass[7])
F1_IF2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF2_tan$byClass[7])
F1_IF3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF3_tan$byClass[7])
F1_IF4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF4_tan$byClass[7])
F1_IF5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF5_tan$byClass[7])


F1_IF_SYNTSPEAK1_tan<- c(F1_IF1_SYNTSPEAK1_tan,F1_IF2_SYNTSPEAK1_tan,F1_IF3_SYNTSPEAK1_tan,
                         F1_IF4_SYNTSPEAK1_tan, F1_IF5_SYNTSPEAK1_tan)

#Kappa

# Kappa_IF1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF1_tan$overall[2])
# Kappa_IF2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF2_tan$overall[2])
# Kappa_IF3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF3_tan$overall[2])
# Kappa_IF4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF4_tan$overall[2])
# Kappa_IF5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF5_tan$overall[2])
# 
# 
# Kappa_IF_SYNTSPEAK1_tan<- c(Kappa_IF1_SYNTSPEAK1_tan,Kappa_IF2_SYNTSPEAK1_tan,Kappa_IF3_SYNTSPEAK1_tan,
#                             Kappa_IF4_SYNTSPEAK1_tan, 
#                             Kappa_IF5_SYNTSPEAK1_tan)

#Precision

Precision_IF1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF1_tan$byClass[5])
Precision_IF2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF2_tan$byClass[5])
Precision_IF3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF3_tan$byClass[5])
Precision_IF4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF4_tan$byClass[5])
Precision_IF5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF5_tan$byClass[5])

Precision_IF_SYNTSPEAK1_tan<- c(Precision_IF1_SYNTSPEAK1_tan,Precision_IF2_SYNTSPEAK1_tan,
                                Precision_IF3_SYNTSPEAK1_tan,
                                Precision_IF4_SYNTSPEAK1_tan, Precision_IF5_SYNTSPEAK1_tan)

#Sensitivity

Sens_IF1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF1_tan$byClass[1])
Sens_IF2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF2_tan$byClass[1])
Sens_IF3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF3_tan$byClass[1])
Sens_IF4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF4_tan$byClass[1])
Sens_IF5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF5_tan$byClass[1])

Sens_IF_SYNTSPEAK1_tan<- c(Sens_IF1_SYNTSPEAK1_tan,Sens_IF2_SYNTSPEAK1_tan,Sens_IF3_SYNTSPEAK1_tan, 
                           Sens_IF4_SYNTSPEAK1_tan,
                           Sens_IF5_SYNTSPEAK1_tan)

#Specificity 

Spec_IF1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF1_tan$byClass[2])
Spec_IF2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF2_tan$byClass[2])
Spec_IF3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF3_tan$byClass[2])
Spec_IF4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF4_tan$byClass[2])
Spec_IF5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_IF5_tan$byClass[2])

Spec_IF_SYNTSPEAK1_tan<- c(Spec_IF1_SYNTSPEAK1_tan,Spec_IF2_SYNTSPEAK1_tan,Spec_IF3_SYNTSPEAK1_tan, 
                           Spec_IF4_SYNTSPEAK1_tan,
                           Spec_IF5_SYNTSPEAK1_tan)

#PRINT RESULTS ON A TABLE
nsvm<- c('IF1', 'IF2', 'IF3', 'IF4','IF5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score',  'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_tan<- data.frame(nsvm, acc_IF_SYNTSPEAK1_tan, F1_IF_SYNTSPEAK1_tan, 
                                Precision_IF_SYNTSPEAK1_tan, 
                                Sens_IF_SYNTSPEAK1_tan, Spec_IF_SYNTSPEAK1_tan)
names(tbl_SYNTSPEAK1_tan)<- cnames_1

ltabl_SYNTSPEAK1_tan<-kable(tbl_SYNTSPEAK1_tan, format = "latex", longtable = T, caption = "Results of SYNT vs 
                            SPEAK1 SVM
                            with Instantaneous Frequencies of each IMFs as features - Kernel: tannomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_IF_SYNTSPEAK1_tan, F1_IF_SYNTSPEAK1_tan, Kappa_IF_SYNTSPEAK1_tan, Precision_IF_SYNTSPEAK1_tan, 
   Sens_IF_SYNTSPEAK1_tan, Spec_IF_SYNTSPEAK1_tan)







