#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - IF

#ACCURACY

acc_IF1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF1_poly$overall[1])
acc_IF2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF2_poly$overall[1])
acc_IF3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF3_poly$overall[1])
acc_IF4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF4_poly$overall[1])
acc_IF5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF5_poly$overall[1])


acc_IF_SYNTSPEAK1_poly<- c(acc_IF1_SYNTSPEAK1_poly,acc_IF2_SYNTSPEAK1_poly,acc_IF3_SYNTSPEAK1_poly, 
                           acc_IF4_SYNTSPEAK1_poly, 
                           acc_IF5_SYNTSPEAK1_poly)

#F1 score

F1_IF1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF1_poly$byClass[7])
F1_IF2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF2_poly$byClass[7])
F1_IF3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF3_poly$byClass[7])
F1_IF4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF4_poly$byClass[7])
F1_IF5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF5_poly$byClass[7])


F1_IF_SYNTSPEAK1_poly<- c(F1_IF1_SYNTSPEAK1_poly,F1_IF2_SYNTSPEAK1_poly,F1_IF3_SYNTSPEAK1_poly,
                          F1_IF4_SYNTSPEAK1_poly, F1_IF5_SYNTSPEAK1_poly)

#Kappa

# Kappa_IF1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF1_poly$overall[2])
# Kappa_IF2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF2_poly$overall[2])
# Kappa_IF3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF3_poly$overall[2])
# Kappa_IF4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF4_poly$overall[2])
# Kappa_IF5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF5_poly$overall[2])
# 
# 
# Kappa_IF_SYNTSPEAK1_poly<- c(Kappa_IF1_SYNTSPEAK1_poly,Kappa_IF2_SYNTSPEAK1_poly,Kappa_IF3_SYNTSPEAK1_poly,
#                              Kappa_IF4_SYNTSPEAK1_poly, 
#                              Kappa_IF5_SYNTSPEAK1_poly)

#Precision

Precision_IF1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF1_poly$byClass[5])
Precision_IF2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF2_poly$byClass[5])
Precision_IF3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF3_poly$byClass[5])
Precision_IF4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF4_poly$byClass[5])
Precision_IF5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF5_poly$byClass[5])

Precision_IF_SYNTSPEAK1_poly<- c(Precision_IF1_SYNTSPEAK1_poly,Precision_IF2_SYNTSPEAK1_poly,
                                 Precision_IF3_SYNTSPEAK1_poly,
                            Precision_IF4_SYNTSPEAK1_poly, Precision_IF5_SYNTSPEAK1_poly)

#Sensitivity

Sens_IF1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF1_poly$byClass[1])
Sens_IF2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF2_poly$byClass[1])
Sens_IF3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF3_poly$byClass[1])
Sens_IF4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF4_poly$byClass[1])
Sens_IF5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF5_poly$byClass[1])

Sens_IF_SYNTSPEAK1_poly<- c(Sens_IF1_SYNTSPEAK1_poly,Sens_IF2_SYNTSPEAK1_poly,Sens_IF3_SYNTSPEAK1_poly, 
                            Sens_IF4_SYNTSPEAK1_poly,
                            Sens_IF5_SYNTSPEAK1_poly)

#Specificity 

Spec_IF1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF1_poly$byClass[2])
Spec_IF2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF2_poly$byClass[2])
Spec_IF3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF3_poly$byClass[2])
Spec_IF4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF4_poly$byClass[2])
Spec_IF5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_IF5_poly$byClass[2])

Spec_IF_SYNTSPEAK1_poly<- c(Spec_IF1_SYNTSPEAK1_poly,Spec_IF2_SYNTSPEAK1_poly,Spec_IF3_SYNTSPEAK1_poly, 
                            Spec_IF4_SYNTSPEAK1_poly,
                            Spec_IF5_SYNTSPEAK1_poly)

#PRINT RESULTS ON A TABLE
nsvm<- c('IF1', 'IF2', 'IF3', 'IF4','IF5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score', 'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_poly<- data.frame(nsvm, acc_IF_SYNTSPEAK1_poly, F1_IF_SYNTSPEAK1_poly, 
                                 Precision_IF_SYNTSPEAK1_poly, 
                                 Sens_IF_SYNTSPEAK1_poly, Spec_IF_SYNTSPEAK1_poly)
names(tbl_SYNTSPEAK1_poly)<- cnames_1

ltabl_SYNTSPEAK1_poly<-kable(tbl_SYNTSPEAK1_poly, format = "latex", longtable = T, caption = "Results of SYNT vs 
SPEAK1 SVM
                        with Instantaneous Frequencies of each IMFs as features - Kernel: Polynomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_IF_SYNTSPEAK1_poly, F1_IF_SYNTSPEAK1_poly, Kappa_IF_SYNTSPEAK1_poly, Precision_IF_SYNTSPEAK1_poly, 
   Sens_IF_SYNTSPEAK1_poly, Spec_IF_SYNTSPEAK1_poly)







