#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - stat

#ACCURACY

acc_stat1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat1_poly$overall[1])
acc_stat2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat2_poly$overall[1])
acc_stat3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat3_poly$overall[1])
acc_stat4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat4_poly$overall[1])
acc_stat5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat5_poly$overall[1])


acc_stat_SYNTSPEAK1_poly<- c(acc_stat1_SYNTSPEAK1_poly,acc_stat2_SYNTSPEAK1_poly,acc_stat3_SYNTSPEAK1_poly, 
                             acc_stat4_SYNTSPEAK1_poly, 
                             acc_stat5_SYNTSPEAK1_poly)

#F1 score

F1_stat1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat1_poly$byClass[7])
F1_stat2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat2_poly$byClass[7])
F1_stat3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat3_poly$byClass[7])
F1_stat4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat4_poly$byClass[7])
F1_stat5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat5_poly$byClass[7])


F1_stat_SYNTSPEAK1_poly<- c(F1_stat1_SYNTSPEAK1_poly,F1_stat2_SYNTSPEAK1_poly,F1_stat3_SYNTSPEAK1_poly,
                            F1_stat4_SYNTSPEAK1_poly, F1_stat5_SYNTSPEAK1_poly)

#Kappa

# Kappa_stat1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat1_poly$overall[2])
# Kappa_stat2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat2_poly$overall[2])
# Kappa_stat3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat3_poly$overall[2])
# Kappa_stat4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat4_poly$overall[2])
# Kappa_stat5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat5_poly$overall[2])
# 
# 
# Kappa_stat_SYNTSPEAK1_poly<- c(Kappa_stat1_SYNTSPEAK1_poly,Kappa_stat2_SYNTSPEAK1_poly,Kappa_stat3_SYNTSPEAK1_poly,
#                                Kappa_stat4_SYNTSPEAK1_poly, 
#                                Kappa_stat5_SYNTSPEAK1_poly)

#Precision

Precision_stat1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat1_poly$byClass[5])
Precision_stat2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat2_poly$byClass[5])
Precision_stat3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat3_poly$byClass[5])
Precision_stat4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat4_poly$byClass[5])
Precision_stat5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat5_poly$byClass[5])

Precision_stat_SYNTSPEAK1_poly<- c(Precision_stat1_SYNTSPEAK1_poly,Precision_stat2_SYNTSPEAK1_poly,
                                   Precision_stat3_SYNTSPEAK1_poly,
                                   Precision_stat4_SYNTSPEAK1_poly, Precision_stat5_SYNTSPEAK1_poly)

#Sensitivity

Sens_stat1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat1_poly$byClass[1])
Sens_stat2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat2_poly$byClass[1])
Sens_stat3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat3_poly$byClass[1])
Sens_stat4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat4_poly$byClass[1])
Sens_stat5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat5_poly$byClass[1])

Sens_stat_SYNTSPEAK1_poly<- c(Sens_stat1_SYNTSPEAK1_poly,Sens_stat2_SYNTSPEAK1_poly,Sens_stat3_SYNTSPEAK1_poly, 
                              Sens_stat4_SYNTSPEAK1_poly,
                              Sens_stat5_SYNTSPEAK1_poly)

#Specstaticity 

Spec_stat1_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat1_poly$byClass[2])
Spec_stat2_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat2_poly$byClass[2])
Spec_stat3_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat3_poly$byClass[2])
Spec_stat4_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat4_poly$byClass[2])
Spec_stat5_SYNTSPEAK1_poly<- as.numeric(CM_syntspeak1_stat5_poly$byClass[2])

Spec_stat_SYNTSPEAK1_poly<- c(Spec_stat1_SYNTSPEAK1_poly,Spec_stat2_SYNTSPEAK1_poly,Spec_stat3_SYNTSPEAK1_poly, 
                              Spec_stat4_SYNTSPEAK1_poly,
                              Spec_stat5_SYNTSPEAK1_poly)

#PRINT RESULTS ON A TABLE
nsvm<- c('stat1', 'stat2', 'stat3', 'stat4','stat5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score',  'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_poly<- data.frame(nsvm, acc_stat_SYNTSPEAK1_poly, F1_stat_SYNTSPEAK1_poly, 
                                 Precision_stat_SYNTSPEAK1_poly, 
                                 Sens_stat_SYNTSPEAK1_poly, Spec_stat_SYNTSPEAK1_poly)
names(tbl_SYNTSPEAK1_poly)<- cnames_1

ltabl_SYNTSPEAK1_poly<-kable(tbl_SYNTSPEAK1_poly, format = "latex", longtable = T, caption = "Results of SYNT vs 
                             SPEAK1 SVM
                             with Instantaneous Frequencies of each stats as features - Kernel: Polynomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_stat_SYNTSPEAK1_poly, F1_stat_SYNTSPEAK1_poly, Kappa_stat_SYNTSPEAK1_poly, Precision_stat_SYNTSPEAK1_poly, 
   Sens_stat_SYNTSPEAK1_poly, Spec_stat_SYNTSPEAK1_poly)







