#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - stat

#ACCURACY

acc_stat1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat1_bess$overall[1])
acc_stat2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat2_bess$overall[1])
acc_stat3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat3_bess$overall[1])
acc_stat4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat4_bess$overall[1])
acc_stat5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat5_bess$overall[1])


acc_stat_SYNTSPEAK1_bess<- c(acc_stat1_SYNTSPEAK1_bess,acc_stat2_SYNTSPEAK1_bess,acc_stat3_SYNTSPEAK1_bess, 
                             acc_stat4_SYNTSPEAK1_bess, 
                             acc_stat5_SYNTSPEAK1_bess)

#F1 score

F1_stat1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat1_bess$byClass[7])
F1_stat2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat2_bess$byClass[7])
F1_stat3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat3_bess$byClass[7])
F1_stat4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat4_bess$byClass[7])
F1_stat5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat5_bess$byClass[7])


F1_stat_SYNTSPEAK1_bess<- c(F1_stat1_SYNTSPEAK1_bess,F1_stat2_SYNTSPEAK1_bess,F1_stat3_SYNTSPEAK1_bess,
                            F1_stat4_SYNTSPEAK1_bess, F1_stat5_SYNTSPEAK1_bess)

#Kappa

# Kappa_stat1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat1_bess$overall[2])
# Kappa_stat2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat2_bess$overall[2])
# Kappa_stat3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat3_bess$overall[2])
# Kappa_stat4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat4_bess$overall[2])
# Kappa_stat5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat5_bess$overall[2])
# 
# 
# Kappa_stat_SYNTSPEAK1_bess<- c(Kappa_stat1_SYNTSPEAK1_bess,Kappa_stat2_SYNTSPEAK1_bess,Kappa_stat3_SYNTSPEAK1_bess,
#                                Kappa_stat4_SYNTSPEAK1_bess, 
#                                Kappa_stat5_SYNTSPEAK1_bess)

#Precision

Precision_stat1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat1_bess$byClass[5])
Precision_stat2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat2_bess$byClass[5])
Precision_stat3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat3_bess$byClass[5])
Precision_stat4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat4_bess$byClass[5])
Precision_stat5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat5_bess$byClass[5])

Precision_stat_SYNTSPEAK1_bess<- c(Precision_stat1_SYNTSPEAK1_bess,Precision_stat2_SYNTSPEAK1_bess,
                                   Precision_stat3_SYNTSPEAK1_bess,
                                   Precision_stat4_SYNTSPEAK1_bess, Precision_stat5_SYNTSPEAK1_bess)

#Sensitivity

Sens_stat1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat1_bess$byClass[1])
Sens_stat2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat2_bess$byClass[1])
Sens_stat3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat3_bess$byClass[1])
Sens_stat4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat4_bess$byClass[1])
Sens_stat5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat5_bess$byClass[1])

Sens_stat_SYNTSPEAK1_bess<- c(Sens_stat1_SYNTSPEAK1_bess,Sens_stat2_SYNTSPEAK1_bess,Sens_stat3_SYNTSPEAK1_bess, 
                              Sens_stat4_SYNTSPEAK1_bess,
                              Sens_stat5_SYNTSPEAK1_bess)

#Specstaticity 

Spec_stat1_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat1_bess$byClass[2])
Spec_stat2_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat2_bess$byClass[2])
Spec_stat3_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat3_bess$byClass[2])
Spec_stat4_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat4_bess$byClass[2])
Spec_stat5_SYNTSPEAK1_bess<- as.numeric(CM_syntspeak1_stat5_bess$byClass[2])

Spec_stat_SYNTSPEAK1_bess<- c(Spec_stat1_SYNTSPEAK1_bess,Spec_stat2_SYNTSPEAK1_bess,Spec_stat3_SYNTSPEAK1_bess, 
                              Spec_stat4_SYNTSPEAK1_bess,
                              Spec_stat5_SYNTSPEAK1_bess)

#PRINT RESULTS ON A TABLE
nsvm<- c('stat1', 'stat2', 'stat3', 'stat4','stat5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score',  'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_bess<- data.frame(nsvm, acc_stat_SYNTSPEAK1_bess, F1_stat_SYNTSPEAK1_bess, 
                                 Precision_stat_SYNTSPEAK1_bess, 
                                 Sens_stat_SYNTSPEAK1_bess, Spec_stat_SYNTSPEAK1_bess)
names(tbl_SYNTSPEAK1_bess)<- cnames_1

ltabl_SYNTSPEAK1_bess<-kable(tbl_SYNTSPEAK1_bess, format = "latex", longtable = T, caption = "Results of SYNT vs 
                             SPEAK1 SVM
                             with Instantaneous Frequencies of each stats as features - Kernel: bessnomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_stat_SYNTSPEAK1_bess, F1_stat_SYNTSPEAK1_bess, Kappa_stat_SYNTSPEAK1_bess, Precision_stat_SYNTSPEAK1_bess, 
   Sens_stat_SYNTSPEAK1_bess, Spec_stat_SYNTSPEAK1_bess)







