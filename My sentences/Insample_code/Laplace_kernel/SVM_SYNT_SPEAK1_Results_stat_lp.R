#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - stat

#ACCURACY

acc_stat1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat1_lp$overall[1])
acc_stat2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat2_lp$overall[1])
acc_stat3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat3_lp$overall[1])
acc_stat4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat4_lp$overall[1])
acc_stat5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat5_lp$overall[1])


acc_stat_SYNTSPEAK1_lp<- c(acc_stat1_SYNTSPEAK1_lp,acc_stat2_SYNTSPEAK1_lp,acc_stat3_SYNTSPEAK1_lp, 
                           acc_stat4_SYNTSPEAK1_lp, 
                           acc_stat5_SYNTSPEAK1_lp)

#F1 score

F1_stat1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat1_lp$byClass[7])
F1_stat2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat2_lp$byClass[7])
F1_stat3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat3_lp$byClass[7])
F1_stat4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat4_lp$byClass[7])
F1_stat5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat5_lp$byClass[7])


F1_stat_SYNTSPEAK1_lp<- c(F1_stat1_SYNTSPEAK1_lp,F1_stat2_SYNTSPEAK1_lp,F1_stat3_SYNTSPEAK1_lp,
                          F1_stat4_SYNTSPEAK1_lp, F1_stat5_SYNTSPEAK1_lp)

#Kappa

# Kappa_stat1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat1_lp$overall[2])
# Kappa_stat2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat2_lp$overall[2])
# Kappa_stat3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat3_lp$overall[2])
# Kappa_stat4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat4_lp$overall[2])
# Kappa_stat5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat5_lp$overall[2])
# 
# 
# Kappa_stat_SYNTSPEAK1_lp<- c(Kappa_stat1_SYNTSPEAK1_lp,Kappa_stat2_SYNTSPEAK1_lp,Kappa_stat3_SYNTSPEAK1_lp,
#                              Kappa_stat4_SYNTSPEAK1_lp, 
     #                        Kappa_stat5_SYNTSPEAK1_lp)

#Precision

Precision_stat1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat1_lp$byClass[5])
Precision_stat2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat2_lp$byClass[5])
Precision_stat3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat3_lp$byClass[5])
Precision_stat4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat4_lp$byClass[5])
Precision_stat5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat5_lp$byClass[5])

Precision_stat_SYNTSPEAK1_lp<- c(Precision_stat1_SYNTSPEAK1_lp,Precision_stat2_SYNTSPEAK1_lp,
                                 Precision_stat3_SYNTSPEAK1_lp,
                                 Precision_stat4_SYNTSPEAK1_lp, Precision_stat5_SYNTSPEAK1_lp)

#Sensitivity

Sens_stat1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat1_lp$byClass[1])
Sens_stat2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat2_lp$byClass[1])
Sens_stat3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat3_lp$byClass[1])
Sens_stat4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat4_lp$byClass[1])
Sens_stat5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat5_lp$byClass[1])

Sens_stat_SYNTSPEAK1_lp<- c(Sens_stat1_SYNTSPEAK1_lp,Sens_stat2_SYNTSPEAK1_lp,Sens_stat3_SYNTSPEAK1_lp, 
                            Sens_stat4_SYNTSPEAK1_lp,
                            Sens_stat5_SYNTSPEAK1_lp)

#Specstaticity 

Spec_stat1_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat1_lp$byClass[2])
Spec_stat2_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat2_lp$byClass[2])
Spec_stat3_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat3_lp$byClass[2])
Spec_stat4_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat4_lp$byClass[2])
Spec_stat5_SYNTSPEAK1_lp<- as.numeric(CM_syntspeak1_stat5_lp$byClass[2])

Spec_stat_SYNTSPEAK1_lp<- c(Spec_stat1_SYNTSPEAK1_lp,Spec_stat2_SYNTSPEAK1_lp,Spec_stat3_SYNTSPEAK1_lp, 
                            Spec_stat4_SYNTSPEAK1_lp,
                            Spec_stat5_SYNTSPEAK1_lp)

#PRINT RESULTS ON A TABLE
nsvm<- c('stat1', 'stat2', 'stat3', 'stat4','stat5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score',  'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_lp<- data.frame(nsvm, acc_stat_SYNTSPEAK1_lp, F1_stat_SYNTSPEAK1_lp, 
                               Precision_stat_SYNTSPEAK1_lp, 
                               Sens_stat_SYNTSPEAK1_lp, Spec_stat_SYNTSPEAK1_lp)
names(tbl_SYNTSPEAK1_lp)<- cnames_1

ltabl_SYNTSPEAK1_lp<-kable(tbl_SYNTSPEAK1_lp, format = "latex", longtable = T, caption = "Results of SYNT vs 
                           SPEAK1 SVM
                           with Instantaneous Frequencies of each stats as features - Kernel: lpnomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_stat_SYNTSPEAK1_lp, F1_stat_SYNTSPEAK1_lp, Kappa_stat_SYNTSPEAK1_lp, Precision_stat_SYNTSPEAK1_lp, 
   Sens_stat_SYNTSPEAK1_lp, Spec_stat_SYNTSPEAK1_lp)







