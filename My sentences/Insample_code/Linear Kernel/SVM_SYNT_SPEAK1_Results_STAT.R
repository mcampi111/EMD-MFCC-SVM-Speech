#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - stat

#ACCURACY

acc_stat1_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat1_van$overall[1])
acc_stat2_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat2_van$overall[1])
acc_stat3_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat3_van$overall[1])
acc_stat4_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat4_van$overall[1])
acc_stat5_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat5_van$overall[1])


acc_stat_SYNTSPEAK1_van<- c(acc_stat1_van_SYNTSPEAK1,acc_stat2_van_SYNTSPEAK1,acc_stat3_van_SYNTSPEAK1, 
                        acc_stat4_van_SYNTSPEAK1, acc_stat5_van_SYNTSPEAK1)

acc_stat_SYNTSPEAK1<- c(acc_stat1_van_SYNTSPEAK1,acc_stat2_van_SYNTSPEAK1,acc_stat3_van_SYNTSPEAK1, 
                        acc_stat4_van_SYNTSPEAK1, acc_stat5_van_SYNTSPEAK1)

#F1 score

F1_stat1_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat1_van$byClass[7])
F1_stat2_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat2_van$byClass[7])
F1_stat3_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat3_van$byClass[7])
F1_stat4_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat4_van$byClass[7])
F1_stat5_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat5_van$byClass[7])


F1_stat_SYNTSPEAK1<- c(F1_stat1_van_SYNTSPEAK1,F1_stat2_van_SYNTSPEAK1,F1_stat3_van_SYNTSPEAK1, F1_stat4_van_SYNTSPEAK1, F1_stat5_van_SYNTSPEAK1)

#Kappa

# Kappa_stat1_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat1_van$overall[2])
# Kappa_stat2_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat2_van$overall[2])
# Kappa_stat3_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat3_van$overall[2])
# Kappa_stat4_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat4_van$overall[2])
# Kappa_stat5_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat5_van$overall[2])
# 
# 
# Kappa_stat_SYNTSPEAK1<- c(Kappa_stat1_van_SYNTSPEAK1,Kappa_stat2_van_SYNTSPEAK1,Kappa_stat3_van_SYNTSPEAK1, Kappa_stat4_van_SYNTSPEAK1, 
#                           Kappa_stat5_van_SYNTSPEAK1)

#Precision

Precision_stat1_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat1_van$byClass[5])
Precision_stat2_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat2_van$byClass[5])
Precision_stat3_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat3_van$byClass[5])
Precision_stat4_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat4_van$byClass[5])
Precision_stat5_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat5_van$byClass[5])

Precision_stat_SYNTSPEAK1<- c(Precision_stat1_van_SYNTSPEAK1,Precision_stat2_van_SYNTSPEAK1,Precision_stat3_van_SYNTSPEAK1,
                              Precision_stat4_van_SYNTSPEAK1, Precision_stat5_van_SYNTSPEAK1)

#Sensitivity

Sens_stat1_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat1_van$byClass[1])
Sens_stat2_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat2_van$byClass[1])
Sens_stat3_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat3_van$byClass[1])
Sens_stat4_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat4_van$byClass[1])
Sens_stat5_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat5_van$byClass[1])

Sens_stat_SYNTSPEAK1<- c(Sens_stat1_van_SYNTSPEAK1,Sens_stat2_van_SYNTSPEAK1,Sens_stat3_van_SYNTSPEAK1, Sens_stat4_van_SYNTSPEAK1,
                         Sens_stat5_van_SYNTSPEAK1)

#Specstaticity 

Spec_stat1_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat1_van$byClass[2])
Spec_stat2_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat2_van$byClass[2])
Spec_stat3_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat3_van$byClass[2])
Spec_stat4_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat4_van$byClass[2])
Spec_stat5_van_SYNTSPEAK1<- as.numeric(CM_syntspeak1_stat5_van$byClass[2])

Spec_stat_SYNTSPEAK1<- c(Spec_stat1_van_SYNTSPEAK1,Spec_stat2_van_SYNTSPEAK1,Spec_stat3_van_SYNTSPEAK1, Spec_stat4_van_SYNTSPEAK1,
                         Spec_stat5_van_SYNTSPEAK1)

#PRINT RESULTS ON A TABLE
nsvm<- c('stat1_van', 'stat2_van', 'stat3_van', 'stat4_van','stat5_van')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score',  'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1<- data.frame(nsvm, acc_stat_SYNTSPEAK1, F1_stat_SYNTSPEAK1,  
                            Precision_stat_SYNTSPEAK1, 
                            Sens_stat_SYNTSPEAK1, Spec_stat_SYNTSPEAK1)
names(tbl_SYNTSPEAK1)<- cnames_1

ltabl_SYNTSPEAK1<-kable(tbl_SYNTSPEAK1, format = "latex", longtable = T, caption = "Results of SYNT vs SPEAK1 SVM
                        with Instantaneous Frequencies of each stats as features - Kernel:RBF.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_stat_SYNTSPEAK1, F1_stat_SYNTSPEAK1, Kappa_stat_SYNTSPEAK1, Precision_stat_SYNTSPEAK1, 
   Sens_stat_SYNTSPEAK1, Spec_stat_SYNTSPEAK1)







