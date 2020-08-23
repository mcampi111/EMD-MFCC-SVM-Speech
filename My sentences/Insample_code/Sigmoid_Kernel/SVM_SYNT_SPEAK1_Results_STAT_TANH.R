#Tabulating results using kable function
library('knitr')
library('kableExtra')
library('tcpl')


#SVM SYNTHETIC vs SPEAKER 1 - stat

#ACCURACY

acc_stat1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat1_tan$overall[1])
acc_stat2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat2_tan$overall[1])
acc_stat3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat3_tan$overall[1])
acc_stat4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat4_tan$overall[1])
acc_stat5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat5_tan$overall[1])


acc_stat_SYNTSPEAK1_tan<- c(acc_stat1_SYNTSPEAK1_tan,acc_stat2_SYNTSPEAK1_tan,acc_stat3_SYNTSPEAK1_tan, 
                            acc_stat4_SYNTSPEAK1_tan, 
                            acc_stat5_SYNTSPEAK1_tan)

#F1 score

F1_stat1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat1_tan$byClass[7])
F1_stat2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat2_tan$byClass[7])
F1_stat3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat3_tan$byClass[7])
F1_stat4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat4_tan$byClass[7])
F1_stat5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat5_tan$byClass[7])


F1_stat_SYNTSPEAK1_tan<- c(F1_stat1_SYNTSPEAK1_tan,F1_stat2_SYNTSPEAK1_tan,F1_stat3_SYNTSPEAK1_tan,
                           F1_stat4_SYNTSPEAK1_tan, F1_stat5_SYNTSPEAK1_tan)

#Kappa

# Kappa_stat1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat1_tan$overall[2])
# Kappa_stat2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat2_tan$overall[2])
# Kappa_stat3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat3_tan$overall[2])
# Kappa_stat4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat4_tan$overall[2])
# Kappa_stat5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat5_tan$overall[2])
# 
# 
# Kappa_stat_SYNTSPEAK1_tan<- c(Kappa_stat1_SYNTSPEAK1_tan,Kappa_stat2_SYNTSPEAK1_tan,Kappa_stat3_SYNTSPEAK1_tan,
#                               Kappa_stat4_SYNTSPEAK1_tan, 
#                               Kappa_stat5_SYNTSPEAK1_tan)
# 
#Precision

Precision_stat1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat1_tan$byClass[5])
Precision_stat2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat2_tan$byClass[5])
Precision_stat3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat3_tan$byClass[5])
Precision_stat4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat4_tan$byClass[5])
Precision_stat5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat5_tan$byClass[5])

Precision_stat_SYNTSPEAK1_tan<- c(Precision_stat1_SYNTSPEAK1_tan,Precision_stat2_SYNTSPEAK1_tan,
                                  Precision_stat3_SYNTSPEAK1_tan,
                                  Precision_stat4_SYNTSPEAK1_tan, Precision_stat5_SYNTSPEAK1_tan)

#Sensitivity

Sens_stat1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat1_tan$byClass[1])
Sens_stat2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat2_tan$byClass[1])
Sens_stat3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat3_tan$byClass[1])
Sens_stat4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat4_tan$byClass[1])
Sens_stat5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat5_tan$byClass[1])

Sens_stat_SYNTSPEAK1_tan<- c(Sens_stat1_SYNTSPEAK1_tan,Sens_stat2_SYNTSPEAK1_tan,Sens_stat3_SYNTSPEAK1_tan, 
                             Sens_stat4_SYNTSPEAK1_tan,
                             Sens_stat5_SYNTSPEAK1_tan)

#Specstaticity 

Spec_stat1_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat1_tan$byClass[2])
Spec_stat2_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat2_tan$byClass[2])
Spec_stat3_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat3_tan$byClass[2])
Spec_stat4_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat4_tan$byClass[2])
Spec_stat5_SYNTSPEAK1_tan<- as.numeric(CM_syntspeak1_stat5_tan$byClass[2])

Spec_stat_SYNTSPEAK1_tan<- c(Spec_stat1_SYNTSPEAK1_tan,Spec_stat2_SYNTSPEAK1_tan,Spec_stat3_SYNTSPEAK1_tan, 
                             Spec_stat4_SYNTSPEAK1_tan,
                             Spec_stat5_SYNTSPEAK1_tan)

#PRINT RESULTS ON A TABLE
nsvm<- c('stat1', 'stat2', 'stat3', 'stat4','stat5')

cnames_1<- list('SYNT vs SPEAK1 ', 'Accuracy', 'F1-score', 'Precision', 'Sens.', 'Spec.') 

tbl_SYNTSPEAK1_tan<- data.frame(nsvm, acc_stat_SYNTSPEAK1_tan, F1_stat_SYNTSPEAK1_tan, 
                                Precision_stat_SYNTSPEAK1_tan, 
                                Sens_stat_SYNTSPEAK1_tan, Spec_stat_SYNTSPEAK1_tan)
names(tbl_SYNTSPEAK1_tan)<- cnames_1

ltabl_SYNTSPEAK1_tan<-kable(tbl_SYNTSPEAK1_tan, format = "latex", longtable = T, caption = "Results of SYNT vs 
                            SPEAK1 SVM
                            with Instantaneous Frequencies of each stats as features - Kernel: tannomial.")%>%
  kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color  
                full_width = F) 

rm(acc_stat_SYNTSPEAK1_tan, F1_stat_SYNTSPEAK1_tan, Kappa_stat_SYNTSPEAK1_tan, Precision_stat_SYNTSPEAK1_tan, 
   Sens_stat_SYNTSPEAK1_tan, Spec_stat_SYNTSPEAK1_tan)








