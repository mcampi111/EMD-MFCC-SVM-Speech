library('knitr')
library('kableExtra')
library('tcpl')

#FEATURE STUDy ARIMA GARCH - sum of the weigths over the kernels by keeping fixed each feature
# pi_m = accuracy 

load("C:\\Users\\Marta\\Desktop\\Speech_OUT\\SVM_Speaker1_out\\RBF\\CM_speak1_CEP_IMF4_RBF.RData")

pi_m_RBF_MLFC_IMF4_coeff1<- CM_syntspeak1_MLFC_IMF4_coeff1_new$overall[1]
pi_m_RBF_MLFC_IMF4_coeff2<- CM_syntspeak1_MLFC_IMF4_coeff2_new$overall[1]
pi_m_RBF_MLFC_IMF4_coeff3<- CM_syntspeak1_MLFC_IMF4_coeff3_new$overall[1]
pi_m_RBF_MLFC_IMF4_coeff4<- CM_syntspeak1_MLFC_IMF4_coeff4_new$overall[1]
pi_m_RBF_MLFC_IMF4_coeff5<- CM_syntspeak1_MLFC_IMF4_coeff5_new$overall[1]
pi_m_RBF_MLFC_IMF4_coeff6<- CM_syntspeak1_MLFC_IMF4_coeff6_new$overall[1]
pi_m_RBF_MLFC_IMF4_coeff7<- CM_syntspeak1_MLFC_IMF4_coeff7_new$overall[1]
pi_m_RBF_MLFC_IMF4_coeff8<- CM_syntspeak1_MLFC_IMF4_coeff8_new$overall[1]
pi_m_RBF_MLFC_IMF4_coeff9<- CM_syntspeak1_MLFC_IMF4_coeff9_new$overall[1]
pi_m_RBF_MLFC_IMF4_coeff10<- CM_syntspeak1_MLFC_IMF4_coeff10_new$overall[1]
pi_m_RBF_MLFC_IMF4_coeff11<- CM_syntspeak1_MLFC_IMF4_coeff11_new$overall[1]
pi_m_RBF_MLFC_IMF4_coeff12<- CM_syntspeak1_MLFC_IMF4_coeff12_new$overall[1]


load("C:\\Users\\Marta\\Desktop\\Speech_OUT\\SVM_Speaker1_out\\LP\\CM_speak1_CEP_IMF4_LP.RData")

pi_m_LP_MLFC_IMF4_coeff1<- CM_syntspeak1_MLFC_IMF4_coeff1_new$overall[1]
pi_m_LP_MLFC_IMF4_coeff2<- CM_syntspeak1_MLFC_IMF4_coeff2_new$overall[1]
pi_m_LP_MLFC_IMF4_coeff3<- CM_syntspeak1_MLFC_IMF4_coeff3_new$overall[1]
pi_m_LP_MLFC_IMF4_coeff4<- CM_syntspeak1_MLFC_IMF4_coeff4_new$overall[1]
pi_m_LP_MLFC_IMF4_coeff5<- CM_syntspeak1_MLFC_IMF4_coeff5_new$overall[1]
pi_m_LP_MLFC_IMF4_coeff6<- CM_syntspeak1_MLFC_IMF4_coeff6_new$overall[1]
pi_m_LP_MLFC_IMF4_coeff7<- CM_syntspeak1_MLFC_IMF4_coeff7_new$overall[1]
pi_m_LP_MLFC_IMF4_coeff8<- CM_syntspeak1_MLFC_IMF4_coeff8_new$overall[1]
pi_m_LP_MLFC_IMF4_coeff9<- CM_syntspeak1_MLFC_IMF4_coeff9_new$overall[1]
pi_m_LP_MLFC_IMF4_coeff10<- CM_syntspeak1_MLFC_IMF4_coeff10_new$overall[1]
pi_m_LP_MLFC_IMF4_coeff11<- CM_syntspeak1_MLFC_IMF4_coeff11_new$overall[1]
pi_m_LP_MLFC_IMF4_coeff12<- CM_syntspeak1_MLFC_IMF4_coeff12_new$overall[1]


load("C:\\Users\\Marta\\Desktop\\Speech_OUT\\SVM_Speaker1_out\\POLY\\CM_speak1_CEP_IMF4_POLY.RData")

pi_m_poly_MLFC_IMF4_coeff1<- CM_syntspeak1_MLFC_IMF4_coeff1_new$overall[1]
pi_m_poly_MLFC_IMF4_coeff2<- CM_syntspeak1_MLFC_IMF4_coeff2_new$overall[1]
pi_m_poly_MLFC_IMF4_coeff3<- CM_syntspeak1_MLFC_IMF4_coeff3_new$overall[1]
pi_m_poly_MLFC_IMF4_coeff4<- CM_syntspeak1_MLFC_IMF4_coeff4_new$overall[1]
pi_m_poly_MLFC_IMF4_coeff5<- CM_syntspeak1_MLFC_IMF4_coeff5_new$overall[1]
pi_m_poly_MLFC_IMF4_coeff6<- CM_syntspeak1_MLFC_IMF4_coeff6_new$overall[1]
pi_m_poly_MLFC_IMF4_coeff7<- CM_syntspeak1_MLFC_IMF4_coeff7_new$overall[1]
pi_m_poly_MLFC_IMF4_coeff8<- CM_syntspeak1_MLFC_IMF4_coeff8_new$overall[1]
pi_m_poly_MLFC_IMF4_coeff9<- CM_syntspeak1_MLFC_IMF4_coeff9_new$overall[1]
pi_m_poly_MLFC_IMF4_coeff10<- CM_syntspeak1_MLFC_IMF4_coeff10_new$overall[1]
pi_m_poly_MLFC_IMF4_coeff11<- CM_syntspeak1_MLFC_IMF4_coeff11_new$overall[1]
pi_m_poly_MLFC_IMF4_coeff12<- CM_syntspeak1_MLFC_IMF4_coeff12_new$overall[1]

load("C:\\Users\\Marta\\Desktop\\Speech_OUT\\SVM_Speaker1_out\\TAN\\CM_speak1_CEP_IMF4_TAN.RData")

pi_m_tan_MLFC_IMF4_coeff1<- CM_syntspeak1_MLFC_IMF4_coeff1_new$overall[1]
pi_m_tan_MLFC_IMF4_coeff2<- CM_syntspeak1_MLFC_IMF4_coeff2_new$overall[1]
pi_m_tan_MLFC_IMF4_coeff3<- CM_syntspeak1_MLFC_IMF4_coeff3_new$overall[1]
pi_m_tan_MLFC_IMF4_coeff4<- CM_syntspeak1_MLFC_IMF4_coeff4_new$overall[1]
pi_m_tan_MLFC_IMF4_coeff5<- CM_syntspeak1_MLFC_IMF4_coeff5_new$overall[1]
pi_m_tan_MLFC_IMF4_coeff6<- CM_syntspeak1_MLFC_IMF4_coeff6_new$overall[1]
pi_m_tan_MLFC_IMF4_coeff7<- CM_syntspeak1_MLFC_IMF4_coeff7_new$overall[1]
pi_m_tan_MLFC_IMF4_coeff8<- CM_syntspeak1_MLFC_IMF4_coeff8_new$overall[1]
pi_m_tan_MLFC_IMF4_coeff9<- CM_syntspeak1_MLFC_IMF4_coeff9_new$overall[1]
pi_m_tan_MLFC_IMF4_coeff10<- CM_syntspeak1_MLFC_IMF4_coeff10_new$overall[1]
pi_m_tan_MLFC_IMF4_coeff11<- CM_syntspeak1_MLFC_IMF4_coeff11_new$overall[1]
pi_m_tan_MLFC_IMF4_coeff12<- CM_syntspeak1_MLFC_IMF4_coeff12_new$overall[1]


load("C:\\Users\\Marta\\Desktop\\Speech_OUT\\SVM_Speaker1_out\\BESS\\CM_speak1_CEP_IMF4_BESS.RData")

pi_m_bess_MLFC_IMF4_coeff1<- CM_syntspeak1_MLFC_IMF4_coeff1_new$overall[1]
pi_m_bess_MLFC_IMF4_coeff2<- CM_syntspeak1_MLFC_IMF4_coeff2_new$overall[1]
pi_m_bess_MLFC_IMF4_coeff3<- CM_syntspeak1_MLFC_IMF4_coeff3_new$overall[1]
pi_m_bess_MLFC_IMF4_coeff4<- CM_syntspeak1_MLFC_IMF4_coeff4_new$overall[1]
pi_m_bess_MLFC_IMF4_coeff5<- CM_syntspeak1_MLFC_IMF4_coeff5_new$overall[1]
pi_m_bess_MLFC_IMF4_coeff6<- CM_syntspeak1_MLFC_IMF4_coeff6_new$overall[1]
pi_m_bess_MLFC_IMF4_coeff7<- CM_syntspeak1_MLFC_IMF4_coeff7_new$overall[1]
pi_m_bess_MLFC_IMF4_coeff8<- CM_syntspeak1_MLFC_IMF4_coeff8_new$overall[1]
pi_m_bess_MLFC_IMF4_coeff9<- CM_syntspeak1_MLFC_IMF4_coeff9_new$overall[1]
pi_m_bess_MLFC_IMF4_coeff10<- CM_syntspeak1_MLFC_IMF4_coeff10_new$overall[1]
pi_m_bess_MLFC_IMF4_coeff11<- CM_syntspeak1_MLFC_IMF4_coeff11_new$overall[1]
pi_m_bess_MLFC_IMF4_coeff12<- CM_syntspeak1_MLFC_IMF4_coeff12_new$overall[1]

load("C:\\Users\\Marta\\Desktop\\Speech_OUT\\SVM_Speaker1_out\\VANN\\CM_speak1_CEP_IMF4_van.RData")

pi_m_van_MLFC_IMF4_coeff1<- CM_syntspeak1_MLFC_IMF4_coeff1_new$overall[1]
pi_m_van_MLFC_IMF4_coeff2<- CM_syntspeak1_MLFC_IMF4_coeff2_new$overall[1]
pi_m_van_MLFC_IMF4_coeff3<- CM_syntspeak1_MLFC_IMF4_coeff3_new$overall[1]
pi_m_van_MLFC_IMF4_coeff4<- CM_syntspeak1_MLFC_IMF4_coeff4_new$overall[1]
pi_m_van_MLFC_IMF4_coeff5<- CM_syntspeak1_MLFC_IMF4_coeff5_new$overall[1]
pi_m_van_MLFC_IMF4_coeff6<- CM_syntspeak1_MLFC_IMF4_coeff6_new$overall[1]
pi_m_van_MLFC_IMF4_coeff7<- CM_syntspeak1_MLFC_IMF4_coeff7_new$overall[1]
pi_m_van_MLFC_IMF4_coeff8<- CM_syntspeak1_MLFC_IMF4_coeff8_new$overall[1]
pi_m_van_MLFC_IMF4_coeff9<- CM_syntspeak1_MLFC_IMF4_coeff9_new$overall[1]
pi_m_van_MLFC_IMF4_coeff10<- CM_syntspeak1_MLFC_IMF4_coeff10_new$overall[1]
pi_m_van_MLFC_IMF4_coeff11<- CM_syntspeak1_MLFC_IMF4_coeff11_new$overall[1]
pi_m_van_MLFC_IMF4_coeff12<- CM_syntspeak1_MLFC_IMF4_coeff12_new$overall[1]


#WEIGHTS

delta<-  0.1    #0.5

#RBF

diff_RBF_MLFC_IMF4_coeff1<- pi_m_RBF_MLFC_IMF4_coeff1 - delta
diff_RBF_MLFC_IMF4_coeff2<- pi_m_RBF_MLFC_IMF4_coeff2 - delta
diff_RBF_MLFC_IMF4_coeff3<- pi_m_RBF_MLFC_IMF4_coeff3 - delta
diff_RBF_MLFC_IMF4_coeff4<- pi_m_RBF_MLFC_IMF4_coeff4- delta
diff_RBF_MLFC_IMF4_coeff5<- pi_m_RBF_MLFC_IMF4_coeff5 - delta
diff_RBF_MLFC_IMF4_coeff6<- pi_m_RBF_MLFC_IMF4_coeff6 - delta
diff_RBF_MLFC_IMF4_coeff7<- pi_m_RBF_MLFC_IMF4_coeff7 - delta
diff_RBF_MLFC_IMF4_coeff8<- pi_m_RBF_MLFC_IMF4_coeff8 - delta
diff_RBF_MLFC_IMF4_coeff9<- pi_m_RBF_MLFC_IMF4_coeff9- delta
diff_RBF_MLFC_IMF4_coeff10<- pi_m_RBF_MLFC_IMF4_coeff10 - delta
diff_RBF_MLFC_IMF4_coeff11<- pi_m_RBF_MLFC_IMF4_coeff11 - delta
diff_RBF_MLFC_IMF4_coeff12<- pi_m_RBF_MLFC_IMF4_coeff12 - delta

#LP

diff_LP_MLFC_IMF4_coeff1<- pi_m_LP_MLFC_IMF4_coeff1 - delta
diff_LP_MLFC_IMF4_coeff2<- pi_m_LP_MLFC_IMF4_coeff2 - delta
diff_LP_MLFC_IMF4_coeff3<- pi_m_LP_MLFC_IMF4_coeff3 - delta
diff_LP_MLFC_IMF4_coeff4<- pi_m_LP_MLFC_IMF4_coeff4- delta
diff_LP_MLFC_IMF4_coeff5<- pi_m_LP_MLFC_IMF4_coeff5 - delta
diff_LP_MLFC_IMF4_coeff6<- pi_m_LP_MLFC_IMF4_coeff6 - delta
diff_LP_MLFC_IMF4_coeff7<- pi_m_LP_MLFC_IMF4_coeff7 - delta
diff_LP_MLFC_IMF4_coeff8<- pi_m_LP_MLFC_IMF4_coeff8 - delta
diff_LP_MLFC_IMF4_coeff9<- pi_m_LP_MLFC_IMF4_coeff9- delta
diff_LP_MLFC_IMF4_coeff10<- pi_m_LP_MLFC_IMF4_coeff10 - delta
diff_LP_MLFC_IMF4_coeff11<- pi_m_LP_MLFC_IMF4_coeff11 - delta
diff_LP_MLFC_IMF4_coeff12<- pi_m_LP_MLFC_IMF4_coeff12 - delta

#POLY

diff_poly_MLFC_IMF4_coeff1<- pi_m_poly_MLFC_IMF4_coeff1 - delta
diff_poly_MLFC_IMF4_coeff2<- pi_m_poly_MLFC_IMF4_coeff2 - delta
diff_poly_MLFC_IMF4_coeff3<- pi_m_poly_MLFC_IMF4_coeff3 - delta
diff_poly_MLFC_IMF4_coeff4<- pi_m_poly_MLFC_IMF4_coeff4- delta
diff_poly_MLFC_IMF4_coeff5<- pi_m_poly_MLFC_IMF4_coeff5 - delta
diff_poly_MLFC_IMF4_coeff6<- pi_m_poly_MLFC_IMF4_coeff6 - delta
diff_poly_MLFC_IMF4_coeff7<- pi_m_poly_MLFC_IMF4_coeff7 - delta
diff_poly_MLFC_IMF4_coeff8<- pi_m_poly_MLFC_IMF4_coeff8 - delta
diff_poly_MLFC_IMF4_coeff9<- pi_m_poly_MLFC_IMF4_coeff9- delta
diff_poly_MLFC_IMF4_coeff10<- pi_m_poly_MLFC_IMF4_coeff10 - delta
diff_poly_MLFC_IMF4_coeff11<- pi_m_poly_MLFC_IMF4_coeff11 - delta
diff_poly_MLFC_IMF4_coeff12<- pi_m_poly_MLFC_IMF4_coeff12 - delta


#TANH
diff_tan_MLFC_IMF4_coeff1<- pi_m_tan_MLFC_IMF4_coeff1 - delta
diff_tan_MLFC_IMF4_coeff2<- pi_m_tan_MLFC_IMF4_coeff2 - delta
diff_tan_MLFC_IMF4_coeff3<- pi_m_tan_MLFC_IMF4_coeff3 - delta
diff_tan_MLFC_IMF4_coeff4<- pi_m_tan_MLFC_IMF4_coeff4- delta
diff_tan_MLFC_IMF4_coeff5<- pi_m_tan_MLFC_IMF4_coeff5 - delta
diff_tan_MLFC_IMF4_coeff6<- pi_m_tan_MLFC_IMF4_coeff6 - delta
diff_tan_MLFC_IMF4_coeff7<- pi_m_tan_MLFC_IMF4_coeff7 - delta
diff_tan_MLFC_IMF4_coeff8<- pi_m_tan_MLFC_IMF4_coeff8 - delta
diff_tan_MLFC_IMF4_coeff9<- pi_m_tan_MLFC_IMF4_coeff9- delta
diff_tan_MLFC_IMF4_coeff10<- pi_m_tan_MLFC_IMF4_coeff10 - delta
diff_tan_MLFC_IMF4_coeff11<- pi_m_tan_MLFC_IMF4_coeff11 - delta
diff_tan_MLFC_IMF4_coeff12<- pi_m_tan_MLFC_IMF4_coeff12 - delta

#BESS
diff_bess_MLFC_IMF4_coeff1<- pi_m_bess_MLFC_IMF4_coeff1 - delta
diff_bess_MLFC_IMF4_coeff2<- pi_m_bess_MLFC_IMF4_coeff2 - delta
diff_bess_MLFC_IMF4_coeff3<- pi_m_bess_MLFC_IMF4_coeff3 - delta
diff_bess_MLFC_IMF4_coeff4<- pi_m_bess_MLFC_IMF4_coeff4- delta
diff_bess_MLFC_IMF4_coeff5<- pi_m_bess_MLFC_IMF4_coeff5 - delta
diff_bess_MLFC_IMF4_coeff6<- pi_m_bess_MLFC_IMF4_coeff6 - delta
diff_bess_MLFC_IMF4_coeff7<- pi_m_bess_MLFC_IMF4_coeff7 - delta
diff_bess_MLFC_IMF4_coeff8<- pi_m_bess_MLFC_IMF4_coeff8 - delta
diff_bess_MLFC_IMF4_coeff9<- pi_m_bess_MLFC_IMF4_coeff9- delta
diff_bess_MLFC_IMF4_coeff10<- pi_m_bess_MLFC_IMF4_coeff10 - delta
diff_bess_MLFC_IMF4_coeff11<- pi_m_bess_MLFC_IMF4_coeff11 - delta
diff_bess_MLFC_IMF4_coeff12<- pi_m_bess_MLFC_IMF4_coeff12 - delta


#BESS

diff_vann_MLFC_IMF4_coeff1<- pi_m_van_MLFC_IMF4_coeff1 - delta
diff_vann_MLFC_IMF4_coeff2<- pi_m_van_MLFC_IMF4_coeff2 - delta
diff_vann_MLFC_IMF4_coeff3<- pi_m_van_MLFC_IMF4_coeff3 - delta
diff_vann_MLFC_IMF4_coeff4<- pi_m_van_MLFC_IMF4_coeff4- delta
diff_vann_MLFC_IMF4_coeff5<- pi_m_van_MLFC_IMF4_coeff5 - delta
diff_vann_MLFC_IMF4_coeff6<- pi_m_van_MLFC_IMF4_coeff6 - delta
diff_vann_MLFC_IMF4_coeff7<- pi_m_van_MLFC_IMF4_coeff7 - delta
diff_vann_MLFC_IMF4_coeff8<- pi_m_van_MLFC_IMF4_coeff8 - delta
diff_vann_MLFC_IMF4_coeff9<- pi_m_van_MLFC_IMF4_coeff9- delta
diff_vann_MLFC_IMF4_coeff10<- pi_m_van_MLFC_IMF4_coeff10 - delta
diff_vann_MLFC_IMF4_coeff11<- pi_m_van_MLFC_IMF4_coeff11 - delta
diff_vann_MLFC_IMF4_coeff12<- pi_m_van_MLFC_IMF4_coeff12 - delta




# mean_RBF<- sum(diff_RBF_MLFC_IMF4_coeff1,diff_RBF_MLFC_IMF4_coeff2,diff_RBF_MLFC_IMF4_coeff3,diff_RBF_MLFC_IMF4_coeff4,
#                diff_RBF_MLFC_IMF4_coeff5,diff_RBF_MLFC_IMF4_coeff6,diff_RBF_MLFC_IMF4_coeff7,diff_RBF_MLFC_IMF4_coeff8,
#                diff_RBF_MLFC_IMF4_coeff9,diff_RBF_MLFC_IMF4_coeff10,diff_RBF_MLFC_IMF4_coeff11,diff_RBF_MLFC_IMF4_coeff12)/12
# 
# mean_LP<- sum(diff_LP_MLFC_IMF4_coeff1,diff_LP_MLFC_IMF4_coeff2,diff_LP_MLFC_IMF4_coeff3,diff_LP_MLFC_IMF4_coeff4,
#               diff_LP_MLFC_IMF4_coeff5,diff_LP_MLFC_IMF4_coeff6,diff_LP_MLFC_IMF4_coeff7,diff_LP_MLFC_IMF4_coeff8,
#               diff_LP_MLFC_IMF4_coeff9,diff_LP_MLFC_IMF4_coeff10,diff_LP_MLFC_IMF4_coeff11,diff_LP_MLFC_IMF4_coeff12)/12
# 
# mean_poly<- sum(diff_poly_MLFC_IMF4_coeff1,diff_poly_MLFC_IMF4_coeff2,diff_poly_MLFC_IMF4_coeff3,
#                 diff_poly_MLFC_IMF4_coeff4, diff_poly_MLFC_IMF4_coeff5,diff_poly_MLFC_IMF4_coeff6,
#                 diff_poly_MLFC_IMF4_coeff7,diff_poly_MLFC_IMF4_coeff8,diff_poly_MLFC_IMF4_coeff9,
#                 diff_poly_MLFC_IMF4_coeff10,diff_poly_MLFC_IMF4_coeff11,diff_poly_MLFC_IMF4_coeff12)/12
# 
# mean_tan<- sum(diff_tan_MLFC_IMF4_coeff1,diff_tan_MLFC_IMF4_coeff2,diff_tan_MLFC_IMF4_coeff3,
#                diff_tan_MLFC_IMF4_coeff4, diff_tan_MLFC_IMF4_coeff5,diff_tan_MLFC_IMF4_coeff6,
#                diff_tan_MLFC_IMF4_coeff7,diff_tan_MLFC_IMF4_coeff8,diff_tan_MLFC_IMF4_coeff9,
#                diff_tan_MLFC_IMF4_coeff10,diff_tan_MLFC_IMF4_coeff11,diff_tan_MLFC_IMF4_coeff12)/12
# 
# mean_bess<- sum(diff_bess_MLFC_IMF4_coeff1,diff_bess_MLFC_IMF4_coeff2,diff_bess_MLFC_IMF4_coeff3,
#                 diff_bess_MLFC_IMF4_coeff4, diff_bess_MLFC_IMF4_coeff5,diff_bess_MLFC_IMF4_coeff6,
#                 diff_bess_MLFC_IMF4_coeff7,diff_bess_MLFC_IMF4_coeff8,diff_bess_MLFC_IMF4_coeff9,
#                 diff_bess_MLFC_IMF4_coeff10,diff_bess_MLFC_IMF4_coeff11,diff_bess_MLFC_IMF4_coeff12)/12
# 
# mean_vann<- sum(diff_vann_MLFC_IMF4_coeff1,diff_vann_MLFC_IMF4_coeff2,diff_vann_MLFC_IMF4_coeff3,
#                 diff_vann_MLFC_IMF4_coeff4, diff_vann_MLFC_IMF4_coeff5,diff_vann_MLFC_IMF4_coeff6,
#                 diff_vann_MLFC_IMF4_coeff7,diff_vann_MLFC_IMF4_coeff8,diff_vann_MLFC_IMF4_coeff9,
#                 diff_vann_MLFC_IMF4_coeff10,diff_vann_MLFC_IMF4_coeff11,diff_vann_MLFC_IMF4_coeff12)/12
# 
# vec_mean_MFCC_IMF4<- c(mean_RBF, mean_LP, mean_poly, mean_tan, mean_bess, mean_vann)
# 
# #BEFORE COMPUTING THE SUM - CHECK FIRST THE R CONDITION AND THEN
# #NORMALIZE THEM ACCORDING TO IT!
# 
# sum<- sum(diff_RBF_MLFC_IMF4_coeff1, diff_RBF_MLFC_IMF4_coeff2, diff_RBF_MLFC_IMF4_coeff3,
#           diff_RBF_MLFC_IMF4_coeff4, diff_RBF_MLFC_IMF4_coeff5, diff_RBF_MLFC_IMF4_coeff6,
#           diff_RBF_MLFC_IMF4_coeff7, diff_RBF_MLFC_IMF4_coeff8, diff_RBF_MLFC_IMF4_coeff9,
#           diff_RBF_MLFC_IMF4_coeff10, diff_RBF_MLFC_IMF4_coeff11, diff_RBF_MLFC_IMF4_coeff12,
#           diff_LP_MLFC_IMF4_coeff1, diff_LP_MLFC_IMF4_coeff2, diff_LP_MLFC_IMF4_coeff3,
#           diff_LP_MLFC_IMF4_coeff4, diff_LP_MLFC_IMF4_coeff5, diff_LP_MLFC_IMF4_coeff6,
#           diff_LP_MLFC_IMF4_coeff7, diff_LP_MLFC_IMF4_coeff8, diff_LP_MLFC_IMF4_coeff9,
#           diff_LP_MLFC_IMF4_coeff10, diff_LP_MLFC_IMF4_coeff11, diff_LP_MLFC_IMF4_coeff12,
#           diff_poly_MLFC_IMF4_coeff1, diff_poly_MLFC_IMF4_coeff2, diff_poly_MLFC_IMF4_coeff3,
#           diff_poly_MLFC_IMF4_coeff4, diff_poly_MLFC_IMF4_coeff5, diff_poly_MLFC_IMF4_coeff6,
#           diff_poly_MLFC_IMF4_coeff7, diff_poly_MLFC_IMF4_coeff8, diff_poly_MLFC_IMF4_coeff9,
#           diff_poly_MLFC_IMF4_coeff10, diff_poly_MLFC_IMF4_coeff11, diff_poly_MLFC_IMF4_coeff12,
#           #diff_tan_MLFC_IMF4_coeff1, diff_tan_MLFC_IMF4_coeff2, diff_tan_MLFC_IMF4_coeff3,
#           #diff_tan_MLFC_IMF4_coeff4, diff_tan_MLFC_IMF4_coeff5, diff_tan_MLFC_IMF4_coeff6,
#           #diff_tan_MLFC_IMF4_coeff7, diff_tan_MLFC_IMF4_coeff8, diff_tan_MLFC_IMF4_coeff9,
#           #diff_tan_MLFC_IMF4_coeff10, diff_tan_MLFC_IMF4_coeff11, diff_tan_MLFC_IMF4_coeff12,
#           diff_bess_MLFC_IMF4_coeff1, diff_bess_MLFC_IMF4_coeff2, diff_bess_MLFC_IMF4_coeff3,
#           diff_bess_MLFC_IMF4_coeff4, diff_bess_MLFC_IMF4_coeff5, diff_bess_MLFC_IMF4_coeff6,
#           diff_bess_MLFC_IMF4_coeff7, diff_bess_MLFC_IMF4_coeff8, diff_bess_MLFC_IMF4_coeff9,
#           diff_bess_MLFC_IMF4_coeff10, diff_bess_MLFC_IMF4_coeff11, diff_bess_MLFC_IMF4_coeff12,
#           diff_vann_MLFC_IMF4_coeff1, diff_vann_MLFC_IMF4_coeff2, diff_vann_MLFC_IMF4_coeff3,
#           diff_vann_MLFC_IMF4_coeff4, diff_vann_MLFC_IMF4_coeff5, diff_vann_MLFC_IMF4_coeff6,
#           diff_vann_MLFC_IMF4_coeff7, diff_vann_MLFC_IMF4_coeff8, diff_vann_MLFC_IMF4_coeff9,
#           diff_vann_MLFC_IMF4_coeff10, diff_vann_MLFC_IMF4_coeff11, diff_vann_MLFC_IMF4_coeff12)
# 
# w_RBF_MLFC_IMF4_coeff1<- diff_RBF_MLFC_IMF4_coeff1/ sum
# w_RBF_MLFC_IMF4_coeff2<- diff_RBF_MLFC_IMF4_coeff2/ sum
# w_RBF_MLFC_IMF4_coeff3<- diff_RBF_MLFC_IMF4_coeff3/ sum 
# w_RBF_MLFC_IMF4_coeff4<- diff_RBF_MLFC_IMF4_coeff4/ sum 
# w_RBF_MLFC_IMF4_coeff5<- diff_RBF_MLFC_IMF4_coeff5/ sum 
# w_RBF_MLFC_IMF4_coeff6<- diff_RBF_MLFC_IMF4_coeff6/ sum
# w_RBF_MLFC_IMF4_coeff7<- diff_RBF_MLFC_IMF4_coeff7/ sum
# w_RBF_MLFC_IMF4_coeff8<- diff_RBF_MLFC_IMF4_coeff8/ sum 
# w_RBF_MLFC_IMF4_coeff9<- diff_RBF_MLFC_IMF4_coeff9/ sum 
# w_RBF_MLFC_IMF4_coeff10<- diff_RBF_MLFC_IMF4_coeff10/ sum 
# w_RBF_MLFC_IMF4_coeff11<- diff_RBF_MLFC_IMF4_coeff11/ sum 
# w_RBF_MLFC_IMF4_coeff12<- diff_RBF_MLFC_IMF4_coeff12/ sum 
# 
# 
# w_LP_MLFC_IMF4_coeff1<- diff_LP_MLFC_IMF4_coeff1/ sum
# w_LP_MLFC_IMF4_coeff2<- diff_LP_MLFC_IMF4_coeff2/ sum
# w_LP_MLFC_IMF4_coeff3<- diff_LP_MLFC_IMF4_coeff3/ sum 
# w_LP_MLFC_IMF4_coeff4<- diff_LP_MLFC_IMF4_coeff4/ sum 
# w_LP_MLFC_IMF4_coeff5<- diff_LP_MLFC_IMF4_coeff5/ sum 
# w_LP_MLFC_IMF4_coeff6<- diff_LP_MLFC_IMF4_coeff6/ sum
# w_LP_MLFC_IMF4_coeff7<- diff_LP_MLFC_IMF4_coeff7/ sum
# w_LP_MLFC_IMF4_coeff8<- diff_LP_MLFC_IMF4_coeff8/ sum 
# w_LP_MLFC_IMF4_coeff9<- diff_LP_MLFC_IMF4_coeff9/ sum 
# w_LP_MLFC_IMF4_coeff10<- diff_LP_MLFC_IMF4_coeff10/ sum 
# w_LP_MLFC_IMF4_coeff11<- diff_LP_MLFC_IMF4_coeff11/ sum 
# w_LP_MLFC_IMF4_coeff12<- diff_LP_MLFC_IMF4_coeff12/ sum 
# 
# w_poly_MLFC_IMF4_coeff1<- diff_poly_MLFC_IMF4_coeff1/ sum
# w_poly_MLFC_IMF4_coeff2<- diff_poly_MLFC_IMF4_coeff2/ sum
# w_poly_MLFC_IMF4_coeff3<- diff_poly_MLFC_IMF4_coeff3/ sum 
# w_poly_MLFC_IMF4_coeff4<- diff_poly_MLFC_IMF4_coeff4/ sum 
# w_poly_MLFC_IMF4_coeff5<- diff_poly_MLFC_IMF4_coeff5/ sum 
# w_poly_MLFC_IMF4_coeff6<- diff_poly_MLFC_IMF4_coeff6/ sum
# w_poly_MLFC_IMF4_coeff7<- diff_poly_MLFC_IMF4_coeff7/ sum
# w_poly_MLFC_IMF4_coeff8<- diff_poly_MLFC_IMF4_coeff8/ sum 
# w_poly_MLFC_IMF4_coeff9<- diff_poly_MLFC_IMF4_coeff9/ sum 
# w_poly_MLFC_IMF4_coeff10<- diff_poly_MLFC_IMF4_coeff10/ sum 
# w_poly_MLFC_IMF4_coeff11<- diff_poly_MLFC_IMF4_coeff11/ sum 
# w_poly_MLFC_IMF4_coeff12<- diff_poly_MLFC_IMF4_coeff12/ sum 
# 
# w_tan_MLFC_IMF4_coeff1<- diff_tan_MLFC_IMF4_coeff1/ sum
# w_tan_MLFC_IMF4_coeff2<- diff_tan_MLFC_IMF4_coeff2/ sum
# w_tan_MLFC_IMF4_coeff3<- diff_tan_MLFC_IMF4_coeff3/ sum 
# w_tan_MLFC_IMF4_coeff4<- diff_tan_MLFC_IMF4_coeff4/ sum 
# w_tan_MLFC_IMF4_coeff5<- diff_tan_MLFC_IMF4_coeff5/ sum 
# w_tan_MLFC_IMF4_coeff6<- diff_tan_MLFC_IMF4_coeff6/ sum
# w_tan_MLFC_IMF4_coeff7<- diff_tan_MLFC_IMF4_coeff7/ sum
# w_tan_MLFC_IMF4_coeff8<- diff_tan_MLFC_IMF4_coeff8/ sum 
# w_tan_MLFC_IMF4_coeff9<- diff_tan_MLFC_IMF4_coeff9/ sum 
# w_tan_MLFC_IMF4_coeff10<- diff_tan_MLFC_IMF4_coeff10/ sum 
# w_tan_MLFC_IMF4_coeff11<- diff_tan_MLFC_IMF4_coeff11/ sum 
# w_tan_MLFC_IMF4_coeff12<- diff_tan_MLFC_IMF4_coeff12/ sum  
# 
# w_bess_MLFC_IMF4_coeff1<- diff_bess_MLFC_IMF4_coeff1/ sum
# w_bess_MLFC_IMF4_coeff2<- diff_bess_MLFC_IMF4_coeff2/ sum
# w_bess_MLFC_IMF4_coeff3<- diff_bess_MLFC_IMF4_coeff3/ sum 
# w_bess_MLFC_IMF4_coeff4<- diff_bess_MLFC_IMF4_coeff4/ sum 
# w_bess_MLFC_IMF4_coeff5<- diff_bess_MLFC_IMF4_coeff5/ sum 
# w_bess_MLFC_IMF4_coeff6<- diff_bess_MLFC_IMF4_coeff6/ sum
# w_bess_MLFC_IMF4_coeff7<- diff_bess_MLFC_IMF4_coeff7/ sum
# w_bess_MLFC_IMF4_coeff8<- diff_bess_MLFC_IMF4_coeff8/ sum 
# w_bess_MLFC_IMF4_coeff9<- diff_bess_MLFC_IMF4_coeff9/ sum 
# w_bess_MLFC_IMF4_coeff10<- diff_bess_MLFC_IMF4_coeff10/ sum 
# w_bess_MLFC_IMF4_coeff11<- diff_bess_MLFC_IMF4_coeff11/ sum 
# w_bess_MLFC_IMF4_coeff12<- diff_bess_MLFC_IMF4_coeff12/ sum 
# 
# w_vann_MLFC_IMF4_coeff1<- diff_vann_MLFC_IMF4_coeff1/ sum
# w_vann_MLFC_IMF4_coeff2<- diff_vann_MLFC_IMF4_coeff2/ sum
# w_vann_MLFC_IMF4_coeff3<- diff_vann_MLFC_IMF4_coeff3/ sum 
# w_vann_MLFC_IMF4_coeff4<- diff_vann_MLFC_IMF4_coeff4/ sum 
# w_vann_MLFC_IMF4_coeff5<- diff_vann_MLFC_IMF4_coeff5/ sum 
# w_vann_MLFC_IMF4_coeff6<- diff_vann_MLFC_IMF4_coeff6/ sum
# w_vann_MLFC_IMF4_coeff7<- diff_vann_MLFC_IMF4_coeff7/ sum
# w_vann_MLFC_IMF4_coeff8<- diff_vann_MLFC_IMF4_coeff8/ sum 
# w_vann_MLFC_IMF4_coeff9<- diff_vann_MLFC_IMF4_coeff9/ sum 
# w_vann_MLFC_IMF4_coeff10<- diff_vann_MLFC_IMF4_coeff10/ sum 
# w_vann_MLFC_IMF4_coeff11<- diff_vann_MLFC_IMF4_coeff11/ sum 
# w_vann_MLFC_IMF4_coeff12<- diff_vann_MLFC_IMF4_coeff12/ sum 
# 
# 
# 
# 
# 
# #for the table - weights
# w_RBF_syntspeak1<- c(w_RBF_MLFC_IMF4_coeff1, w_RBF_MLFC_IMF4_coeff2, w_RBF_MLFC_IMF4_coeff3,w_RBF_MLFC_IMF4_coeff4,
#                      w_RBF_MLFC_IMF4_coeff5,w_RBF_MLFC_IMF4_coeff6, w_RBF_MLFC_IMF4_coeff7,w_RBF_MLFC_IMF4_coeff8,
#                      w_RBF_MLFC_IMF4_coeff9, w_RBF_MLFC_IMF4_coeff10, w_RBF_MLFC_IMF4_coeff11, 
#                      w_RBF_MLFC_IMF4_coeff12)
# w_LP_syntspeak1<- c(w_LP_MLFC_IMF4_coeff1, w_LP_MLFC_IMF4_coeff2, w_LP_MLFC_IMF4_coeff3,w_LP_MLFC_IMF4_coeff4,
#                     w_LP_MLFC_IMF4_coeff5,w_LP_MLFC_IMF4_coeff6, w_LP_MLFC_IMF4_coeff7,w_LP_MLFC_IMF4_coeff8,
#                     w_LP_MLFC_IMF4_coeff9, w_LP_MLFC_IMF4_coeff10, w_LP_MLFC_IMF4_coeff11, 
#                     w_LP_MLFC_IMF4_coeff12)
# 
# w_poly_syntspeak1<- c(w_poly_MLFC_IMF4_coeff1, w_poly_MLFC_IMF4_coeff2, w_poly_MLFC_IMF4_coeff3,w_poly_MLFC_IMF4_coeff4,
#                       w_poly_MLFC_IMF4_coeff5,w_poly_MLFC_IMF4_coeff6, w_poly_MLFC_IMF4_coeff7,w_poly_MLFC_IMF4_coeff8,
#                       w_poly_MLFC_IMF4_coeff9, w_poly_MLFC_IMF4_coeff10, w_poly_MLFC_IMF4_coeff11, 
#                       w_poly_MLFC_IMF4_coeff12)
# 
# w_tan_syntspeak1<- c(w_tan_MLFC_IMF4_coeff1, w_tan_MLFC_IMF4_coeff2, w_tan_MLFC_IMF4_coeff3,w_tan_MLFC_IMF4_coeff4,
#                      w_tan_MLFC_IMF4_coeff5,w_tan_MLFC_IMF4_coeff6, w_tan_MLFC_IMF4_coeff7,w_tan_MLFC_IMF4_coeff8,
#                      w_tan_MLFC_IMF4_coeff9, w_tan_MLFC_IMF4_coeff10, w_tan_MLFC_IMF4_coeff11, 
#                      w_tan_MLFC_IMF4_coeff12)
# 
# w_bess_syntspeak1<- c(w_bess_MLFC_IMF4_coeff1, w_bess_MLFC_IMF4_coeff2, w_bess_MLFC_IMF4_coeff3,w_bess_MLFC_IMF4_coeff4,
#                       w_bess_MLFC_IMF4_coeff5,w_bess_MLFC_IMF4_coeff6, w_bess_MLFC_IMF4_coeff7,w_bess_MLFC_IMF4_coeff8,
#                       w_bess_MLFC_IMF4_coeff9, w_bess_MLFC_IMF4_coeff10, w_bess_MLFC_IMF4_coeff11, 
#                       w_bess_MLFC_IMF4_coeff12)
# 
# w_vann_syntspeak1<- c(w_vann_MLFC_IMF4_coeff1, w_vann_MLFC_IMF4_coeff2, w_vann_MLFC_IMF4_coeff3,w_vann_MLFC_IMF4_coeff4,
#                       w_vann_MLFC_IMF4_coeff5,w_vann_MLFC_IMF4_coeff6, w_vann_MLFC_IMF4_coeff7,w_vann_MLFC_IMF4_coeff8,
#                       w_vann_MLFC_IMF4_coeff9, w_vann_MLFC_IMF4_coeff10, w_vann_MLFC_IMF4_coeff11, 
#                       w_vann_MLFC_IMF4_coeff12)
# 
# # #since we sum over the kernels and keeping fixed the features then we use these vectors
# # w_IF_ARIMAGARCH<- c(w_RBF_IF, w_LP_IF, w_poly_IF, w_tan_IF, w_bess_IF)
# # w_IMF_ARIMAGARCH<- c(w_RBF_IMF, w_LP_IMF, w_poly_IMF, w_tan_IMF, w_bess_IMF )
# # w_stat_ARIMAGARCH<- c(w_RBF_stat, w_LP_stat,w_poly_stat, w_tan_stat, w_bess_stat )
# # w_SC_ARIMAGARCH<- c(w_RBF_SC,w_LP_SC, w_poly_SC,w_tan_SC, w_bess_SC)
# 
# 
# 
# 
# #for the table - diff
# w_RBF_syntspeak1<- c(diff_RBF_MLFC_IMF4_coeff1, diff_RBF_MLFC_IMF4_coeff2, diff_RBF_MLFC_IMF4_coeff3,
#                      diff_RBF_MLFC_IMF4_coeff4, diff_RBF_MLFC_IMF4_coeff5, diff_RBF_MLFC_IMF4_coeff6,
#                      diff_RBF_MLFC_IMF4_coeff7,diff_RBF_MLFC_IMF4_coeff8, diff_RBF_MLFC_IMF4_coeff9,
#                      diff_RBF_MLFC_IMF4_coeff10,diff_RBF_MLFC_IMF4_coeff11,diff_RBF_MLFC_IMF4_coeff12)
# w_LP_syntspeak1<- c(diff_LP_MLFC_IMF4_coeff1, diff_LP_MLFC_IMF4_coeff2, diff_LP_MLFC_IMF4_coeff3,
#                     diff_LP_MLFC_IMF4_coeff4, diff_LP_MLFC_IMF4_coeff5, diff_LP_MLFC_IMF4_coeff6,
#                     diff_LP_MLFC_IMF4_coeff7,diff_LP_MLFC_IMF4_coeff8, diff_LP_MLFC_IMF4_coeff9,
#                     diff_LP_MLFC_IMF4_coeff10,diff_LP_MLFC_IMF4_coeff11,diff_LP_MLFC_IMF4_coeff12)
# 
# w_poly_syntspeak1<- c(diff_poly_MLFC_IMF4_coeff1, diff_poly_MLFC_IMF4_coeff2, diff_poly_MLFC_IMF4_coeff3,
#                       diff_poly_MLFC_IMF4_coeff4, diff_poly_MLFC_IMF4_coeff5, diff_poly_MLFC_IMF4_coeff6,
#                       diff_poly_MLFC_IMF4_coeff7,diff_poly_MLFC_IMF4_coeff8, diff_poly_MLFC_IMF4_coeff9,
#                       diff_poly_MLFC_IMF4_coeff10,diff_poly_MLFC_IMF4_coeff11,diff_poly_MLFC_IMF4_coeff12)
# w_tan_syntspeak1<- c(diff_tan_MLFC_IMF4_coeff1, diff_tan_MLFC_IMF4_coeff2, diff_tan_MLFC_IMF4_coeff3,
#                      diff_tan_MLFC_IMF4_coeff4, diff_tan_MLFC_IMF4_coeff5, diff_tan_MLFC_IMF4_coeff6,
#                      diff_tan_MLFC_IMF4_coeff7,diff_tan_MLFC_IMF4_coeff8, diff_tan_MLFC_IMF4_coeff9,
#                      diff_tan_MLFC_IMF4_coeff10,diff_tan_MLFC_IMF4_coeff11,diff_tan_MLFC_IMF4_coeff12)
# w_bess_syntspeak1<- c(diff_bess_MLFC_IMF4_coeff1, diff_bess_MLFC_IMF4_coeff2, diff_bess_MLFC_IMF4_coeff3,
#                       diff_bess_MLFC_IMF4_coeff4, diff_bess_MLFC_IMF4_coeff5, diff_bess_MLFC_IMF4_coeff6,
#                       diff_bess_MLFC_IMF4_coeff7,diff_bess_MLFC_IMF4_coeff8, diff_bess_MLFC_IMF4_coeff9,
#                       diff_bess_MLFC_IMF4_coeff10,diff_bess_MLFC_IMF4_coeff11,diff_bess_MLFC_IMF4_coeff12)
# 
# w_vann_syntspeak1<- c(diff_vann_MLFC_IMF4_coeff1, diff_vann_MLFC_IMF4_coeff2, diff_vann_MLFC_IMF4_coeff3,
#                       diff_vann_MLFC_IMF4_coeff4, diff_vann_MLFC_IMF4_coeff5, diff_vann_MLFC_IMF4_coeff6,
#                       diff_vann_MLFC_IMF4_coeff7,diff_vann_MLFC_IMF4_coeff8, diff_vann_MLFC_IMF4_coeff9,
#                       diff_vann_MLFC_IMF4_coeff10,diff_vann_MLFC_IMF4_coeff11,diff_vann_MLFC_IMF4_coeff12)
# 
# 
# 
# 
# #save(w_RBF, w_LP, w_poly, w_tan, w_bess, 
# #    file = "C:\\Users\\Marta\\Desktop\\Feature_study\\weigth_features_ARIMA_GARCH.RData")
# 
# 
# #PRINT Speech_OUT ON A TABLE
# nsvm<- c('MFCC1', 'MFCC2', 'MFCC3', 'MFCC4', 'MFCC5', 'MFCC6', 'MFCC7', 'MFCC8', 'MFCC9', 'MFCC10',
#          'MFCC11', 'MFCC12')
# 
# cnames_1<- list('Synthetic vs Speaker1 ', 'RBF', 'LP',  'Poly', 'Tan', 'Bess', 'Vann')
# 
# tbl_syntspeak1<- data.frame(nsvm, w_RBF_syntspeak1, w_LP_syntspeak1, w_poly_syntspeak1, w_tan_syntspeak1,
#                             w_bess_syntspeak1, w_vann_syntspeak1)
# names(tbl_syntspeak1)<- cnames_1
# 
# ltabl_syntspeak1<-kable(tbl_syntspeak1, format = "latex", longtable = T, caption = "Weigth Kernels Synt vs Speak1")%>%
#   kable_styling(latex_options = c("striped", "hold_position"),       #stripe_color
#                 full_width = F)
# 
# 
# 
# 
# 
# 
# 
