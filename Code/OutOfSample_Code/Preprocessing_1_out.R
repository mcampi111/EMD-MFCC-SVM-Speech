# #PREPROCESSING IMFS
#speak1

feat_IMF_data_speak1_new_PREC<- lapply(1:m, function(x) matrix(NA, nrow=60000, ncol=5))

for (i in 1:m) {
  for (j in 1:5) {

    feat_IMF_data_speak1_new_PREC[[i]][,j]<- (feat_IMF_data_speak1_new[[i]][,j] - mean(feat_IMF_data_speak1_new[[i]][,j]))/(sqrt(var(feat_IMF_data_speak1_new[[i]][,j])))
  }

}

feat_speak1_IF_new_PREC<- lapply(1:m, function(x) matrix(NA, nrow=60000, ncol=5))

for (i in 1:m) {
  for (j in 1:5) {

    feat_speak1_IF_new_PREC[[i]][,j]<- (feat_speak1_IF_new[[i]][,j] - mean(feat_speak1_IF_new[[i]][,j]))/(sqrt(var(feat_speak1_IF_new[[i]][,j])))
  }

}


feat_speak1_coeff_new_PREC1<- matrix(NA, nrow=20, ncol=60000*5*3)
feat_speak1_coeff_PREC11<- matrix(NA, nrow=1, ncol=60000)

for (i in 1:m) {
  for (j in seq(60000, 900000, by = 60000)) {
       
    feat_speak1_coeff_PREC11<- (feat_speak1_coeff_new[i,(j-59999): j] - mean(feat_speak1_coeff_new[i,(j-59999): j]))/(sqrt(var(feat_speak1_coeff_new[i,(j-59999): j])))
    feat_speak1_coeff_new_PREC1[i,(j-59999): j]<- feat_speak1_coeff_PREC11
  
    feat_speak1_coeff_PREC11<- matrix(NA, nrow=1, ncol=60000)
    
    }
  
}

rm(feat_speak1_coeff_PREC11)


#synthetic

feat_IMF_data_synt_new_PREC<- lapply(1:m, function(x) matrix(NA, nrow=60000, ncol=5))

for (i in 1:m) {
  for (j in 1:5) {
    
    feat_IMF_data_synt_new_PREC[[i]][,j]<- (feat_IMF_data_synt_new[[i]][,j] - mean(feat_IMF_data_synt_new[[i]][,j]))/(sqrt(var(feat_IMF_data_synt_new[[i]][,j])))
  }
  
}

feat_synt_IF_new_PREC<- lapply(1:m, function(x) matrix(NA, nrow=60000, ncol=5))

for (i in 1:m) {
  for (j in 1:5) {
    
    feat_synt_IF_new_PREC[[i]][,j]<- (feat_synt_IF_new[[i]][,j] - mean(feat_synt_IF_new[[i]][,j]))/(sqrt(var(feat_synt_IF_new[[i]][,j])))
  }
  
}


feat_synt_coeff_new_PREC1<- matrix(NA, nrow=20, ncol=60000*5*3)
feat_synt_coeff_PREC11<- matrix(NA, nrow=1, ncol=60000)

for (i in 1:m) {
  for (j in seq(60000, 900000, by = 60000)) {
    
    feat_synt_coeff_PREC11<- (feat_synt_coeff_new[i,(j-59999): j] - mean(feat_synt_coeff_new[i,(j-59999): j]))/(sqrt(var(feat_synt_coeff_new[i,(j-59999): j])))
    feat_synt_coeff_new_PREC1[i,(j-59999): j]<- feat_synt_coeff_PREC11
    
    feat_synt_coeff_PREC11<- matrix(NA, nrow=1, ncol=60000)
    
  }
  
}

rm(feat_synt_coeff_PREC11)

