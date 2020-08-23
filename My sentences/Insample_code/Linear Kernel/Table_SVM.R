#TABLE FOR SVM 
library('knitr')
library('kableExtra')
library('tcpl')


tbl<- list(ltabl_SYNTSPEAK1)

#PRINTING ALL OF THEM ON A LATEX FILE -------------------------------------------------------------------------------


for(i in (1:(length(tbl)))){
  
  sink("C:\\Users\\Marta\\Desktop\\output_R\\Tables_SVMs.txt", append = TRUE)
  print(tbl[i])
  sink()
  
}

sink.reset()

rm(ltabl_SYNTSPEAK1)
