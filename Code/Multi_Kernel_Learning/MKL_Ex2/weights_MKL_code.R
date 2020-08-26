#RUN THIS TOGETHER WITH MKL_SPEECH_HARVARD


#case all the 5 MFCC-IMFs
S = sum(diff_IMF1,diff_IMF2, diff_IMF3, diff_IMF4, diff_IMF5)


w_IMF1 = diff_IMF1/S
w_IMF2 = diff_IMF2/S
w_IMF3 = diff_IMF3/S
w_IMF4 = diff_IMF4/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF1,w_IMF2,w_IMF3,w_IMF4,w_IMF5)

w1<- w_final[8,1]
w2<- w_final[2,2]
w3<- w_final[7,3]
w4<- w_final[2,4]
w5<- w_final[1,5]

#cases with 4 MFCC-IMFs
#1
S = sum(diff_IMF1,diff_IMF2, diff_IMF3, diff_IMF4)

w_IMF1 = diff_IMF1/S
w_IMF2 = diff_IMF2/S
w_IMF3 = diff_IMF3/S
w_IMF4 = diff_IMF4/S

w_final = data.frame(w_IMF1,w_IMF2,w_IMF3,w_IMF4)


w1 = w_final[8,1]
w2 = w_final[2,2]
w3 = w_final[7,3]
w4 = w_final[2,4]
w5 = 0

#2
S = sum(diff_IMF1,diff_IMF2, diff_IMF3, diff_IMF5)

w_IMF1 = diff_IMF1/S
w_IMF2 = diff_IMF2/S
w_IMF3 = diff_IMF3/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF1,w_IMF2,w_IMF3,w_IMF5)

w1 = w_final[8,1]
w2 = w_final[2,2]
w3 = w_final[7,3]
w4 = 0
w5 = w_final[1,5]

#3
S = sum(diff_IMF1,diff_IMF2, diff_IMF4, diff_IMF5)

w_IMF1 = diff_IMF1/S
w_IMF2 = diff_IMF2/S
w_IMF4 = diff_IMF4/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF1,w_IMF2,w_IMF4,w_IMF5)

w1<- w_final[8,1]
w2<- w_final[2,2]
w3<- 0
w4<- w_final[2,4]
w5<- w_final[1,5]



#4
S = sum(diff_IMF1,diff_IMF3, diff_IMF4, diff_IMF5)

w_IMF1 = diff_IMF1/S
w_IMF3 = diff_IMF3/S
w_IMF4 = diff_IMF4/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF1,w_IMF3,w_IMF4,w_IMF5)

w1<- w_final[8,1]
w2<- 0
w3<- w_final[7,3]
w4<- w_final[2,4]
w5<- w_final[1,5]



#5
S = sum(diff_IMF2,diff_IMF3, diff_IMF4, diff_IMF5)

w_IMF2 = diff_IMF2/S
w_IMF3 = diff_IMF3/S
w_IMF4 = diff_IMF4/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF2,w_IMF3,w_IMF4,w_IMF5)

w1<- 0
w2<- w_final[2,2]
w3<- w_final[7,3]
w4<- w_final[2,4]
w5<- w_final[1,5]

#cases with 3 MFCC-IMFs
#1
S = sum(diff_IMF1,diff_IMF2, diff_IMF3)

w_IMF1 = diff_IMF1/S
w_IMF2 = diff_IMF2/S
w_IMF3 = diff_IMF3/S

w_final = data.frame(w_IMF1,w_IMF2,w_IMF3)

w1<- w_final[8,1]
w2<- w_final[2,2]
w3<- w_final[7,3]
w4<- 0
w5<- 0

#2
S = sum(diff_IMF1,diff_IMF2, diff_IMF4)

w_IMF1 = diff_IMF1/S
w_IMF2 = diff_IMF2/S
w_IMF4 = diff_IMF4/S

w_final = data.frame(w_IMF1,w_IMF2,w_IMF4)


w1<- w_final[8,1]
w2<- w_final[2,2]
w3<- 0
w4<- w_final[2,4]
w5<- 0

#3
S = sum(diff_IMF1,diff_IMF2, diff_IMF5)

w_IMF1 = diff_IMF1/S
w_IMF2 = diff_IMF2/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF1,w_IMF2,w_IMF5)


w1<- w_final[8,1]
w2<- w_final[2,2]
w3<- 0
w4<- 0
w5<- w_final[1,5]


#4
S = sum(diff_IMF1,diff_IMF4, diff_IMF5)

w_IMF1 = diff_IMF1/S
w_IMF4 = diff_IMF4/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF1,w_IMF4,w_IMF5)

w1<- w_final[8,1]
w2<- 0
w3<- 0
w4<- w_final[2,4]
w5<- w_final[1,5]


#5
S = sum(diff_IMF1,diff_IMF3, diff_IMF4)

w_IMF1 = diff_IMF1/S
w_IMF3 = diff_IMF3/S
w_IMF4 = diff_IMF4/S

w_final = data.frame(w_IMF1,w_IMF3,w_IMF4)

w1<- w_final[8,1]
w2<- 0
w3<- w_final[7,3]
w4<- w_final[2,4]
w5<- 0


#6
S = sum(diff_IMF1,diff_IMF3, diff_IMF5)

w_IMF1 = diff_IMF1/S
w_IMF3 = diff_IMF3/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF1,w_IMF3,w_IMF5)


w1<- w_final[8,1]
w2<- 0
w3<- w_final[7,3]
w4<- 0
w5<- w_final[1,5]

#7
S = sum(diff_IMF3,diff_IMF4, diff_IMF5)

w_IMF3 = diff_IMF3/S
w_IMF4 = diff_IMF4/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF3,w_IMF4,w_IMF5)

w1<- 0
w2<- 0
w3<- w_final[7,3]
w4<- w_final[2,4]
w5<- w_final[1,5]




#8
S = sum(diff_IMF2,diff_IMF4, diff_IMF5)

w_IMF2 = diff_IMF2/S
w_IMF4 = diff_IMF4/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF2,w_IMF4,w_IMF5)

w1<- 0
w2<- w_final[2,2]
w3<- 0
w4<- w_final[2,4]
w5<- w_final[1,5]



#9
S = sum(diff_IMF2,diff_IMF3, diff_IMF5)

w_IMF2 = diff_IMF2/S
w_IMF3 = diff_IMF3/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF2,w_IMF3,w_IMF5)


w1<- 0
w2<- w_final[2,2]
w3<- w_final[7,3]
w4<- 0
w5<- w_final[1,5]



#10
S = sum(diff_IMF2,diff_IMF3, diff_IMF4)

w_IMF2 = diff_IMF2/S
w_IMF3 = diff_IMF3/S
w_IMF4 = diff_IMF4/S

w_final = data.frame(w_IMF2,w_IMF3,w_IMF4)

w1<- 0
w2<- w_final[2,2]
w3<- w_final[7,3]
w4<- w_final[2,4]
w5<- 0

#cases with 2 MFCC-IMFs
#1
S = sum(diff_IMF1,diff_IMF2)

w_IMF1 = diff_IMF1/S
w_IMF2 = diff_IMF2/S

w_final = data.frame(w_IMF1,w_IMF2)

w1<- w_final[8,1]
w2<- w_final[2,2]
w3<- 0
w4<- 0
w5<- 0

#2
S = sum(diff_IMF1,diff_IMF3)

w_IMF1 = diff_IMF1/S
w_IMF3 = diff_IMF3/S

w_final = data.frame(w_IMF1,w_IMF3)

w1<- w_final[8,1]
w2<- 0
w3<- w_final[7,3]
w4<- 0
w5<- 0


#3
S = sum(diff_IMF1,diff_IMF4)

w_IMF1 = diff_IMF1/S
w_IMF4 = diff_IMF4/S

w_final = data.frame(w_IMF1,w_IMF4)

w1<- w_final[8,1]
w2<- 0
w3<- 0
w4<- w_final[2,4]
w5<- 0

#4
S = sum(diff_IMF1,diff_IMF5)

w_IMF1 = diff_IMF1/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF1,w_IMF5)


w1<- w_final[8,1]
w2<- 0
w3<- 0
w4<- 0
w5<- w_final[1,5]


#5
S = sum(diff_IMF2,diff_IMF3)

w_IMF2 = diff_IMF2/S
w_IMF3 = diff_IMF3/S

w_final = data.frame(w_IMF2,w_IMF3)

w1<- 0
w2<- w_final[2,2]
w3<- w_final[7,3]
w4<- 0
w5<- 0

#6
S = sum(diff_IMF2,diff_IMF4)

w_IMF2 = diff_IMF2/S
w_IMF4 = diff_IMF4/S

w_final = data.frame(w_IMF2,w_IMF4)

w1<- 0
w2<- w_final[2,2]
w3<- 0
w4<- w_final[2,4]
w5<- 0



#7
S = sum(diff_IMF2,diff_IMF5)

w_IMF2 = diff_IMF2/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF2,w_IMF5)


w1<- 0
w2<- w_final[2,2]
w3<- 0
w4<- 0
w5<- w_final[1,5]



#8
S = sum(diff_IMF3,diff_IMF4)

w_IMF3 = diff_IMF3/S
w_IMF4 = diff_IMF4/S

w_final = data.frame(w_IMF3,w_IMF4)


w1<- 0
w2<- 0
w3<- w_final[7,3]
w4<- w_final[2,4]
w5<- 0



#9
S = sum(diff_IMF3,diff_IMF5)

w_IMF3 = diff_IMF3/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF3,w_IMF5)


w1<- 0
w2<- 0
w3<- w_final[7,3]
w4<- 0
w5<- w_final[1,5]


#9
S = sum(diff_IMF4,diff_IMF5)

w_IMF4 = diff_IMF4/S
w_IMF5 = diff_IMF5/S

w_final = data.frame(w_IMF4,w_IMF5)


w1<- 0
w2<- 0
w3<- 0
w4<- w_final[2,4]
w5<- w_final[1,5]

