setwd('C:/Users/Hwichang Jeong/Desktop/corona_정리/corona/Kstat_contact_matrix/Kstat')
data = read.csv('(Kstat) 코로나19 이후 접촉도 변화 조사_3차(22년 6월)_Data.csv',fileEncoding = "euc-kr")
library(stringr)
library(dplyr)

colnames(data)

contact_num_index = seq(from=45,344,by=6) # maximum 50

#############################################################################
# Age, Weekday/Weekend, Number of contact, Age group of contactees, Mask time
contact_data = data[,c(11,16,43,contact_num_index,24)]
contact_data[is.na(contact_data)]=0

#############################################################################################################################
# Contact matrix, Weekday contact matrix, Weekend contact matrix, Low mask time contact matrix, High mask time contact matrix
phi =  matrix(0,nrow=10,ncol=10)
phi_week = matrix(0,nrow=10,ncol=10)
phi_weekend = matrix(0,nrow=10,ncol=10)

phi_mask_low =  matrix(0,nrow=10,ncol=10)
phi_mask_low_week = matrix(0,nrow=10,ncol=10)
phi_mask_low_weekend = matrix(0,nrow=10,ncol=10)

phi_mask_high =  matrix(0,nrow=10,ncol=10)
phi_mask_high_week = matrix(0,nrow=10,ncol=10)
phi_mask_high_weekend = matrix(0,nrow=10,ncol=10)

################################
# Age group of contact matrix
age=c(0,3,7,13,16,19,30,40,50,60)

#############################################################################################################################
# Construct contact matrix

contact_week = contact_data %>% filter(SQ5AA==5)
contact_weekend = contact_data %>% filter(SQ5AA==2)

contact_mask_low = contact_data %>% filter(RQ1A<4)
contact_mask_low_week = contact_mask_low %>% filter(SQ5AA==5)
contact_mask_low_weekend = contact_mask_low %>% filter(SQ5AA==2)

contact_mask_high = contact_data %>% filter(RQ1A>=4)
contact_mask_high_week = contact_mask_high %>% filter(SQ5AA==5)
contact_mask_high_weekend = contact_mask_high %>% filter(SQ5AA==2)

for( i in 1:10){
  if(i !=10){
    temp = contact_data %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_week = contact_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_weekend = contact_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    
    temp_mask_low = contact_mask_low %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_mask_low_week = contact_mask_low_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_mask_low_weekend = contact_mask_low_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    
    temp_mask_high = contact_mask_high %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_mask_high_week = contact_mask_high_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_mask_high_weekend = contact_mask_high_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    
  }else{
    temp = contact_data %>% filter(SQ2>=age[10])
    temp_week = contact_week %>% filter(SQ2>=age[10])
    temp_weekend = contact_weekend %>% filter(SQ2>=age[10])
    
    temp_mask_low = contact_mask_low %>% filter(SQ2>=age[10])
    temp_mask_low_week = contact_mask_low_week %>% filter(SQ2>=age[10])
    temp_mask_low_weekend = contact_mask_low_weekend %>% filter(SQ2>=age[10])
    
    temp_mask_high = contact_mask_high %>% filter(SQ2>=age[10])
    temp_mask_high_week = contact_mask_high_week %>% filter(SQ2>=age[10])
    temp_mask_high_weekend = contact_mask_high_weekend %>% filter(SQ2>=age[10])
    
  }
  for(j in 1:10){
    if(j !=10){
      phi[i,j] = sum(sapply(1:nrow(temp),function(x) length(which(temp[x,4:53]==j))))/nrow(temp)
      phi_week[i,j] = sum(sapply(1:nrow(temp_week),function(x) length(which(temp_week[x,4:53]==j))))/nrow(temp_week)
      phi_weekend[i,j] = sum(sapply(1:nrow(temp_weekend),function(x) length(which(temp_weekend[x,4:53]==j))))/nrow(temp_weekend)
      
      phi_mask_low[i,j] = sum(sapply(1:nrow(temp_mask_low),function(x) length(which(temp_mask_low[x,4:53]==j))))/nrow(temp_mask_low)
      phi_mask_low_week[i,j] = sum(sapply(1:nrow(temp_mask_low_week),function(x) length(which(temp_mask_low_week[x,4:53]==j))))/nrow(temp_mask_low_week)
      phi_mask_low_weekend[i,j] = sum(sapply(1:nrow(temp_mask_low_weekend),function(x) length(which(temp_mask_low_weekend[x,4:53]==j))))/nrow(temp_mask_low_weekend)
      
      phi_mask_high[i,j] = sum(sapply(1:nrow(temp_mask_high),function(x) length(which(temp_mask_high[x,4:53]==j))))/nrow(temp_mask_high)
      phi_mask_high_week[i,j] = sum(sapply(1:nrow(temp_mask_high_week),function(x) length(which(temp_mask_high_week[x,4:53]==j))))/nrow(temp_mask_high_week)
      phi_mask_high_weekend[i,j] = sum(sapply(1:nrow(temp_mask_high_weekend),function(x) length(which(temp_mask_high_weekend[x,4:53]==j))))/nrow(temp_mask_high_weekend)
      
    }else{
      phi[i,j] = sum(sapply(1:nrow(temp),function(x) length(which(temp[x,4:53]>=j))))/nrow(temp)
      phi_week[i,j] = sum(sapply(1:nrow(temp_week),function(x) length(which(temp_week[x,4:53]>=j))))/nrow(temp_week)
      phi_weekend[i,j] = sum(sapply(1:nrow(temp_weekend),function(x) length(which(temp_weekend[x,4:53]>=j))))/nrow(temp_weekend)
      
      phi_mask_low[i,j] = sum(sapply(1:nrow(temp_mask_low),function(x) length(which(temp_mask_low[x,4:53]>=j))))/nrow(temp_mask_low)
      phi_mask_low_week[i,j] = sum(sapply(1:nrow(temp_mask_low_week),function(x) length(which(temp_mask_low_week[x,4:53]>=j))))/nrow(temp_mask_low_week)
      phi_mask_low_weekend[i,j] = sum(sapply(1:nrow(temp_mask_low_weekend),function(x) length(which(temp_mask_low_weekend[x,4:53]>=j))))/nrow(temp_mask_low_weekend)
      
      phi_mask_high[i,j] = sum(sapply(1:nrow(temp_mask_high),function(x) length(which(temp_mask_high[x,4:53]>=j))))/nrow(temp_mask_high)
      phi_mask_high_week[i,j] = sum(sapply(1:nrow(temp_mask_high_week),function(x) length(which(temp_mask_high_week[x,4:53]>=j))))/nrow(temp_mask_high_week)
      phi_mask_high_weekend[i,j] = sum(sapply(1:nrow(temp_mask_high_weekend),function(x) length(which(temp_mask_high_weekend[x,4:53]>=j))))/nrow(temp_mask_high_weekend)
      
    }
  }
}
######################
# Ibuka contact matrix
phi
phi_mask_low
phi_mask_high
######################

########################################################################################################
# Sebastian contact matrix

#1. Weighted average of weekday contact and weekend contact
phi_res = (5/7) * phi_week + (2/7) * phi_weekend
phi_mask_low_res = (5/7) * phi_mask_low_week + (2/7) * phi_mask_low_weekend
phi_mask_high_res = (5/7) * phi_mask_high_week + (2/7) * phi_mask_high_weekend

#2. Load Korea population structure
n= rep(0,10)
setwd('C:/Users/Hwichang Jeong/Desktop/corona_정리/corona/GIT - 복사본/KOREA')
skage = read.csv('skage.csv',header=T)
skage = skage[1:101,5]
skage = as.numeric(sapply(skage, function(x) gsub(',','',x)))
sum(skage[1:3])
for(i in 1:9){
  n[i] = sum(skage[(age[i]+1):(age[(i+1)])])
}
n[10] = sum(skage[61:101])

#3. Calculating the symmetric component by reflecting the population structure.
phi_res_sb = matrix(0,nrow=10,ncol=10)
phi_res_sb_mask_low = matrix(0,nrow=10,ncol=10)
phi_res_sb_mask_high = matrix(0,nrow=10,ncol=10)
for(i in 1:10){
  for(j in 1:10){
    phi_res_sb[i,j] = (phi_res[i,j]*n[i] + phi_res[j,i]*n[j])/(2*n[i])
    phi_res_sb_mask_low[i,j] = (phi_mask_low_res[i,j]*n[i] + phi_mask_low_res[j,i]*n[j])/(2*n[i])
    phi_res_sb_mask_high[i,j] = (phi_mask_high_res[i,j]*n[i] + phi_mask_high_res[j,i]*n[j])/(2*n[i])
  }
}
########################################################################################################
########################################################################################################

name = c('[0,3)','[3,7)','[7,13)','[13,16)','[16,19)','[19,30)','[30,40)','[40,50)','[50,60)','[60,+)')

phi = as.data.frame(phi)
rownames(phi) = name
colnames(phi) = name

phi_mask_low = as.data.frame(phi_mask_low)
rownames(phi_mask_low) = name
colnames(phi_mask_low) = name

phi_mask_high = as.data.frame(phi_mask_high)
rownames(phi_mask_high) = name
colnames(phi_mask_high) = name

phi_res_sb = as.data.frame(phi_res_sb)
rownames(phi_res_sb) = name
colnames(phi_res_sb) = name

phi_res_sb_mask_low = as.data.frame(phi_res_sb_mask_low)
rownames(phi_res_sb_mask_low) = name
colnames(phi_res_sb_mask_low) = name

phi_res_sb_mask_high = as.data.frame(phi_res_sb_mask_high)
rownames(phi_res_sb_mask_high) = name
colnames(phi_res_sb_mask_high) = name

setwd('C:/Users/Hwichang Jeong/Desktop/corona_정리/corona/Kstat_contact_matrix/Kstat')
write.csv(phi,'./contact matrix/IBUKA/2022/contact_2206_IBUKA.csv')
write.csv(phi_mask_low,'./contact matrix/IBUKA/2022/contact_2206_IBUKA_mask_low.csv')
write.csv(phi_mask_high,'./contact matrix/IBUKA/2022/contact_2206_IBUKA_mask_high.csv')
write.csv(phi_res_sb,'./contact matrix/Sebastian/2022/contact_2206_Sebastian.csv')
write.csv(phi_res_sb_mask_low,'./contact matrix/Sebastian/2022/contact_2206_Sebastian_mask_low.csv')
write.csv(phi_res_sb_mask_high,'./contact matrix/Sebastian/2022/contact_2206_Sebastian_mask_high.csv')
