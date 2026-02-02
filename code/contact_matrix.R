setwd('C:/project/Social-contact-pattern-in-South-Korea-a-KoMix-study/data/Survey on Changes in Contact Patterns After COVID-19 (23.11)')
data= read.csv('Data(2311).csv',fileEncoding = "utf-8")
library(stringr)
library(dplyr)

colnames(data)
contact_num_index = 8:57 # Age group columns (Q8A_1_1 to Q8A_50_1): 50 columns

#############################################################################
# If the number of COVID-19 vaccinated NA, we replace NA to 0
data$Q3A[is.na(data$Q3A)]=0

# Age, Weekday/Weekend, Number of contact, Age group of contactees.
# New column positions: SQ2(1), SQ5AA(2), Q8(7), Q8A_1_1~Q8A_50_1(8-57), Q4(5), Q7(6), Q2(3), Q3A(4)
contact_data = data[,c(1,2,7,contact_num_index,5,6,3,4)]
contact_data[is.na(contact_data)]=0

#############################################################################################################################
# Contact matrix, Weekday contact matrix, Weekend contact matrix
phi =  matrix(0,nrow=10,ncol=10)
phi_week = matrix(0,nrow=10,ncol=10)
phi_weekend = matrix(0,nrow=10,ncol=10)

################################
# Age group of contact matrix
age=c(0,3,7,13,16,19,30,40,50,60)
head(contact_data)
#############################################################################################################################
# Construct contact matrix
contact_week = contact_data %>% filter(SQ5AA==5)
contact_weekend = contact_data %>% filter(SQ5AA==2)

for( i in 1:10){
  if(i !=10){
    temp = contact_data %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_week = contact_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_weekend = contact_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
  }else{
    temp = contact_data %>% filter(SQ2>=age[10])
    temp_week = contact_week %>% filter(SQ2>=age[10])
    temp_weekend = contact_weekend %>% filter(SQ2>=age[10])
  }
  for(j in 1:10){
    
    if(j !=10){
      phi[i,j] = sum(sapply(1:nrow(temp),function(x) length(which(temp[x,4:53]==j))))/nrow(temp)
      phi_week[i,j] = sum(sapply(1:nrow(temp_week),function(x) length(which(temp_week[x,4:53]==j))))/nrow(temp_week)
      phi_weekend[i,j] = sum(sapply(1:nrow(temp_weekend),function(x) length(which(temp_weekend[x,4:53]==j))))/nrow(temp_weekend)
    }else{
      phi[i,j] = sum(sapply(1:nrow(temp),function(x) length(which(temp[x,4:53]>=j))))/nrow(temp)
      phi_week[i,j] = sum(sapply(1:nrow(temp_week),function(x) length(which(temp_week[x,4:53]>=j))))/nrow(temp_week)
      phi_weekend[i,j] = sum(sapply(1:nrow(temp_weekend),function(x) length(which(temp_weekend[x,4:53]>=j))))/nrow(temp_weekend)
    }
  }
}

########################################################################################################
# Sebastian contact matrix

#1. Weighted average of weekday contact and weekend contact
phi_res = (5/7) * phi_week + (2/7) * phi_weekend

#2. Load Korea population structure
n= rep(0,10)
setwd('C:/project/Social-contact-pattern-in-South-Korea-a-KoMix-study/data')
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

for(i in 1:10){
  for(j in 1:10){
    phi_res_sb[i,j] = (phi_res[i,j]*n[i] + phi_res[j,i]*n[j])/(2*n[i])
  }
}

########################################################################################################
name = c('[0,3)','[3,7)','[7,13)','[13,16)','[16,19)','[19,30)','[30,40)','[40,50)','[50,60)','[60,+)')

phi_res_sb = as.data.frame(phi_res_sb)
rownames(phi_res_sb) = name
colnames(phi_res_sb) = name

setwd('C:/project/Social-contact-pattern-in-South-Korea-a-KoMix-study')

write.csv(phi_res_sb,'./contact matrix/contact_2311_Sebastian.csv')
