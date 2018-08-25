library(dplyr)

#bank t test, chiaq test

setwd("C:/Users/HS/Documents/GitHub/analsying-bank-data")
bank<-read.csv("bank-additional-full.csv", header = TRUE)
gbank<-read.table("german.data.txt", h=F, sep="")

str(bank)
summary(bank$age)
quantile(bank$age)


################
#1. age 나이
#1.1 age 그냥
var.test(age~y, bank)
# p-value < 2.2e-16 :  유의수준 0.05 하에서 <두 집단의 분산이 같다는 귀무가설>을 기각한다. (분산비가 1이 아니다)
# good, bad의 분산은 다르다.
t.test(age~y, data=bank, paired=FALSE, var.equal=FALSE)
t.test(age~y, data=bank, paired=FALSE, var.equal=TRUE)
# p-value = 1.805e-06(0.000001805) :  유의수준 0.01 하에서 <모평균에 차이가 없다는 귀무가설>을 기각한다. 
# good, bad 별로 나이의 평균에 차이가 있다. 

#1.2 age 사분위수 (ordinal)
bank$age4 <-ifelse(bank$age <= 32, '1', 
       ifelse(bank$age <= 38, '2', 
              ifelse(bank$age <= 47, '3', '4')))
xtabs(~ age4+y, data=bank)
prop.table(xtabs(~ age4+y, data=bank))
chisq.test(xtabs(~ age4+y, data=bank))
# p-value < 2.2e-16 : 유의수준 0.01 하에서 <goodbad와 age4 는 독립이다 라는 귀무가설>을 기각한다. 
# goodbad와 나이대는 독립이 아니다. 관계가 있다.
# 우량인지 불량인지에 따라서 나이대에 차이가 있다. 


###############
#2. job 직업
#2.1 job (factor)
xtabs(~job+y, data=bank)

chisq.test(xtabs(~job+y, data=bank))
# p-value < 2.2e-16 : 유의수준 0.01 하에서 <goodbad와 job은 독립이다 라는 귀무가설>을 기각한다. 
# goodbad와 직업은 독립이 아니다. 관계가 있다.
# 우량인지 불량인지에 따라서 직업의 차이가 있다. 



###############
#3. marital 결혼
#3.1 marital (factor)
xtabs(~marital+y, data=bank)
prop.table(xtabs(~ marital+y, data=bank))
chisq.test(xtabs(~bank[,3]+y, data=bank))
# p-value < 2.2e-16 : 유의수준 0.01 하에서 <goodbad와 marital은 독립이다 라는 귀무가설>을 기각한다. 
# goodbad와 결혼상태는 독립이 아니다. 관계가 있다.
# 우량인지 불량인지에 따라서 결혼상태의 차이가 있다. 



###############
#4. education
#4.1 education (factor)

xtabs(~education+y, data=bank)
prop.table(xtabs(~ education+y, data=bank))
chisq.test(xtabs(~education+y, data=bank))
fisher.test(xtabs(~education+y, data=bank))


##############
#자동화

names(bank)
bank_factor<- bank[,c(2,3,4,5,6,7,8,9,10,15,21)]
names(bank_factor)
ncol(bank_factor)


for (k in 1:(ncol(bank_factor)-1)){
  print('------------')
  print(k)
  print(names(bank_factor[k]))
  print(chisq.test(xtabs(~bank_factor[,k]+y, data=bank_factor)))
}


bank_num <- bank[,c(1,11,12,13,14,16,17,18,19,20,21)]
str(bank_num)
for (k in 1:(ncol(bank_num)-1)){
  print('------------')
  print(k)
  print(names(bank_num[k]))
  print(var.test(bank_num[,k]~y, bank_num))
  print(t.test(bank_num[,k]~y, data=bank_num, paired=FALSE, var.equal=FALSE))
}


#######################
library(dplyr)
#german 자동ㅎ
gbank<-read.table("german.data.txt", h=F, sep="")
colnames(gbank) <- c("chk_ac_status_1",
                     "duration_month_2", "credit_history_3", "purpose_4",
                     "credit_amount_5","savings_ac_bond_6","p_employment_since_7", 
                     "instalment_pct_8", "personal_status_9","other_debtors_or_grantors_10", 
                     "present_residence_since_11","property_type_12","age_in_yrs_13",
                     "other_instalment_type_14", "housing_type_15", 
                     "number_cards_this_bank_16","job_17","no_people_liable_for_mntnance_18",
                     "telephone_19", "foreign_worker_20", 
                     "good_bad_21")

bank_factor<- gbank %>% select(which(sapply(.,class)=='factor'))
bank_factor <- cbind(bank_factor, gbank$good_bad_21)
names(bank_factor)
ncol(bank_factor)
str(gbank)

for (k in 1:(ncol(bank_factor)-1)){
  print('------------')
  print(k)
  print(names(bank_factor[k]))
  print(chisq.test(xtabs(~bank_factor[,k]+gbank$good_bad_21, data=bank_factor)))
}
#job(17), tel(19), 

bank_num<- gbank %>% select(which(sapply(.,class)!='factor'))
names(bank_num)


str(bank_num)
for (k in 1:(ncol(bank_num)-1)){
  print('------------')
  print(k)
  print(names(bank_num[k]))
  print(var.test(bank_num[,k]~good_bad_21, bank_num))
  print(t.test(bank_num[,k]~good_bad_21, data=bank_num, paired=FALSE, var.equal=TRUE))
}
# present residence(11), numof card(16),no_people(18)_




######################
#상관분석
library(ggplot2)
install.packages("GGally")
library(GGally)

str(reduced_gbank)

pairs(reduced_gbank)

reduced_gbank<- gbank[,-c(11,16,17,18,19)]
reduced_gbank$good_bad_21 <- as.factor(reduced_gbank$good_bad_21)
ggpairs(reduced_gbank,aes(color=good_bad_21))



#############
m1<- glm(good_bad_21 ~. , data= reduced_gbank, family= 'binomial')
step(m1)
summary(m1)



















