
setwd("C:/Users/HS/Documents/GitHub/analsying-bank-data")

bank<-read.csv("bank-additional-full.csv", header = TRUE)
set.seed(1802)
names(bank)
bank.glm <- glm(y ~ ., 
                data   = bank,
                family = "binomial")
summary(bank.glm)


glm.mari<- glm(y ~ marital, data= bank, family ="binomial")
summary(glm.mari)


#모형을 생성함에 있어서 유의한 변수들에는 어떤 것들이 있는지와 특히 유의한 변수들은 무엇이 있는지 확인 해보는 과정입니다.
#total.sulfur.dioxide, volatile.acidity , sulphates, alcohol 
y_obs<- bank$y
yhat_glm <- predict(bank.glm)
binomial_deviance(y_obs, yhat_glm)



###########3

library(MASS)
library(ISLR)
library(py)



#PCA
bank.pca <-princomp(bank, scale=TRUE)
bank.pca.predict <-predict()
bank.pca.result<-cbind



#####
#data set 
#traing, test set 나누기
#75% 
sam_size <- floor(0.75 * nrow(bank))

#set the see dto make your partition 
set.seed(1023)
idx <- sample(seq_len(nrow(bank), size = sam_size))

train <- bank[idx, ]
test <- bank[-idx, ]

#################
library(dplyr)

bank$yy <- ifelse(bank$y =='yes', '1','0')
table(bank$yy)

bank$yy <- as.factor(bank$yy)

######################
names(bank)

#데이터 준비
set.seed(123)
tr_idx <- sample(nrow(bank) , nrow(bank)*0.9)
str(tr_idx)
class(bank$yy)
table(bank$y)

train <- bank[tr_idx, ]
test <- bank[-tr_idx, ]

#
#x
train <- bank[tr_idx,-c(21)]
test <- bank[-tr_idx,-c(21)]
train_label<- bank[tr_idx, 1]
test_label<- bank[-tr_idx, 1]


#################
#logistic

#full model
logi_model <- glm(yy ~ . ,data=train, family='binomial')
summary(logi_model)
anova(logi_model, test ='Chisq')
#stepwise
logi_step <- step(object = logi_model, trace=F)




logi_model_pred <- predict(object = logi_model,newdata= test, type ='response')
logi_step_pred <- predict(object = logi_step, newdata= test, type = 'response')


##################




