install.packages("")
library(ggplot2)
library(gridExtra)

setwd("C:/Users/HS/Documents/GitHub/analsying-bank-data")

bank<-read.csv("bank-additional-full.csv", header = TRUE)
head(bank)
names(bank)


summary(bank)
str(bank)



table(bank$job)
table(bank$marital)
table(bank$education)
table(bank$default)
table(bank$housing)
table(bank$loan)
table(bank$contact)
table(bank$month)
table(bank$day_of_week)
table(bank$duration)
table(bank$campaign)
table(bank$poutcome)
table(bank$y)
4640/(365848+4640)


table(bank$y)

p0<- ggplot(bank)+geom_bar(aes(age), stat="count")+
  ggtitle("age") +
  xlab("age")
p1<- ggplot(bank)+geom_bar(aes(job), stat="count")+
  ggtitle("job") +
  xlab("job") + coord_flip()
p2<- ggplot(bank)+geom_bar(aes(marital), stat="count")+
  ggtitle("marital") +
  xlab("marital")
p3<- ggplot(bank)+geom_bar(aes(education), stat="count")+
  ggtitle("education") +
  xlab("education") +coord_flip()
p4<- ggplot(bank)+geom_bar(aes(default), stat="count")+
  ggtitle("default") +
  xlab("default")
p5<- ggplot(bank)+geom_bar(aes(housing), stat="count")+
  ggtitle("housing") +
  xlab("housing")
p6<- ggplot(bank)+geom_bar(aes(loan), stat="count")+
  ggtitle("loan") +
  xlab("loan")



p0
grid.arrange(p1,p2,p3,p4, ncol=2)
p5
p6


a<- bank[,c(16:20)]
a
cor(bank)
cor(a)
plot(a)


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


#################
#logistic

#full model
logi_model <- glm(y ~ . ,data=train, family='binomial')
summary(logi_model)
anova(logi_model, test ='Chisq')
#stepwise
logi_step <- step(object = logi_model, trace=F)




logi_model_pred <- predict.lm(object = logi_model,newdata= test, type ='response')
logi_model_pred <- predict(logi_model)

logi_step_pred <- predict(object = logi_step, newdata= test, type = 'response')
N



##############
#tree


setwd("C:/Users/HS/Documents/GitHub/analsying-bank-data")
bank<-read.csv("bank-additional-full.csv", header = TRUE)


#데이터 준비
set.seed(123)
tr_idx <- sample(nrow(bank) , nrow(bank)*0.9)
str(tr_idx)

#x
train <- bank[tr_idx,]
test <- bank[-tr_idx,]
train_label<- bank[tr_idx, 22]
test_label<- bank[-tr_idx, 22]


#균등하게 분할됐음


#모델 훈련
install.packages("tree")
library(tree)

names(bank)
tree_model <- tree(y~. , data=train)
plot(tree_model)
text(tree_model)

#가지치기, cross`validation`
cv.trees <-cv.tree(tree_model, FUN=prune.misclass)
plot(cv.trees)
#3개의 가지가 있는 의사결정 나무가 가장 분산이 낮다

#3개의 가지 모형 

prune.trees <- prune.misclass(tree_model, best=3)
plot(prune.trees)
text(prune.trees)

mtcars

#예측 평가
install.packages('e1071')
library(e1071)
treepred <- predict(prune.trees, test, type='factor')
confusionMatrix(treepred