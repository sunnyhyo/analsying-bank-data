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
train_bank <- sample(seq_len(nrow(bank), size = sam_size))

train <- bank[train_bank, ]
test <- bank[-train_bank, ]

