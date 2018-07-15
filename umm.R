###
#bank eda
library(ggplot2)
library(MASS) 
#################
#카이스퀘어
tbl <- table(bank$job, bank$y)
tbl
chisq.test(tbl)
#p-value < 2.2e-16  
# job별로 y 차이가 유의미함

#age
quantile(bank$age)
bank$ageage<-ifelse( bank$age <= 25, "youth", 
                     ifelse(bank$age <=50, "middel", "old"))
table(bank$ageage)

#y ~ job anova
bank$job <- as.factor(bank$job)
bank$y<- as.factor(bank$y)
table(bank$job)
table(bank$y)
class(bank$job)
class(bank$y)
aov.job <- aov(y ~ job, data=bank)
summary(aov.job)
# Pr(>f) <2e-16 ***
# job 별로 y 유의미하게 차이가 나는가?!

# job pca
#job 을 유형화를 해야하나?
bank$job <- as.numeric(bank$job)
job.pca <-princomp(bank$job)
job.pca
job.pca.predict <-predict()
job.pca.result<-cbind


##################
#anova 
#### bank client data:
bank$y<- as.numeric(bank$y)
bank1<-bank[,c(2:7,21,23)]
names(bank1)
aov.bank1 <- aov(y ~ ., data=bank1)
summary(aov.bank1)

#### related with the last contact of the current campaign:
bank2<-bank[,c(8:10,21,23)]
names(bank2)
aov.bank2 <- aov(y~., data=bank2)
summary(aov.bank2)
table(bank$duration)
class(bank$duration)
bank$durdur <- ifelse(bank$duration <=102 ,"short",
                      ifelse(bank$duration >= 319, "long", "middle"))
quantile(bank$duration)
names(bank)


########
#logistic
bank$y <-as.numeric(bank$y)
table(bank$y)
glm.fit<- glm(y~ .,data=bank,family="binomial")


#
pair(bank)
plot(bank)
nrow(bank)
