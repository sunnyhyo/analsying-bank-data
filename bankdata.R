install.packages("")
library(ggplot2)
library(gridExtra)

setwd("C:/Users/HS/Desktop/bank marketing")

bank<-read.csv("bank-additional-full.csv", header = TRUE)
head(bank)


summary(bank)


table(bank$default)

table(bank$y)
names(bank)

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



